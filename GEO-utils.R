library(RPostgreSQL)
library(GEOquery)
library(limma)
OUTDIR <- "./GDS-GPL570/"

myStamp <- function() timestamp(prefix="[", suffix="]", quiet=TRUE)

setClass("AnnoGDS", representation(id="character",
                                   gds="GDS",
                                   matrix="matrix",
                                   rowDesc="character",
                                   matrixID="integer",
                                   rowIDs="integer",
                                   colIDs="integer",
                                   pca="data.frame",
                                   pcaScore="matrix",
                                   pcaDesignMatrixID="integer",
                                   pcaContrastMatrixID="integer"))
AnnoGDS <- function(id, gds) {
    new("AnnoGDS", id=id, gds=gds)
}
gdsID <- function(annoGDS) annoGDS@id
gds <- function(annoGDS) annoGDS@gds


downloadGDS <- function(id, destdir=OUTDIR) {
    gds <- getGEOfile(id, destdir=destdir, AnnotGPL=TRUE)
}


autoLog <- function(matrix) {
    qx <- as.numeric(quantile(matrix, c(0., 0.25, 0.5, 0.75, 0.99, 1.0), na.rm=T))
    LogC <- (qx[5] > 100) ||
        (qx[6]-qx[1] > 50 && qx[2] > 0) ||
            (qx[2] > 0 && qx[2] < 1 && qx[4] > 1 && qx[4] < 2)
    if (LogC) {
        matrix[which(matrix <= 0)] <- NaN
        matrix <- log2(matrix)
    } else {
        return(matrix)
    }
}

doSelect <- function(con, sql, n=-1) {
    rs <- dbSendQuery(con, sql)
    res <- fetch(rs, n=n)
    dbClearResult(rs)
    return(res)
}

parseGdsMatrix <- function(annoGDS) {
    table <- Table(dataTable(gds(annoGDS)))
    cnames <- colnames(table)
    if(cnames[1]!="ID_REF" && cnames[2]!="IDENTIFIER") {
        warning("GDS table with inconsistent format found: ID=%s, first two coumns: %s, %s",
                id, cnames[1], cnames[2])
        return(annoGDS)
    } else {
        idref <- table[,1L]
        identifier <- table[,2L]
        mat <- table[,3:ncol(table),drop=FALSE]
        matv <- unlist(mat)
        nmat <- matrix(as.numeric(matv), nrow=nrow(mat), byrow=FALSE, dimnames=dimnames(mat))
        rownames(nmat) <- as.character(idref)
        lmat <- autoLog(nmat)
        isValid <- apply(lmat, 1, function(x) all(!is.na(x) & !is.nan(x)))
        resmat <- lmat[isValid,,drop=FALSE]
        resRowDesc <- as.character(identifier[isValid])
        stopifnot(nrow(resmat)==length(resRowDesc))
        annoGDS@matrix <- resmat
        annoGDS@rowDesc <- resRowDesc
        return(annoGDS)
    }
}

downloadParseGDS <- function(id, destdir=OUTDIR) {
    gds <- getGEO(id, destdir=destdir, AnnotGPL=TRUE)
    res <- AnnoGDS(id, gds)
    res <- parseGdsMatrix(res)
    return(res)
}
getNextID <- function(con, table, idColumn="ID") {
    sql <- sprintf("SELECT MAX(%s) FROM %s",idColumn, table)
    rs <- dbSendQuery(con, sql)
    id <- fetch(rs, n=1)[1,1]
    dbClearResult(rs)
    if(is.na(id))
        id <- 0
    return(id+1)
}
getNextMatrixID <- function(con) {
    getNextID(con, "numeric_matrix")
 
}
getNextRowID <- function(con) {
    getNextID(con, "numeric_matrix_row")
}
getNextColumnID <- function(con) {
    getNextID(con, "numeric_matrix_column")
}
getNextDataID <- function(con) {
    getNextID(con, "numeric_matrix_data")
}
getNextPCAID <- function(con) {
    getNextID(con, "numeric_matrix_pca")
}
getNextLimmaDegID <- function(con) {
    getNextID(con, "limma_deg")
}

getMatrixID <- function(con, annoGDS) {
    sql <- sprintf("SELECT matrix_id FROM gds_matrix WHERE gdsID='%s'", gdsID(annoGDS))
    res <- doSelect(con, sql, n=1)[1,1]
    return(res)
}
    
hasGDS <- function(con, annoGDS) {
    matrix_id <- getMatrixID(con, annoGDS)
    if(is.null(matrix_id)) {
        return(FALSE)
    } else {
        return(TRUE)
    }
    
}
getPCAscore <- function(con, annoGDS) {
    pcaScoreID <- getPCAScoreMatrixID(con, annoGDS)
    retrieveMatrix(con, pcaScoreID)
}

getPCAScoreMatrixID <- function(con, annoGDS) {
    id <- gdsID(annoGDS)
    sql <- paste("SELECT DISTINCT(d.matrix_id) FROM gds_matrix g JOIN numeric_matrix_pca p ON g.matrix_id=p.matrix_id JOIN numeric_matrix_pca_score s ON p.id=s.pca_id JOIN numeric_matrix_data d ON s.numeric_matrix_data_id = d.id WHERE g.gdsID='",
                 id, "'", sep="")
    res <- doSelect(con, sql)
    if(nrow(res)==0) {
        return(NA)
    } else if(nrow(res)>1) {
        stop("Cannot happen: getPCAScoreMatrixID error")
    }
    return(res[1,1])
}
getLimmaDegID <- function(con, annoGDS) { ## slow
    id <- gdsID(annoGDS)
    sql <- paste("SELECT d.id FROM gds_matrix g JOIN numeric_matrix_row r ON g.matrix_id=r.matrix_id",
                 " JOIN limma_deg d ON r.id=d.exprs_row_id",
                 " WHERE g.gdsID='",
                 id, "'", sep="")
    res <- doSelect(con, sql)
    if(nrow(res)==0) {
        return(NA)
    } 
    return(res[,1])
}
hasLimmaDeg <- function(con, annoGDS) {
    id <- gdsID(annoGDS)
    sql <- paste("SELECT COUNT(1) FROM gds_matrix g JOIN numeric_matrix_row r ON g.matrix_id=r.matrix_id",
                 " JOIN limma_deg d ON r.id=d.exprs_row_id",
                 " WHERE g.gdsID='",
                 id, "'", sep="")
    return(sqlCountLt0(con, sql))
}
getExpVarMinDim <- function(con, annoGDS, thr=0.80) {
    pcaScore <- getPCAscore(con, annoGDS)
    if(is.null(pcaScore))
        return(NULL)
    id <- gdsID(annoGDS)
    sql <- paste("SELECT MIN(pca_dimension) FROM gds_matrix g JOIN numeric_matrix_pca p ON g.matrix_id=p.matrix_id WHERE g.gdsID='",
                 id, "' AND cumulative_exp_var_perc>=", thr, sep="")
    res <- doSelect(con, sql)[1,1]
    return(res)
}
importPCAdesignContrast <- function(con, annoGDS, thr=0.80) {
    pcaScore <- getPCAscore(con, annoGDS)
    minDim <- getExpVarMinDim(con, annoGDS, thr=thr)
    if(minDim>=ncol(pcaScore)-1) {
        minDim <- ncol(pcaScore)-2 ## must have freedom left
    }
    des <- cbind(baseline=1, pcaScore[,1:minDim, drop=FALSE])
    contrasts <- makeContrasts(contrasts=colnames(des)[-1], levels=colnames(des))
    desinfo <- importMatrix(con, des, importData=TRUE, matrixName="PCA design", matrixDesc=sprintf("thr=%f", thr))
    annoGDS@pcaDesignMatrixID <- as.integer(desinfo$matrix_id)
    continfo <- importMatrix(con, contrasts, importData=TRUE, matrixName="PCA design contrast", matrixDesc=sprintf("thr=%f", thr))
    annoGDS@pcaContrastMatrixID <- as.integer(continfo$matrix_id)
    return(annoGDS)
}
getPCADesignMatrix <- function(con, annoGDS) {
    retrieveMatrix(con, annoGDS@pcaDesignMatrixID)
}
getPCAContrastMatrix <- function(con, annoGDS) {
    retrieveMatrix(con, annoGDS@pcaContrastMatrixID)
}
importLimma <- function(con, annoGDS, verbose=TRUE) {
    if(hasLimmaDeg(con, annoGDS)) {
        if(verbose)
            cat("Limma for GDS ", gdsID(annoGDS), " has been run. Skip\n")
        return(annoGDS)
    } else {
        if(verbose)
            cat("Run limma\n")
        ## todo: for now matrix is read from the object
        mat <- annoGDS@matrix
        design <- getPCADesignMatrix(con, annoGDS)
        contrast <- getPCAContrastMatrix(con, annoGDS)
        contrastColIDs <- getColIDs(con, annoGDS@pcaContrastMatrixID)
        fit <- lmFit(mat, design=design)
        fit <- contrasts.fit(fit, contrast)
        efit <- eBayes(fit)
        for(i in 1:ncol(contrast)) {
            if(verbose)
                cat(sprintf("  %s Contrast %d/%d\n", myStamp(), i, ncol(contrast)))
            tt <- topTable(efit, coef=i, number=nrow(mat))
            limmaID <- getNextLimmaDegID(con)
            ids <- limmaID:(limmaID+nrow(tt)-1)
            contrast_col_id <- contrastColIDs[i]
            exprs_row_id_ind<- match(rownames(tt), rownames(mat))
            exprs_row_id <- annoGDS@rowIDs[exprs_row_id_ind]
            df <- data.frame(id=ids,
                             contrast_col_id=contrast_col_id,
                             exprs_row_id=exprs_row_id,
                             logFC=tt$logFC,
                             AveExpr=tt$AveExpr,
                             t=tt$t,
                             p=tt$P.Value,
                             adjP=tt$adj.P.Val,
                             B=tt$B)
            dbWriteTable(con, name="limma_deg", value=df, overwrite=FALSE, append=TRUE, row.names=FALSE)
            if(verbose)
                cat("  Contrast ", i, "/", ncol(contrast), " finished\n")
        }
    }
}

hasPCA <- function(con, annoGDS) {
    pcaScoreMatrixID <- getPCAScoreMatrixID(con, annoGDS)
    if(length(pcaScoreMatrixID)==1 & !is.na(pcaScoreMatrixID)) {
        return(TRUE)
    } else {
        return(FALSE)
    } 
}
pcaNormScores <- function(pres) {
    scores <- pres$x
    lam <- pres$sdev
    n <- NROW(scores)
    lam <- lam * sqrt(n)
    res <- t(t(scores)/lam)
    return(res)
}

importMatrix <- function(con, matrix, importData=FALSE, rowDesc=NULL, colDesc=NULL, matrixName=NULL, matrixDesc=NULL) {
    matid <- getNextMatrixID(con)
    rowid <- getNextRowID(con)
    colid <- getNextColumnID(con)

    mname <- opt2str(matrixName)
    mdesc <- opt2str(matrixDesc)

    begin <- "BEGIN TRANSACTION;"
    insertMatrix <- sprintf("INSERT INTO numeric_matrix VALUES (%d, %d, %d, %s, %s);",
                            matid, nrow(matrix), ncol(matrix), mname, mdesc)
    inserts <- paste(c(begin, insertMatrix), collapse="\n")
    res <- tryCatch(dbSendQuery(con, inserts), error=function(x) {
        dbRollback(con)
        warning("Rollback")
        return(NULL)
    })
    dbCommit(con)
    
    rnames <- nnRownames(matrix)
    if(is.null(rowDesc)) {
        rowDesc <- rep("NULL", nrow(matrix))
    } else if (length(rowDesc)!=nrow(matrix)) {
        stop("length(rowDesc) must equal nrow(matrix)")
    }
    rowids <- rowid:(rowid+nrow(matrix)-1)
    rowdf <- data.frame(id=rowids,
                        matrix_id=matid,
                        ind=1:nrow(matrix),
                        name=rnames,
                        desc=rowDesc)
    dbWriteTable(con, name="numeric_matrix_row", value=rowdf, overwrite=FALSE, append=TRUE, row.names=FALSE)
    
    cnames <- nnColnames(matrix)
    if(is.null(colDesc)) {
        colDesc <- rep("NULL", ncol(matrix))
    } else if (length(colDesc)!=ncol(matrix)) {
        stop("length(colDesc) must equal ncol(matrix)")
    }
    colids <- colid:(colid+ncol(matrix)-1)
    coldf <- data.frame(id=colids,
                        matrix_id=matid,
                        ind=1:ncol(matrix),
                        name=cnames,
                        desc=colDesc)
    dbWriteTable(con, name="numeric_matrix_column", value=coldf, overwrite=FALSE, append=TRUE, row.names=FALSE)

    ## data
    dataids <- NULL
    if(importData) {
        dataid <- getNextDataID(con)
        dataids <- dataid:(dataid+nrow(matrix)*ncol(matrix)-1)
        df <- data.frame(ID=dataids,
                         matrix_id=matid,
                         row_id=rep(rowids, ncol(matrix)),
                         col_id=rep(colids, each=nrow(matrix)),
                         value=as.vector(matrix))
        dbWriteTable(con, name="numeric_matrix_data", value=df, overwrite=FALSE, append=TRUE, row.names=FALSE)
    }
    
    return(list(matrix_id=as.integer(matid), row_id=as.integer(rowids), col_id=as.integer(colids), data_id=as.integer(dataids)))
}
retrieveMatrix <- function(con, matrix_id) {
    if(!hasMatrix(con, matrix_id))
        stop("matrix with id", matrix_id,"not does exist")
    dimSQL <- paste("SELECT nrow, ncol FROM numeric_matrix WHERE id=", matrix_id, sep="")
    rs <- dbSendQuery(con, dimSQL)
    dims <- fetch(rs, n=-1)
    nRow <- dims[1,1]
    nCol <- dims[1,2]
    mat <- rep(NA, length=nRow*nCol)
    dbClearResult(rs)
    sql <- paste("SELECT r.ind AS ROWIND, c.ind AS COLIND, d.value FROM numeric_matrix_data d JOIN numeric_matrix_row r ON d.row_id=r.id JOIN numeric_matrix_column c on d.col_id=c.id WHERE d.matrix_id=", matrix_id, sep="")
    rs <- dbSendQuery(con, sql)
    dmat <- fetch(rs, n=-1)
    dbClearResult(rs)
    pos <- dmat$rowind+(dmat$colind -1)* nRow
    mat[pos] <- dmat[,"value"]
    mat <- matrix(mat, nrow=nRow, ncol=nCol)

    rsql <- paste("SELECT name AS rowname FROM numeric_matrix_row WHERE matrix_id=", matrix_id, " ORDER BY ind", sep="")
    rnames <- doSelect(con, rsql)[,"rowname"]
    csql <- paste("SELECT name AS colname FROM numeric_matrix_column WHERE matrix_id=", matrix_id, " ORDER BY ind", sep="")
    cnames <- doSelect(con, csql)[,"colname"]

    rownames(mat) <- rnames
    colnames(mat) <- cnames
    return(mat)
}
getRowIDs <- function(con, matrixID) {
    sql <- paste("SELECT id FROM numeric_matrix_row WHERE matrix_id=", matrixID, " ORDER BY ind", sep="")
    res <- doSelect(con, sql)[,1L]
    return(res)
}
getColIDs <- function(con, matrixID) {
    sql <- paste("SELECT id FROM numeric_matrix_column WHERE matrix_id=", matrixID, " ORDER BY ind", sep="")
    res <- doSelect(con, sql)[,1L]
    return(res)
}
importAnnoGDSMatrix <- function(con, annoGDS, importData=FALSE) {
    if(!hasGDS(con, annoGDS)) {
        res <- importMatrix(con, annoGDS@matrix, importData=importData, rowDesc=annoGDS@rowDesc)
        annoGDS@matrixID <- as.integer(res$matrix_id)
        annoGDS@rowIDs <- as.integer(res$row_id)
        annoGDS@colIDs <- as.integer(res$col_id)
        sql <- sprintf("INSERT INTO gds_matrix VALUES (%d, '%s', '%s')",
                       annoGDS@matrixID, gdsID(annoGDS), "GPL570")
        dbSendQuery(con, sql)
    } else {
        cat("GDS ", gdsID(annoGDS), " has been imported - read from database\n")
        annoGDS@matrixID <- as.integer(getMatrixID(con, annoGDS))
        annoGDS@rowIDs <- as.integer(getRowIDs(con, annoGDS@matrixID))
        annoGDS@colIDs <- as.integer(getColIDs(con, annoGDS@matrixID))
    }

    return(annoGDS)
}
fillPCA <- function(annoGDS) {
    vmat <- annoGDS@matrix
    pca <- prcomp(t(vmat))
    vars <- pca$sdev^2
    expvars <- vars/sum(vars)
    cumexpvars <- cumsum(expvars)
    scores <- pca$x
    normScores <- pcaNormScores(pca)
    pcadf <- data.frame(matrix_id=annoGDS@matrixID,
                        pca_dimension=seq(along=vars),
                        exp_var=vars,
                        exp_var_perc=expvars,
                        cumulative_exp_var_perc = cumexpvars)
    annoGDS@pca <- pcadf
    annoGDS@pcaScore <- normScores
    return(annoGDS)
##    design.mat <- cbind(1, normScores[, 1:dimThr])
##    colnames(design.mat)[1] <- "baseline"
##    contrast.mat <- makeContrasts(contrasts=colnames(design.mat)[-1], levels=colnames(design.mat))
##    fit <- lmFit(vmat, design=design.mat)
##    fit <- contrasts.fit(fit, contrast.mat)
##    efit <- eBayes(fit)
##    topTables <- lapply(1:ncol(contrast.mat), function(i)
##                        topTable(efit, coef=i, number=nrow(mat)))
##    browser()
}

importAnnoGDSPCA <- function(con, annoGDS) {
    if(hasPCA(con, annoGDS)) {
        cat("PCA of GDS ", gdsID(annoGDS), " has been imported - read from database\n")
        annoGDS@pcaScore <- getPCAscore(con, annoGDS)
    } else {
        pcaID <- getNextPCAID(con)
        pca <- annoGDS@pca
        pcaScore <- annoGDS@pcaScore
        pcaIDs <- pcaID:(pcaID+ncol(pcaScore)-1)
        pcaSqls <- paste("INSERT INTO numeric_matrix_pca VALUES(",
                         pcaIDs,",",
                         pca$matrix_id, ",",
                         pca$pca_dimension, ",",
                         pca$exp_var, ",",
                         pca$exp_var_perc, ",",
                         pca$cumulative_exp_var_perc, ");")
        pcaScoreMatrixImport <- importMatrix(con, annoGDS@pcaScore, importData=TRUE)
        pcaValSqls <- paste("INSERT INTO numeric_matrix_pca_score VALUES (",
                            pcaScoreMatrixImport$data_id,",",
                            rep(annoGDS@colIDs, ncol(pcaScore)), ",",
                            rep(pcaIDs, each=nrow(annoGDS@pcaScore)), ");")
        
        
        sqls <- paste(c(pcaSqls, pcaValSqls), collapse="\n")
        dbSendQuery(con, "BEGIN TRANSACTION;")
        res <- tryCatch(dbSendQuery(con, sqls), error=function(x) {
            dbRollback(con)
            warning("Rollback, ", x)
            return(NULL)
        })
        
        dbCommit(con)
    }
    return(annoGDS)
}

opt2str <- function(x)   ifelse(is.null(x), "NULL", paste("'", x, "'", sep=""))
nnRownames <- function(x) {
    rn <- rownames(x)
    if(is.null(rn))
        rn <- as.character(1:nrow(x))
    return(rn)
}
nnColnames <- function(x) {
    cn <- colnames(x)
    if(is.null(cn))
        cn <- as.character(1:ncol(x))
    return(cn)
}
importMatrix <- function(con, matrix, importData=FALSE, rowDesc=NULL, colDesc=NULL, matrixName=NULL, matrixDesc=NULL) {
    matid <- getNextMatrixID(con)
    rowid <- getNextRowID(con)
    colid <- getNextColumnID(con)

    mname <- opt2str(matrixName)
    mdesc <- opt2str(matrixDesc)
    begin <- "BEGIN TRANSACTION;"
    insertMatrix <- sprintf("INSERT INTO numeric_matrix VALUES (%d, %d, %d, %s, %s);",
                            matid, nrow(matrix), ncol(matrix), mname, mdesc)

    rnames <- nnRownames(matrix)
    if(is.null(rowDesc)) {
        rowDesc <- rep("NULL", nrow(matrix))
    } else if (length(rowDesc)!=nrow(matrix)) {
        stop("length(rowDesc) must equal nrow(matrix)")
    }
    rowids <- rowid:(rowid+nrow(matrix)-1)
    insertRows <- sprintf("INSERT INTO numeric_matrix_row VALUES (%d, %d, %d, '%s', '%s');",
                          rowids,
                          matid,
                          1:nrow(matrix),
                          rnames, rowDesc)
    
    cnames <- nnColnames(matrix)
    if(is.null(colDesc)) {
        colDesc <- rep("NULL", ncol(matrix))
    } else if (length(colDesc)!=ncol(matrix)) {
        stop("length(colDesc) must equal ncol(matrix)")
    }
    colids <- colid:(colid+ncol(matrix)-1)
    insertCols <- sprintf("INSERT INTO numeric_matrix_column VALUES (%d, %d, %d, '%s', '%s');",
                          colids,
                          matid,
                          1:ncol(matrix),
                          cnames, colDesc)
    if(importData) {
        dataid <- getNextDataID(con)
        dataids <- dataid:(dataid+nrow(matrix)*ncol(matrix)-1)
        insertData <- paste("INSERT INTO numeric_matrix_data VALUES (",
                            dataids,
                            ",",
                            matid,
                            ",",
                            rep(rowids,ncol(matrix)),
                            ",",
                            rep(colids,each=nrow(matrix)),
                            ",",
                            unlist(matrix),
                            ");")
    } else {
        dataids <- NULL
        insertData <- ""
    }
    inserts <- paste(c(begin, insertMatrix, insertRows, insertCols, insertData), collapse="\n")
    res <- tryCatch(dbSendQuery(con, inserts), error=function(x) {
        dbRollback(con)
        warning("Rollback")
        return(NULL)
    })
    dbCommit(con)
    return(list(matrix_id=matid, row_id=rowids, col_id=colids, data_id=dataids))
}
listMatrix <- function(con) {
    sql <- "SELECT ID FROM numeric_matrix"
    rs <- dbSendQuery(con, sql)
    res <- fetch(rs, -1)
    dbClearResult(rs)
    if(nrow(res)>0) {
        return(res[,1L])
    } else {
        return(NULL)
    }
}
sqlCountEquals1 <- function(con, sql) {
    identical(as.integer(doSelect(con, sql)[1,1]), 1L)
}
sqlCountLt0 <- function(con, sql) {
    return(doSelect(con, sql)[1,1]>0)
}
hasMatrix <- function(con, id) {
    stopifnot(!missing(id) & id!="")
    sql <- paste("SELECT COUNT(1) FROM numeric_matrix WHERE ID=", id, sep="")
    res <- as.integer(doSelect(con, sql)[1,1])
    return(identical(res, 1L))
}
deleteMatrix <- function(con, id) {
    if(hasMatrix(con, id)) {
        dbSendQuery(con, "BEGIN TRANSACTION;")
        dbSendQuery(con, paste("DELETE FROM numeric_matrix WHERE ID=", id, ";"))
        dbCommit(con)
    } else {
        warning("matrix with id ", id, " does not exist")
        return(FALSE)
    }
}
deleteAllMatrix <- function(con) {
    matrix <- listMatrix(con)
    sapply(matrix, function(x) deleteMatrix(con, x))
    return(matrix)
}
testCommit <- function(con) {
    fs <- dbSendQuery(con, "CREATE TABLE tmp (value integer)")
    dbSendQuery(con, "BEGIN TRANSACTION;")
    dbSendQuery(con, "INSERT INTO tmp VALUES (3);")
    dbSendQuery(con, "INSERT INTO tmp VALUES (4);")
    dbSendQuery(con, "COMMIT");
    fs <- dbSendQuery(con, "SELECT * FROM tmp")
    res <- fetch(fs, n=-1)
    if(identical(res, data.frame(value=c(3L,4L)))) {
        cat("PASS: commit\n")
    } else {
        cat("FAIL: commit\n")
    }
    dbClearResult(fs)
    dbSendQuery(con, "DROP TABLE tmp")
}

identicalMatrix <- function(mat1, mat2) {
    mat1Rnames <- nnRownames(mat1)
    mat1Cnames <- nnColnames(mat1)
    mat2Rnames <- nnRownames(mat2)
    mat2Cnames <- nnColnames(mat2)
    rIden <- identical(mat1Rnames, mat2Rnames)
    cIden <- identical(mat1Cnames, mat2Cnames)
    vIden <- all(abs(mat1-mat2)<1E-12)
    return(rIden && cIden && vIden)
}

processGDS <- function(con, id) {
    gdsObj <- downloadParseGDS(id)
    cat(sprintf("  %s Importing data\n", myStamp()))
    gdsObj <- importAnnoGDSMatrix(con, gdsObj, importData=FALSE)
    cat(sprintf("  %s Dimension: %d/%d\n", myStamp(), nrow(gdsObj@matrix), ncol(gdsObj@matrix)))
    cat(sprintf("  %s PCA\n", myStamp()))
    gdsObj <- fillPCA(gdsObj)
    gdsObj <- importAnnoGDSPCA(con, gdsObj)
    gdsObj <- importPCAdesignContrast(con, gdsObj)
    gdsDesign <- getPCADesignMatrix(con, gdsObj)
    gdsContrast <- getPCAContrastMatrix(con, gdsObj)
    cat(sprintf("  %s Design matrix dimension: %d/%d\n", myStamp(), nrow(gdsDesign), ncol(gdsDesign)))
    cat(sprintf("  %s Contrast matrix dimension: %d/%d\n", myStamp(), nrow(gdsContrast), ncol(gdsContrast)))
    cat(sprintf("  %s Limma\n", myStamp(), id))
    gdsObj <- importLimma(con, gdsObj)
    return(gdsObj)
}

testCommitMatrix <- function(con, nrow=100, ncol=100) {
    set.seed(1887)
    mat <- matrix(rnorm(nrow*ncol), nrow=nrow)
    matinfo <- importMatrix(con, mat, importData=TRUE, matrixName="test", matrixDesc="test committing matrix")
    tryCatch( {
        matID <- matinfo$matrix_id
        omat <- retrieveMatrix(con, matID)
        iden <- identicalMatrix(mat, omat)
        if(iden) {
            message("PASS: testCommitMatrix")
        } else {
            message("FAIL: testCommitMatrix")
        }
    }, finally=function(x) deleteMatrix(con, matID))
    return(invisible())
}

argGet <- function(opt="-id", default=NULL) {
    args <- commandArgs()
    optInd <- which(args==opt)
    if(length(optInd)!=1 || is.na(optInd))
        return(default)
    return(args[optInd+1])
}
