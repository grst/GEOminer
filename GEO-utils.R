library(RPostgreSQL)
library(GEOquery)
library(limma)
OUTDIR <- "/media/ydserverGEO/GDS-GPL570/"

setClass("AnnoGDS", representation(id="character",
                                   gds="GDS",
                                   matrix="matrix"))
AnnoGDS <- function(id, gds) {
    new("AnnoGDS", id=id, gds=gds)
}
gdsID <- function(annoGDS) annoGDS@id
gds <- function(annoGDS) annoGDS@gds


downloadGDS <- function(id) {
    gds <- getGEOfile(id, destdir=OUTDIR, AnnotGPL=TRUE)
}
downloadParseGDS <- function(id) {
    gds <- getGEO(id, destdir=OUTDIR, AnnotGPL=TRUE)
    res <- AnnoGDS(id, gds)
    return(res)
}
getNextID <- function(con, table, idColumn="ID") {
    sql <- sprintf("SELECT MAX(%s) FROM %s",idColumn, table)
    rs <- dbSendQuery(con, sql)
    id <- fetch(rs, n=1)[1,1]
    if(is.na(id))
        id <- 0
    return(id+1)
}
getNextMatrixID <- function(con) {
    getNextID(con, "gds_matrix")
 
}
getNextRowID <- function(con) {
    getNextID(con, "gds_matrix_row")
}
getNextColumnID <- function(con) {
    getNextID(con, "gds_matrix_column")
}
getNextDataID <- function(con) {
    getNextID(con, "gds_matrix_data")
}
isGDSimported <- function(con, annoGDS) {
    sql <- sprintf("SELECT COUNT(ID) FROM gds_matrix WHERE gdsID='%s'", gdsID(annoGDS))
    rs <- dbSendQuery(con, sql)
    res <- fetch(rs, n=1)[1,1]
    if(res==1) {
        return(TRUE)
    } else if (res==0) {
        return(FALSE)
    } else {
        stop("Should not happen!")
    }
    
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

pcaNormScores <- function(pres) {
    scores <- pres$x
    lam <- pres$sdev
    n <- NROW(scores)
    lam <- lam * sqrt(n)
    res <- t(t(scores)/lam)
    return(res)
}
fillMatrix <- function(annoGDS) {
    annoGDS@matrix <- matrix()
    id <- gdsID(annoGDS)
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
        if(ncol(mat)<=3) {
            warning("GDS has no more than 3 samples. Ignored")
            return(annoGDS)
        } else {
            matv <- unlist(mat)
            nmat <- matrix(as.numeric(matv), nrow=nrow(mat), byrow=FALSE, dimnames=dimnames(mat))
            rownames(nmat) <- as.character(idref)
            annoGDS@matrix <- autoLog(nmat)
            return(annoGDS)
        }
    }
}

fillPCA <- function(annoGDS, varThr=0.8) {
    mat <- annoGDS@matrix
    isValid <- apply(mat, 1, function(x) all(!is.na(x) & !is.nan(x)))
    vmat <- mat[isValid,,drop=FALSE]
    pca <- prcomp(t(vmat))
    vars <- pca$sdev^2
    expvars <- vars/sum(vars)
    cumexpvars <- cumsum(expvars)
    dimThr <- which(cumexpvars>=varThr)[1]
    scores <- pca$x
    normScores <- pcaNormScores(pca)
    design.mat <- cbind(1, normScores[, 1:dimThr])
    colnames(design.mat)[1] <- "baseline"
    contrast.mat <- makeContrasts(contrasts=colnames(design.mat)[-1], levels=colnames(design.mat))
    fit <- lmFit(vmat, design=design.mat)
    fit <- contrasts.fit(fit, contrast.mat)
    efit <- eBayes(fit)
    topTables <- lapply(1:ncol(contrast.mat), function(i)
                        topTable(efit, coef=i, number=nrow(mat)))
    browser()
}

importGDS <- function(annoGDS, con) {
    if(isGDSimported(con, annoGDS)) {
        message("%s is already imported: SQL procedure is skipped")
    }
    id <- gdsID(annoGDS)
    table <- Table(dataTable(gds(annoGDS)))
    cnames <- colnames(table)
    if(cnames[1]!="ID_REF" && cnames[2]!="IDENTIFIER") {
        warning("GDS table with inconsistent format found: ID=%s, first two coumns: %s, %s",
                id, cnames[1], cnames[2])
        return(NULL)
    } else {
        idref <- table[,1L]
        identifier <- table[,2L]
        mat <- table[,3:ncol(table),drop=FALSE]

        matid <- getNextMatrixID(con)
        rowid <- getNextRowID(con)
        colid <- getNextColumnID(con)
        dataid <- getNextDataID(con)
        begin <- "BEGIN TRANSACTION;"
        insertMatrix <- sprintf("INSERT INTO gds_matrix VALUES (%d, '%s', 'GPL570', %d, %d);",
                                matid, id, nrow(mat), ncol(mat))
        insertRows <- sprintf("INSERT INTO gds_matrix_row VALUES (%d, %d, %d, '%s', '%s');",
                             rowid:(rowid+nrow(mat)-1),
                             matid,
                             1:nrow(mat),
                             as.character(idref), as.character(identifier))

        insertCols <- sprintf("INSERT INTO gds_matrix_column VALUES (%d, %d, %d, '%s');",
                             colid:(colid+ncol(mat)-1),
                             matid,
                             1:ncol(mat),
                             colnames(mat))
        insertData <- paste("INSERT INTO gds_matrix_data VALUES (",
                            dataid:(dataid+nrow(mat)*ncol(mat)-1),
                            ",",
                            matid,
                            ",",
                            rep(1:nrow(mat), ncol(mat)),
                            ",",
                            rep(1:ncol(mat), each=nrow(mat)),
                            ",",
                            unlist(mat),
                            ");")
        inserts <- paste(c(begin, insertMatrix, insertRows, insertCols, insertData), collapse="\n")
        dbSendQuery(con, inserts)
        dbCommit(con)
    }
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
        cat("PASS: commit")
    } else {
        cat("FAIL: commit")
    }
    dbSendQuery(con, "DROP TABLE tmp")
}
