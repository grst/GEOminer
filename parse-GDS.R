source("GEO-utils.R")

argGetID <- function(opt="-id") {
    args <- commandArgs()
    optInd <- which(args==opt)
    if(length(optInd)!=1 || is.na(optInd))
        stop("-id not found")
    return(args[optInd+1])
}

## gdsFile <- "GDS-GPL570-list.txt"

## gdsIds <- readLines(gdsFile)


##gdsMatrix <- fillMatrix(test)
##gdsPCA <- fillPCA(gdsMatrix)

## nids <- length(gdsIds)
con <- dbConnect(PostgreSQL(), user="david", password="PragHamburg2012",
                 dbname="gdsfull")
## deleteAllMatrix(con)

## test <- downloadParseGDS(gdsIds[1])


## for(i in seq(along=gdsIds)) {
##     id <- gdsIds[i]
##    cat(sprintf("%s Parsing %s (%d/%d)\n", myStamp(), id, i, nids))
id <- argGetID()
cat(sprintf("%s Parsing %s \n", myStamp(), id))
try(processGDS(con, id))                          
##}
dbDisconnect(con)


## test
## (n100.100.time <- system.time(testCommitMatrix(con, nrow=100, ncol=100)))
## (n22000.5.time <- system.time(testCommitMatrix(con, nrow=22000, ncol=5)))
## (n22000.20.time <- system.time(testCommitMatrix(con, nrow=22000, ncol=20)))
## (n44000.20.time <- system.time(testCommitMatrix(con, nrow=44000, ncol=20)))
