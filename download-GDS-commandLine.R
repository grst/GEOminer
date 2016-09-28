#!/usr/bin/Rscript
source("/home/david/Dokumente/programming/GEO/GEO-utils.R")

DEFAULT_OUTDIR <- "/home/david/Dokumente/programming/GEO/liver/liver"
id <- argGet("-id", default=NULL)
if(is.null(id))
    stop("option -id not found")
outdir <- argGet("-outdir",
                 default=DEFAULT_OUTDIR)
downloadGDS(id, destdir=outdir)


## test
## (n100.100.time <- system.time(testCommitMatrix(con, nrow=100, ncol=100)))
## (n22000.5.time <- system.time(testCommitMatrix(con, nrow=22000, ncol=5)))
## (n22000.20.time <- system.time(testCommitMatrix(con, nrow=22000, ncol=20)))
## (n44000.20.time <- system.time(testCommitMatrix(con, nrow=44000, ncol=20)))
