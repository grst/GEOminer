## download GDS
source("GEO-utils.R")

gdsFile <- "GDS-GPL570-list.txt"

gdsIds <- readLines(gdsFile)

ngds <- length(gdsIds)
for(i in seq(along=gdsIds)) {
    gdsId <- gdsIds[i]
    cat(sprintf("Download %s (%d/%d)\n", gdsId, i, ngds))
    downloadGDS(gdsId)
}
## con <- dbConnect(PostgreSQL(), user="david", password="PragHamburg2012",dbname="gds")
## importGDS(test, con)
## dbDisconnect(con)


