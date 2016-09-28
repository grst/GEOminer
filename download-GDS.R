## download GDS
source("GEO-utils.R")

OUTDIR <- "GDS-GPL570"

gdsFile <- "GDS-GPL570-list.txt"

gdsIds <- readLines(gdsFile)

for(gdsId in gdsIds)
    downloadGDS(gdsId)
## con <- dbConnect(PostgreSQL(), user="david", password="PragHamburg2012",dbname="gds")
## importGDS(test, con)
## dbDisconnect(con)


