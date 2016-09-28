#!Rscript

###
# USAGE geo_to_eset.Rscript GDS507 /path/to/outdir
library(GEOquery)

args = commandArgs(trailingOnly = TRUE)
geo_id = args[1]
outdir = args[2]
outfile = paste(outdir, "/", geo_id, ".Rdata", sep="")

gds = getGEO(geo_id)
eset = GDS2eSet(gds)

print(paste("Storing expression set to", outfile))
save(eset, file=outfile)



