#!Rscript

#########
# USAGE Rscript geo_to_eset.Rscript <GDS507> </path/to/outdir>
# 
# Download a GEO Dataset (GDS) from GEO by Accession Number, 
# convert it to a biobase ExpressionSet and save it to
# an R object. The variable storing the ExpressionSet will
# be named 'eset'
#########

library(GEOquery)

args = commandArgs(trailingOnly = TRUE)
geo_id = args[1]
outdir = args[2]
outfile = paste(outdir, "/", geo_id, ".Rdata", sep="")
print(paste("GEO ID: ", geo_id))
print(paste("OUTFILE: ", outfile))

gds = getGEO(geo_id)
eset = GDS2eSet(gds)

print(paste("Storing expression set to", outfile))
save(eset, file=outfile)



