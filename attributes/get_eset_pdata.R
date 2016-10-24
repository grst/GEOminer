library(Biobase) 
library(tools)

args = commandArgs(trailingOnly = TRUE)
esetFile = args[1]
outPath = args[2]
geoId = file_path_sans_ext(basename(esetFile))
print(geoId)

outFile = file.path(outPath, paste(geoId, "_pdata.tsv", sep=""))

load(esetFile)
print(paste("writing to", outFile))
write.table(pData(eset), file=outFile, sep='\t')
