library(Biobase) 

args = commandArgs(trailingOnly = TRUE)
esetFile = args[1]

load(esetFile)
writeLines(paste(basename(esetFile), paste(colnames(pData(eset)), collapse="\t"), sep="\t"))
