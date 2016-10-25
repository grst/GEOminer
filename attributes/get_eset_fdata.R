#!/bin/env Rscript 

library(tools)
library(Biobase)

args = commandArgs(trailingOnly = TRUE)
esetFile = args[1]

geoId = file_path_sans_ext(basename(esetFile))

load(esetFile)
writeLines(paste(c(geoId, colnames(fData(eset))), collapse='\t'), stdout())
