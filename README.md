# GEOminer
batch download of studies from the [Gene Expression Omnibus](https://www.ncbi.nlm.nih.gov/geo/) (GEO)

## Usage
### Download using accession number
```
Rscript geo_to_eset.R <ACC_NO> <output_dir> 
```
Downloads the GEO Dataset (GDS) or GEO Series (GSE) and converts it to a `biobase` ExpressionSet. 
The ExpressionSet will be stored in a Rdata object with the variable name `eset`. 
The Rdata-object will be stored in the file `<output_dir>/<ACC_NO>.Rdata`. 

### Batch download
In the folder `geo_ids` there is a collection of accession numbers. You can easily compile a similar/more current list from the GEO website.

Use xargs to download multiple studies:
```
xargs -I '{}' "Rscript geo_to_eset.R {} <output_dir>" < list_of_geo_ids
```

Alternatively, use e.g. [chunksub](https://github.com/grst/chunksub) to run the download on a High-Performance Cluster (HPC)
