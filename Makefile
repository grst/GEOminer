GDS-total:GDS_GPL570_Dec2014.txt
	grep Platform GDS_GPL570_Dec2014.txt  | cut -d " " -f 5 | awk '{a+=$$0}END{print a}'

## GDS/GSE total samples (almost : excluding some mixed platforms)
GDS-GSE-total:GDS_GSE_GPL570_Dec2014.txt
	grep "GPL570 [[:digit:]]* Samples" GDS_GSE_GPL570_Dec2014.txt  | sed 's/.*GPL570 //g' | sed 's/ Samples//g' | grep -e ^[[:digit:]]*$ | awk '{a+=$$0}END{print a}'

GDS-GPL570-list.txt:GDS_GPL570_Dec2014.txt
	grep "^DataSet.*GDS" GDS_GPL570_Dec2014.txt | sed 's/.*GDS/GDS/g' | cut -f 1 > GDS-GPL570-list.txt

download:download-GDS.R
	Rscript download-GDS.R > download-GDS.log
