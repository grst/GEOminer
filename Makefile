GPL570-GDS-total:GDS_GPL570_Dec2014.txt
	grep Platform GDS_GPL570_Dec2014.txt  | cut -d " " -f 5 | awk '{a+=$$0}END{print a}'

## GDS/GSE total samples (almost : excluding some mixed platforms)
GPL570-GDS-GSE-total:GDS_GSE_GPL570_Dec2014.txt
	grep "GPL570 [[:digit:]]* Samples" GDS_GSE_GPL570_Dec2014.txt  | sed 's/.*GPL570 //g' | sed 's/ Samples//g' | grep -e ^[[:digit:]]*$ | awk '{a+=$$0}END{print a}'

GDS-GPL570-list.txt:GDS_GPL570_Dec2014.txt
	grep "^DataSet.*GDS" GDS_GPL570_Dec2014.txt | sed 's/.*GDS/GDS/g' | cut -f 1 > GDS-GPL570-list.txt

GDS-list.txt:GDS_Dec2014.txt
	grep "^DataSet.*GDS" GDS_Dec2014.txt | sed 's/.*GDS/GDS/g' | cut -f 1 > GDS-list.txt

download-GDS-list.bash:GDS-list.txt
	awk '{print "Rscript download-GDS-commandLine.R -id " $$0 "> download-" $$0 ".log 2> download-" $$0 ".err"}' GDS-list.txt > download-GDS-list.bash
	chmod +x download-GDS-list.bash

download:download-GDS.R
	Rscript download-GDS.R > download-GDS.log

parse:parse-GDS.R
	Rscript parse-GDS.R > parse-GDS.log 2> parse-GDS.err

## restart jobs as individual threads
prepareMorningRun:
	awk '{if(NR>=412) print "Rscript parse-GDS.R -id ",$$0,"> parse-GDS-" $$0 ".log 2> parse-GDS-" $$0 ".err"}' GDS-GPL570-list.txt > morningRun
	chmod +x morningRun

doMorningRun:morningRun
	./morningRun
