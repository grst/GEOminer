#!/bin/bash

######
# Extract the accession number from a GEO text download
##

grep "Accession: " "$1" | grep -o 'GSE[0-9]\+'
