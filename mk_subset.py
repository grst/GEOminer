#!/bin/env python3

"""
USAGE:
    mk_subset.py path_to_esets id_list.txt

GEO Series can come with multiple ExpressionSets. We store them as
GSE???_1.Rdata GSE???_2.Rdata, ...

This script generates a list of all files that belong to the
identifiers provided in id_list.

"""

import sys
import itertools
import os

root_path = sys.argv[1]
id_list = sys.argv[2]

with open(id_list) as f:
    for gse_id in f.readlines():
        gse_id = gse_id.strip()
        for i, _ in enumerate(itertools.repeat(True), start=1):
            current_file = os.path.join(root_path, "{}_{}.Rdata".format(gse_id, i))
            if os.path.isfile(current_file):
                print(current_file)
            else:
                break

