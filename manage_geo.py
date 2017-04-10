#!/bin/env python3

"""Manage the local GEO installation.

Usage:
  manage_geo --help
  manage_geo put FILE GSE GPL SUFFIX [--force] [--move]
  manage_geo get ID [SUFFIX]
  manage_geo get_list LIST_OF_ID [SUFFIX]

The subcommands explained:
  put       put a file into the directory structure
  get       get a file from the directory structure by identifier
  get_list  get a list of files from the directory structure by a list of identifiers

Arguments:
  FILE              a file to deposit
  GSE               GSE identifier, e.g. 'GSE12345'
  GPL               GPL identifier, e.g. 'GPL1250'
  ID                either a GSE or GPL identifier
  SUFFIX            the file type, e.g. 'rdata', 'exprs', 'pdata', 'fdata'. It is part
                    of the filename, e.g. GSE12345-GPL1250_exprs.gct
  LIST_OF_ID        a list of ID's as a file, one ID per line. This can be either GSE???? or GPL???? or GSE???-GPL???.
  --force           overwrite file if it already exists.
  --move            move file to destination, default is copy.

"""


from docopt import docopt
import os
from config import config
import shutil
import re
import sys


RE_GSE = re.compile(r'GSE\d+')
RE_GPL = re.compile(r'GPL\d+')


def get_series_path(gse):
    """
    Return a path to a series which mimicks the directory structure on the `GEO ftp server`_.

    Args:
        gse:

    Returns:


    .. _GEO ftp server: ftp://ftp.ncbi.nlm.nih.gov/geo/series

    >>> get_series_path('GSE1')
    'series/GSEnnn/GSE1'
    >>> get_series_path('GSE123')
    'series/GSEnnn/GSE123'
    >>> get_series_path('GSE2345')
    'series/GSE2nnn/GSE2345'
    >>> get_series_path('GSE12345')
    'series/GSE12nnn/GSE12345'
    >>> get_series_path('GSE12345-GPL1261')
    'series/GSE12nnn/GSE12345'
    """
    assert gse[:3] == 'GSE', "invalid GSE identifier"
    gse = gse.split('-')[0] if '-' in gse else gse
    gse_nnn = gse[3:]
    gse_nnn = 'GSE' + gse_nnn[-5:-3] + 'nnn'
    return os.path.join("series", gse_nnn, gse)


def get_filename(gse, gpl, suffix, extension):
    """

    Args:
        gse:
        gpl:
        suffix:
        extension:

    Returns:
        str: the filename

    >>> get_filename('GSE12345', 'GPL1250', 'rdata', 'Rdata')
    'GSE12345-GPL1250_rdata.Rdata'
    >>> get_filename('GSE12345', 'GPL1250', 'rdata', '.Rdata')
    'GSE12345-GPL1250_rdata.Rdata'
    """
    assert gse[:3] == 'GSE', 'invalid GSE'
    assert gpl[:3] == 'GPL', 'invalid GPL'
    if extension.startswith('.'):
        extension = extension[1:]
    return "{}-{}_{}.{}".format(gse, gpl, suffix, extension)


def get_ids_from_filename(filename):
    """

    Args:
        filename:

    Returns:
        (str, str): gse and gpl identifier

    >>> get_ids_from_filename('/foo/bar/GSE134-GPL222_suffix.ext')
    ('GSE134', 'GPL222')
    >>> get_ids_from_filename('does not contain an id')
    (None, None)
    >>> get_ids_from_filename('GSE1-GSE2-GSE3.Rdata')
    ('GSE1', None)

    """
    basename = os.path.basename(filename)
    gse_m = RE_GSE.search(basename)
    gpl_m = RE_GPL.search(basename)
    return gse_m.group(0) if gse_m is not None else None, gpl_m.group(0) if gpl_m is not None else None


def put(arguments):
    in_file = os.path.realpath(arguments['FILE'])
    gse = arguments['GSE']
    gpl = arguments['GPL']
    suffix = arguments['SUFFIX']
    extension = os.path.splitext(arguments['FILE'])[1]
    series_path = os.path.join(config['root_dir'], get_series_path(gse))
    dest_filename = get_filename(gse, gpl, suffix, extension)
    dest_file = os.path.join(series_path, dest_filename)

    assert os.path.isfile(in_file), "Input file is not a file. "
    assert extension, "Invalid extension. "
    if os.path.exists(dest_file) and not arguments['--force']:
        raise FileExistsError('The destination file already exists and --force is not set: {}'.format(dest_file))

    os.makedirs(series_path, exist_ok=True)
    if arguments['--move']:
        sys.stderr.write('Move {} to {}'.format(in_file, dest_file))
        shutil.move(in_file, dest_file)
        sys.stderr.write('    .. DONE\n')
    else:
        sys.stderr.write('Copy {} to {}'.format(in_file, dest_file))
        shutil.copyfile(in_file, dest_file)
        sys.stderr.write('    .. DONE\n')


def has_suffix(file, suffix):
    return suffix is None or os.path.splitext(file)[0].endswith('_' + suffix)


def get(arguments):
    series_dir = os.path.join(config['root_dir'], 'series')
    id = arguments['ID']
    suffix = arguments['SUFFIX']
    if id.startswith('GSE'):
        series_path = os.path.join(config['root_dir'], get_series_path(id))
        for file in os.listdir(series_path):
            if os.path.isfile(os.path.join(series_path, file)) and has_suffix(file, suffix):
                sys.stdout.write(os.path.join(series_path, file) + '\n')

    elif id.startswith('GPL'):
        for root, dirs, files in os.walk(series_dir):
            for file in files:
                gse, gpl = get_ids_from_filename(file)
                if gpl == id and has_suffix(file, suffix):
                    sys.stdout.write(os.path.join(root, file) + '\n')
    else:
        raise Exception("Identifier not supported. ")


def get_list(arguments):
    suffix = arguments["SUFFIX"]
    with open(arguments['LIST_OF_ID']) as f:
        # strip comments from id file.
        id_list = [l.strip() for l in f.readlines() if not l.startswith('#')]
    series_dir = os.path.join(config['root_dir'], 'series')
    for root, dirs, files in os.walk(series_dir):
        for file in files:
            gse, gpl = get_ids_from_filename(file)
            if (gse in id_list or gpl in id_list or '{}-{}'.format(gse, gpl) in id_list) and has_suffix(file, suffix):
                sys.stdout.write(os.path.join(root, file) + '\n')


def main():
    arguments = docopt(__doc__, version='manage_geo 0.1.0')

    # print(arguments)

    if arguments['put']:
        put(arguments)

    elif arguments['get']:
        get(arguments)

    elif arguments['get_list']:
        get_list(arguments)


if __name__ == '__main__':
    main()

