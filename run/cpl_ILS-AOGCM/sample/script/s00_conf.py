import os
import sys
import subprocess
import json

import s00_conf_makeGridData as makeGridData
import s00_conf_remap as remap
import s00_conf_rasterize as rasterize


def head(dir_out):
    s = f'\
#\n\
path_report: "{dir_out}/report.txt"\n\
'

    return s


