import os
import sys
import subprocess
import json

import s00_conf_makeGridData as makeGridData
import s00_conf_remap as remap
import s00_conf_rasterize as rasterize


def head(path_report):
    s = f'\
#\n\
path_report: "{path_report}"\n\
'

    return s


