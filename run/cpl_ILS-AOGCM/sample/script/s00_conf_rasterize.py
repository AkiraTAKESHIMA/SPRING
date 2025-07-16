import os
import sys


sys.path.append('../../../common')
import const, util

import s00_const as lconst


def block_raster(rst):
    s = '\
\n\
[raster]\n\
  nx: {nx}\n\
  ny: {ny}\n\
  west: {west}\n\
  east: {east}\n\
  south: {south}\n\
  north: {north}\n\
  is_south_to_north: {is_south_to_north}\n\
[end]\n\
'.format(**rst)

    return s


def block_output(step):
    s = f'\
\n\
[output]\n\
  include_min: false\n\
  ratio_min: 0.999999d0\n\
\n\
  dir: "{lconst.dir_tmp[step]}"\n\
  f_area_sum : "area.bin"\n\
  f_ratio_sum: "ratio.bin"\n\
  f_mask     : "mask.bin", int1\n\
[end]\n'

    return s                                                                                            
