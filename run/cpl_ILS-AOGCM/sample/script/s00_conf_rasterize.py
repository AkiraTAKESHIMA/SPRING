import os
import sys

import s00_const as const


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
  dir: "{const.directory["tmp"][step]["self"]}"\n\
  f_area_sum : "area.bin"\n\
  f_ratio_sum: "ratio.bin"\n\
  f_mask     : "mask.bin", int1\n\
[end]\n'

    return s                                                                                            
