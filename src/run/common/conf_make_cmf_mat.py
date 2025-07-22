import sys
import subprocess
import json

import const, util
from conf_make_grid_data import block_options


def block_common(cmf):
    s = '\
#\n\
[common]\n\
  nx_raster: {nx_raster}\n\
  ny_raster: {ny_raster}\n\
  nx_grid: {nx_grid}\n\
  ny_grid: {ny_grid}\n\
  west: {west}\n\
  east: {east}\n\
  south: {south}\n\
  north: {north}\n\
[end]\n'\
.format(**cmf)

    return s


def block_cmf(cmf, dir_in, dir_out):
    s = f'\
\n\
[cama-flood]\n\
  dir: "{dir_in}"\n\
  fin_catmxy: {util.str_file_bin(cmf["fin_catmxy"])}\n\
  fin_nextxy: {util.str_file_bin(cmf["fin_nextxy"])}\n'
    if util.key_val_exist(cmf, 'fin_basin'):
        s += f'\
  fin_basin: {util.str_file_bin(cmf["fin_basin"])}\n'

    s += f'\
  catmxy_noriv_coastal: {cmf["catmxy_index"]["noriv_coastal"]}\n\
  catmxy_noriv_inland : {cmf["catmxy_index"]["noriv_inland"]}\n\
  catmxy_ocean        : {cmf["catmxy_index"]["ocean"]}\n\
  nextxy_river_mouth  : {cmf["nextxy_index"]["river_mouth"]}\n\
  nextxy_river_inland : {cmf["nextxy_index"]["river_inland"]}\n\
  nextxy_ocean        : {cmf["nextxy_index"]["ocean"]}\n\
  dir: "{dir_out}"\n'

    for gridName in ['rst', 'grd']:
        varName  = 'idx'
        for landType in ['river', 'river_end', 'noriv', 'ocean']:
            key = f'fout_{gridName}{varName}_{landType}'
            if util.key_val_exist(cmf, key):
                s += f'\
  {key}: {util.str_file_bin(cmf[key])}\n'

        varName = 'bsn' 
        key = f'fout_{gridName}{varName}'
        if util.key_val_exist(cmf, key):
                s += f'\
  {key}: {util.str_file_bin(cmf[key])}\n'

    s += f'\
  idx_miss: {cmf["idx_miss"]}\n'

    if 'grdidx_condition' in cmf.keys():
        s += f'\
  grdidx_condition: {cmf["grdidx_condition"]}\n\
[end]\n\
'

    return s


def block_matsiro(mat, dir_out):
    s = f'\
\n\
[matsiro]\n\
  dir: "{dir_out}"\n'

    for gridName in ['rst', 'grd']:
        for varName in ['msk', 'idx', 'idx_bnd', 'idx_mkbnd']:
            for landType in ['river', 'noriv']:
                key = f'fout_{gridName}{varName}_{landType}'
                if key in mat.keys():
                    s += f'\
  {key}: {util.str_file_bin(mat[key])}\n'

    s += f'\
  idx_miss: {mat["idx_miss"]}\n\
[end]\n'

    return s

