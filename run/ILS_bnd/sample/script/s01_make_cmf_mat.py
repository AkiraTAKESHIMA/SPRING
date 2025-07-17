import os
import sys
import subprocess
import json

sys.path.append('../../../common')
import const, util

import s00_const as lconst
import s00_util as lutil


def run():
    cnf = json.load(open(lconst.f_cnf, 'r'))
    lutil.adjust_settings(cnf)

    f_conf = f'{lconst.dir_set[step]}/a.conf'

    os.makedirs(lconst.dir_set[step], exist_ok=True)
    os.makedirs(lconst.dir_tmp[step], exist_ok=True)
    os.makedirs(lconst.dir_log[step], exist_ok=True)

    print(f'config: {f_conf}')
    fp = open(f_conf, 'w')

    fp.write('\
#\n\
[common]\n\
  nx_grid: {ncx}\n\
  ny_grid: {ncy}\n\
  nx_raster: {ndx}\n\
  ny_raster: {ndy}\n\
  west: {west}\n\
  east: {east}\n\
  south: {south}\n\
  north: {north}\n\
[end]\n\
'.format(**cnf['CaMa-Flood']))

    fp.write(f'\
\n\
[cama-flood]\n\
  dir: "{cnf["CaMa-Flood"]["dir"]}"\n\
  fin_nextxy: {util.str_file_bin(cnf["CaMa-Flood"]["f_nextxy"])}\n\
  fin_catmxy: {util.str_file_bin(cnf["CaMa-Flood"]["f_catmxy"])}\n\
'+'\
  nextxy_river_mouth  : {river_mouth}\n\
  nextxy_river_inland : {river_inland}\n\
  nextxy_ocean        : {ocean}\n\
'.format(**cnf['CaMa-Flood']['nextxy_index'])\
+'\
  catmxy_noriv_coastal: {noriv_coastal}\n\
  catmxy_noriv_inland : {noriv_inland}\n\
  catmxy_ocean        : {ocean}\n\
'.format(**cnf['CaMa-Flood']['catmxy_index'])\
+ f'\
\n\
  dir: "{lconst.dir_tmp[1]}/CaMa-Flood"\n\
  fout_grdidx_river: "grid/index_river.bin"\n\
  fout_grdidx_noriv: "grid/index_noriv.bin"\n\
  fout_rstidx_river: "raster/index_river.bin"\n\
  fout_rstidx_noriv: "raster/index_noriv.bin"\n\
  idx_miss: -9999\n\
[end]\n\
')

    fp.write(f'\
\n\
[matsiro]\n\
  dir: "{lconst.dir_tmp[1]}/MATSIRO"\n\
  fout_grdmsk_river: "grid/land_mask_river.bin", real, endian=big\n\
  fout_grdmsk_noriv: "grid/land_mask_noriv.bin", real, endian=big\n\
\n\
  fout_grdidx_river: "grid/index_river.bin"\n\
  fout_grdidx_noriv: "grid/index_noriv.bin"\n\
  fout_rstidx_river: "raster/index_river.bin"\n\
  fout_rstidx_noriv: "raster/index_noriv.bin"\n\
\n\
  fout_grdidx_bnd_river: "grid/index_bnd_river.bin"\n\
  fout_grdidx_bnd_noriv: "grid/index_bnd_noriv.bin"\n\
\n\
  fout_grdidx_mkbnd_river: "grid/index_mkbnd_river.bin"\n\
  fout_grdidx_mkbnd_noriv: "grid/index_mkbnd_noriv.bin"\n\
  fout_rstidx_mkbnd_river: "raster/index_mkbnd_river.bin"\n\
  fout_rstidx_mkbnd_noriv: "raster/index_mkbnd_noriv.bin"\n\
\n\
  idx_miss: -9999\n\
[end]\n\
')

    fp.write(lutil.block_options(cnf['options']['earth']))

    fp.close()

    f_log = f'{lconst.dir_log[step]}/a.out'
    f_err = f'{lconst.dir_log[step]}/a.err'
    util.exec_program(const.prog_make_cmf_mat, f_conf, f_log, f_err)

    util.make_slink(f'{lconst.dir_tmp[step]}/CaMa-Flood', f'{const.dir_out}/grid/CaMa-Flood')
    util.make_slink(f'{lconst.dir_tmp[step]}/MATSIRO', f'{const.dir_out}/grid/MATSIRO')


if __name__ == '__main__':
    step = int(sys.argv[0][1:3])

    run()
