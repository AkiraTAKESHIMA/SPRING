import os
import sys
import subprocess
import json

import s00_util as util
import s00_conf as conf


def run():
    cnf = json.load(open('test.json','r'))
    util.adjust_settings(cnf)

    f_conf = os.path.join(cnf['directory']['set']['01'],'define_mat.conf')
    os.makedirs(os.path.dirname(f_conf), exist_ok=True)

    print(f_conf)
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
'.format(**cnf['cmf']))

    fp.write('\
\n\
[cama-flood]\n\
  fin_nextxy: "{path}", {dtype}, 1, {endian}\n\
'.format(**cnf['cmf']['nextxy'])\
+'\
  fin_catmxy: "{path}", {dtype}, 1, {endian}\n\
'.format(**cnf['cmf']['catmxy'])\
+'\
  nextxy_river_mouth  : {river_mouth}\n\
  nextxy_river_inland : {river_inland}\n\
  nextxy_ocean        : {ocean}\n\
'.format(**cnf['cmf']['nextxy']['index'])\
+'\
  catmxy_noriv_coastal: {noriv_coastal}\n\
  catmxy_noriv_inland : {noriv_inland}\n\
  catmxy_ocean        : {ocean}\n\
'.format(**cnf['cmf']['catmxy']['index'])\
+'\
\n\
  dir: "{dir_out}/cmf"\n\
  fout_grdidx_river: "grid/index_river.bin"\n\
  fout_grdidx_noriv: "grid/index_noriv.bin"\n\
  fout_rstidx_river: "raster/index_river.bin"\n\
  fout_rstidx_noriv: "raster/index_noriv.bin"\n\
'.format(dir_out=cnf['directory']['tmp']['01'])\
+'\
\n\
  idx_miss: -9999\n\
[end]\n\
')

    fp.write('\
\n\
[matsiro]\n\
  dir: "{dir_out}/matsiro"\n\
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
'.format(dir_out=cnf['directory']['tmp']['01']))

    fp.write(conf.block_options(cnf['options']['earth']))

    fp.close()



if __name__ == '__main__':
    run()
