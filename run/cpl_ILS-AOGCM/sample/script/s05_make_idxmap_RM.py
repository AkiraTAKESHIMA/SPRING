import os
import sys
import subprocess
import json

sys.path.append('../../../common')
import const, util

import s00_const as lconst
import s00_util as lutil
import s00_conf as lconf


def block_common(RM):
    s = f'\
\n\
[common]\n\
  nx_grid: {RM["ncx"]}\n\
  ny_grid: {RM["ncy"]}\n\
  nx_raster: {RM["nx"]}\n\
  ny_raster: {RM["ny"]}\n\
[end]\n'

    return s

def block_cmf():
    s = f'\
\n\
[cama-flood]\n\
  dir: "{lconst.dir_tmp[lutil.istep("run_FLOW")]}"\n\
  fin_catmxy: "tmp/map/1min/catmxy.bin"\n\
  fin_nextxy: "map/nextxy.bin"\n\
  fin_basin : "map/basin.bin"\n\
\n\
  dir: "{lconst.dir_tmp[step]}"\n\
  fout_grdidx_river    : "grdidx_river.bin"\n\
  fout_grdidx_river_end: "grdidx_river-end.bin"\n\
  fout_grdidx_noriv    : "grdidx_noriv.bin"\n\
  fout_grdidx_ocean    : "grdidx_ocean.bin"\n\
  fout_rstidx_river    : "rstidx_river.bin"\n\
  fout_rstidx_river_end: "rstidx_river-end.bin"\n\
  fout_rstidx_noriv    : "rstidx_noriv.bin"\n\
  fout_rstidx_ocean    : "rstidx_ocean.bin"\n\
  fout_rstbsn          : "rstbsn.bin"\n\
\n\
  catmxy_noriv_coastal: 0\n\
  catmxy_noriv_inland : -1\n\
  catmxy_ocean        : -9999\n\
  nextxy_river_mouth  : -9\n\
  nextxy_river_inland : -10\n\
  nextxy_ocean        : -9999\n\
  idx_miss: -9999\n\
[end]\n'

    return s


if __name__ == '__main__':
    step = int(sys.argv[0][1:3])

    cnf = json.load(open(lconst.f_cnf,'r'))
    lutil.adjust_config(cnf)

    os.makedirs(lconst.dir_set[step], exist_ok=True)
    os.makedirs(lconst.dir_tmp[step], exist_ok=True)
    os.makedirs(lconst.dir_log[step], exist_ok=True)
    os.makedirs(lconst.dir_out[step], exist_ok=True)

    f_conf = f'{lconst.dir_tmp[step]}/a.conf'

    print(f_conf)
    fp = open(f_conf, 'w')
    fp.write(lconf.head(lconst.dir_tmp[step]))
    fp.write(block_common(cnf['RM']))
    fp.write(block_cmf())
    fp.write(lconf.remap.block_options(cnf['options']))
    fp.close()

    f_log = f'{lconst.dir_log[step]}/a.out'
    f_err = f'{lconst.dir_log[step]}/a.err'
    util.exec_program(const.prog_make_cmf_mat, f_conf, f_log, f_err)
