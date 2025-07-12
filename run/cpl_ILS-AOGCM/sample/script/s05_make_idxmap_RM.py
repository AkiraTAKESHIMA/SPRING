import os
import sys
import subprocess
import json

import s00_const as const
import s00_util as util
import s00_conf as conf


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
  dir: "{const.directory["tmp"][util.istep("run_FLOW")]["self"]}/map"\n\
  fin_catmxy: "1min/catmxy.bin"\n\
  fin_nextxy: "nextxy.bin"\n\
  fin_basin: "basin.bin"\n\
\n\
  dir: "{const.directory["tmp"][step]["self"]}"\n\
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
    step = 5

    cnf = json.load(open('test.json','r'))
    util.adjust_config(cnf)

    f_conf, f_report = util.get_f_conf(step)

    print(f_conf)
    fp = open(f_conf,'w')
    fp.write(conf.head(f_report))
    fp.write(block_common(cnf['RM']))
    fp.write(block_cmf())
    fp.write(conf.remap.block_options(cnf['options']))
    fp.close()
