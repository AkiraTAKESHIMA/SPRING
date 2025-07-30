import os
import sys
import subprocess
import copy
import json

import const, util, conf
from const import k_lt, k_gs, k_rt, k_opt
from util import istep, file_bin

import s00_const as lconst
import s00_util as lutil


def make_cmf_mat(cnf, step, update_data):
    dir_tmp = lconst.dir_tmp[step]

    RM_pre = cnf[k_gs]['RM_pre']
    for landType in ['river', 'river_end', 'noriv', 'ocean']:
        RM_pre[f'fout_rstidx_{landType}'] = file_bin(f'{dir_tmp}/{landType}/rstidx.bin','int4')
        RM_pre[f'fout_grdidx_{landType}'] = file_bin(f'{dir_tmp}/{landType}/grdidx.bin','int4')
    RM_pre['fout_rstbsn'] = file_bin(f'{dir_tmp}/river/rstbsn.bin','int4')

    f_conf = f'{lconst.dir_set[step]}/a.conf'
    print('config: '+f_conf)
    fp = open(f_conf, 'w')
    fp.write(conf.make_cmf_mat.block_common(RM_pre))
    fp.write(conf.make_cmf_mat.block_cmf(RM_pre, RM_pre['dir'], ''))
    fp.write(conf.make_cmf_mat.block_options(cnf[k_opt]))
    fp.close()

    if update_data:
        f_log = f'{lconst.dir_log[step]}/a.out'
        f_err = f'{lconst.dir_log[step]}/a.err'
        util.exec_program(const.prog_make_cmf_mat, f_conf, f_log, f_err)

    RM_cmn = {
      'type': 'raster',
      'nx_raster': RM_pre['nx_raster'],
      'ny_raster': RM_pre['ny_raster'],
      'nx_grid': RM_pre['nx_grid'],
      'ny_grid': RM_pre['ny_grid'],
      'west' : RM_pre['west'], 
      'east' : RM_pre['east'], 
      'south': RM_pre['south'], 
      'north': RM_pre['north'], 
      'is_south_to_north': RM_pre['is_south_to_north'], 
      'idx_miss': RM_pre['idx_miss'],
      'dir': RM_pre['dir'],
    }
    RM_simple_cmn = {
      'type': 'latlon',
      'nx': RM_pre['nx_grid'],
      'ny': RM_pre['ny_grid'],
      'nij': RM_pre['nx_grid']*RM_pre['ny_grid'],
      'west' : RM_pre['west'], 
      'east' : RM_pre['east'], 
      'south': RM_pre['south'], 
      'north': RM_pre['north'], 
      'is_south_to_north': RM_pre['is_south_to_north'], 
      'idx_miss': RM_pre['idx_miss'],
      'dir': RM_pre['dir'],
    }

    for landType in ['river', 'river_end', 'noriv', 'ocean']:
        cnf[k_gs][f'RM_{landType}'] = dict(
          **RM_cmn, **{
            'name': f'RM_{landType}',
            'fin_rstidx': RM_pre[f'fout_rstidx_{landType}'],
            'fin_grdidx': RM_pre[f'fout_grdidx_{landType}'],
          })

        cnf[k_gs][f'RM_simple_{landType}'] = dict(
          **RM_simple_cmn, **{
            'name': f'RM_simple_{landType}',
            'fin_grdidx': RM_pre[f'fout_grdidx_{landType}'],
          })

        cnf[k_gs][f'IO_RM_row_{landType}'] = dict(
          **RM_simple_cmn, **{
            'name': f'IO_RM_row_{landType}',
          })

    del(cnf[k_gs]['RM_pre'])


def run(update_data):
    step = int(__name__.split('.')[-1][1:3])

    cnf = util.read_cnf(step)
    cnf = lutil.adjust_config(cnf)

    os.makedirs(lconst.dir_set[step], exist_ok=True)
    os.makedirs(lconst.dir_tmp[step], exist_ok=True)
    os.makedirs(lconst.dir_log[step], exist_ok=True)

    make_cmf_mat(cnf, step, update_data)

    util.make_new_f_cnf(step, cnf)
