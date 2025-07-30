import os
import sys
import subprocess
import json

import const, util, conf
from const import k_lt, k_gs, k_rt, k_opt
from util import istep, file_bin

import s00_const as lconst
import s00_util as lutil


def make_grid_data(cnf, step, update_data, landType):
    dir_tmp = f'{lconst.dir_tmp[step]}/{landType}'
    c = cnf[k_gs][f'RM_{landType}']
    c['fout_grdidx'] = file_bin(dir_tmp+'/grdidx.bin','int4')
    c['fout_grdara'] = file_bin(dir_tmp+'/grdara.bin','dble')
    c['fout_grdx']   = file_bin(dir_tmp+'/grdxyz.bin','dble',1)
    c['fout_grdy']   = file_bin(dir_tmp+'/grdxyz.bin','dble',2)
    c['fout_grdz']   = file_bin(dir_tmp+'/grdxyz.bin','dble',3)
    c['fout_grdlon'] = file_bin(dir_tmp+'/grdlonlat.bin','dble',1)
    c['fout_grdlat'] = file_bin(dir_tmp+'/grdlonlat.bin','dble',2)

    f_conf = f'{lconst.dir_set[step]}/{landType}.conf'
    print('config: '+f_conf)
    fp = open(f_conf, 'w')
    fp.write(conf.make_grid_data.head(f'{lconst.dir_tmp[step]}/{landType}'))
    fp.write(conf.make_grid_data.block_gs(
               c, '', '', 
               ['grdidx'], 
               ['idx', 'ara', 'xyz', 'lonlat']
            ))
    fp.write(conf.make_grid_data.block_options(cnf['options']))
    fp.close()

    if update_data:
        f_log = f'{lconst.dir_log[step]}/{landType}.out'
        f_err = f'{lconst.dir_log[step]}/{landType}.err'
        util.exec_program(const.prog_make_grid_data, f_conf, f_log, f_err)

    RM = cnf[k_gs][f'RM_{landType}']
    for var in ['idx', 'ara', 'x', 'y', 'z', 'lon', 'lat']:
        RM[f'fin_grd{var}'] = RM[f'fout_grd{var}']
        del(RM[f'fout_grd{var}'])

        cnf[k_gs][f'RM_simple_{landType}'][f'fin_grd{var}'] = RM[f'fin_grd{var}']
        cnf[k_gs][f'IO_RM_row_{landType}'][f'fin_grd{var}'] = RM[f'fin_grd{var}']



def run(update_data):
    step = int(__name__.split('.')[-1][1:3])

    cnf = util.read_cnf(step)
    cnf = lutil.adjust_config(cnf)

    os.makedirs(lconst.dir_set[step], exist_ok=True)
    os.makedirs(lconst.dir_tmp[step], exist_ok=True)
    os.makedirs(lconst.dir_log[step], exist_ok=True)

    for landType in ['river', 'river_end', 'noriv', 'ocean']:
        make_grid_data(cnf, step, update_data, landType)

    util.make_new_f_cnf(step, cnf)

