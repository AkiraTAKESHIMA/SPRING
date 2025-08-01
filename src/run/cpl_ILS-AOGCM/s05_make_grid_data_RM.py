import os
import sys
import subprocess
import json

import const, util, conf
from const import k
from util import env, istep, file_bin

import s___const as lconst
import s___util as lutil


def make_grid_data(cnf, update_data, landType):
    dir_tmp = f'{env.dir_tmp}/{landType}'
    c = cnf[k.m][f'RM_{landType}']
    c['fout_grdidx'] = file_bin(dir_tmp+'/grdidx.bin','int4')
    c['fout_grdara'] = file_bin(dir_tmp+'/grdara.bin','dble')
    c['fout_grdx']   = file_bin(dir_tmp+'/grdxyz.bin','dble',1)
    c['fout_grdy']   = file_bin(dir_tmp+'/grdxyz.bin','dble',2)
    c['fout_grdz']   = file_bin(dir_tmp+'/grdxyz.bin','dble',3)
    c['fout_grdlon'] = file_bin(dir_tmp+'/grdlonlat.bin','dble',1)
    c['fout_grdlat'] = file_bin(dir_tmp+'/grdlonlat.bin','dble',2)

    f_conf = f'{env.dir_set}/{landType}.conf'
    print('config: '+f_conf)
    fp = open(f_conf, 'w')
    fp.write(conf.make_grid_data.head(f'{env.dir_tmp}/{landType}'))
    fp.write(conf.make_grid_data.block_gs(
               c, '', '', 
               ['grdidx'], 
               ['idx', 'ara', 'xyz', 'lonlat']
            ))
    fp.write(conf.make_grid_data.block_options(cnf['options']))
    fp.close()

    if update_data:
        f_log = f'{env.dir_log}/{landType}.out'
        f_err = f'{env.dir_log}/{landType}.err'
        util.exec_program(const.prog_make_grid_data, f_conf, f_log, f_err)

    RM = cnf[k.m][f'RM_{landType}']
    for var in ['idx', 'ara', 'x', 'y', 'z', 'lon', 'lat']:
        RM[f'fin_grd{var}'] = RM[f'fout_grd{var}']
        del(RM[f'fout_grd{var}'])

        cnf[k.m][f'RM_simple_{landType}'][f'fin_grd{var}'] = RM[f'fin_grd{var}']
        cnf[k.m][f'IO_RM_row_{landType}'][f'fin_grd{var}'] = RM[f'fin_grd{var}']



def run(update_data):
    step = int(__name__.split('.')[-1][1:3])

    cnf = util.read_cnf(step)
    cnf = lutil.adjust_config(cnf)

    env.set_dir(step)

    for landType in ['river', 'river_end', 'noriv', 'ocean']:
        make_grid_data(cnf, update_data, landType)

    util.make_new_f_cnf(cnf)

