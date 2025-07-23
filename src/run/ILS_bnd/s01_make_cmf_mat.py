import os
import sys
import subprocess
import json

import const, util, conf

import s00_const as lconst
import s00_util as lutil


def make_cmf_mat(cnf, step):
    f_conf = f'{lconst.dir_set[step]}/a.conf'
    print(f'config: {f_conf}')
    fp = open(f_conf, 'w')
    fp.write(conf.make_cmf_mat.block_common(cnf['CMF']))
    fp.write(conf.make_cmf_mat.block_cmf(cnf['CMF'], 
               cnf['CMF']['dir'], f'{lconst.dir_tmp[step]}/CMF'))
    fp.write(conf.make_cmf_mat.block_matsiro(cnf['MATSIRO'],
                cnf['MATSIRO']['dir']))
    fp.write(conf.make_cmf_mat.block_options(cnf['options']))
    fp.close()

    f_log = f'{lconst.dir_log[step]}/a.out'
    f_err = f'{lconst.dir_log[step]}/a.err'
    util.exec_program(const.prog_make_cmf_mat, f_conf, f_log, f_err)

    util.make_slink(f'{lconst.dir_tmp[step]}/CMF', 
                    f'{const.dir_out}/grid/CMF')
    util.make_slink(f'{lconst.dir_tmp[step]}/MATSIRO', 
                    f'{const.dir_out}/grid/MATSIRO')


def run():
    step = int(__name__.split('.')[-1][1:3])

    cnf = json.load(open(lconst.f_cnf,'r'))
    cnf = lutil.adjust_config(cnf)

    os.makedirs(lconst.dir_set[step], exist_ok=True)
    os.makedirs(lconst.dir_tmp[step], exist_ok=True)
    os.makedirs(lconst.dir_log[step], exist_ok=True)

    make_cmf_mat(cnf, step)
