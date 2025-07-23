import os
import sys
import subprocess
import json

import const, util, conf

import s00_const as lconst
import s00_util as lutil
import s00_conf as lconf


def make_grid_data(cnf, step, meshName):
    dir_tmp = lconst.dir_tmp[step]

    f_conf = f'{lconst.dir_set[step]}/{meshName}.conf'
    print('config: '+f_conf)
    fp = open(f_conf,'w')
    fp.write(conf.head(dir_tmp))
    fp.write(conf.make_grid_data.block_gs(
               cnf[meshName], None, [], dir_tmp, 
               ['idx', 'ara', 'xyz', 'lonlat']))
    fp.write(conf.remap.block_options(cnf['options']))
    fp.close()

    f_log = f'{lconst.dir_log[step]}/{meshName}.out'
    f_err = f'{lconst.dir_log[step]}/{meshName}.err'
    util.exec_program(const.prog_make_grid_data, f_conf, f_log, f_err)


def run():
    step = int(__name__.split('.')[-1][1:3])

    util.job.put_job(lconst.job)

    cnf = json.load(open(lconst.f_cnf,'r'))
    lutil.adjust_config(cnf)

    os.makedirs(lconst.dir_set[step], exist_ok=True)
    os.makedirs(lconst.dir_tmp[step], exist_ok=True)
    os.makedirs(lconst.dir_log[step], exist_ok=True)

    make_grid_data(cnf, step, 'AGCM')

