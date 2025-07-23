import os
import sys
import subprocess
import json

sys.path.append('../../../common')
import const, util, conf

import s00_const as lconst
import s00_util as lutil


def make_grid_data(cnf, step, landType):
    f_conf = f'{lconst.dir_set[step]}/{landType}.conf'
    print('config: '+f_conf)
    fp = open(f_conf, 'w')
    fp.write(conf.make_grid_data.head(f'{lconst.dir_tmp[step]}/{landType}'))
    fp.write(conf.make_grid_data.block_gs(
               cnf[f'RM_{landType}'], landType, ['grdidx'], 
               f'{lconst.dir_tmp[step]}/{landType}',
               ['idx', 'ara', 'xyz', 'lonlat']
            ))
    fp.write(conf.make_grid_data.block_options(cnf['options']))
    fp.close()

    f_log = f'{lconst.dir_log[step]}/{landType}.out'
    f_err = f'{lconst.dir_log[step]}/{landType}.err'
    util.exec_program(const.prog_make_grid_data, f_conf, f_log, f_err)


def run():
    step = int(__name__.split('.')[-1][1:3])

    util.job.put_job(lconst.job)

    cnf = json.load(open(lconst.f_cnf, 'r'))
    lutil.adjust_config(cnf)

    os.makedirs(lconst.dir_set[step], exist_ok=True)
    os.makedirs(lconst.dir_tmp[step], exist_ok=True)
    os.makedirs(lconst.dir_log[step], exist_ok=True)

    for landType in ['river', 'river_end', 'noriv', 'ocean']:
        make_grid_data(cnf, step, landType)

