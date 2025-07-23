import os
import sys
import json

import const, util, conf

import s00_const as lconst
import s00_util as lutil
from s00_const import k_lt, k_gs, k_rt


def make_grid_data(cnf, step, gsName, landType):
    dir_tmp = f'{lconst.dir_tmp[step]}/{gsName}'

    f_conf = f'{lconst.dir_set[step]}/{gsName}.conf'
    print('config: '+f_conf)
    fp = open(f_conf, 'w')
    fp.write(conf.make_grid_data.head(dir_tmp))
    fp.write(conf.make_grid_data.block_gs(
               cnf[k_gs][gsName], landType,
               ['rstidx', 'grdidx'],
               dir_tmp, ['ara']))
    fp.write(conf.make_grid_data.block_options(cnf['options']))
    fp.close()

    f_log = f'{lconst.dir_log[step]}/{gsName}.out'
    f_err = f'{lconst.dir_log[step]}/{gsName}.err'
    util.exec_program(const.prog_make_grid_data, f_conf, f_log, f_err)



def run():
    step = int(__name__.split('.')[-1][1:3])

    cnf = json.load(open(lconst.f_cnf,'r'))
    cnf = lutil.adjust_config(cnf)

    os.makedirs(lconst.dir_set[step], exist_ok=True)
    os.makedirs(lconst.dir_tmp[step], exist_ok=True)
    os.makedirs(lconst.dir_log[step], exist_ok=True)

    make_grid_data(cnf, step, 'CMF', 'river')

