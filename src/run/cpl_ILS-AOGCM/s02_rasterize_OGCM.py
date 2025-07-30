import os
import sys
import subprocess
import json

import const, util, conf
from const import k_lt, k_gs, k_rt, k_opt
from util import istep, file_bin

import s00_const as lconst
import s00_util as lutil


def rasterize(cnf, step, update_data):
    f_conf = f'{lconst.dir_set[step]}/a.conf'
    print('config: '+f_conf)
    fp = open(f_conf,'w')
    fp.write(conf.remap.head(lconst.dir_tmp[step]))
    fp.write(conf.remap.block_gs(cnf[k_gs]['OGCM_land']))
    fp.write(conf.rasterize.block_raster(cnf[k_gs]['RM']))
    fp.write(conf.rasterize.block_output(lconst.dir_tmp[step]))
    fp.write(conf.remap.block_options(cnf[k_opt]))
    fp.close()

    if update_data:
        f_log = f'{lconst.dir_log[step]}/a.out'
        f_err = f'{lconst.dir_log[step]}/a.err'
        util.exec_program(const.prog_rasterize, f_conf, f_log, f_err)

    dir_tmp = os.path.join(os.getcwd(), lconst.dir_tmp[step])

    c = cnf['FLOW']
    c['lndmsk'] = dir_tmp+'/mask.bin'


def run(update_data):
    step = int(__name__.split('.')[-1][1:3])

    cnf = util.read_cnf(step)
    cnf = lutil.adjust_config(cnf)

    os.makedirs(lconst.dir_set[step], exist_ok=True)
    os.makedirs(lconst.dir_tmp[step], exist_ok=True)
    os.makedirs(lconst.dir_log[step], exist_ok=True)

    rasterize(cnf, step, update_data)

    util.make_new_f_cnf(step, cnf)

