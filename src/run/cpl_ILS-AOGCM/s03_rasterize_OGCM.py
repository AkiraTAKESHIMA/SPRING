import os
import sys
import subprocess
import json

import const, util, conf

import s00_const as lconst
import s00_util as lutil
import s00_conf as lconf


def rasterize(cnf, step):
    f_conf = f'{lconst.dir_set[step]}/a.conf'
    print('config: '+f_conf)
    fp = open(f_conf,'w')
    fp.write(conf.remap.head(lconst.dir_tmp[step]))
    fp.write(conf.remap.block_gs(cnf['OGCM_land']))
    fp.write(conf.rasterize.block_raster(cnf['RM']))
    fp.write(conf.rasterize.block_output(lconst.dir_tmp[step]))
    fp.write(conf.remap.block_options(cnf['options']))
    fp.close()

    f_log = f'{lconst.dir_log[step]}/a.out'
    f_err = f'{lconst.dir_log[step]}/a.err'
    util.exec_program(const.prog_rasterize, f_conf, f_log, f_err)


def run():
    step = int(__name__.split('.')[-1][1:3])

    cnf = json.load(open(lconst.f_cnf,'r'))
    lutil.adjust_config(cnf)

    os.makedirs(lconst.dir_set[step], exist_ok=True)
    os.makedirs(lconst.dir_tmp[step], exist_ok=True)
    os.makedirs(lconst.dir_log[step], exist_ok=True)

    rasterize(cnf, step)

