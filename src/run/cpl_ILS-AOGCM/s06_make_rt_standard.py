import os
import sys
import subprocess
import json

import const, util, conf
from const import k_lt, k_gs, k_rt, k_rtc, k_int_rt, k_opt
from util import istep, file_bin

import s00_const as lconst
import s00_util as lutil


def make_rt(cnf, step, update_data, srcMeshNameFmt, tgtMeshNameFmt, lst_landType):
    for landType in lst_landType:
        srcMeshName = srcMeshNameFmt.format(landType=landType)
        tgtMeshName = tgtMeshNameFmt.format(landType=landType)
        runname = f'rt_{srcMeshName}_to_{tgtMeshName}'
        print(f'{srcMeshName} to {tgtMeshName}')

        f_conf = f'{lconst.dir_set[step]}/{runname}.conf'
        dir_tmp = f'{lconst.dir_tmp[step]}/{runname}'

        print('config: '+f_conf)
        fp = open(f_conf, 'w')
        fp.write(conf.remap.head(dir_tmp))
        fp.write(conf.remap.block_gs(cnf[k_gs][srcMeshName]))
        fp.write(conf.remap.block_gs(cnf[k_gs][tgtMeshName]))
        fp.write(conf.remap.block_remapping(cnf[k_rtc], dir_tmp))
        fp.write(conf.remap.block_options(cnf[k_opt]))
        fp.close()

        if update_data:
            f_log = f'{lconst.dir_log[step]}/{runname}.out'
            f_err = f'{lconst.dir_log[step]}/{runname}.err'
            util.exec_program(const.prog_remap, f_conf, f_log, f_err)

    rtName = f'{srcMeshNameFmt}_to_{tgtMeshNameFmt}'
    util.check_landTypes(cnf[k_int_rt], rtName, lst_landType)
    cnf[k_int_rt][rtName]['_dir'] = f'{lconst.dir_tmp[step]}/rt_{rtName}'



def make_rt_all(cnf, step, update_data):
    make_rt(cnf, step, update_data, 'OGCM_{landType}', 'AGCM', ['ocean'])

    make_rt(cnf, step, update_data, 'AGCM', 'OGCM_{landType}', ['ocean'])

    make_rt(cnf, step, update_data, 'RM_{landType}', 'AGCM', ['river', 'noriv', 'ocean'])


def run(update_data):
    step = int(__name__.split('.')[-1][1:3])

    cnf = util.read_cnf(step)
    cnf = lutil.adjust_config(cnf)

    os.makedirs(lconst.dir_set[step], exist_ok=True)
    os.makedirs(lconst.dir_tmp[step], exist_ok=True)
    os.makedirs(lconst.dir_log[step], exist_ok=True)

    make_rt_all(cnf, step, update_data)

    util.make_new_f_cnf(step, cnf)

