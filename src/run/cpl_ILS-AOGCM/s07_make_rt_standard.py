import os
import sys
import subprocess
import json

sys.path.append('../../../common')
import const, util, conf

import s00_const as lconst
import s00_util as lutil
import s00_conf as lconf


def make_rt(cnf, step, srcMeshNameFmt, tgtMeshNameFmt, landType):
    srcMeshName = srcMeshNameFmt.format(landType=landType)
    tgtMeshName = tgtMeshNameFmt.format(landType=landType)
    runname = f'rt_{srcMeshName}_to_{tgtMeshName}'
    print(f'{srcMeshName} to {tgtMeshName}')

    f_conf = f'{lconst.dir_set[step]}/{runname}.conf'
    dir_tmp = f'{lconst.dir_tmp[step]}/{runname}'
    f_log = f'{lconst.dir_log[step]}/{runname}.out'
    f_err = f'{lconst.dir_log[step]}/{runname}.err'

    print('config: '+f_conf)
    fp = open(f_conf, 'w')
    fp.write(conf.remap.head(dir_tmp))
    fp.write(conf.remap.block_gs(cnf[srcMeshName]))
    fp.write(conf.remap.block_gs(cnf[tgtMeshName]))
    fp.write(conf.remap.block_remapping(cnf['remapping_table'], dir_tmp))
    fp.write(conf.remap.block_options(cnf['options']))
    fp.close()

    util.exec_program(const.prog_remap, f_conf, f_log, f_err)


def run():
    step = int(__name__.split('.')[-1][1:3])

    util.job.put_job(lconst.job)

    cnf = json.load(open(lconst.f_cnf, 'r'))
    lutil.adjust_config(cnf)

    os.makedirs(lconst.dir_set[step], exist_ok=True)
    os.makedirs(lconst.dir_tmp[step], exist_ok=True)
    os.makedirs(lconst.dir_log[step], exist_ok=True)

    for landType in ['ocean']:
        make_rt(cnf, step, 'OGCM_{landType}', 'AGCM', landType)

    for landType in ['ocean']:
        make_rt(cnf, step, 'AGCM', 'OGCM_{landType}', landType)

    for landType in ['river', 'noriv', 'ocean']:
        make_rt(cnf, step, 'RM_{landType}', 'AGCM', landType)

