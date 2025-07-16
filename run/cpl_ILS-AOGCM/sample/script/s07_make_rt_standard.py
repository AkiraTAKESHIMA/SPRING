import os
import sys
import subprocess
import json

sys.path.append('../../../common')
import const, util

import s00_const as lconst
import s00_util as lutil
import s00_conf as lconf


def make_rt(srcMeshNameFmt, tgtMeshNameFmt, lst_landType):
    for landType in lst_landType:
        srcMeshName = srcMeshNameFmt.format(landType=landType)
        tgtMeshName = tgtMeshNameFmt.format(landType=landType)
        runname = f'rt_{srcMeshName}_to_{tgtMeshName}'

        f_conf = f'{lconst.dir_set[step]}/{runname}.conf'
        dir_tmp = f'{lconst.dir_tmp[step]}/{runname}'
        f_log = f'{lconst.dir_log[step]}/{runname}.out'
        f_err = f'{lconst.dir_log[step]}/{runname}.err'

        print(f_conf)
        fp = open(f_conf, 'w')
        fp.write(lconf.head(dir_tmp))
        fp.write(lconf.remap.block_gs(cnf[srcMeshName]))
        fp.write(lconf.remap.block_gs(cnf[tgtMeshName]))
        fp.write(lconf.remap.block_remapping(cnf['remapping_table'], dir_tmp))
        fp.write(lconf.remap.block_options(cnf['options']))
        fp.close()

        util.exec_program(const.prog_remap, f_conf, f_log, f_err)


if __name__ == '__main__':
    step = int(sys.argv[0][1:3])

    cnf = json.load(open(lconst.f_cnf, 'r'))
    lutil.adjust_config(cnf)

    os.makedirs(lconst.dir_set[step], exist_ok=True)
    os.makedirs(lconst.dir_tmp[step], exist_ok=True)
    os.makedirs(lconst.dir_log[step], exist_ok=True)
    os.makedirs(lconst.dir_out[step], exist_ok=True)

    make_rt('OGCM_{landType}', 'AGCM', ['ocean'])
    make_rt('AGCM', 'OGCM_{landType}', ['ocean'])
    make_rt('RM_{landType}', 'AGCM', ['river', 'noriv', 'ocean'])
