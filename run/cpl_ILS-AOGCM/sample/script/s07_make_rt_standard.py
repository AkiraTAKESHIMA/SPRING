import os
import sys
import subprocess
import json

sys.path.append('../../../common')
import const, util

import s00_const as lconst
import s00_util as lutil
import s00_conf as lconf


def make_rt_OGCM_to_AGCM():
    landType = 'ocean'
    runname = f'OGCM_{landType}_to_AGCM'
    f_conf = f'{lconst.dir_set[step]["self"]}/{runname}.conf'
    dir_tmp = f'{lconst.dir_tmp[step]["child"][runname]}'
    f_log = f'{lconst.dir_log[step]["self"]}/{runname}.out'
    f_err = f'{lconst.dir_log[step]["self"]}/{runname}.err'

    print(f_conf)
    fp = open(f_conf, 'w')
    fp.write(lconf.head(dir_tmp))
    fp.write(lconf.remap.block_gs(cnf['OGCM']))
    fp.write(lconf.remap.block_gs(cnf['AGCM']))
    fp.write(lconf.remap.block_remapping(cnf['remapping'], dir_tmp))
    fp.write(lconf.remap.block_options(cnf['options']))
    fp.close()

    util.exec_program(const.prog_remap, f_conf, f_log, f_err)


def make_rt_RM_to_AGCM():
    for landType in ['river', 'noriv', 'ocean']:
        runname = f'RM_{landType}_to_AGCM'
        f_conf = f'{lconst.dir_set[step]["self"]}/{runname}.conf'
        dir_tmp = f'{lconst.dir_tmp[step]["child"][runname]}'
        f_log = f'{lconst.dir_log[step]["self"]}/{runname}.out'
        f_err = f'{lconst.dir_log[step]["self"]}/{runname}.err'

        print(f_conf)
        fp = open(f_conf, 'w')
        fp.write(lconf.head(dir_tmp))
        fp.write(lconf.remap.block_gs(cnf['RM'], landType))
        fp.write(lconf.remap.block_gs(cnf['AGCM']))
        fp.write(lconf.remap.block_remapping(cnf['remapping'], dir_tmp))
        fp.write(lconf.remap.block_options(cnf['options']))
        fp.close()

        util.exec_program(const.prog_remap, f_conf, f_log, f_err)


if __name__ == '__main__':
    step = 7

    cnf = json.load(open(lconst.f_cnf, 'r'))
    lutil.adjust_config(cnf)

    os.makedirs(lconst.dir_set[step]['self'], exist_ok=True)
    os.makedirs(lconst.dir_tmp[step]['self'], exist_ok=True)
    os.makedirs(lconst.dir_log[step]['self'], exist_ok=True)
    os.makedirs(lconst.dir_out[step]['self'], exist_ok=True)

    make_rt_OGCM_to_AGCM()
    #make_rt_RM_to_AGCM()
