import os
import sys
import subprocess
import json

sys.path.append('../../../common')
import const, util

import s00_const as lconst
import s00_util as lutil
import s00_conf as lconf


def make_grid_data(cnf, step, meshName):
    f_conf = f'{lconst.dir_set[step]}/{meshName}.conf'

    print(f_conf)
    fp = open(f_conf,'w')
    fp.write(lconf.head(lconst.dir_tmp[step]))
    fp.write(lconf.makeGridData.block_gs(cnf[meshName], step))
    fp.write(lconf.remap.block_options(cnf['options']))
    fp.close()

    f_log = f'{lconst.dir_log[step]}/{meshName}.out'
    f_err = f'{lconst.dir_log[step]}/{meshName}.err'
    util.exec_program(const.prog_make_grid_data, f_conf, f_log, f_err)


if __name__ == '__main__':
    step = int(sys.argv[0][1:3])

    cnf = json.load(open(lconst.f_cnf,'r'))
    lutil.adjust_config(cnf)

    os.makedirs(lconst.dir_set[step], exist_ok=True)
    os.makedirs(lconst.dir_tmp[step], exist_ok=True)
    os.makedirs(lconst.dir_log[step], exist_ok=True)
    os.makedirs(lconst.dir_out[step], exist_ok=True)

    make_grid_data(cnf, step, 'AGCM')
