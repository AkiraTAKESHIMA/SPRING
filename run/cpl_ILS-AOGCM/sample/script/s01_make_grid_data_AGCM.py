import os
import sys
import subprocess
import json

sys.path.append('../../../common')
import const, util

import s00_const as lconst
import s00_util as lutil
import s00_conf as lconf



if __name__ == '__main__':
    step = 1

    cnf = json.load(open(lconst.f_cnf,'r'))
    lutil.adjust_config(cnf)

    f_conf = f'{lconst.dir_set[step]["self"]}/a.conf'

    os.makedirs(lconst.dir_set[step]['self'], exist_ok=True)
    os.makedirs(lconst.dir_tmp[step]['self'], exist_ok=True)
    os.makedirs(lconst.dir_log[step]['self'], exist_ok=True)
    os.makedirs(lconst.dir_out[step]['self'], exist_ok=True)

    print(f_conf)
    fp = open(f_conf,'w')
    fp.write(lconf.head(lconst.dir_tmp[step]['self']))
    fp.write(lconf.makeGridData.block_gs(cnf['AGCM'], step))
    fp.write(lconf.remap.block_options(cnf['options']))
    fp.close()

    f_log = f'{lconst.dir_log[step]["self"]}/a.out'
    f_err = f'{lconst.dir_log[step]["self"]}/a.err'
    util.exec_program(const.prog_make_grid_data, f_conf, f_log, f_err)
