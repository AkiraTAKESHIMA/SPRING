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
    step = 6

    cnf = json.load(open(lconst.f_cnf, 'r'))
    lutil.adjust_config(cnf)

    os.makedirs(lconst.dir_set[step]['self'], exist_ok=True)
    os.makedirs(lconst.dir_tmp[step]['self'], exist_ok=True)
    os.makedirs(lconst.dir_log[step]['self'], exist_ok=True)
    os.makedirs(lconst.dir_out[step]['self'], exist_ok=True)

    for landType in ['river', 'noriv', 'ocean']:
        dir_tmp = f'{lconst.dir_tmp[step]["self"]}/{landType}'
        f_conf = f'{lconst.dir_set[step]["self"]}/{landType}.conf'

        print(f_conf)
        fp = open(f_conf, 'w')
        fp.write(lconf.head(dir_tmp))
        fp.write(lconf.makeGridData.block_gs(cnf['RM'], step, landType))
        fp.write(lconf.remap.block_options(cnf['options']))
        fp.close()

        f_log = f'{lconst.dir_log[step]["self"]}/{landType}.out'
        f_err = f'{lconst.dir_log[step]["self"]}/{landType}.err'
        util.exec_program(const.prog_make_grid_data, f_conf, f_log, f_err)
