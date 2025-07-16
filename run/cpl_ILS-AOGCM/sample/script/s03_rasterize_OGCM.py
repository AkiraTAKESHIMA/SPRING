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
    step = int(sys.argv[0][1:3])

    cnf = json.load(open(lconst.f_cnf,'r'))
    lutil.adjust_config(cnf)

    f_conf = f'{lconst.dir_set[step]}/a.conf'

    os.makedirs(lconst.dir_set[step], exist_ok=True)
    os.makedirs(lconst.dir_tmp[step], exist_ok=True)
    os.makedirs(lconst.dir_log[step], exist_ok=True)
    os.makedirs(lconst.dir_out[step], exist_ok=True)

    print(f_conf)
    fp = open(f_conf,'w')
    fp.write(lconf.head(lconst.dir_tmp[step]))
    fp.write(lconf.remap.block_gs_polygon(cnf['OGCM_land']))
    fp.write(lconf.rasterize.block_raster(cnf['RM']))
    fp.write(lconf.rasterize.block_output(step))
    fp.write(lconf.remap.block_options(cnf['options']))
    fp.close()

    f_log = f'{lconst.dir_log[step]}/a.out'
    f_err = f'{lconst.dir_log[step]}/a.err'
    util.exec_program(const.prog_rasterize, f_conf, f_log, f_err)
