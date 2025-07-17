import os
import sys
import json

sys.path.append('../../../common')
import const, util, conf

import s00_const as lconst
import s00_util as lutil


def make_grid_data(cnf, meshName, landType):
    dir_tmp = f'{lconst.dir_tmp[step]}/{meshName}'

    f_conf = f'{lconst.dir_set[step]}/{meshName}.conf'
    print('config: '+f_conf)
    fp = open(f_conf, 'w')
    fp.write(conf.makeGridData.head(dir_tmp))
    fp.write(conf.makeGridData.block_gs(
               cnf[meshName], landType,
               ['rstidx', 'grdidx'],
               dir_tmp, ['ara']))
    fp.write(conf.makeGridData.block_options(cnf['options']))
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

    make_grid_data(cnf, 'CaMa-Flood_river', 'river')

