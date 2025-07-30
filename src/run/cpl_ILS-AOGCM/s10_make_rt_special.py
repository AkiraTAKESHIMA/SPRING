import os
import sys
import subprocess
import json

sys.path.append('../../../common')
import const, util, conf
from const import k_lt, k_gs, k_rt, k_rtc, k_int_rt, k_opt
from util import istep, file_bin

import s00_const as lconst
import s00_util as lutil


def make_rt_all(cnf, step, update_data):
    dir_rt = f'{lconst.dir_tmp[istep("make_rt_standard-1")]}/rt_AGCM_to_OGCM_ocean'
    nij_rt = util.get_nij(f'{dir_rt}/grid.bin', cnf[k_rtc]['dtype_idx'])

    for component in ['LSM', 'RM']:
        if component == 'LSM':
            dir_lsm = lconst.dir_tmp[istep('make_grid_data_LSM')]
            lst_landType = ['river', 'noriv_real', 'noriv_virt']
        else:
            dir_lsm = lconst.dir_tmp[istep('make_grid_data_RM')]
            lst_landType = ['river_end']

        for landType in lst_landType:
            srcMeshName = f'{component}_simple_{landType}'
            rtName = f'{srcMeshName}_to_OGCM_via_AGCM'
            print(f'{srcMeshName} to OGCM via AGCM')
            dir_tmp = f'{lconst.dir_tmp[step]}/rt_{rtName}'

            f_conf = f'{lconst.dir_set[step]}/rt_{rtName}.conf'
            print('config: '+f_conf)
            fp = open(f_conf, 'w')
            fp.write(conf.cpl_make_rt.head(lconst.dir_tmp[step]))
            fp.write(conf.cpl_make_rt.block_in_rt(dir_rt, nij_rt, cnf[k_rtc]))
            fp.write(conf.cpl_make_rt.block_agcm(cnf[k_gs]['AGCM']))
            fp.write(conf.cpl_make_rt.block_lsm(cnf[k_gs][srcMeshName]))
            fp.write(conf.cpl_make_rt.block_rt(dir_tmp, cnf[k_rtc]))
            fp.write(conf.cpl_make_rt.block_options(cnf[k_opt]))
            fp.close()

            if update_data:
                f_log = f'{lconst.dir_log[step]}/{rtName}.out'
                f_err = f'{lconst.dir_log[step]}/{rtName}.err'
                util.exec_program(const.prog_cpl_make_rt, f_conf, f_log, f_err)

        srcMeshName = f'{component}_simple_{{landType}}'
        rtName = f'{srcMeshName}_to_OGCM_via_AGCM'
        util.check_landTypes(cnf[k_int_rt], rtName, lst_landType)
        cnf[k_int_rt][rtName]['_dir'] = f'{lconst.dir_tmp[step]}/rt_{rtName}'


def run(update_data):
    step = int(__name__.split('.')[-1][1:3])

    cnf = util.read_cnf(step)
    cnf = lutil.adjust_config(cnf)

    os.makedirs(lconst.dir_set[step], exist_ok=True)
    os.makedirs(lconst.dir_tmp[step], exist_ok=True)
    os.makedirs(lconst.dir_log[step], exist_ok=True)

    make_rt_all(cnf, step, update_data)

    util.make_new_f_cnf(step, cnf)
