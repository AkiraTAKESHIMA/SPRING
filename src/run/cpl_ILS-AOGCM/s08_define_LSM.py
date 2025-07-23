import os
import sys
import subprocess
import json

import const, util, conf

import s00_const as lconst
import s00_util as lutil
import s00_conf as lconf


def define_lsm(cnf, step):
    dir_in_rt = f'{lconst.dir_tmp[util.istep("make_rt_standard-1", lconst.job)]}'
    rmp = cnf['remapping_table']

    f_conf = f'{lconst.dir_set[step]}/a.conf'

    print('config: '+f_conf)
    fp = open(f_conf,'w')
    fp.write(conf.cpl_define_mat.head(lconst.dir_tmp[step]))
    fp.write(conf.cpl_define_mat.block_in_rt(
               'ogcm_ocean_to_agcm', 
               f'{dir_in_rt}/rt_OGCM_ocean_to_AGCM', rmp))
    fp.write(conf.cpl_define_mat.block_in_rt(
               'rm_river_to_agcm',
               f'{dir_in_rt}/rt_RM_river_to_AGCM', rmp))
    fp.write(conf.cpl_define_mat.block_in_rt(
               'rm_noriv_to_agcm', 
               f'{dir_in_rt}/rt_RM_noriv_to_AGCM', rmp))
    fp.write(conf.cpl_define_mat.block_in_rt(
               'rm_ocean_to_agcm', 
               f'{dir_in_rt}/rt_RM_ocean_to_AGCM', rmp))
    fp.write(conf.cpl_define_mat.block_in_agcm(
        cnf['AGCM'], 
        f'{lconst.dir_tmp[util.istep("make_grid_data_AGCM", lconst.job)]}'
    ))
    fp.write(conf.cpl_define_mat.block_in_rm(
        cnf['RM'],
        dir_in_grdidx=f'{lconst.dir_tmp[util.istep("make_idxmap_RM", lconst.job)]}',
        dir_in_rstidx=f'{lconst.dir_tmp[util.istep("make_idxmap_RM", lconst.job)]}',
        dir_in_grdara=f'{lconst.dir_tmp[util.istep("make_grid_data_RM", lconst.job)]}',
    ))
    fp.write(conf.cpl_define_mat.block_out_rt(
        'lsm_river_to_agcm', f'{lconst.dir_tmp[step]}/rt_LSM_river_to_AGCM', rmp))
    fp.write(conf.cpl_define_mat.block_out_rt(
        'lsm_noriv_to_agcm', f'{lconst.dir_tmp[step]}/rt_LSM_noriv_to_AGCM', rmp))
    fp.write(conf.cpl_define_mat.block_out_rt(
        'agcm_to_lsm_river', f'{lconst.dir_tmp[step]}/rt_AGCM_to_LSM_river', rmp))
    fp.write(conf.cpl_define_mat.block_out_rt(
        'agcm_to_lsm_noriv', f'{lconst.dir_tmp[step]}/rt_AGCM_to_LSM_noriv', rmp))

    fp.write(conf.cpl_define_mat.block_out_agcm(f'{lconst.dir_tmp[step]}/AGCM'))
    fp.write(conf.cpl_define_mat.block_out_lsm(f'{lconst.dir_tmp[step]}/LSM'))
    fp.write(conf.cpl_define_mat.block_options(cnf['options']))
    fp.close()

    f_log = f'{lconst.dir_log[step]}/a.out'
    f_err = f'{lconst.dir_log[step]}/a.err'
    util.exec_program(const.prog_cpl_define_mat, f_conf, f_log, f_err)


def run():
    step = int(__name__.split('.')[-1][1:3])

    util.job.put_job(lconst.job)

    cnf = json.load(open(lconst.f_cnf, 'r'))
    lutil.adjust_config(cnf)

    os.makedirs(lconst.dir_set[step], exist_ok=True)
    os.makedirs(lconst.dir_tmp[step], exist_ok=True)
    os.makedirs(lconst.dir_log[step], exist_ok=True)

    define_lsm(cnf, step)
