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


def merge_rt(cnf, step, update_data, 
             rtName):
    rt = cnf[k_rt][rtName]
    rtiName = rt['sourceTables']
    rti = cnf[k_int_rt][rtiName]
    srcMeshNameFmt, tgtMeshNameFmt = util.meshName_from_rtName(rtiName)
    srcMeshName, tgtMeshName = util.meshName_from_rtName(rtName)

    dir_tmp = f'{lconst.dir_tmp[step]}/rt_{rtName}'

    f_conf = f'{lconst.dir_set[step]}/rt_{rtName}.conf'
    print('config: '+f_conf)
    fp = open(f_conf, 'w')
    fp.write(conf.merge_rt.head(dir_tmp))
    fp.write(conf.merge_rt.block_input_bgn())
    for landType in rt['landTypes']:
        srcMeshName_this = srcMeshNameFmt.format(landType=landType)
        tgtMeshName_this = tgtMeshNameFmt.format(landType=landType)
        dir_rt = rti['_dir'].format(landType=landType)
        nij = util.get_nij(dir_rt+'/grid.bin', cnf[k_rtc]['dtype_idx'])
        fp.write(conf.merge_rt.block_input_rt(nij, dir_rt, cnf[k_rtc]))
    fp.write(conf.merge_rt.block_input_end())
    fp.write(conf.merge_rt.block_output(cnf[k_rtc], dir_tmp, rt))
    fp.write(conf.merge_rt.block_options(cnf[k_opt]))
    fp.close()

    if update_data:
        f_log = f'{lconst.dir_log[step]}/rt_{rtName}.out'
        f_err = f'{lconst.dir_log[step]}/rt_{rtName}.err'
        util.exec_program(const.prog_merge_rt, f_conf, f_log, f_err)



def merge_rt_all(cnf, step, update_data):

    for rtKey in cnf[k_rt].keys():
        #if rtKey != "RM_to_OGCM_via_AGCM": continue
        print(rtKey)
        merge_rt(cnf, step, update_data, rtKey)

    # IO_bnd to LSM
    #merge_rt(cnf, step, update_data, 
    #         'IO_LSM_bnd_{landType}', 'LSM_bnd_{landType}', 
    #         ['river', 'noriv'],
    #         lconst.dir_tmp[istep('make_rt_standard-2')], 
    #         'target')

    # IO_met to LSM
    #merge_rt('IO_met', 'LSM_{landType}', 
    #         ['river', 'noriv_real', 'noriv_virt'],
    #         lconst.dir_tmp[istep('make_rt_standard-2')],
    #         'target')

    # IO_metnc to LSM
    #merge_rt('IO_metnc', 'LSM_{landType}', 
    #         ['river', 'noriv_real', 'noriv_virt'],
    #         lconst.dir_tmp[istep('make_rt_standard-2')],
    #         'target')

    # AGCM to LSM
    #merge_rt('AGCM', 'LSM_{landType}', 
    #         ['river', 'noriv'], 
    #         lconst.dir_tmp[istep('define_LSM')],
    #         'target')

    # LSM to AGCM
    #merge_rt('LSM_{landType}', 'AGCM', 
    #         ['river', 'noriv'], 
    #         lconst.dir_tmp[istep('define_LSM')],
    #         'target')

    # LSM to IO_row
    #merge_rt('LSM_latlon_{landType}', 'IO_LSM_row', 
    #         ['river', 'noriv_real', 'noriv_virt'],
    #         lconst.dir_tmp[istep('make_rt_standard-2')],
    #         'target')

    # LSM to IO_latlon
    #merge_rt('LSM_{landType}', 'IO_latlon', 
    #         ['river', 'noriv_real', 'noriv_virt'],
    #         lconst.dir_tmp[istep('make_rt_standard-2')],
    #         'target')

    # LSM to OGCM via AGCM

    # LSM_noriv to OGCM via AGCM


def run(update_data):
    step = int(__name__.split('.')[-1][1:3])

    cnf = util.read_cnf(step)
    cnf = lutil.adjust_config(cnf)

    os.makedirs(lconst.dir_set[step], exist_ok=True)
    os.makedirs(lconst.dir_tmp[step], exist_ok=True)
    os.makedirs(lconst.dir_log[step], exist_ok=True)

    merge_rt_all(cnf, step, update_data)

    util.make_new_f_cnf(step, cnf)


