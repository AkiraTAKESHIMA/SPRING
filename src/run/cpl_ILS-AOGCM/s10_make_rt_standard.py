import os
import sys
import subprocess
import json

sys.path.append('../../../common')
import const, util, conf

import s00_const as lconst
import s00_util as lutil
import s00_conf as lconf


#def make_rtdummy_AGCM_to_RM():


def make_rt(cnf, step,
            srcMeshNameFmt, tgtMeshNameFmt, 
            use_grdara_src, use_grdara_tgt, 
            lst_landType):
    for landType in lst_landType:
        srcMeshName = srcMeshNameFmt.format(landType=landType)
        tgtMeshName = tgtMeshNameFmt.format(landType=landType)
        runname = f'rt_{srcMeshName}_to_{tgtMeshName}'
        print(f'{srcMeshName} to {tgtMeshName}')

        dir_tmp = f'{lconst.dir_tmp[step]}/{runname}'

        f_conf = f'{lconst.dir_set[step]}/{runname}.conf'
        print('config: '+f_conf)
        fp = open(f_conf, 'w')
        fp.write(conf.head(dir_tmp))
        fp.write(conf.remap.block_gs(cnf[srcMeshName], use_grdara_src))
        fp.write(conf.remap.block_gs(cnf[tgtMeshName], use_grdara_tgt))
        fp.write(conf.remap.block_remapping(cnf['remapping_table'], dir_tmp))
        fp.write(conf.remap.block_options(cnf['options']))
        fp.close()

        f_log = f'{lconst.dir_log[step]}/{runname}.out'
        f_err = f'{lconst.dir_log[step]}/{runname}.err'
        util.exec_program(const.prog_remap, f_conf, f_log, f_err)


def make_rt_all(cnf, step):

    # AGCM to RM

    # IO_bnd to LSM
    make_rt(cnf, step, 
            'IO_LSM_bnd_{landType}', 'LSM_bnd_{landType}', 
            False, False,
            ['river', 'noriv'])

    # IO_met to LSM
    make_rt(cnf, step,
            'IO_met', 'LSM_{landType}', 
            False, False, 
            ['river', 'noriv_real', 'noriv_virt'])

    # IO_metnc to LSM
    make_rt(cnf, step,
            'IO_metnc', 'LSM_{landType}', 
            False, False,
            ['river', 'noriv_real', 'noriv_virt'])

    # LSM to RM
    make_rt(cnf, step,
            'LSM_latlon_{landType}', 'RM_latlon_{landType}', 
            False, False,
            ['river'])

    # RM to LSM
    make_rt(cnf, step,
            'RM_latlon_{landType}', 'LSM_latlon_{landType}', 
            False, False,
            ['river'])

    # LSM to IO_row
    # rt_area will be incorrect if rt is calculated as if they are latlon grids,
    # so grdara is given to IO_LSM_row. This is different from grid area of latlon grid,
    # so verification data for LSM_latlon_* will be incorrect.
    make_rt(cnf, step,
            'LSM_latlon_{landType}', 'IO_LSM_row_{landType}', 
            False, True,
            ['river', 'noriv_real', 'noriv_virt'])

    # LSM to IO_latlon
    make_rt(cnf, step,
            'LSM_{landType}', 'IO_latlon', 
            False, False,
            ['river', 'noriv_real', 'noriv_virt'])

    # RM to IO_row
    # grdara is given to IO_RM_row for the same reason with LSM to IO_row.
    make_rt(cnf, step,
            'RM_latlon_{landType}', 'IO_RM_row_{landType}', 
            False, True,
            ['river'])


def run():
    step = int(__name__.split('.')[-1][1:3])

    util.job.put_job(lconst.job)

    cnf = json.load(open(lconst.f_cnf,'r'))
    lutil.adjust_config(cnf)

    os.makedirs(lconst.dir_set[step], exist_ok=True)
    os.makedirs(lconst.dir_tmp[step], exist_ok=True)
    os.makedirs(lconst.dir_log[step], exist_ok=True)

    make_rt_all(cnf, step)

