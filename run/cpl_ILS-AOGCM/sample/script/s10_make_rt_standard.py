import os
import sys
import subprocess
import json

sys.path.append('../../../common')
import const, util

import s00_const as lconst
import s00_util as lutil
import s00_conf as lconf


#def make_rtdummy_AGCM_to_RM():

def make_rt(srcMeshNameFmt, tgtMeshNameFmt, lst_landType):
    for landType in lst_landType:
        srcMeshName = srcMeshNameFmt.format(landType=landType)
        tgtMeshName = tgtMeshNameFmt.format(landType=landType)
        runname = f'rt_{srcMeshName}_to_{tgtMeshName}'

        dir_tmp = f'{lconst.dir_tmp[step]}/{runname}'

        f_conf = f'{lconst.dir_set[step]}/{runname}.conf'
        print(f_conf)
        fp = open(f_conf, 'w')
        fp.write(lconf.head(dir_tmp))
        fp.write(lconf.remap.block_gs(cnf[srcMeshName]))
        fp.write(lconf.remap.block_gs(cnf[tgtMeshName]))
        fp.write(lconf.remap.block_remapping(cnf['remapping_table'], dir_tmp))
        fp.write(lconf.remap.block_options(cnf['options']))
        fp.close()

        f_log = f'{lconst.dir_log[step]}/{runname}.out'
        f_err = f'{lconst.dir_log[step]}/{runname}.err'
        util.exec_program(const.prog_remap, f_conf, f_log, f_err)



def make_rt_IO_bnd_to_LSM():
    for landType in ['river', 'noriv']:
        runname = f'rt_IO_bnd_to_LSM_{landType}'
        dir_tmp = lconst.dir_tmp[step]['child'][runname]

        f_conf = f'{lconst.dir_set[step]}/{runname}.conf'
        print(f_conf)
        fp = open(f_conf, 'w')
        fp.write(lconf.head(dir_tmp))
        fp.write(lconf.remap.block_gs(cnf[f'IO_LSM_bnd_{landType}']))
        fp.write(lconf.remap.block_gs(cnf[f'LSM_bnd_{landType}']))
        fp.write(lconf.remap.block_remapping(cnf['remapping_table'], dir_tmp))
        fp.write(lconf.remap.block_options(cnf['options']))
        fp.close()

        f_log = f'{lconst.dir_log[step]}/{runname}.out'
        f_err = f'{lconst.dir_log[step]}/{runname}.err'
        util.exec_program(const.prog_remap, f_conf, f_log, f_err)


def make_rt_IO_met_to_LSM():
    for landType in ['river', 'noriv_real', 'noriv_virt']:
        runname = f'rt_IO_met_to_LSM_{landType}'
        dir_tmp = lconst.dir_tmp[step]['child'][runname]

        f_conf = f'{lconst.dir_set[step]}/{runname}.conf'
        print(f_conf)
        fp = open(f_conf, 'w')
        fp.write(lconf.head(dir_tmp))
        fp.write(lconf.remap.block_gs(cnf['IO_met']))
        fp.write(lconf.remap.block_gs(cnf[f'LSM_{landType}']))
        fp.write(lconf.remap.block_remapping(cnf['remapping_table'], dir_tmp))
        fp.write(lconf.remap.block_options(cnf['options']))
        fp.close()

        f_log = f'{lconst.dir_log[step]}/{runname}.out'
        f_err = f'{lconst.dir_log[step]}/{runname}.err'
        util.exec_program(const.prog_remap, f_conf, f_log, f_err)
    



if __name__ == '__main__':
    step = int(sys.argv[0][1:3])

    cnf = json.load(open(lconst.f_cnf,'r'))
    lutil.adjust_config(cnf)

    f_conf = f'{lconst.dir_set[step]}/a.conf'

    os.makedirs(lconst.dir_set[step], exist_ok=True)
    os.makedirs(lconst.dir_tmp[step], exist_ok=True)
    os.makedirs(lconst.dir_log[step], exist_ok=True)
    os.makedirs(lconst.dir_out[step], exist_ok=True)

    # AGCM to RM

    # IO_bnd to LSM
    make_rt('IO_LSM_bnd_{landType}', 'LSM_bnd_{landType}', ['river', 'noriv'])

    # IO_met to LSM
    make_rt('IO_met', 'LSM_{landType}', ['river', 'noriv_real', 'noriv_virt'])

    # IO_metnc to LSM
    make_rt('IO_metnc', 'LSM_{landType}', ['river', 'noriv_real', 'noriv_virt'])

    # LSM to RM
    make_rt('LSM_latlon_{landType}', 'RM_latlon_{landType}', ['river'])

    # RM to LSM
    make_rt('RM_latlon_{landType}', 'LSM_latlon_{landType}', ['river'])

    # LSM to IO_row
    make_rt('LSM_latlon_{landType}', 'IO_LSM_row', ['river', 'noriv_real', 'noriv_virt'])

    # LSM to IO_latlon
    make_rt('LSM_{landType}', 'IO_latlon', ['river', 'noriv_real', 'noriv_virt'])

    # RM to IO_row
    make_rt('RM_latlon_{landType}', 'IO_RM_row', ['river'])

