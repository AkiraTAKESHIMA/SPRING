import os
import sys
import subprocess
import json

import const, util, conf
from const import k_lt, k_gs, k_rt, k_rtc, k_int_rt, k_opt
from util import istep, file_bin

import s00_const as lconst
import s00_util as lutil


def make_rt(cnf, step, update_data,
            rt, is_tmp):

    rtNameFmt = rt['name']
    srcMeshNameFmt, tgtMeshNameFmt = util.meshName_from_rtName(rtNameFmt)

    for landType in rt['landTypes']:
        rtName = rtNameFmt.format(landType=landType)
        srcMeshName = srcMeshNameFmt.format(landType=landType)
        tgtMeshName = tgtMeshNameFmt.format(landType=landType)

        dir_tmp = f'{lconst.dir_tmp[step]}/rt_{rtName}'

        f_conf = f'{lconst.dir_set[step]}/rt_{rtName}.conf'
        print('config: '+f_conf)
        fp = open(f_conf, 'w')
        fp.write(conf.head(dir_tmp))
        fp.write(conf.remap.block_gs(cnf[k_gs][srcMeshName], rt['use_src_grdara']))
        fp.write(conf.remap.block_gs(cnf[k_gs][tgtMeshName], rt['use_tgt_grdara']))
        fp.write(conf.remap.block_remapping(cnf[k_rtc], dir_tmp))
        fp.write(conf.remap.block_options(cnf[k_opt]))
        fp.close()

        if update_data:
            f_log = f'{lconst.dir_log[step]}/rt_{rtName}.out'
            f_err = f'{lconst.dir_log[step]}/rt_{rtName}.err'
            util.exec_program(const.prog_remap, f_conf, f_log, f_err)

            if not is_tmp:
                util.make_slink(f'{dir_tmp}', f'{const.dir_out}/rt_{rtName}')

    rt['_dir'] = dir_tmp


def make_rt_all(cnf, step, update_data):
    for rtName in cnf[k_int_rt].keys():
        print(rtName)
        #if rtName != 'MATSIRO_simple_{landType}_to_IO_MATSIRO_row_{landType}': continue
        rt = cnf[k_int_rt][rtName]
        make_rt(cnf, step, update_data, rt, True)

    for rtName in cnf[k_rt].keys():
        rt = cnf[k_rt][rtName]
        if 'sourceTables' in rt.keys(): continue
        make_rt(cnf, step, update_data, rt, False)


def run(update_data):
    step = int(__name__.split('.')[-1][1:3])

    cnf = util.read_cnf(step)
    cnf = lutil.adjust_config(cnf)

    # Check 'landType'
    if len(cnf[k_lt]) == 1:
        if cnf[k_lt][0] != 'river':
            raise Exception(f'The list "{k_lt}" must be `["river"]` '\
                            'when its length is 1.')
    elif len(cnf[k_lt]) == 2:
        if not ('river' in cnf[k_lt] and 'noriv' in cnf[k_lt]):
            raise Exception(f'The list "{k_lt}" must be `["river", "noriv"]` '\
                            'when its length is 1.')
    else:
        raise Exception(f'Length of the list "{k_lt}" must be '\
                        'less than or equal to 2.')

    os.makedirs(lconst.dir_set[step], exist_ok=True)
    os.makedirs(lconst.dir_tmp[step], exist_ok=True)
    os.makedirs(lconst.dir_log[step], exist_ok=True)

    make_rt_all(cnf, step, update_data)

    util.make_new_f_cnf(step, cnf)

    return

    # IO_bnd to MATSIRO
    make_rt(cnf, step, 
            'IO_MATSIRO_bnd_{landType}', 'MATSIRO_bnd_{landType}', 
            cnf[k_lt],
            False, False, 'IO_MATSIRO_bnd_to_MATSIRO_bnd')

    # IO_met to MATSIRO
    make_rt(cnf, step, 
            'IO_met', 'MATSIRO_{landType}', 
            cnf['landType'],
            False, False, 'IO_met_to_MATSIRO')

    # IO_metnc to MATSIRO
    make_rt(cnf, step, 
            'IO_metnc', 'MATSIRO_{landType}',
            cnf['landType'],
            False, False, 'IO_metnc_to_MATSIRO')

    # MATSIRO to CMF
    # *** Values of "area.bin" is incorrect but not used for remapping
    make_rt(cnf, step, 
            'MATSIRO_latlon_{landType}', 'CMF_latlon_{landType}',
            ['river'],
            False, False, 'MATSIRO_to_CMF')

    # CMF to MATSIRO
    # *** Values of "area.bin" is incorrect but not used for remapping
    make_rt(cnf, step, 
            'CMF_latlon_{landType}', 'MATSIRO_latlon_{landType}',
            ['river'],
            False, False, 'CMF_to_MATSIRO')

    # MATSIRO to IO_row
    # *** Verification data for source grid is incorrect
    make_rt(cnf, step, 
            'MATSIRO_latlon_{landType}', 'IO_MATSIRO_row_{landType}',
            cnf['landType'],
            False, True, 'MATSIRO_to_IO_MATSIRO_row')

    # MATSIRO to IO_latlon
    make_rt(cnf, step, 
            'MATSIRO_{landType}', 'IO_latlon',
            cnf['landType'],
            False, False, 'MATSIRO_to_IO_latlon')

    # CMF to IO_row
    # *** Verification data for source grid is incorrect
    make_rt(cnf, step, 
            'CMF_latlon_{landType}', 'IO_CMF_row_{landType}',
            cnf['landType'],
            False, True, 'CMF_to_IO_CMF_row')

