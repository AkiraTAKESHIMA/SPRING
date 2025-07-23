import os
import sys
import subprocess
import json

import const, util, conf

import s00_const as lconst
import s00_util as lutil
from s00_const import k_lt, k_gs, k_rt


def make_rt(cnf, step, rtName, 
            s, t, use_grdara_src, use_grdara_tgt,
            is_tmp):
    print(f'{s["name"]} to {t["name"]}')
    runname = f'{s["name"]}_to_{t["name"]}'

    dir_tmp = f'{lconst.dir_tmp[step]}/{runname}'

    f_conf = f'{lconst.dir_set[step]}/{runname}.conf'
    print('config: '+f_conf)
    fp = open(f_conf, 'w')
    fp.write(conf.head(dir_tmp))
    fp.write(conf.remap.block_gs(s, use_grdara_src))
    fp.write(conf.remap.block_gs(t, use_grdara_tgt))
    fp.write(conf.remap.block_remapping(cnf['options_remapping_tables'], dir_tmp))
    fp.write(conf.remap.block_options(cnf['options']))
    fp.close()

    f_log = f'{lconst.dir_log[step]}/{runname}.out'
    f_err = f'{lconst.dir_log[step]}/{runname}.err'
    util.exec_program(const.prog_remap, f_conf, f_log, f_err)

    if not is_tmp:
        util.make_slink(f'{dir_tmp}', f'{const.dir_out}/{rtName}')


def run():
    step = int(__name__.split('.')[-1][1:3])

    cnf = json.load(open('conf.json','r'))
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

    # Check if grid systems specified in remapping tables exist
    for key in cnf[k_rt].keys():
        rtName = key
        for landType in cnf['landTypes']:
            for side in ['source', 'target']:
                gsName = cnf[k_rt][rtName][side].format(landType=landType)
                if gsName not in cnf['grid_systems'].keys():
                    raise Exception(f'An undefined grid system "{gsName}" '\
                                    f'appeared in "{rtName}".')

    # Make directories
    os.makedirs(lconst.dir_set[step], exist_ok=True)
    os.makedirs(lconst.dir_tmp[step], exist_ok=True)
    os.makedirs(lconst.dir_log[step], exist_ok=True)

    # Make remapping tables
    for rtName in cnf[k_rt].keys():
        rt = cnf[k_rt][rtName]
        if '{landType}' in rt['source'] or \
           '{landType}' in rt['target']:
            for landType in cnf['landTypes']:
                srcGsName = rt['source'].format(landType=landType)
                tgtGsName = rt['target'].format(landType=landType)

                make_rt(cnf, step, rtName,
                        cnf[k_gs][srcGsName], cnf[k_gs][tgtGsName],
                        rt['use_src_grdara'], rt['use_tgt_grdara'],
                        False)

    return

    # IO_bnd to MATSIRO
    make_rt(cnf, step, 
            'IO_MATSIRO_bnd_{landType}', 'MATSIRO_bnd_{landType}', 
            cnf['landType'],
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

