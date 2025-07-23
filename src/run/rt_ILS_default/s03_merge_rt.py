import os
import sys
import subprocess
import json

import const, util, conf
from util import istep

import s00_const as lconst
import s00_util as lutil
from s00_const import k_lt, k_gs, k_rt


def slink_rt(rt):
    dir_tmp = f'{lconst.dir_tmp[istep("make_rt")]}/{rt["name"]}'
    util.make_slink(f'{dir_tmp}', 
                    f'{const.dir_out}/{rt["name"]}')


def merge_rt(cnf, step, rt, rmp):
    dir_tmp_make_rt = f'{lconst.dir_tmp[istep("make_rt")]}'

    srcGsName_river = rt['source'].format(landType='river')
    srcGsName_noriv = rt['source'].format(landType='noriv')
    tgtGsName_river = rt['target'].format(landType='river')
    tgtGsName_noriv = rt['target'].format(landType='noriv')

    rtName_river = f'{srcGsName_river}_to_{tgtGsName_river}'
    rtName_noriv = f'{srcGsName_noriv}_to_{tgtGsName_noriv}'

    dir_rt_river = f'{dir_tmp_make_rt}/{rtName_river}'
    dir_rt_noriv = f'{dir_tmp_make_rt}/{rtName_noriv}'
    length_rt_river = util.get_nij(f'{dir_rt_river}/grid.bin', rmp['dtype_idx'])
    length_rt_noriv = util.get_nij(f'{dir_rt_noriv}/grid.bin', rmp['dtype_idx'])

    dir_tmp = f'{lconst.dir_tmp[step]}/{rt["name"]}'

    f_conf = f'{lconst.dir_set[step]}/{rt["name"]}.conf'
    print('config: '+f_conf)
    fp = open(f_conf, 'w')
    fp.write(conf.merge_rt.head(dir_tmp))
    fp.write(conf.merge_rt.block_input_bgn())
    fp.write(conf.merge_rt.block_input_rt(length_rt_river, dir_rt_river, rmp))
    fp.write(conf.merge_rt.block_input_rt(length_rt_noriv, dir_rt_noriv, rmp))
    fp.write(conf.merge_rt.block_input_end(opt_idx_duplication=rt['opt_idx_duplication']))
    fp.write(conf.merge_rt.block_output(
               rmp, dir_tmp, rt['grid_coef'], rt['opt_coef_sum_modify']))
    fp.write(conf.merge_rt.block_options(cnf['options']))
    fp.close()

    f_log = f'{lconst.dir_log[step]}/{rt["name"]}.out'
    f_err = f'{lconst.dir_log[step]}/{rt["name"]}.err'
    util.exec_program(const.prog_merge_rt, f_conf, f_log, f_err)

    util.make_slink(f'{dir_tmp}', 
                    f'{const.dir_out}/{rt["name"]}')


def run():
    step = int(__name__.split('.')[-1][1:3])

    cnf = json.load(open(lconst.f_cnf,'r'))
    cnf = lutil.adjust_config(cnf)

    os.makedirs(lconst.dir_set[step], exist_ok=True)
    os.makedirs(lconst.dir_tmp[step], exist_ok=True)
    os.makedirs(lconst.dir_log[step], exist_ok=True)

    for rtName in cnf[k_rt].keys():
        rt = cnf[k_rt][rtName]
        if not ('{landType}' in rt['source'] or '{landType}' in rt['target']): continue

        if len(cnf[k_lt]) == 1:
            slink_rt(rt)
        else:
            merge_rt(cnf, step, rt, cnf['options_remapping_tables'])


    return

    # IO_bnd to MATSIRO
    merge_rt(cnf, step,
             'IO_MATSIRO_bnd_{landType}', 'MATSIRO_bnd_{landType}', 
             'IO_bnd_to_MATSIRO',
             'stop', 'target', "1.d0")

    # IO_met to MATSIRO
    merge_rt(cnf, step,
             'IO_met', 'MATSIRO_{landType}', 
             'IO_met_to_MATSIRO',
             'stop', 'target', "1.d0")

    # IO_metnc to MATSIRO
    merge_rt(cnf, step, 
             'IO_metnc', 'MATSIRO_{landType}',
             'IO_metnc_to_MATSIRO',
             'stop', 'target', "1.d0")

    # MATSIRO to IO_row
    merge_rt(cnf, step,
             'MATSIRO_latlon_{landType}', 'IO_MATSIRO_row_{landType}',
             'MATSIRO_to_IO_row',
             'stop', 'target', "1.d0")

    # MATSIRO to IO_latlon
    merge_rt(cnf, step,
             'MATSIRO_{landType}', 'IO_latlon',
             'MATSIRO_to_IO_latlon',
             'stop', 'target', "1.d0")


