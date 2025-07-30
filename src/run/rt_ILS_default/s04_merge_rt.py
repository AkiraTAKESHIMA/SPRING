import os
import sys
import subprocess
import json

import const, util, conf
from const import k_lt, k_gs, k_rt, k_rtc, k_int_rt, k_opt
from util import istep, file_bin

import s00_const as lconst
import s00_util as lutil


def slink_rt(rt):
    dir_tmp = f'{lconst.dir_tmp[istep("make_rt")]}/{rt["name"]}'
    util.make_slink(f'{dir_tmp}', 
                    f'{const.dir_out}/{rt["name"]}')


def merge_rt(cnf, step, update_data, rt, rmp):
    if 'sourceTables' not in rt.keys():
        return

    if len(cnf[k_lt]) == 1:
        slink_rt(rt)
        return

    dir_tmp_make_rt = f'{lconst.dir_tmp[istep("make_rt")]}'

    dir_tmp = f'{lconst.dir_tmp[step]}/rt_{rt["name"]}'

    f_conf = f'{lconst.dir_set[step]}/rt_{rt["name"]}.conf'
    print('config: '+f_conf)
    fp = open(f_conf, 'w')
    fp.write(conf.merge_rt.head(dir_tmp))
    fp.write(conf.merge_rt.block_input_bgn())
    for landType in rt[k_lt]:
        rtName = rt['sourceTables'].format(landType=landType)
        dir_rt = f'{dir_tmp_make_rt}/rt_{rtName}'
        nij = util.get_nij(f'{dir_rt}/grid.bin', rmp['dtype_idx'])
        fp.write(conf.merge_rt.block_input_rt(nij, dir_rt, rmp))
    fp.write(conf.merge_rt.block_input_end(opt_idx_duplication=rt['opt_idx_duplication']))
    fp.write(conf.merge_rt.block_output(rmp, dir_tmp, rt))
    fp.write(conf.merge_rt.block_options(cnf[k_opt]))
    fp.close()

    if update_data:
        f_log = f'{lconst.dir_log[step]}/rt_{rt["name"]}.out'
        f_err = f'{lconst.dir_log[step]}/rt_{rt["name"]}.err'
        util.exec_program(const.prog_merge_rt, f_conf, f_log, f_err)

        util.make_slink(f'{dir_tmp}', 
                        f'{const.dir_out}/remapping_table/{rt["name"]}')



def run(update_data):
    step = int(__name__.split('.')[-1][1:3])

    cnf = util.read_cnf(step)
    cnf = lutil.adjust_config(cnf)

    os.makedirs(lconst.dir_set[step], exist_ok=True)
    os.makedirs(lconst.dir_tmp[step], exist_ok=True)
    os.makedirs(lconst.dir_log[step], exist_ok=True)

    for rtName in cnf[k_rt].keys():
        merge_rt(cnf, step, update_data, cnf[k_rt][rtName], cnf[k_rtc])

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


