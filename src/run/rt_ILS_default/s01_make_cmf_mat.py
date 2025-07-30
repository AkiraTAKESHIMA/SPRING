import os
import sys
import subprocess
import copy
import json

import const, util, conf
from const import k_lt, k_gs, k_rt, k_rtc, k_int_rt, k_opt
from util import istep, file_bin

import s00_const as lconst
import s00_util as lutil


def make_links_cmf(cnf, step):
    for landType in ['river']:
        dir_out = lutil.get_outdir_grid('CMF', landType)
        path_rstidx = f'{dir_out}/rstidx.bin'
        path_grdidx = f'{dir_out}/grdidx.bin'
        os.makedirs(dir_out, exist_ok=True)
        CMF = cnf[k_gs][f'CMF_{landType}']
        util.make_slink(f'{os.path.join(CMF["dir"], CMF["fin_rstidx"]["path"])}',
                        f'{path_rstidx}')
        util.make_slink(f'{os.path.join(CMF["dir"], CMF["fin_grdidx"]["path"])}',
                        f'{path_grdidx}')


def make_links_mat(cnf, step):
    for landType in ['river']:
        dir_out = lutil.get_outdir_grid('MATSIRO', landType)
        path_rstidx = f'{dir_out}/rstidx.bin'
        path_grdidx = f'{dir_out}/grdidx.bin'
        path_grdbndidx = f'{dir_out}/grdbndidx.bin'
        os.makedirs(dir_out, exist_ok=True)
        MAT = cnf[k_gs][f'MATSIRO_{landType}']
        util.make_slink(f'{os.path.join(MAT["dir"], MAT["fin_rstidx"]["path"])}',
                        f'{path_rstidx}')
        util.make_slink(f'{os.path.join(MAT["dir"], MAT["fin_grdidx"]["path"])}',
                        f'{path_grdidx}')
        util.make_slink(f'{os.path.join(MAT["dir"], MAT["fin_grdbndidx"]["path"])}',
                        f'{path_grdbndidx}')


def make_cmf_mat(cnf, step, update_data):

    dir_tmp = f'{lconst.dir_tmp[step]}'
    dir_tmp_cmf = f'{dir_tmp}/CMF'
    dir_tmp_mat = f'{dir_tmp}/MATSIRO'

    dir_out_cmf = f'{const.dir_out}/CMF'
    dir_out_mat = f'{const.dir_out}/MATSIRO'

    if 'fin_rstidx' in cnf[k_gs]['CMF_river'].keys() and \
       'fin_rstidx' in cnf[k_gs]['MATSIRO_river'].keys():
        print('CMF and MATSIRO grid data already exist.')
        if update_data:
            make_links_cmf(cnf, step)
            make_links_mat(cnf, step)
        return

    MAT_river, MAT_noriv = None, None
    if 'river' in cnf[k_lt]: MAT_river = cnf[k_gs]['MATSIRO_river']
    if 'noriv' in cnf[k_lt]: MAT_noriv = cnf[k_gs]['MATSIRO_noriv']

    CMF_pre = cnf[k_gs]['CMF_pre']
    for landType in cnf[k_lt]:
        CMF = cnf[k_gs][f'CMF_{landType}']
        if 'fin_rstidx' not in CMF.keys():
            CMF_pre[f'fout_rstidx_{landType}'] = file_bin(f'{landType}/rstidx.bin')
            CMF_pre[f'fout_grdidx_{landType}'] = file_bin(f'{landType}/grdidx.bin')
    for landType in cnf[k_lt]:
        MAT = cnf[k_gs][f'MATSIRO_{landType}']
        if 'fin_rstidx' not in MAT.keys():
            MAT['fout_rstidx'] = file_bin(f'{landType}/rstidx.bin')
            MAT['fout_grdidx'] = file_bin(f'{landType}/grdidx.bin')
            MAT['fout_grdbndidx'] = file_bin(f'{landType}/grdbndidx.bin')

    f_conf = f'{lconst.dir_set[step]}/a.conf'
    print('config: '+f_conf)
    fp = open(f_conf, 'w')
    fp.write(conf.make_cmf_mat.block_common(CMF_pre))
    fp.write(conf.make_cmf_mat.block_cmf(CMF_pre, CMF_pre['dir'], dir_tmp_cmf))
    fp.write(conf.make_cmf_mat.block_matsiro(MAT_river, MAT_noriv, dir_tmp_mat))
    fp.write(conf.make_cmf_mat.block_options(cnf[k_opt]))
    fp.close()

    if update_data:
        f_log = f'{lconst.dir_log[step]}/a.out'
        f_err = f'{lconst.dir_log[step]}/a.err'
        util.exec_program(const.prog_make_cmf_mat, f_conf, f_log, f_err)

        if 'fin_rstidx' in cnf[k_gs]['CMF_river']:
            make_links_cmf(cnf, step)
        else:
            util.make_slink(f'{dir_tmp_cmf}', f'{const.dir_out}/grid/CMF')
        if 'fin_rstidx' in cnf[k_gs]['MATSIRO_river']:
            make_links_mat(cnf, step)
        else:
            util.make_slink(f'{dir_tmp_mat}', f'{const.dir_out}/grid/MATSIRO')

    if 'fin_rstidx' not in cnf[k_gs]['CMF_river']:
        for landType in cnf[k_lt]:
            CMF = cnf[k_gs][f'CMF_{landType}']
            CMF['_dir'] = dir_tmp_cmf
            for var in ['rstidx', 'grdidx']:
                CMF[f'fin_{var}'] = copy.deepcopy(CMF_pre[f'fout_{var}_{landType}'])
                del(CMF_pre[f'fout_{var}_{landType}'])

    if 'fin_rstidx' not in cnf[k_gs]['MATSIRO_river']:
        for landType in cnf[k_lt]:
            MAT = cnf[k_gs][f'MATSIRO_{landType}']
            MAT['_dir'] = dir_tmp_mat
            for var in ['rstidx', 'grdidx', 'grdbndidx']:
                MAT[f'fin_{var}'] = copy.deepcopy(MAT[f'fout_{var}'])
                del(MAT[f'fout_{var}'])


def run(update_data):
    step = int(__name__.split('.')[-1][1:3])

    cnf = util.read_cnf(step)
    cnf = lutil.adjust_config(cnf)

    os.makedirs(lconst.dir_set[step], exist_ok=True)
    os.makedirs(lconst.dir_tmp[step], exist_ok=True)
    os.makedirs(lconst.dir_log[step], exist_ok=True)

    make_cmf_mat(cnf, step, update_data)

    util.make_new_f_cnf(step, cnf)
