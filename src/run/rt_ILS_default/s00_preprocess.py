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


def preprocess(cnf):
    CMF = cnf[k_gs]['CMF']

    meshName = 'CMF_pre'
    cnf[k_gs][meshName] = copy.deepcopy(CMF)

    for layer, landType in enumerate(cnf[k_lt]):
        meshName = f'CMF_{landType}'
        util.add_mesh(cnf[k_gs], meshName)
        c = cnf[k_gs][meshName]
        for key in ['type', 'nx_raster', 'ny_raster', 'nx_grid', 'ny_grid',
                    'west', 'east', 'south', 'north', 'is_south_to_north',
                    'idx_miss', 'idx_condition', 
                    'dir', '_dir']:
            util.copy_dict_elem(c, CMF, key)
        for key_in, key_out in zip(
          [f'fin_rstidx_{landType}', f'fin_grdidx_{landType}'],
          [ 'fin_rstidx'           ,  'fin_grdidx'           ]):
            util.copy_dict_elem(c, CMF, key_out, key_in)

        meshName = f'CMF_simple_{landType}'
        util.add_mesh(cnf[k_gs], meshName)
        cnf[k_gs][meshName] = {
          'type': 'latlon',
          'nx': CMF['nx_grid'],
          'ny': CMF['ny_grid'],
        }
        c = cnf[k_gs][meshName]
        for key in ['west', 'east', 'south', 'north', 'is_south_to_north']:
            util.copy_dict_elem(c, CMF, key)

        meshName = f'IO_CMF_row_{landType}'
        util.add_mesh(cnf[k_gs], meshName)
        c = cnf[k_gs][meshName]
        for key in cnf[k_gs][f'CMF_simple_{landType}'].keys():
            util.copy_dict_elem(c, cnf[k_gs][f'CMF_simple_{landType}'], key)
        for key_in, key_out in zip(
          ['dir', '_dir', 'fin_grdara'],
          ['dir', '_dir', 'fin_grdara']):
            util.copy_dict_elem(c, CMF, key_out, key_in)
        c['idx_bgn'] = (c['nx']*c['ny'])*layer + 1


    for layer, landType in enumerate(cnf[k_lt]):
        meshName = f'MATSIRO_{landType}'
        util.add_mesh(cnf[k_gs], meshName)
        c = cnf[k_gs][meshName]
        for key in ['type', 'nx_raster', 'ny_raster', 'nx_grid', 'ny_grid',
                    'west', 'east', 'south', 'north', 'is_south_to_north',
                    'idx_condition',]:
            util.copy_dict_elem(c, CMF, key)
        util.set_dict_default(c, 'idx_miss', -9999)

        MAT = cnf[k_gs][meshName]
        MAT_simple_cmn = {
          'type': 'latlon',
          'nx': MAT['nx_grid'],
          'ny': MAT['ny_grid'],
          'west' : MAT['west'],
          'east' : MAT['east'],
          'south': MAT['south'],
          'north': MAT['north'],
          'is_south_to_north': MAT['is_south_to_north'],
          'idx_miss': MAT['idx_miss'],
        }

        meshName = f'MATSIRO_simple_{landType}'
        util.add_mesh(cnf[k_gs], meshName)
        c = cnf[k_gs][meshName]
        for key in MAT_simple_cmn.keys():
            util.copy_dict_elem(c, MAT_simple_cmn, key)
        for key_in, key_out in zip(
          ['dir', '_dir', 'fin_grdidx', 'fin_grdara'],
          ['dir', '_dir', 'fin_grdidx', 'fin_grdara']):
            util.copy_dict_elem(c, MAT, key_out, key_in)

        meshName = f'MATSIRO_bnd_simple_{landType}'
        util.add_mesh(cnf[k_gs], meshName)
        c = cnf[k_gs][meshName]
        for key in MAT_simple_cmn.keys():
            util.copy_dict_elem(c, MAT_simple_cmn, key)
        for key_in, key_out in zip(
          ['dir', '_dir', 'fin_grdbndidx'],
          ['dir', '_dir', 'fin_grdidx'   ]):
            util.copy_dict_elem(c, MAT, key_out, key_in)

        meshName = f'IO_MATSIRO_bnd_simple_{landType}'
        util.add_mesh(cnf[k_gs], meshName)
        c = cnf[k_gs][meshName]
        for key in MAT_simple_cmn.keys():
            util.copy_dict_elem(c, MAT_simple_cmn, key)
        c['idx_bgn'] = (c['nx']*c['ny'])*layer + 1

        meshName = f'IO_MATSIRO_row_{landType}'
        util.add_mesh(cnf[k_gs], meshName)
        c = cnf[k_gs][meshName]
        for key in MAT_simple_cmn.keys():
            util.copy_dict_elem(c, MAT_simple_cmn, key)
        for key_in, key_out in zip(
          ['dir', '_dir', 'fin_grdara'],
          ['dir', '_dir', 'fin_grdara']):
            util.copy_dict_elem(c, MAT, key_out, key_in)
        c['idx_bgn'] = (c['nx']*c['ny'])*layer + 1

    del(cnf[k_gs]['CMF'])



def run(update_data):
    step = int(__name__.split('.')[-1][1:3])

    cnf = util.read_cnf(step)
    cnf = lutil.adjust_config(cnf)

    os.makedirs(lconst.dir_set[step], exist_ok=True)
    os.makedirs(lconst.dir_tmp[step], exist_ok=True)
    os.makedirs(lconst.dir_log[step], exist_ok=True)

    preprocess(cnf)

    util.make_new_f_cnf(step, cnf)
