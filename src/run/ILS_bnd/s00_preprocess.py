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
    cnf[k_gs][meshName] = {}
    c = cnf[k_gs][meshName]
    for key in ['type', 'nx_raster', 'ny_raster', 'nx_grid', 'ny_grid',
                'west', 'east', 'south', 'north', 'is_south_to_north',
                'idx_miss',
                'dir', '_dir', 'fin_catmxy', 'fin_nextxy', 
                'catmxy_index', 'nextxy_index',
                'fin_rstidx_river', 'fin_grdidx_river',
                'fin_rstidx_noriv', 'fin_grdidx_noriv']:
        util.copy_dict_elem(c, CMF, key)

    for landType in cnf[k_lt]:
        meshName = f'CMF_{landType}'
        cnf[k_gs][meshName] = {}
        c = cnf[k_gs][meshName]
        for key in ['type', 'nx_raster', 'ny_raster', 'nx_grid', 'ny_grid',
                    'west', 'east', 'south', 'north', 'is_south_to_north',
                    'idx_miss']:
            util.copy_dict_elem(c, CMF, key)
        util.copy_dict_elem(c, CMF, 'dir')
        util.copy_dict_elem(c, CMF, '_dir')
        util.copy_dict_elem(c, CMF, 'fin_rstidx', f'fin_rstidx_{landType}')
        util.copy_dict_elem(c, CMF, 'fin_grdidx', f'fin_grdidx_{landType}')

    for layer, landType in enumerate(cnf[k_lt]):
        meshName = f'MATSIRO_{landType}'
        if meshName not in cnf[k_gs].keys():
            cnf[k_gs][meshName] = {}
        c = cnf[k_gs][meshName]
        for key in ['type', 'nx_raster', 'ny_raster', 'nx_grid', 'ny_grid',
                    'west', 'east', 'south', 'north', 'is_south_to_north']:
            util.copy_dict_elem(c, CMF, key)
        util.set_dict_default(c, 'idx_miss', -9999)
        c['mx_raster_1deg'] = int(c['nx_raster'] / (c['east']-c['west']))
        c['my_raster_1deg'] = int(c['ny_raster'] / (c['north']-c['south']))

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
