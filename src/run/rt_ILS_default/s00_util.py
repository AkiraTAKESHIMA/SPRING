import os
import sys
import shutil
import copy

import const, util

import s00_const as lconst
from s00_const import k_lt, k_gs, k_rt


def format_any(a, d_fill):
    if type(a) is list:
        return [format_any(x, d_fill) for x in a]
    elif type(a) is dict:
        for key in a.keys():
            a[key] = format_any(a[key], d_fill)
        return a
    elif type(a) is str:
        return a.format(**d_fill)
    else:
        return a


def substitute_gs_landType(cnfin):
    cnf = {}
    for key in cnfin.keys():
        if key == k_gs: continue
        cnf[key] = copy.deepcopy(cnfin[key])

    cin = cnfin[k_gs]
    c = {}
    for gsNameFmt in cin.keys():
        if '{landType}' in gsNameFmt:
            for landType in cnfin['landTypes']:
                gsName = gsNameFmt.format(landType=landType)
                #print(gsName)
                c[gsName] = copy.deepcopy(cin[gsNameFmt])
                c[gsName]['name'] = gsName
        else:
            gsName = gsNameFmt
            #print(gsName)
            c[gsName] = copy.deepcopy(cin[gsName])
            c[gsName]['name'] = gsName
            continue
    cnf[k_gs] = c

    for gsName in cnf[k_gs].keys():
        for key in cnf[k_gs][gsName].keys():
            cnf[k_gs][gsName][key] = format_any(cnf[k_gs][gsName][key], {'landType': landType})

    return cnf


def copy_gs_shared(cnfin):
    cnf = copy.deepcopy(cnfin)
    for gsName in cnf[k_gs].keys():
        if not 'share' in cnf[k_gs][gsName].keys(): continue
        gsName_shared = cnf[k_gs][gsName]['share']
        #print(f'{gsName_shared} < {gsName}')
        if gsName_shared not in cnf[k_gs].keys():
            raise Exception(f'An undefined grid system "{cnf[k_gs][key]["share"]}" '\
                            f'appeared in "{gsName}" > "share".')
        for key in cnf[k_gs][gsName_shared].keys():
            if key in cnf[k_gs][gsName].keys(): continue
            cnf[k_gs][gsName][key] = copy.deepcopy(cnf[k_gs][gsName_shared][key])

    return cnf


def set_rt_default(cnf):
    for rtName in cnf[k_rt].keys():
        rt = cnf[k_rt][rtName]
        rt['name'] = rtName
        for key in ['use_src_grdara', 'use_tgt_grdara']:
            if key not in rt.keys():
                rt[key] = False
        if 'grid_coef' not in rt.keys():
            rt['grid_coef'] = 'target'
        if 'opt_coef_sum_modify' not in rt.keys():
            rt['opt_coef_sum_modify'] = 1.0
        if 'opt_idx_duplication' not in rt.keys():
            rt['opt_idx_duplication'] = 'stop'

    return cnf


def adjust_config(cnf):
    cnf = substitute_gs_landType(cnf)

    cnf = copy_gs_shared(cnf)  

    cnf = set_rt_default(cnf)

    cnf = util.join_topdir(cnf, cnf['dir_top'])

    return cnf
