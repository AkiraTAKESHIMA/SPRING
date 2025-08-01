import os
import sys
import copy

import const, util
from const import k


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


def substitute_gs_landType_each(cnfin, lst_landType, landTypeAnchor):
    cnf = {}
    for key in cnfin.keys():
        if key == k.m: continue
        cnf[key] = copy.deepcopy(cnfin[key])

    cin = cnfin[k.m]
    c = {}
    for gsNameFmt in cin.keys():
        if f'{{{landTypeAnchor}}}' in gsNameFmt:
            #print(gsNameFmt)
            for landType in lst_landType:
                gsName = gsNameFmt.format(**{landTypeAnchor: landType})
                #print(gsName)
                c[gsName] = copy.deepcopy(cin[gsNameFmt])
                c[gsName]['name'] = gsName
                for key in c[gsName].keys():
                    c[gsName][key] = format_any(c[gsName][key], {landTypeAnchor: landType})
        else:
            gsName = gsNameFmt
            #print(gsName)
            c[gsName] = copy.deepcopy(cin[gsName])
            c[gsName]['name'] = gsName
    cnf[k.m] = c

    return cnf


def substitute_gs_landType(cnfin):
    if type(cnfin[k.lt]) is list:
        cnf = substitute_gs_landType_each(cnfin, cnfin[k.lt], 'landType')
    elif type(cnfin[k.lt]) is dict:
        cnf = copy.deepcopy(cnfin)
        for key in cnfin[k.lt].keys():
            cnf = substitute_gs_landType_each(cnf, cnfin[k.lt][key], 'landType'+key)

    #for gsName in cnf[k.m].keys():
    #    print(gsName)

    return cnf


def copy_gs_shared(cnfin):
    cnf = copy.deepcopy(cnfin)
    for gsName in cnf[k.m].keys():
        if not 'share' in cnf[k.m][gsName].keys(): continue
        gsName_shared = cnf[k.m][gsName]['share']
        #print(f'{gsName_shared} < {gsName}')
        if gsName_shared not in cnf[k.m].keys():
            raise Exception(f'An undefined grid system "{cnf[k.m][key]["share"]}" '\
                            f'appeared in "{gsName}" > "share".')
        for key in cnf[k.m][gsName_shared].keys():
            if key in cnf[k.m][gsName].keys(): continue
            cnf[k.m][gsName][key] = copy.deepcopy(cnf[k.m][gsName_shared][key])

    return cnf


def set_rt_default(cnf):
    for rtName in cnf[k.irt].keys():
        rt = cnf[k.irt][rtName]
        rt['name'] = rtName

        if 'use_src_grdara' not in rt.keys():
            rt['use_src_grdara'] = False

        if 'use_tgt_grdara' not in rt.keys():
            rt['use_tgt_grdara'] = False

        if 'grid_coef' not in rt.keys():
            rt['grid_coef'] = 'target'

        if 'grid_sort' not in rt.keys():
            rt['grid_sort'] = 'target'

        if 'opt_coef_sum_modify' not in rt.keys() and \
           'opt_coef_sum_modify_ulim' not in rt.keys():
            rt['opt_coef_sum_modify'] = 1.0

        if 'opt_idx_duplication' not in rt.keys():
            rt['opt_idx_duplication'] = 'stop'

    for rtName in cnf[k.rt].keys():
        rt = cnf[k.rt][rtName]
        rt['name'] = rtName

        if 'sourceTable' not in rt.keys():
            if 'use_src_grdara' not in rt.keys():
                rt['use_src_grdara'] = False
            if 'use_tgt_grdara' not in rt.keys():
                rt['use_tgt_grdara'] = False

        if 'grid_coef' not in rt.keys():
            rt['grid_coef'] = 'target'

        if 'grid_sort' not in rt.keys():
            rt['grid_sort'] = 'target'

        if 'opt_coef_sum_modify' not in rt.keys() and \
           'opt_coef_sum_modify_ulim' not in rt.keys():
            rt['opt_coef_sum_modify'] = 1.0

        if 'opt_idx_duplication' not in rt.keys():
            rt['opt_idx_duplication'] = 'stop'

    return cnf



def add_cnfkey_dir(cnf):
    if '_dir' in cnf.keys() and 'dir' not in cnf.keys():
        cnf['dir'] = cnf['_dir']

    for key in cnf.keys():
        if type(cnf[key]) is dict:
            cnf[key] = add_cnfkey_dir(cnf[key])

    return cnf


def join_topdir(cnf, dir_top):
    cnf = add_cnfkey_dir(cnf)

    for key in cnf.keys():
        if type(cnf[key]) is dict:
            cnf[key] = join_topdir(cnf[key], dir_top)
        elif type(cnf[key]) is str:
            if key == 'dir':
                cnf[key] = os.path.join(dir_top, cnf[key])
            elif key == '_dir':
                #if 'dir' in cnf.keys():
                #    raise Exception('"_dir" and "dir" cannot be given for one data.')
                cnf['dir'] = os.path.abspath(cnf['_dir'])
    return cnf



