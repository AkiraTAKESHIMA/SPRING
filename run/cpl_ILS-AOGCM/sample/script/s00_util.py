import os
import sys

import s00_const as const


def get_f_conf(step, child_name=None, run_name=None):
    if child_name is None:
        dir_set_this = os.path.join(const.directory['set'][step]['self'])
        dir_tmp_this = os.path.join(const.directory['tmp'][step]['self'])
    else:
        dir_set_this = os.path.join(const.directory['set'][step]['child'][child_name])
        dir_tmp_this = os.path.join(const.directory['tmp'][step]['child'][child_name])

    os.makedirs(dir_set_this, exist_ok=True)
    os.makedirs(dir_tmp_this, exist_ok=True)

    if run_name is None:
        f_conf = os.path.join(dir_set_this, f'conf')
        f_report = os.path.join(dir_tmp_this, f'report')
    else:
        f_conf = os.path.join(dir_set_this, f'conf.{run_name}')
        f_report = os.path.join(dir_tmp_this, f'report.{run_name}')

    return f_conf, f_report, dir_set_this, dir_tmp_this


def set_file(f, dtype):
    if 'dtype' not in f.keys():
        f['dtype'] = dtype
    if 'endian' not in f.keys():
        f['endian'] = const.endian_default
    if 'rec' not in f.keys():
        f['rec'] = 1


def set_gs_latlon(gs):
    for key in ['f_lon_bound', 'f_lat_bound']:
        if key in gs.keys():
            set_file(gs[key], const.dtype_dble)
    if 'fin_grdidx' in gs.keys():
        set_file(gs['fin_grdidx'], const.dtype_int4)


def set_gs_raster(gs):
    if 'fin_grdidx' in gs.keys():
        set_file(gs['fin_grdidx'], const.dtype_int4)


def set_gs_polygon(gs):
    if 'f_lon_vertex' in gs.keys():
        set_file(gs['f_lon_vertex'], const.dtype_dble)
        set_file(gs['f_lat_vertex'], const.dtype_dble)

    if 'f_x_vertex' in gs.keys():
        set_file(gs['f_x_vertex'], const.dtype_dble)
        set_file(gs['f_y_vertex'], const.dtype_dble)
        set_file(gs['f_z_vertex'], const.dtype_dble)

    if 'f_arctyp' in gs.keys():
        set_file(gs['f_arctyp'], const.dtype_int4)

    if 'fin_grdidx' in gs.keys():
        set_file(gs['fin_grdidx'], const.dtype_int4)


def set_gs(gs):
    if gs['type'] == 'latlon':
        set_gs_latlon(gs)
    elif gs['type'] == 'raster':
        set_gs_raster(gs)
    elif gs['type'] == 'polygon':
        set_gs_polygon(gs)

    if 'idx_miss' not in gs.keys():
        gs['idx_miss'] = const.idx_miss_default


def adjust_config(cnf):
    set_gs(cnf['OGCM'])
    set_gs(cnf['AGCM'])
    set_gs(cnf['RM'])

    cnf['RM']['directory'] = const.directory['tmp'][istep('make_idxmap_RM')]['self']
    cnf['LSM']['directory'] = const.directory['tmp'][istep('define_LSM')]['self']

    if 'dtype_idx' not in cnf['remapping'].keys():
        cnf['remapping']['dtype_idx'] = 'int4'
    if 'endian' not in cnf['remapping'].keys():
        cnf['remapping']['endian'] = 'little'


def istep(name):
    for key in const.job.keys():
        if const.job[key]['self'] == name:
            return key
    raise Exception(f'Invalid value in $name: {name}')


def byte(dtype):
    if dtype == const.dtype_int1:
        b = 1
    elif dtype == const.dtype_int2:
        b = 2
    elif dtype == const.dtype_int4:
        b = 4
    elif dtype == const.dtype_int8:
        b = 8
    elif dtype == const.dtype_real:
        b = 4
    elif dtype == const.dtype_dble:
        b = 8
    else:
        raise Exception(f'Invalid value in $dtype: {dtype}')

    return b
