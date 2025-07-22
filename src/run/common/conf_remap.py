import os
import sys
import subprocess
import json

import const, util
from conf_make_grid_data import block_options


def head(dir_out, fname_report='report'):
    s = f'\
#\n\
path_report: "{dir_out}/{fname_report}.txt"\n'

    return s


def block_gs(gs, use_grdara=False):
    if gs['type'] == 'latlon':
        return block_gs_latlon(gs, use_grdara)
    elif gs['type'] == 'raster':
        return block_gs_raster(gs, use_grdara)
    elif gs['type'] == 'polygon':
        return block_gs_polygon(gs, use_grdara)


def block_gs_latlon(gs, use_grdara):
    s = f'\
\n\
[grid_system_latlon]\n\
  name: "{gs["name"]}"\n\
  nx: {gs["nx"]}\n\
  ny: {gs["ny"]}\n'

    if util.key_val_exist(gs, 'dir'):
        s += f'\
  dir: "{gs["dir"]}"\n'

    if util.key_val_exist(gs, 'west'):
        s += f'\
  west: {gs["west"]}\n\
  east: {gs["east"]}\n'
    elif util.key_val_exist(gs, 'f_lon_bound'):
        s += f'\
  f_lon_bound: {util.str_file_bin(gs["f_lon_bound"])}\n'
    else:
        raise Exception('Longit. of grid lines were not defined.')

    if util.key_val_exist(gs, 'south'):
        s += f'\
  south: {gs["south"]}\n\
  north: {gs["north"]}\n'
    elif util.key_val_exist(gs, 'f_lat_bound'):
        s += f'\
  f_lat_bound: {util.str_file_bin(gs["f_lat_bound"])}\n'
    else:
        raise Exception('Latit. of grid lines were not defined.')

    s += f'\
  is_south_to_north: {gs["is_south_to_north"]}\n'

    if util.key_val_exist(gs, 'fin_grdidx'):
        s += f'\
  fin_grdidx: {util.str_file_bin(gs["fin_grdidx"])}\n'

    if use_grdara:
        s += f'\
  fin_grdara: {util.str_file_bin(gs["fin_grdara"])}\n'

    if util.key_val_exist(gs, 'idx_bgn'):
        s += f'\
  idx_bgn: {gs["idx_bgn"]}\n'

    if util.key_val_exist(gs, 'idx_miss'):
        if gs['idx_miss'] is not None:
            s += f'\
  idx_miss: {gs["idx_miss"]}\n'

    s += '\
[end]\n'

    return s


def block_gs_raster(gs, use_grdara):
    s = f'\
\n\
[grid_system_raster]\n\
  name: "{gs["name"]}"\n\
  nx: {gs["nx_raster"]}\n\
  ny: {gs["ny_raster"]}\n\
  west: {gs["west"]}\n\
  east: {gs["east"]}\n\
  south: {gs["south"]}\n\
  north: {gs["north"]}\n'

    if util.key_val_exist(gs, 'xi'):
        s += f'\
  xi: {gs["xi"]}\n\
  xf: {gs["xf"]}\n\
  yi: {gs["yi"]}\n\
  yf: {gs["yf"]}\n'

    s += f'\
  is_south_to_north: {gs["is_south_to_north"]}\n'

    if util.key_val_exist(gs, 'dir'):
        s += f'\
  dir: "{gs["dir"]}"\n'

    s += f'\
  fin_rstidx: {util.str_file_bin(gs["fin_rstidx"])}\n'

    if util.key_val_exist(gs, 'fin_grdidx'):
        s += f'\
  fin_grdidx: {util.str_file_bin(gs["fin_grdidx"])}\n'

    if use_grdara:
        s += f'\
  fin_grdara: {util.str_file_bin(gs["fin_grdara"])}\n'

    s += f'\
  in_grid_sz: {gs["nx_grid"]}, {gs["ny_grid"]}\n'

    if util.key_val_exist(gs, 'grdidx_condition'):
        s += f'\
  grdidx_condition: {gs["grdidx_condition"]}\n'

    if util.key_val_exist(gs, 'idx_miss'):
        s += f'\
  idx_miss: {gs["idx_miss"]}\n'

    s += f'\
[end]\n'

    return s


def block_gs_polygon(gs, use_grdara):
    s = f'\
\n\
[grid_system_polygon]\n\
  name: "{gs["name"]}"\n\
  np: {gs["np"]}\n\
  nij: {gs["nij"]}\n'

    if util.key_val_exist(gs, 'dir'):
        s += f'\
  dir: "{gs["dir"]}"\n'

    if util.key_val_exist(gs, 'f_lon_vertex'):
        keys = ['f_lon_vertex', 'f_lat_vertex']
    elif util.key_val_exist(gs, 'f_x_vertex'):
        keys = ['f_x_vertex', 'f_y_vertex', 'f_z_vertex']
    else:
        raise Exception('Coordinates of vertices were not given.')
    for key in keys:
        s += f'\
  {key}: {util.str_file_bin(gs[key])}\n'

    if util.key_val_exist(gs, 'arc_parallel'):
        s += f'\
  arc_parallel: {gs[arc_parallel]}\n'
    elif util.key_val_exist(gs, 'f_arctyp'):
        s += f'\
  f_arctyp: {util.str_file_bin(gs["f_arctyp"])}\n'

    if util.key_val_exist(gs, 'fin_grdidx'):
        s += f'\
  fin_grdidx: {util.str_file_bin(gs["fin_grdidx"])}\n'

    if use_grdara:
        s += f'\
  fin_grdara: {util.str_file_bin(gs["fin_grdara"])}\n'

    if util.key_val_exist(gs, 'coord_unit'):
        s += f'\
  coord_unit: {gs["coord_unit"]}\n'

    if util.key_val_exist(gs, 'coord_miss'):
        s += f'\
  coord_miss: {gs["coord_miss"]}\n'

    if util.key_val_exist(gs, 'idx_miss'):
        s += f'\
  idx_miss: {gs["idx_miss"]}\n'

    s += f'\
[end]\n'

    return s



def block_remapping(
        rmp, dir_out, 
        fname_rt_grid='grid', fname_rt_area='area', fname_rt_coef='coef'):
    s = f'\
\n\
[remapping]\n'

    for key in ['opt_coef_sum_modify',
                'opt_coef_sum_modify_ulim',
                'grid_coef',
                'grid_sort',
                'allow_empty']:
        if util.key_val_exist(rmp, key):
            s += f'\
  {key}: {rmp[key]}\n'

    s += f'\
  dir: "{dir_out}"\n\
  fout_rt_sidx: "{fname_rt_grid}.bin", {rmp["dtype_idx"]}, 1, {rmp["endian"]}\n\
  fout_rt_tidx: "{fname_rt_grid}.bin", {rmp["dtype_idx"]}, 2, {rmp["endian"]}\n\
  fout_rt_area: "{fname_rt_area}.bin", endian={rmp["endian"]}\n\
  fout_rt_coef: "{fname_rt_coef}.bin", endian={rmp["endian"]}\n'

    if rmp['make_verification_data']:
        s += '\
\n\
  vrf_source_form: auto\n\
  fout_vrf_grdidx     : "vrf/src_idx.bin"\n\
  fout_vrf_grdara_true: "vrf/src_val.bin", rec=1\n\
  fout_vrf_grdara_rt  : "vrf/src_val.bin", rec=2\n\
  fout_vrf_rerr_grdara: "vrf/src_val.bin", rec=3\n\
\n\
  vrf_target_form: auto\n\
  fout_vrf_grdidx     : "vrf/tgt_idx.bin"\n\
  fout_vrf_grdara_true: "vrf/tgt_val.bin", rec=1\n\
  fout_vrf_grdara_rt  : "vrf/tgt_val.bin", rec=2\n\
  fout_vrf_rerr_grdara: "vrf/tgt_val.bin", rec=3\n'

    s += '\
[end]\n'

    return s

