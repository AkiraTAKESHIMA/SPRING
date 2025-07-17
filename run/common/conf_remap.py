import os
import sys
import subprocess
import json

sys.path.append('../../../common')
import const, util


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

    if 'dir' in gs.keys():
        s += f'\
  dir: "{gs["dir"]}"\n'

    if 'west' in gs.keys():
        s += f'\
  west: {gs["west"]}\n\
  east: {gs["east"]}\n\
'
    else:
        s += f'\
  f_lon_bound: {util.str_file_bin(gs["f_lon_bound"])}\n\
'

    if 'south' in gs.keys():
        s += f'\
  south: {gs["south"]}\n\
  north: {gs["north"]}\n\
'
    else:
        s += f'\
  f_lat_bound: {util.str_file_bin(gs["f_lat_bound"])}\n'

    s += f'\
  is_south_to_north: {gs["is_south_to_north"]}\n'

    if 'fin_grdidx' in gs.keys():
        s += f'\
  fin_grdidx: {util.str_file_bin(gs["fin_grdidx"])}\n'

    if use_grdara:
        s += f'\
  fin_grdara: {util.str_file_bin(gs["fin_grdara"])}\n'

    if 'idx_bgn' in gs.keys():
        s += f'\
  idx_bgn: {gs["idx_bgn"]}\n'

    s += '\
[end]\n'

    return s


def block_gs_raster(gs, use_grdara):
    s = f'\
\n\
[grid_system_raster]\n\
  name: "{gs["name"]}"\n\
  nx: {gs["nx"]}\n\
  ny: {gs["ny"]}\n\
  west: {gs["west"]}\n\
  east: {gs["east"]}\n\
  south: {gs["south"]}\n\
  north: {gs["north"]}\n\
  is_south_to_north: {gs["is_south_to_north"]}\n\
  dir: "{gs["dir"]}"\n\
  fin_rstidx: {util.str_file_bin(gs["fin_rstidx"])}\n\
  fin_grdidx: {util.str_file_bin(gs["fin_grdidx"])}\n'

    if use_grdara:
        s += f'\
  fin_grdara: {util.str_file_bin(gs["fin_grdara"])}\n'

    s += f'\
  in_grid_sz: {gs["ncx"]}, {gs["ncy"]}\n\
  idx_miss: {gs["idx_miss"]}\n\
[end]\n'

    return s


def block_gs_polygon(gs, use_grdara):
    s = f'\
\n\
[grid_system_polygon]\n\
  name: "{gs["name"]}"\n\
  np: {gs["np"]}\n\
  nij: {gs["nij"]}\n\
\n\
  dir: "{gs["dir"]}"\n'

    if 'f_lon_vertex' in gs.keys():
        keys = ['f_lon_vertex', 'f_lat_vertex']
    elif 'f_x_vertex' in gs.keys():
        keys = ['f_x_vertex', 'f_y_vertex', 'f_z_vertex']
    for key in keys:
        s += f'\
  {key}: {util.str_file_bin(gs[key])}\n'

    for key in ['f_arctyp', 'fin_grdidx']:
        if key in gs.keys():
            s += f'\
  {key}: {util.str_file_bin(gs[key])}\n'

    if use_grdara:
            s += f'\
  fin_grdara: {util.str_file_bin(gs["fin_grdara"])}\n'

    s += f'\
  coord_unit: {gs["coord_unit"]}\n\
  coord_miss: {gs["coord_miss"]}\n\
  idx_miss: {gs["idx_miss"]}\n\
[end]\n'

    return s



def block_remapping(rmp, dir_tmp_this):
    s = f'\
\n\
[remapping]\n\
  dir: "{dir_tmp_this}"\n\
  fout_rt_sidx: "grid.bin", {rmp["dtype_idx"]}, 1, {rmp["endian"]}\n\
  fout_rt_tidx: "grid.bin", {rmp["dtype_idx"]}, 2, {rmp["endian"]}\n\
  fout_rt_area: "area.bin", endian={rmp["endian"]}\n\
  fout_rt_coef: "coef.bin", endian={rmp["endian"]}\n'

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


def block_options(opt):
    s = '\
\n\
[options]\n\
  old_files: remove\n'

    if opt['Earth']['shape'] == 'sphere':
        s += f'\
  earth_shape: {opt["Earth"]["shape"]}\n\
  earth_r    : {opt["Earth"]["diameter"]}\n'

    elif opt['Earth']['shape'] == 'ellips':
        s += f'\
  earth_shape: {opt["Earth"]["shape"]}\n\
  earth_r    : {opt["Earth"]["diameter"]}\n\
  earth_e2   : {opt["Earth"]["square_eccentricity"]}\n'

    s += '\
[end]\n'

    return s
