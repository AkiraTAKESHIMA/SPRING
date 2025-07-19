import os
import sys

import util


def head(dir_out):
    s = f'\
#\n\
path_report: "{dir_out}/report.txt"\n'

    return s


def block_gs(gs, landType, lst_varname_in, dir_out, lst_varname_out):
    if gs['type'] == 'latlon':
        return block_gs_latlon(gs, landType, lst_varname_in, dir_out, lst_varname_out)
    elif gs['type'] == 'raster':
        return block_gs_raster(gs, landType, lst_varname_in, dir_out, lst_varname_out)
    elif gs['type'] == 'polygon':
        return block_gs_polygon(gs, landType, lst_varname_in, dir_out, lst_varname_out)



def block_gs_latlon(gs, landType, lst_varname_in, dir_out, lst_varname_out):
    if landType is None:
        _landType = ''
    else:
        _landType = '_' + landType

    if 'grdidx' in lst_varname_in:
        out_form = 'index'
    else:
        out_form = 'auto'

    s = f'\
\n\
[grid_system_latlon]\n\
  nx: {gs["nx"]}\n\
  ny: {gs["ny"]}\n\
\n'

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

    if 'grdidx' in lst_varname_in:
            s += f'\
  fin_grdidx: {util.str_file_bin(gs[f"fin_grdidx"])}\n'

    s += f'\
  is_south_to_north: {gs["is_south_to_north"]}\n'

    if util.key_val_exist(gs, 'idx_miss'):
        s += f'\
  idx_miss: {gs["idx_miss"]}\n'

    s += f'\
\n\
  out_form: {out_form}\n\
  dir: "{dir_out}"\n'

    if 'idx' in lst_varname_out:
        s += f'\
  fout_grdidx: "grdidx{_landType}.bin"\n'
    if 'ara' in lst_varname_out:
        s += f'\
  fout_grdara: "grdara{_landType}.bin"\n'
    if 'xyz' in lst_varname_out:
        s += f'\
  fout_grdx  : "grdxyz{_landType}.bin", rec=1\n\
  fout_grdy  : "grdxyz{_landType}.bin", rec=2\n\
  fout_grdz  : "grdxyz{_landType}.bin", rec=3\n'
    if 'lonlat' in lst_varname_out:
        s += f'\
  fout_grdlon: "grdlonlat{_landType}.bin", rec=1\n\
  fout_grdlat: "grdlonlat{_landType}.bin", rec=2\n'

    s += f'\
  ara_miss: -1d20\n\
  xyz_miss: -1d20\n\
  lonlat_miss: -1d20\n\
[end]\n'

    return s


def block_gs_raster(gs, landType, lst_varname_in, dir_out, lst_varname_out):
    if landType is None:
        _landType = ''
    else:
        _landType = '_' + landType

    if 'grdidx' in lst_varname_in:
        out_form = 'index'
    else:
        out_form = 'auto'

    s = f'\
\n\
[grid_system_raster]\n\
  name: "{gs["name"]}"\n\
  nx: {gs["nx_raster"]}\n\
  ny: {gs["ny_raster"]}\n\
  west: {gs["west"]}\n\
  east: {gs["east"]}\n\
  south: {gs["south"]}\n\
  north: {gs["north"]}\n\
  is_south_to_north: {gs["is_south_to_north"]}\n\
\n\
  dir: "{gs["dir"]}"\n\
  fin_rstidx: {util.str_file_bin(gs[f"fin_rstidx"])}\n'

    if 'grdidx' in lst_varname_in:
            s += f'\
  fin_grdidx: {util.str_file_bin(gs[f"fin_grdidx"])}\n'

    if util.key_val_exist(gs, 'idx_miss'):
        s += f'\
  idx_miss: {gs["idx_miss"]}\n'

    s += f'\
  in_grid_sz: {gs["nx_grid"]}, {gs["ny_grid"]}\n'

    s += f'\
\n\
  out_form: {out_form}\n\
  dir: "{dir_out}"\n'

    if 'idx' in lst_varname_out:
        s += f'\
  fout_grdidx: "grdidx{_landType}.bin"\n'
    if 'ara' in lst_varname_out:
        s += f'\
  fout_grdara: "grdara{_landType}.bin"\n'
    if 'xyz' in lst_varname_out:
        s += f'\
  fout_grdx  : "grdxyz{_landType}.bin", rec=1\n\
  fout_grdy  : "grdxyz{_landType}.bin", rec=2\n\
  fout_grdz  : "grdxyz{_landType}.bin", rec=3\n'
    if 'lonlat' in lst_varname_out:
        s += f'\
  fout_grdlon: "grdlonlat{_landType}.bin", rec=1\n\
  fout_grdlat: "grdlonlat{_landType}.bin", rec=2\n'

    s += f'\
\n\
  ara_miss: -1d20\n\
  xyz_miss: -1d20\n\
  lonlat_miss: -1d20\n\
[end]\n'

    return s


def block_gs_polygon(gs, landType, lst_varname_in, dir_out, lst_varname_out):
    if landType is None:
        _landType = ''
    else:
        _landType = '_' + landType

    if 'grdidx' in lst_varname_in:
        out_form = 'index'
    else:
        out_form = 'auto'

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

    if util.key_val_exist(gs, 'coord_unit'):
        s += f'\
  coord_unit: {gs["coord_unit"]}\n'

    if util.key_val_exist(gs, 'coord_miss'):
        s += f'\
  coord_miss: {gs["coord_miss"]}\n'

    if 'grdidx' in lst_varname_in:
            s += f'\
  fin_grdidx: {util.str_file_bin(gs[f"fin_grdidx"])}\n'

    if util.key_val_exist(gs, 'idx_miss'):
        s += f'\
  idx_miss: {gs["idx_miss"]}\n'

    s += f'\
\n\
  out_form: {out_form}\n\
  dir: "{dir_out}"\n'

    if 'idx' in lst_varname_out:
        s += f'\
  fout_grdidx: "grdidx{_landType}.bin"\n'
    if 'ara' in lst_varname_out:
        s += f'\
  fout_grdara: "grdara{_landType}.bin"\n'
    if 'xyz' in lst_varname_out:
        s += f'\
  fout_grdx  : "grdxyz{_landType}.bin", rec=1\n\
  fout_grdy  : "grdxyz{_landType}.bin", rec=2\n\
  fout_grdz  : "grdxyz{_landType}.bin", rec=3\n'
    if 'lonlat' in lst_varname_out:
        s += f'\
  fout_grdlon: "grdlonlat{_landType}.bin", rec=1\n\
  fout_grdlat: "grdlonlat{_landType}.bin", rec=2\n'

    s += f'\
\n\
  ara_miss: -1d20\n\
  xyz_miss: -1d20\n\
  lonlat_miss: -1d20\n\
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
