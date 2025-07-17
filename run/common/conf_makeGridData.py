import os
import sys

import util


def head(dir_out):
    s = f'\
#\n\
path_report: "{dir_out}/report.txt"\n\
'

    return s


def block_gs(gs, landType, lst_varname_in, dir_out, lst_varname_out):
    if gs['type'] == 'latlon':
        pass
    elif gs['type'] == 'raster':
        return block_gs_raster(gs, landType, lst_varname_in, dir_out, lst_varname_out)
    elif gs['type'] == 'polygon':
        pass


def block_gs_raster(gs, landType, lst_varname_in, dir_out, lst_varname_out):
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
\n\
  dir: "{gs["dir"]}"\n'

    for var in ['rstidx', 'grdidx']:
        if var not in lst_varname_in: continue
        if f'fin_{var}' not in gs.keys():
           raise Exception(f'KeyError: `fin_{var}`')
        s += f'\
  fin_{var}: {util.str_file_bin(gs[f"fin_{var}"])}\n'

    s += f'\
  in_grid_sz: {gs["ncx"]}, {gs["ncy"]}\n'

    s += f'\
\n\
  out_form: index\n\
  dir: "{dir_out}"\n'

    if 'idx' in lst_varname_out:
        s += f'\
  fout_grdidx: "grdidx_{landType}.bin"\n'
    if 'ara' in lst_varname_out:
        s += f'\
  fout_grdara: "grdara_{landType}.bin"\n'
    if 'xyz' in lst_varname_out:
        s += f'\
  fout_grdx  : "grdxyz_{landType}.bin", rec=1\n\
  fout_grdy  : "grdxyz_{landType}.bin", rec=2\n\
  fout_grdz  : "grdxyz_{landType}.bin", rec=3\n'
    if 'lonlat' in lst_varname_out:
        s += f'\
  fout_grdlon: "grdlonlat_{landType}.bin", rec=1\n\
  fout_grdlat: "grdlonlat_{landType}.bin", rec=2\n'

    s += f'\
\n\
  idx_miss: -9999\n\
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
