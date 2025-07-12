import os
import sys

import s00_const as const
import s00_util as util
import s00_conf as conf


def block_gs(gs, step, landType=None):
    if gs['type'] == 'latlon':
        return block_gs_latlon(gs, step)
    elif gs['type'] == 'polygon':
        return block_gs_polygon(gs, step)
    elif gs['type'] == 'raster':
        return block_gs_raster(gs, step, landType)
    else:
        raise Exception(f'Invalid value in gs["type"]: {gs["type"]}')


def block_gs_latlon(gs, step):
    s = f'\
\n\
[grid_system_latlon]\n\
  nx: {gs["nx"]}\n\
  ny: {gs["ny"]}\n\
\n\
  dir: "{gs["directory"]}"\n\
  f_lon_bound: "{gs["f_lon_bound"]["path"]}"\n\
  f_lat_bound: "{gs["f_lat_bound"]["path"]}"\n\
  is_south_to_north: {gs["is_south_to_north"]}\n\
\n\
  out_form: auto\n\
  dir: "{const.directory["tmp"][step]["self"]}"\n\
  fout_grdidx: "grdidx.bin"\n\
  fout_grdara: "grdara.bin"\n\
  fout_grdx  : "grdxyz.bin", rec=1\n\
  fout_grdy  : "grdxyz.bin", rec=2\n\
  fout_grdz  : "grdxyz.bin", rec=3\n\
  fout_grdlon: "grdlonlat.bin", rec=1\n\
  fout_grdlat: "grdlonlat.bin", rec=2\n\
[end]\n'

    return s


def block_gs_polygon(gs, step):
    s = f'\
\n\
[grid_system_polygon]\n\
  np : {gs["np"]}\n\
  nij: {gs["nij"]}\n\
  dir: "{gs["directory"]}"\n'

    if 'f_lon_vertex' in gs.keys():
        keys = ['f_lon_vertex', 'f_lat_vertex']
    elif 'f_x_vertex' in gs.keys():
        keys = ['f_x_vertex', 'f_y_vertex', 'f_z_vertex']
    for key in keys:
        s += f'\
  {key}: "{gs[key]["path"]}", {gs[key]["dtype"]}, {gs[key]["rec"]}, {gs[key]["endian"]}\n'

    s += f'\
  coord_unit: {gs["coord_unit"]}\n\
  coord_miss: {gs["coord_miss"]}\n'

    if 'arc_parallel' in gs.keys():
        s += f'\
  arc_parallel: {gs["arc_parallel"]}\n'

    if 'fin_grdidx' in gs.keys():
      f = gs['fin_grdidx']
      s += f'\
  fin_grdidx: "{f["path"]}", {f["dtype"]}, {f["rec"]}, {f["endian"]}\n'

    s += f'\
  idx_miss: {gs["idx_miss"]}\n\
\n\
  out_form: index\n\
  dir: "{const.directory["tmp"][step]["self"]}"\n\
  fout_grdidx: "grdidx.bin"\n\
  fout_grdara: "grdara.bin"\n\
  fout_grdx  : "grdxyz.bin", rec=1\n\
  fout_grdy  : "grdxyz.bin", rec=2\n\
  fout_grdz  : "grdxyz.bin", rec=3\n\
  fout_grdlon: "grdlonlat.bin", rec=1\n\
  fout_grdlat: "grdlonlat.bin", rec=2\n\
[end]\n'

    return s


def block_gs_raster(gs, step, landType):
    s = f'\
\n\
[grid_system_raster]\n\
  name: {gs["name"]}\n\
  nx: {gs["nx"]}\n\
  ny: {gs["ny"]}\n\
  west: {gs["west"]}\n\
  east: {gs["east"]}\n\
  south: {gs["south"]}\n\
  north: {gs["north"]}\n\
  is_south_to_north: {gs["is_south_to_north"]}\n\
\n\
  dir: "{const.directory["tmp"][util.istep("make_idxmap_RM")]["self"]}"\n\
  fin_rstidx: "rstidx_{landType}.bin"\n\
  fin_grdidx: "grdidx_{landType}.bin"\n\
  in_grid_sz: {gs["ncx"]}, {gs["ncy"]}\n\
\n\
  out_form: index\n\
  dir: "{const.directory["tmp"][step]["self"]}"\n\
  fout_grdidx: "grdidx_{landType}.bin"\n\
  fout_grdara: "grdara_{landType}.bin"\n\
  fout_grdx  : "grdxyz_{landType}.bin", rec=1\n\
  fout_grdy  : "grdxyz_{landType}.bin", rec=2\n\
  fout_grdz  : "grdxyz_{landType}.bin", rec=3\n\
  fout_grdlon: "grdlonlat_{landType}.bin", rec=1\n\
  fout_grdlat: "grdlonlat_{landType}.bin", rec=2\n\
\n\
  idx_miss: -9999\n\
  ara_miss: -1d20\n\
  xyz_miss: -1d20\n\
  lonlat_miss: -1d20\n\
[end]\n'

    return s
