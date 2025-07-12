import os
import sys
import subprocess
import json


def block_gs(gs, landType=None):
    if gs['type'] == 'latlon':
        return block_gs_latlon(gs)
    elif gs['type'] == 'raster':
        return block_gs_raster(gs, landType)
    elif gs['type'] == 'polygon':
        return block_gs_polygon(gs)



def block_gs_latlon(gs):
    s = f'\
\n\
[grid_system_latlon]\n\
  name: "{gs["name"]}"\n\
  nx: {gs["nx"]}\n\
  ny: {gs["ny"]}\n\
  dir: "{gs["directory"]}"\n'

    for key in ['f_lon_bound', 'f_lat_bound', 'fin_grdidx']:
        if key in gs.keys():
            f = gs[key]
            s += f'\
  {key}: "{f["path"]}", {f["dtype"]}, {f["rec"]}, {f["endian"]}\n'

    s += f'\
  is_south_to_north: {gs["is_south_to_north"]}\n\
[end]\n'

    return s


def block_gs_raster(gs, landType):
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
  dir: "{gs["directory"]}"\n\
  fin_rstidx: "rstidx_{landType}.bin"\n\
  fin_grdidx: "grdidx_{landType}.bin"\n\
  in_grid_sz: {gs["ncx"]}, {gs["ncy"]}\n\
  idx_miss: {gs["idx_miss"]}\n\
[end]\n'

    return s


def block_gs_polygon(gs):
    s = f'\
\n\
[grid_system_polygon]\n\
  name: "{gs["name"]}"\n\
  np: {gs["np"]}\n\
  nij: {gs["nij"]}\n\
\n\
  dir: "{gs["directory"]}"\n'

    if 'f_lon_vertex' in gs.keys():
        keys = ['f_lon_vertex', 'f_lat_vertex']
    elif 'f_x_vertex' in gs.keys():
        keys = ['f_x_vertex', 'f_y_vertex', 'f_z_vertex']
    for key in keys:
        f = gs[key]
        s += f'\
  {key}: "{f["path"]}", {f["dtype"]}, {f["rec"]}, {f["endian"]}\n'

    for key in ['f_arctyp', 'fin_grdidx']:
        if key in gs.keys():
            f = gs[key]
            s += f'\
  {key}: "{f["path"]}", {f["dtype"]}, {f["rec"]}, {f["endian"]}\n'

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
  f_send: "grid.bin", {rmp["dtype_idx"]}, rec=1\n\
  f_recv: "grid.bin", {rmp["dtype_idx"]}, rec=2\n\
  f_area: "area.bin"\n\
  f_coef: "coef.bin"\n\
\n\
  vrf_send_form: auto\n\
  fout_vrf_grdidx     : "vrf/send_idx.bin"\n\
  fout_vrf_grdara_true: "vrf/send_val.bin", rec=1\n\
  fout_vrf_grdara_rt  : "vrf/send_val.bin", rec=2\n\
  fout_vrf_rerr_grdara: "vrf/send_val.bin", rec=3\n\
\n\
  vrf_recv_form: auto\n\
  fout_vrf_grdidx     : "vrf/send_idx.bin"\n\
  fout_vrf_grdara_true: "vrf/send_val.bin", rec=1\n\
  fout_vrf_grdara_rt  : "vrf/send_val.bin", rec=2\n\
  fout_vrf_rerr_grdara: "vrf/send_val.bin", rec=3\n\
[end]\n'

    return s


def block_options(opt):
    s = '\
\n\
[options]\n\
  old_files: remove\n'

    if opt['earth']['shape'] == 'sphere':
        s += f'\
  earth_shape: {opt["earth"]["shape"]}\n\
  earth_r    : {opt["earth"]["r"]}\n'

    elif opt['earth']['shape'] == 'ellips':
        s += f'\
  earth_shape: {opt["earth"]["shape"]}\n\
  earth_r    : {opt["earth"]["r"]}\n\
  earth_e2   : {opt["earth"]["e2"]}\n'

    s += '\
[end]\n'

    return s
