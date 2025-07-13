import os
import sys
import copy
import numpy as np

import s00_const as lconst


def set_matsiro(cmf):
    return dict(
      ncx = cmf['ncx'], ncy = cmf['ncy'],
      ndx = cmf['ndx'], ndy = cmf['ndy'],
      west = cmf['west'], east = cmf['east'], south = cmf['south'], north = cmf['north'],
      mdx_1deg = int(cmf['ndx'] / (cmf['east']-cmf['west'])),
      mdy_1deg = int(cmf['ndy'] / (cmf['north']-cmf['south'])),
    )


def adjust_settings(cnf):
    cnf['CaMa-Flood']['dir'] = os.path.join(cnf['dir_top'], cnf['CaMa-Flood']['dir'])
    cnf['MATSIRO'] = set_matsiro(cnf['CaMa-Flood'])


def get_tile_bbox_latlon(tileName):
    if tileName[0] not in ['W','E'] or tileName[4] not in ['N','S'] or len(tileName) != 7:
        raise Exception(('Unexpected format of tileName: {}'\
                       '\ntileName must be like "W140N40" or "E010S20".').format(tileName))

    if tileName[0] == 'W':
        west = -int(tileName[1:4])
    else:
        west = int(tileName[1:4])
    east = west + 10

    if tileName[4] == 'N':
        north = int(tileName[5:7])
    else:
        north = -int(tileName[5:7])
    south = north - 10

    return west, east, south, north


def get_raster_bounds(twest, teast, tsouth, tnorth, matsiro):
    dxi = int(np.floor((twest-matsiro['west']) * matsiro['mdx_1deg']+1))
    dxf = int(np.ceil((teast-matsiro['west']) * matsiro['mdx_1deg']))
    dyi = int(np.floor((matsiro['north']-tnorth) * matsiro['mdy_1deg']+1))
    dyf = int(np.ceil((matsiro['north']-tsouth) * matsiro['mdy_1deg']))

    return dxi, dxf, dyi, dyf


def head(landType, tileName, dir_out):
    return f'\
#\n\
path_report: "{dir_out}/report/{tileName}_{landType}.txt"\n\
'


def block_gs_latlon(gs, twest, teast, tsouth, tnorth):
    return f'\
\n\
[grid_system_latlon]\n\
  nx: {{nx}}\n\
  ny: {{ny}}\n\
  west: {twest}\n\
  east: {teast}\n\
  south: {tsouth}\n\
  north: {tnorth}\n\
  is_south_to_north: {{is_south_to_north}}\n\
[end]\n\
'.format(**gs)


def block_gs_modis(modis, f_lon, f_lat):
    return f'\
\n\
[grid_system_polygon]\n\
  np: 4\n\
  nij: 1440000\n\
  dir: "{{dir_coord}}"\n\
  f_lon_vertex: "{f_lon}"\n\
  f_lat_vertex: "{f_lat}"\n\
  coord_unit: degree\n\
  coord_miss: -999.d0\n\
  arc_parallel: True\n\
[end]\n\
'.format(**modis)


def block_gs_raster(landType, dir_out, matsiro, dxi, dxf, dyi, dyf):
    return f'\
\n\
[grid_system_raster]\n\
  nx: {{ndx}}\n\
  ny: {{ndy}}\n\
  west: {{west}}\n\
  east: {{east}}\n\
  south: {{south}}\n\
  north: {{north}}\n\
  xi: {dxi}\n\
  xf: {dxf}\n\
  yi: {dyi}\n\
  yf: {dyf}\n\
  dir: "{dir_out}/MATSIRO"\n\
  fin_rstidx: "raster/index_mkbnd_{landType}.bin"\n\
  fin_grdidx: "grid/index_mkbnd_{landType}.bin"\n\
  in_grid_sz: {{ncx}}, {{ncy}}\n\
  grdidx_condition: raster_in_grid\n\
  idx_miss: -9999\n\
  is_south_to_north: False\n\
[end]\n\
'.format(**matsiro)


def block_remapping(landType, tileName, dir_out, make_verification_data):
    s = f'\
\n\
[remapping]\n\
  dir: "{dir_out}/rt"\n\
  fout_rt_sidx: "mapping_table_idx_{tileName}_{landType}.bin", int4, 1, big\n\
  fout_rt_tidx: "mapping_table_idx_{tileName}_{landType}.bin", int4, 2, big\n\
  fout_rt_area: "mapping_table_area_{tileName}_{landType}.bin", dble, 1, big\n\
  fout_rt_coef: "mapping_table_coef_{tileName}_{landType}.bin", dble, 1, big\n\
\n\
  allow_empty: True\n\
'

    if make_verification_data:
        s += f'\
\n\
  vrf_source_form: auto\n\
  fout_vrf_grdidx     : "vrf/src_idx_{tileName}_{landType}.bin", int4, 1, little\n\
  fout_vrf_grdara_true: "vrf/src_val_{tileName}_{landType}.bin", real, 1, little\n\
  fout_vrf_grdara_rt  : "vrf/src_val_{tileName}_{landType}.bin", real, 2, little\n\
  fout_vrf_rerr_grdara: "vrf/src_val_{tileName}_{landType}.bin", real, 3, little\n\
\n\
  vrf_target_form: auto\n\
  fout_vrf_grdidx     : "vrf/tgt_idx_{tileName}_{landType}.bin", int4, 1, little\n\
  fout_vrf_grdara_true: "vrf/tgt_val_{tileName}_{landType}.bin", real, 1, little\n\
  fout_vrf_grdara_rt  : "vrf/tgt_val_{tileName}_{landType}.bin", real, 2, little\n\
  fout_vrf_rerr_grdara: "vrf/tgt_val_{tileName}_{landType}.bin", real, 3, little\n\
'

    s += '\
[end]\n\
'

    return s


def block_options(earth):
    s = '\
\n\
[options]\n\
  old_files: remove\n\
'

    if earth['shape'] == 'sphere':
        s += '\
  earth_shape: {shape}\n\
  earth_r: {r}\n\
'.format(**earth)

    elif earth['shape'] == 'ellips':
        s += '\
  earth_shape: {shape}\n\
  earth_r: {r}\n\
  earth_e2: {e2}\n\
'

    s += '\
[end]\n\
'
    return s


def conf_remap_latlon(tileName, landType, dataName, data_in, matsiro, options):
    if tileName is None:
        twest, teast, tsouth, tnorth \
          = data_in['west'], data_in['east'], data_in['south'], data_in['north']
    else:
        twest, teast, tsouth, tnorth = get_tile_bbox_latlon(tileName)

    dxi, dxf, dyi, dyf = get_raster_bounds(twest, teast, tsouth, tnorth, matsiro)

    dir_out_this = os.path.join(lconst.dir_tmp[2], dataName)

    s = head(landType, tileName, dir_out_this)

    s += block_gs_latlon(data_in, twest, teast, tsouth, tnorth)

    s += block_gs_raster(landType, lconst.dir_tmp[1], matsiro, dxi, dxf, dyi, dyf)

    s += block_remapping(landType, tileName, dir_out_this, options['make_verification_data'])

    s += block_options(options['earth'])

    return s


def conf_remap_modis(tileName, landType, dataName, data_in, matsiro, options):
    f_lon = os.path.join(data_in['dir_coords'], f'lon_{tileName}.bin')
    f_lat = os.path.join(data_in['dir_coords'], f'lat_{tileName}.bin')

    lon = np.ma.masked_equal(np.fromfile(f_lon),-999)
    lat = np.ma.masked_equal(np.fromfile(f_lat),-999)

    dxi, dxf, dyi, dyf = get_raster_bounds(lon.min(), lon.max(), lat.min(), lat.max(), matsiro)

    del(lon, lat)

    dir_out_this = os.path.join(lconst.dir_tmp[2], dataName)

    s = head(landType, tileName, dir_out_this)

    s += block_gs_modis(modis, f_lon, f_lat)

    s += block_gs_raster(landType, lconst.dir_tmp[1], matsiro, dxi, dxf, dyi, dyf)

    s += block_remapping(landType, tileName, dir_out_this, options['make_verification_data'])

    s += block_options(options['earth'])

    return s


