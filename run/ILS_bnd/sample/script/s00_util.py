import os
import sys
import copy
import numpy as np

sys.path.append('../../../common')
import const, util

import s00_const as lconst


def adjust_settings(cnf):
    cmf = cnf['CaMa-Flood']
    #cmf['dir'] = os.path.join(cnf['dir_top'], cmf['dir'])
    cmf = dict(**cmf, **{
      'fout_grdidx_river': util.file_bin('grid/index_river.bin'),
      'fout_grdidx_noriv': util.file_bin('grid/index_noriv.bin'),
      'fout_rstidx_river': util.file_bin('raster/index_river.bin'),
      'fout_rstidx_noriv': util.file_bin('raster/index_noriv.bin'),
    })
    cnf['CaMa-Flood'] = cmf

    mat = {
      'name': 'MATSIRO',
      'type': 'raster',
      'nx_raster': cmf['nx_raster'],
      'ny_raster': cmf['ny_raster'],
      'nx_grid': cmf['nx_grid'],
      'ny_grid': cmf['ny_grid'],
      'west': cmf['west'], 
      'east': cmf['east'], 
      'south': cmf['south'], 
      'north': cmf['north'], 
      'is_south_to_north': cmf['is_south_to_north'],
      'mx_raster_1deg': int(cmf['nx_raster'] / (cmf['east']-cmf['west'])),  # used for MODIS
      'my_raster_1deg': int(cmf['ny_raster'] / (cmf['north']-cmf['south'])),
      'dir': f'{lconst.dir_tmp[util.istep("make_cmf_mat", lconst.job)]}/MATSIRO',
      'fout_grdmsk_river': util.file_bin('grid/land_mask_river.bin', 'real', 'big', None),
      'fout_grdmsk_noriv': util.file_bin('grid/land_mask_noriv.bin', 'real', 'big', None),
      'fout_grdidx_river': util.file_bin('grid/index_river.bin'),
      'fout_grdidx_noriv': util.file_bin('grid/index_noriv.bin'),
      'fout_rstidx_river': util.file_bin('raster/index_river.bin'),
      'fout_rstidx_noriv': util.file_bin('raster/index_noriv.bin'),
      'fout_grdidx_bnd_river': util.file_bin('grid/index_bnd_river.bin'),
      'fout_grdidx_bnd_noriv': util.file_bin('grid/index_bnd_noriv.bin'),
      'fout_grdidx_mkbnd_river': util.file_bin('grid/index_mkbnd_river.bin'),
      'fout_grdidx_mkbnd_noriv': util.file_bin('grid/index_mkbnd_noriv.bin'),
      'fout_rstidx_mkbnd_river': util.file_bin('raster/index_mkbnd_river.bin'),
      'fout_rstidx_mkbnd_noriv': util.file_bin('raster/index_mkbnd_noriv.bin'),
      'idx_miss': -9999,
    }
    cnf['MATSIRO'] = mat

    for landType in ['river', 'noriv']:
        cnf[f'MATSIRO_{landType}'] = {
          'name': f'MATSIRO_{landType}',
          'type': 'raster',
          'nx_raster': mat['nx_raster'],
          'ny_raster': mat['ny_raster'],
          'nx_grid': mat['nx_grid'],
          'ny_grid': mat['ny_grid'],
          'west': mat['west'], 
          'east': mat['east'], 
          'south': mat['south'], 
          'north': mat['north'], 
          'is_south_to_north': mat['is_south_to_north'],
          'mx_raster_1deg': mat['mx_raster_1deg'],
          'my_raster_1deg': mat['my_raster_1deg'],
          'dir': mat['dir'],
          'fin_rstidx': mat[f'fout_rstidx_{landType}'],
          'fin_grdidx': mat[f'fout_grdidx_{landType}'],
          'idx_miss': mat['idx_miss'],
        }

    util.join_topdir(cnf)


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


def get_raster_bounds(mat, twest, teast, tsouth, tnorth):
    dxi = int(np.floor((twest-mat['west']) * mat['mx_raster_1deg']+1))
    dxf = int(np.ceil((teast-mat['west']) * mat['mx_raster_1deg']))
    dyi = int(np.floor((mat['north']-tnorth) * mat['my_raster_1deg']+1))
    dyf = int(np.ceil((mat['north']-tsouth) * mat['my_raster_1deg']))

    return dxi, dxf, dyi, dyf

