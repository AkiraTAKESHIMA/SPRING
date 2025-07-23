import os
import sys
import copy
import numpy as np

sys.path.append('../../../common')
import const, util

import s00_const as lconst


def adjust_config(cnf):
    cmf = cnf['CMF']
    cmf = dict(**cmf, **{
      'fout_grdidx_river': util.file_bin('grid/index_river.bin'),
      'fout_grdidx_noriv': util.file_bin('grid/index_noriv.bin'),
      'fout_rstidx_river': util.file_bin('raster/index_river.bin'),
      'fout_rstidx_noriv': util.file_bin('raster/index_noriv.bin'),
    })
    cnf['CMF'] = cmf

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
      'dir': f'{os.getcwd()}/{lconst.dir_tmp[util.istep("make_cmf_mat")]}/MATSIRO',
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
          'fin_rstidx': mat[f'fout_rstidx_mkbnd_{landType}'],
          'fin_grdidx': mat[f'fout_grdidx_mkbnd_{landType}'],
          'idx_miss': mat['idx_miss'],
        }

    for dataName in cnf['input_data'].keys():
        d = cnf['input_data'][dataName]
        d['name'] = dataName

    cnf = util.join_topdir(cnf, cnf['dir_top'])

    return cnf
