import os
import sys
import copy

import const, util
from const import k_lt, k_gs, k_rt, k_rtc, k_int_rt, k_opt
from util import istep, file_bin
import pyconf

import s00_const as lconst


def adjust_config(cnf):

    cnf = pyconf.substitute_gs_landType(cnf)
    cnf = pyconf.copy_gs_shared(cnf)
    #cnf = pyconf.set_rt_default(cnf)
    cnf = pyconf.join_topdir(cnf, cnf['dir_top'])

    return cnf


    cmf = cnf[k_gs]['CMF']
    cmf = dict(**cmf, **{
      'fout_grdidx_river': file_bin('grid/index_river.bin'),
      'fout_grdidx_noriv': file_bin('grid/index_noriv.bin'),
      'fout_rstidx_river': file_bin('raster/index_river.bin'),
      'fout_rstidx_noriv': file_bin('raster/index_noriv.bin'),
    })
    cnf[k_gs]['CMF'] = cmf

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
      #'dir': f'{os.getcwd()}/{lconst.dir_tmp[util.istep("make_cmf_mat")]}/MATSIRO',
      '_dir': f'{lconst.dir_tmp[istep("make_cmf_mat")]}/MATSIRO',
      'fout_grdmsk_river': file_bin('grid/land_mask_river.bin', 'real', None, 'big'),
      'fout_grdmsk_noriv': file_bin('grid/land_mask_noriv.bin', 'real', None, 'big'),
      'fout_grdidx_river': file_bin('grid/index_river.bin'),
      'fout_grdidx_noriv': file_bin('grid/index_noriv.bin'),
      'fout_rstidx_river': file_bin('raster/index_river.bin'),
      'fout_rstidx_noriv': file_bin('raster/index_noriv.bin'),
      'fout_grdidx_bnd_river': file_bin('grid/index_bnd_river.bin'),
      'fout_grdidx_bnd_noriv': file_bin('grid/index_bnd_noriv.bin'),
      'fout_grdidx_mkbnd_river': file_bin('grid/index_mkbnd_river.bin'),
      'fout_grdidx_mkbnd_noriv': file_bin('grid/index_mkbnd_noriv.bin'),
      'fout_rstidx_mkbnd_river': file_bin('raster/index_mkbnd_river.bin'),
      'fout_rstidx_mkbnd_noriv': file_bin('raster/index_mkbnd_noriv.bin'),
      'idx_miss': -9999,
    }
    cnf[k_gs]['MATSIRO'] = mat

    for landType in ['river', 'noriv']:
        cnf[k_gs][f'MATSIRO_{landType}'] = {
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
          '_dir': mat['_dir'],
          'fin_rstidx': mat[f'fout_rstidx_mkbnd_{landType}'],
          'fin_grdidx': mat[f'fout_grdidx_mkbnd_{landType}'],
          'idx_miss': mat['idx_miss'],
        }

    for dataName in cnf['input_data'].keys():
        d = cnf['input_data'][dataName]
        d['name'] = dataName

    cnf = pyconf.join_topdir(cnf, cnf['dir_top'])

    return cnf
