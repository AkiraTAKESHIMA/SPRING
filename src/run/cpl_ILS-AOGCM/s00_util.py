import os
import sys
import shutil
import copy

import util

import s00_const as lconst


def adjust_config(cnf):
    cnf['RM']['_dir'] = lconst.dir_tmp[util.istep('make_idxmap_RM', lconst.job)]

    m = 'RM'
    cnf['RM_pre'] = {
      'nx_raster': cnf[m]['nx_raster'],
      'ny_raster': cnf[m]['ny_raster'],
      'nx_grid': cnf[m]['nx_grid'],
      'ny_grid': cnf[m]['ny_grid'],
      'west' : cnf[m]['west'],
      'east' : cnf[m]['east'],
      'south': cnf[m]['south'],
      'north': cnf[m]['north'],
      'dir_in': f'{lconst.dir_tmp[util.istep("run_FLOW", lconst.job)]}',
      'fin_catmxy': util.file_bin('tmp/map/1min/catmxy.bin'),
      'fin_nextxy': util.file_bin('map/nextxy.bin'),
      'fin_basin' : util.file_bin('map/basin.bin'),
      'catmxy_index': {
        'noriv_coastal': 0,
        'noriv_inland' : -1,
        'ocean'        : -9999,
      },
      'nextxy_index': {
        'river_mouth' : -9,
        'river_inland': -10,
        'ocean'       : -9999,
      },
      'dir_out': f'{lconst.dir_tmp[util.istep("make_idxmap_RM", lconst.job)]}',
      'fout_grdidx_river'    : util.file_bin('grdidx_river.bin'),
      'fout_grdidx_river_end': util.file_bin('grdidx_river_end.bin'),
      'fout_grdidx_noriv'    : util.file_bin('grdidx_noriv.bin'),
      'fout_grdidx_ocean'    : util.file_bin('grdidx_ocean.bin'),
      'fout_rstidx_river'    : util.file_bin('rstidx_river.bin'),
      'fout_rstidx_river_end': util.file_bin('rstidx_river_end.bin'),
      'fout_rstidx_noriv'    : util.file_bin('rstidx_noriv.bin'),
      'fout_rstidx_ocean'    : util.file_bin('rstidx_ocean.bin'),
      'fout_rstbsn'          : util.file_bin('rstbsn.bin'),
      'idx_miss': cnf[m]['idx_miss'],
    }

    cnf['LSM']['_dir'] = lconst.dir_tmp[util.istep('define_LSM', lconst.job)]

    for key in cnf['OGCM_ocean'].keys():
        if key not in cnf['OGCM_land'].keys():
            cnf['OGCM_land'][key] = copy.deepcopy(cnf['OGCM_ocean'][key])

    if cnf['AGCM']['type'] == 'latlon':
        cnf['AGCM']['nij'] = cnf['AGCM']['nx'] * cnf['AGCM']['ny']

    for i, landType in enumerate(['river', 'noriv']):
        m = 'LSM'
        cnf[f'LSM_bnd_{landType}'] = {
          'name': f'LSM_bnd_{landType}',
          'type': 'latlon',
          'nx': cnf[m]['nx_grid'],
          'ny': cnf[m]['ny_grid'],
          'west' : cnf[m]['west'],
          'east' : cnf[m]['east'],
          'south': cnf[m]['south'],
          'north': cnf[m]['north'],
          'is_south_to_north': cnf[m]['is_south_to_north'],
          '_dir': f'{lconst.dir_tmp[util.istep("define_LSM", lconst.job)]}/LSM',
          'fin_grdidx': util.file_bin(f'grdidx_bnd_{landType}.bin'),
          'idx_miss': cnf[m]['idx_miss'],
        }

        cnf[f'IO_LSM_bnd_{landType}'] = {
          'name': f'IO_LSM_bnd_{landType}',
          'type': 'latlon',
          'nx': cnf[m]['nx_grid'],
          'ny': cnf[m]['ny_grid'],
          'west' : cnf[m]['west'],
          'east' : cnf[m]['east'],
          'south': cnf[m]['south'],
          'north': cnf[m]['north'],
          'is_south_to_north': cnf[m]['is_south_to_north'],
          'idx_bgn': (cnf[m]['nx_grid']*cnf[m]['ny_grid'])*i + 1,
        }

    for landType in ['river', 'river_end', 'noriv', 'ocean']:
        m = 'RM'
        cnf[f'RM_{landType}'] = {
          'name': f'RM_{landType}',
          'type': 'raster',
          'nx_raster': cnf[m]['nx_raster'],
          'ny_raster': cnf[m]['ny_raster'],
          'nx_grid': cnf[m]['nx_grid'],
          'ny_grid': cnf[m]['ny_grid'],
          'west' : cnf[m]['west'],
          'east' : cnf[m]['east'],
          'south': cnf[m]['south'],
          'north': cnf[m]['north'],
          'is_south_to_north': cnf[m]['is_south_to_north'],
          '_dir': f'{lconst.dir_tmp[util.istep("make_idxmap_RM", lconst.job)]}',
          'fin_rstidx' : util.file_bin(f'rstidx_{landType}.bin'),
          'fin_grdidx' : util.file_bin(f'grdidx_{landType}.bin'),
          'fout_grdidx': util.file_bin('grdidx_{landType}.bin'),
          'fout_grdara': util.file_bin('grdara_{landType}.bin'),
          'fout_grdx'  : util.file_bin('grdxyz_{landType}.bin', rec=1),
          'fout_grdy'  : util.file_bin('grdxyz_{landType}.bin', rec=2),
          'fout_grdz'  : util.file_bin('grdxyz_{landType}.bin', rec=3),
          'fout_grdlon': util.file_bin('grdlonlat_{landType}.bin', rec=1),
          'fout_grdlat': util.file_bin('grdlonlat_{landType}.bin', rec=2),
          'idx_miss': cnf['RM']['idx_miss'],
        }

    for landType in ['river', 'noriv', 'noriv_real', 'noriv_virt']:
        m = 'LSM'
        cnf[f'LSM_{landType}'] = {
          'name': f'LSM_{landType}',
          'type': 'raster',
          'nx_raster': cnf[m]['nx_raster'],
          'ny_raster': cnf[m]['ny_raster'],
          'nx_grid': cnf[m]['nx_grid'],
          'ny_grid': cnf[m]['ny_grid'],
          'west' : cnf[m]['west'],
          'east' : cnf[m]['east'],
          'south': cnf[m]['south'],
          'north': cnf[m]['north'],
          'is_south_to_north': cnf[m]['is_south_to_north'],
          '_dir': f'{lconst.dir_tmp[util.istep("define_LSM", lconst.job)]}/LSM',
          'fin_rstidx'   : util.file_bin(f'rstidx_{landType}.bin'),
          'fin_grdidx'   : util.file_bin(f'grdidx_{landType}.bin'),
          'fin_grdara'   : util.file_bin(f'grdara_{landType}.bin'),
          'fin_grdlonlat': util.file_bin(f'grdlonlat_{landType}.bin'),
          'idx_miss': cnf[m]['idx_miss'],
        }

        m = 'LSM'
        cnf[f'LSM_latlon_{landType}'] = {
          'name': f'LSM_{landType}',
          'type': 'latlon',
          'nx': cnf[m]['nx_grid'],
          'ny': cnf[m]['ny_grid'],
          'nij': cnf[m]['nx_grid']*cnf[m]['ny_grid'],
          'west' : cnf[m]['west'],
          'east' : cnf[m]['east'],
          'south': cnf[m]['south'],
          'north': cnf[m]['north'],
          'is_south_to_north': cnf[m]['is_south_to_north'],
          '_dir': f'{lconst.dir_tmp[util.istep("make_grid_data_LSM", lconst.job)]}',
          'fin_grdidx'   : util.file_bin(f'grdidx_{landType}.bin'),
          'fin_grdara'   : util.file_bin(f'grdara_{landType}.bin'),
          'fin_grdlonlat': util.file_bin(f'grdlonlat_{landType}.bin'),
          'idx_miss': cnf[m]['idx_miss'],
        }

    for landType in ['river', 'river_end', 'noriv', 'ocean']:
        m = 'RM'
        cnf[f'RM_latlon_{landType}'] = {
          'name': f'RM_latlon_{landType}',
          'type': 'latlon',
          'nx': cnf[m]['nx_grid'],
          'ny': cnf[m]['ny_grid'],
          'nij': cnf[m]['nx_grid']*cnf[m]['ny_grid'],
          'west' : cnf[m]['west'],
          'east' : cnf[m]['east'],
          'south': cnf[m]['south'],
          'north': cnf[m]['north'],
          'is_south_to_north': cnf[m]['is_south_to_north'],
          '_dir': f'{lconst.dir_tmp[util.istep("make_grid_data_RM", lconst.job)]}',
          'fin_grdidx'   : util.file_bin(f'grdidx_{landType}.bin'),
          'fin_grdara'   : util.file_bin(f'grdara_{landType}.bin'),
          'fin_grdlonlat': util.file_bin(f'grdlonlat_{landType}.bin'),
          'idx_miss': cnf[m]['idx_miss'],
        }

    m = 'LSM'
    cnf['IO_LSM_row'] = {
      'name': 'IO_LSM_row',
      'type': 'latlon',
      'nx': cnf[m]['nx_grid'],
      'ny': cnf[m]['ny_grid'],
      'west' : cnf[m]['west'],
      'east' : cnf[m]['east'],
      'south': cnf[m]['south'],
      'north': cnf[m]['north'],
      'is_south_to_north': cnf[m]['is_south_to_north'],
    }

    m = 'RM'
    cnf['IO_RM_row'] = {
      'name': 'IO_RM_row',
      'type': 'latlon',
      'nx': cnf[m]['nx_grid'],
      'ny': cnf[m]['ny_grid'],
      'west' : cnf[m]['west'],
      'east' : cnf[m]['east'],
      'south': cnf[m]['south'],
      'north': cnf[m]['north'],
      'is_south_to_north': cnf[m]['is_south_to_north'],
    }

    util.join_topdir(cnf)
