import os
import sys
import shutil
import copy

import s00_const as lconst


def adjust_config(cnf):
    cnf['RM']['dir'] = lconst.dir_tmp[istep('make_idxmap_RM')]

    cnf['LSM']['dir'] = lconst.dir_tmp[istep('define_LSM')]

    for key in cnf['OGCM_ocean'].keys():
        if key not in cnf['OGCM_land'].keys():
            cnf['OGCM_land'][key] = copy.deepcopy(cnf['OGCM_ocean'][key])

    if cnf['AGCM']['type'] == 'latlon':
        cnf['AGCM']['nij'] = cnf['AGCM']['nx'] * cnf['AGCM']['ny']

    for i, landType in enumerate(['river', 'noriv']):
        cnf[f'LSM_bnd_{landType}'] = {
          'name': f'LSM_bnd_{landType}',
          'type': 'latlon',
          'nx': cnf['LSM']['ncx'],
          'ny': cnf['LSM']['ncy'],
          'west' : cnf['LSM']['west'],
          'east' : cnf['LSM']['east'],
          'south': cnf['LSM']['south'],
          'north': cnf['LSM']['north'],
          'is_south_to_north': cnf['LSM']['is_south_to_north'],
          'dir': f'{lconst.dir_tmp[istep("define_LSM")]}/LSM',
          'fin_grdidx': {
            'path': f'grdidx_bnd_{landType}.bin'
          },
          'idx_miss': cnf['LSM']['idx_miss'],
        }

        cnf[f'IO_LSM_bnd_{landType}'] = {
          'name': f'IO_LSM_bnd_{landType}',
          'type': 'latlon',
          'west' : cnf['LSM']['west'],
          'east' : cnf['LSM']['east'],
          'south': cnf['LSM']['south'],
          'north': cnf['LSM']['north'],
          'nx': cnf['LSM']['ncx'],
          'ny': cnf['LSM']['ncy'],
          'is_south_to_north': cnf['LSM']['is_south_to_north'],
          'idx_bgn': (cnf['LSM']['ncx']*cnf['LSM']['ncy'])*i + 1,
        }

    for landType in ['river', 'noriv', 'ocean']:
        cnf[f'RM_{landType}'] = {
          'name': f'RM_{landType}',
          'type': 'raster',
          'nx': cnf['RM']['nx'],
          'ny': cnf['RM']['ny'],
          'ncx': cnf['RM']['ncx'],
          'ncy': cnf['RM']['ncy'],
          'west' : cnf['RM']['west'],
          'east' : cnf['RM']['east'],
          'south': cnf['RM']['south'],
          'north': cnf['RM']['north'],
          'is_south_to_north': cnf['RM']['is_south_to_north'],
          'dir': f'{lconst.dir_tmp[istep("make_idxmap_RM")]}',
          'fin_rstidx': {'path': f'rstidx_{landType}.bin'},
          'fin_grdidx': {'path': f'grdidx_{landType}.bin'},
          'idx_miss': cnf['RM']['idx_miss'],
        }

    for landType in ['river', 'noriv', 'noriv_real', 'noriv_virt']:
        cnf[f'LSM_{landType}'] = {
          'name': f'LSM_{landType}',
          'type': 'raster',
          'nx': cnf['LSM']['nx'],
          'ny': cnf['LSM']['ny'],
          'ncx': cnf['LSM']['ncx'],
          'ncy': cnf['LSM']['ncy'],
          'west' : cnf['LSM']['west'],
          'east' : cnf['LSM']['east'],
          'south': cnf['LSM']['south'],
          'north': cnf['LSM']['north'],
          'is_south_to_north': cnf['LSM']['is_south_to_north'],
          'dir': f'{lconst.dir_tmp[istep("define_LSM")]}/LSM',
          'fin_rstidx'   : {'path': f'rstidx_{landType}.bin'},
          'fin_grdidx'   : {'path': f'grdidx_{landType}.bin'},
          'fin_grdara'   : {'path': f'grdara_{landType}.bin'},
          'fin_grdlonlat': {'path': f'grdlonlat_{landType}.bin'},
          'idx_miss': cnf['LSM']['idx_miss'],
        }

        cnf[f'LSM_latlon_{landType}'] = {
          'name': f'LSM_{landType}',
          'type': 'latlon',
          'nx': cnf['LSM']['ncx'],
          'ny': cnf['LSM']['ncy'],
          'nij': cnf['LSM']['ncx']*cnf['LSM']['ncy'],
          'west' : cnf['LSM']['west'],
          'east' : cnf['LSM']['east'],
          'south': cnf['LSM']['south'],
          'north': cnf['LSM']['north'],
          'is_south_to_north': cnf['LSM']['is_south_to_north'],
          'dir': f'{lconst.dir_tmp[istep("define_LSM")]}/LSM',
          'fin_grdidx'   : {'path': f'grdidx_{landType}.bin'},
          'fin_grdara'   : {'path': f'grdara_{landType}.bin'},
          'fin_grdlonlat': {'path': f'grdlonlat_{landType}.bin'},
          'idx_miss': cnf['LSM']['idx_miss'],
        }

    for landType in ['river', 'river_end', 'noriv', 'ocean']:
        cnf[f'RM_latlon_{landType}'] = {
          'name': f'RM_latlon_{landType}',
          'type': 'latlon',
          'nx': cnf['RM']['ncx'],
          'ny': cnf['RM']['ncy'],
          'nij': cnf['RM']['ncx']*cnf['RM']['ncy'],
          'west' : cnf['RM']['west'],
          'east' : cnf['RM']['east'],
          'south': cnf['RM']['south'],
          'north': cnf['RM']['north'],
          'is_south_to_north': cnf['RM']['is_south_to_north'],
          'fin_grdidx'   : {'path': f'{lconst.dir_tmp[istep("make_idxmap_RM")]}/grdidx_{landType}.bin'},
          'fin_grdara'   : {'path': f'{lconst.dir_tmp[istep("make_grid_data_RM")]}/grdara_{landType}.bin'},
          'fin_grdlonlat': {'path': f'{lconst.dir_tmp[istep("make_grid_data_RM")]}/grdlonlat_{landType}.bin'},
          'idx_miss': cnf['RM']['idx_miss'],
        }

    cnf['IO_LSM_row'] = {
      'name': 'IO_LSM_row',
      'type': 'latlon',
      'nx': cnf['LSM']['ncx'],
      'ny': cnf['LSM']['ncy'],
      'west' : cnf['LSM']['west'],
      'east' : cnf['LSM']['east'],
      'south': cnf['LSM']['south'],
      'north': cnf['LSM']['north'],
      'is_south_to_north': cnf['LSM']['is_south_to_north'],
    }
    
    cnf['IO_RM_row'] = {
      'name': 'IO_RM_row',
      'type': 'latlon',
      'nx': cnf['RM']['ncx'],
      'ny': cnf['RM']['ncy'],
      'west' : cnf['RM']['west'],
      'east' : cnf['RM']['east'],
      'south': cnf['RM']['south'],
      'north': cnf['RM']['north'],
      'is_south_to_north': cnf['RM']['is_south_to_north'],
    }
    


def istep(name):
    for key in lconst.job.keys():
        if lconst.job[key] == name:
            return key
    raise Exception(f'Invalid value in $name: {name}')

