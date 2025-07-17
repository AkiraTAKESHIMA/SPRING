import os
import sys
import shutil
import copy

import s00_const as lconst


def adjust_config(cnf):
    for i, landType in enumerate(['river', 'noriv']):
        m = 'MATSIRO'
        cnf[f'MATSIRO_bnd_{landType}'] = {
          'name': f'MATSIRO_bnd_{landType}',
          'type': 'latlon',
          'nx': cnf[m]['ncx'],
          'ny': cnf[m]['ncy'],
          'west': cnf[m]['west'],
          'east': cnf[m]['east'],
          'south': cnf[m]['south'],
          'north': cnf[m]['north'],
          'is_south_to_north': cnf[m]['is_south_to_north'],
          'dir': cnf[m]['dir'],
          'fin_grdidx': cnf[m][f'f_grdidx_bnd_{landType}'],
          'idx_miss': cnf[m]['idx_miss'],
        }

        m = 'MATSIRO'
        cnf[f'MATSIRO_{landType}'] = {
          'name': f'MATSIRO_{landType}',
          'type': 'raster',
          'nx': cnf[m]['nx'],
          'ny': cnf[m]['ny'],
          'ncx': cnf[m]['ncx'],
          'ncy': cnf[m]['ncy'],
          'west': cnf[m]['west'],
          'east': cnf[m]['east'],
          'south': cnf[m]['south'],
          'north': cnf[m]['north'],
          'is_south_to_north': cnf[m]['is_south_to_north'],
          'dir': cnf[m]['dir'],
          'fin_rstidx': cnf[m][f'f_rstidx_{landType}'],
          'fin_grdidx': cnf[m][f'f_grdidx_{landType}'],
          'fin_grdara': cnf[m][f'f_grdara_{landType}'],
          'idx_miss': cnf[m]['idx_miss'],
        }

        m = 'MATSIRO'
        cnf[f'MATSIRO_latlon_{landType}'] = {
          'name': f'MATSIRO_latlon_{landType}',
          'type': 'latlon',
          'nx': cnf[m]['ncx'],
          'ny': cnf[m]['ncy'],
          'west': cnf[m]['west'],
          'east': cnf[m]['east'],
          'south': cnf[m]['south'],
          'north': cnf[m]['north'],
          'is_south_to_north': cnf[m]['is_south_to_north'],
          'dir': cnf[m]['dir'],
          'fin_grdidx': cnf[m][f'f_grdidx_{landType}'],
          'idx_miss': cnf[m]['idx_miss'],
        }

        m = 'MATSIRO'
        cnf[f'IO_MATSIRO_bnd_{landType}'] = {
          'name': f'IO_MATSIRO_bnd_{landType}',
          'type': 'latlon',
          'nx': cnf[m]['ncx'],
          'ny': cnf[m]['ncy'],
          'west': cnf[m]['west'],
          'east': cnf[m]['east'],
          'south': cnf[m]['south'],
          'north': cnf[m]['north'],
          'is_south_to_north': cnf[m]['is_south_to_north'],
          'idx_bgn': (cnf[m]['ncx']*cnf[m]['ncy'])*i + 1,
          'dir': cnf[m]['dir'],
        }

        m = 'MATSIRO'
        cnf[f'IO_MATSIRO_row_{landType}'] = {
          'name': f'IO_MATSIRO_row_{landType}',
          'type': 'latlon', 
          'nx': cnf[m]['ncx'],
          'ny': cnf[m]['ncy'],
          'west': cnf[m]['west'],
          'east': cnf[m]['east'],
          'south': cnf[m]['south'],
          'north': cnf[m]['north'],
          'is_south_to_north': cnf[m]['is_south_to_north'],
          'dir': cnf[m]['dir'],
          'fin_grdara': cnf[m][f'f_grdara_{landType}'],
        }

    for landType in ['river']:
        m = 'CaMa-Flood'
        cnf[f'CaMa-Flood_{landType}'] = {
          'name': f'CaMa-Flood_{landType}',
          'type': 'raster',
          'ncx': cnf[m]['ncx'],
          'ncy': cnf[m]['ncy'],
          'nx': cnf[m]['nx'],
          'ny': cnf[m]['ny'],
          'west': cnf[m]['west'],
          'east': cnf[m]['east'],
          'south': cnf[m]['south'],
          'north': cnf[m]['north'],
          'is_south_to_north': cnf[m]['is_south_to_north'],
          'dir': cnf[m]['dir'],
          'fin_rstidx': cnf[m][f'f_rstidx_{landType}'],
          'fin_grdidx': cnf[m][f'f_grdidx_{landType}'],
          'idx_miss': cnf[m]['idx_miss'],
        }

        m = 'CaMa-Flood'
        cnf[f'CaMa-Flood_latlon_{landType}'] = {
          'name': f'CaMa-Flood_latlon_{landType}',
          'type': 'latlon', 
          'nx': cnf[m]['ncx'],
          'ny': cnf[m]['ncy'],
          'west': cnf[m]['west'],
          'east': cnf[m]['east'],
          'south': cnf[m]['south'],
          'north': cnf[m]['north'],
          'is_south_to_north': cnf[m]['is_south_to_north'],
          'dir': cnf[m]['dir'],
          'fin_grdidx': cnf[m][f'f_grdidx_{landType}'],
          'idx_miss': cnf[m]['idx_miss'],
        }

        m = 'CaMa-Flood'
        cnf[f'IO_CaMa-Flood_row_{landType}'] = {
          'name': f'IO_CaMa-Flood_row_{landType}',
          'type': 'latlon',
          'nx': cnf[m]['ncx'],
          'ny': cnf[m]['ncy'],
          'west': cnf[m]['west'], 
          'east': cnf[m]['east'],
          'south': cnf[m]['south'],
          'north': cnf[m]['north'],
          'is_south_to_north': cnf[m]['is_south_to_north'],
          '_dir': f'{lconst.dir_tmp[1]}/CaMa-Flood_{landType}',
          'fin_grdara': {"path": f'grdara_{landType}.bin'},
        }


    for key in cnf.keys():
        if type(cnf[key]) is not dict: continue

        if 'dir' in cnf[key].keys():
            cnf[key]['dir'] = os.path.join(cnf['dir_top'], cnf[key]['dir'])

        elif '_dir' in cnf[key].keys():
            cnf[key]['dir'] = os.path.join(os.getcwd(), cnf[key]['_dir'])

