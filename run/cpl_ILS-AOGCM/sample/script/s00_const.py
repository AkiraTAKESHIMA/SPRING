import os
import sys

sys.path.append('../../../common')
import const

f_cnf = 'conf.json'

dir_set = '../set'
dir_tmp = '../tmp'
dir_out = '../out'

job = {
  1: {
    'self': 'make_grid_data_AGCM',
  },
  2: {
    'self': 'make_grid_data_OGCM',
  },
  3: {
    'self': 'rasterize_OGCM',
  },
  4: {
    'self': 'run_FLOW',
  },
  5: {
    'self': 'make_idxmap_RM',
  },
  6: {
    'self': 'make_grid_data_RM',
  },
  7: {
    'self': 'make_rt_standard-1',
    'child': {
      'OGCM_land_to_AGCM' : 'OGCM_land_to_AGCM',
      'OGCM_ocean_to_AGCM': 'OGCM_ocean_to_AGCM',
      'RM_river_to_AGCM'  : 'RM_river_to_AGCM',
      'RM_noriv_to_AGCM'  : 'RM_noriv_to_AGCM',
      'RM_ocean_to_AGCM'  : 'RM_ocean_to_AGCM',
    },
  },
  8: {
    'self': 'define_LSM',
    'child': {
      'rt_LSM-river_to_AGCM': 'rt_LSM-river_to_AGCM',
      'rt_LSM-noriv_to_AGCM': 'rt_LSM-noriv_to_AGCM',
      'rt_AGCM_to_LSM-river': 'rt_AGCM_to_LSM-river',
      'rt_AGCM_to_LSM-noriv': 'rt_AGCM_to_LSM-noriv',
      'AGCM': 'AGCM',
      'LSM' : 'LSM' ,
    },
  },
  9: {
    'self': 'make_grid_data_LSM',
  },
  10: {
    'self': 'make_rt_standard-2',
  },
  11: {
    'self': 'make_rt_special',
  },
  12: {
    'self': 'merge_rt',
  },
}


def set_dir(d_top):
    d = {}
    for step in job.keys():
        d[step] = {}
        d[step]['self'] = os.path.join(d_top, f'{step:02d}_{job[step]["self"]}')
        if 'child' in job[step].keys():
            d[step]['child'] = {}
            for key in job[step]['child'].keys():
                d[step]['child'][key] = os.path.join(d[step]['self'], job[step]['child'][key])
    return d

dir_set = set_dir(const.dir_set)
dir_tmp = set_dir(const.dir_tmp)
dir_log = set_dir(const.dir_log)
dir_out = set_dir(const.dir_out)

