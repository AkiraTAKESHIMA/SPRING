import os
import sys

sys.path.append('../../../common')
import const

f_cnf = 'conf.json'

dir_set = '../set'
dir_tmp = '../tmp'
dir_out = '../out'

job = {
   1: 'make_grid_data_AGCM',
   2: 'make_grid_data_OGCM',
   3: 'rasterize_OGCM',
   4: 'run_FLOW',
   5: 'make_idxmap_RM',
   6: 'make_grid_data_RM',
   7: 'make_rt_standard-1',
#      'OGCM_land_to_AGCM' : 'OGCM_land_to_AGCM',
#      'OGCM_ocean_to_AGCM': 'OGCM_ocean_to_AGCM',
#      'RM_river_to_AGCM'  : 'RM_river_to_AGCM',
#      'RM_noriv_to_AGCM'  : 'RM_noriv_to_AGCM',
#      'RM_ocean_to_AGCM'  : 'RM_ocean_to_AGCM',
   8: 'define_LSM',
#      'rt_LSM_river_to_AGCM': 'rt_LSM_river_to_AGCM',
#      'rt_LSM_noriv_to_AGCM': 'rt_LSM_noriv_to_AGCM',
#      'rt_AGCM_to_LSM_river': 'rt_AGCM_to_LSM_river',
#      'rt_AGCM_to_LSM_noriv': 'rt_AGCM_to_LSM_noriv',
#      'AGCM': 'AGCM',
#      'LSM' : 'LSM' ,
   9: 'make_grid_data_LSM',
  10: 'make_rt_standard-2',
#      'rt_AGCM_to_RM'                       : 'rt_AGCM_to_RM', 
#      'rt_IO_LSM_bnd_river_to_LSM_bnd_river': 'rt_IO_bnd_to_LSM_river',
#      'rt_IO_LSM_bnd_noriv_to_LSM_bnd_noriv': 'rt_IO_bnd_to_LSM_noriv',
#      'rt_IO_met_to_LSM_river'              : 'rt_IO_met_to_LSM_river',
#      'rt_IO_met_to_LSM_noriv_real'         : 'rt_IO_met_to_LSM_noriv_real',
#      'rt_IO_met_to_LSM_noriv_virt'         : 'rt_IO_met_to_LSM_noriv_virt',
#      'rt_IO_metnc_to_LSM_river'            : 'rt_IO_metnc_to_LSM_river',
#      'rt_LSM_river_to_RM'                  : 'rt_LSM_river_to_RM',
#      'rt_RM_to_LSM_river'                  : 'rt_RM_to_LSM_river',
#      'rt_LSM_river_to_IO_row'              : 'rt_LSM_river_to_IO_row',
#      'rt_LSM_noriv_real_to_IO_row'         : 'rt_LSM_noriv_real_to_IO_row',
#      'rt_LSM_noriv_virt_to_IO_row'         : 'rt_LSM_noriv_virt_to_IO_row',
#      'rt_LSM_river_to_IO_rect'             : 'rt_LSM_river_to_IO_rect',
#      'rt_LSM_noriv_real_to_IO_rect'        : 'rt_LSM_noriv_real_to_IO_rect',
#      'rt_LSM_noriv_virt_to_IO_rect'        : 'rt_LSM_noriv_virt_to_IO_rect',
#      'rt_RM_to_IO_row'                     : 'rt_RM_to_IO_row',
  11: 'make_rt_special',
  12: 'merge_rt',
}


def set_dir(d_top):
    d = {}
    for step in job.keys():
        d[step] = os.path.join(d_top, f'{step:02d}_{job[step]}')
    return d

dir_set = set_dir(const.dir_set)
dir_tmp = set_dir(const.dir_tmp)
dir_log = set_dir(const.dir_log)
dir_out = set_dir(const.dir_out)

