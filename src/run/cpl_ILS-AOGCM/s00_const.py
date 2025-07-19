import os
import sys

import const, util

f_cnf = 'conf.json'

job = {
   1: 'make_grid_data_AGCM',
   2: 'make_grid_data_OGCM',
   3: 'rasterize_OGCM',
   4: 'run_FLOW',
   5: 'make_idxmap_RM',
   6: 'make_grid_data_RM',
   7: 'make_rt_standard-1',
   8: 'define_LSM',
   9: 'make_grid_data_LSM',
  10: 'make_rt_standard-2',
  11: 'make_rt_special',
  12: 'merge_rt',
}

dir_set = util.set_dir(const.dir_set, job)
dir_tmp = util.set_dir(const.dir_tmp, job)
dir_log = util.set_dir(const.dir_log, job)
