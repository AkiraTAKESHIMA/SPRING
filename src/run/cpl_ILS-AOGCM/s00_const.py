import os
import sys

import const, util

job = {
   0: 'preprocess', 
   1: 'make_grid_data_GCM',
   2: 'rasterize_OGCM',
   3: 'run_FLOW',
   4: 'make_idxmap_RM',
   5: 'make_grid_data_RM',
   6: 'make_rt_standard-1',
   7: 'define_LSM',
   8: 'make_grid_data_LSM',
   9: 'make_rt_standard-2',
  10: 'make_rt_special',
  11: 'merge_rt',
}

dir_set = util.set_dir(const.dir_set, job)
dir_tmp = util.set_dir(const.dir_tmp, job)
dir_log = util.set_dir(const.dir_log, job)
