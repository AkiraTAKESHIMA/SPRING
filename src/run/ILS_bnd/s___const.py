import os
import sys

import const, util

job = {
  0: 'preprocess', 
  1: 'make_cmf_mat',
  2: 'make_rt',
}

scripts = {
  0: s00_preprocess, 
  1: s01_make_cmf_mat,
  2: s02_make_rt,
}
step_max = len(scripts.keys()) - 1

dir_set = util.set_dir(const.dir_set, job)
dir_tmp = util.set_dir(const.dir_tmp, job)
dir_log = util.set_dir(const.dir_log, job)
