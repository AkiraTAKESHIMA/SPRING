import os
import sys

import const, util

job = {
  0: 'preprocess', 
  1: 'make_cmf_mat',
  2: 'make_rt',
}

dir_set = util.set_dir(const.dir_set, job)
dir_tmp = util.set_dir(const.dir_tmp, job)
dir_log = util.set_dir(const.dir_log, job)
