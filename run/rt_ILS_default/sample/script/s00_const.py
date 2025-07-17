import os, sys

sys.path.append('../../../common')
import const, util

f_cnf = 'conf.json'

job = {
  1: 'make_grid_data',
  2: 'make_rt',
  3: 'merge_rt',
}

dir_set = util.set_dir(const.dir_set, job)
dir_tmp = util.set_dir(const.dir_tmp, job)
dir_log = util.set_dir(const.dir_log, job)
