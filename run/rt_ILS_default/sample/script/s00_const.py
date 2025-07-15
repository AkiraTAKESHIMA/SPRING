import os, sys
sys.path.append('../../../common')
import const

job = {
  1: '01_make_rt',
  2: '02_merge_rt',
}

dir_set, dir_tmp, dir_log = {}, {}, {}
for key in job.keys():
    dir_set[key] = os.path.join(const.dir_set, job[key])
    dir_tmp[key] = os.path.join(const.dir_tmp, job[key])
    dir_log[key] = os.path.join(const.dir_log, job[key])
