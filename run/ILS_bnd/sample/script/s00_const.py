import os, sys
sys.path.append('../../../common')
import const

job = {
  1: '01_make_cmf_mat',
  2: '02_make_rt',
}

dir_set, dir_tmp = {}, {}
for key in job.keys():
    dir_tmp[key] = os.path.join(const.dir_tmp, job[key])
    dir_set[key] = os.path.join(const.dir_set, job[key])

