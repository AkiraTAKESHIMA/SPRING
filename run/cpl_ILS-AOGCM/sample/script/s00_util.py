import os
import sys
import shutil

import s00_const as lconst


def get_f_conf(step, child_name=None, run_name=None, refresh=False):
    if child_name is None:
        dir_set_this = os.path.join(lconst.dir_set[step]['self'])
        dir_tmp_this = os.path.join(lconst.dir_tmp[step]['self'])
    else:
        dir_set_this = os.path.join(lconst.dir_set[step]['child'][child_name])
        dir_tmp_this = os.path.join(lconst.dir_tmp[step]['child'][child_name])

    if refresh:
        for d in [dir_set_this, dir_tmp_this]:
            if os.path.isdir(d):
                shutil.rmtree(d)
            elif os.path.lexists(d) or os.path.isfile(d):
                os.remove(d)

    os.makedirs(dir_set_this, exist_ok=True)
    os.makedirs(dir_tmp_this, exist_ok=True)

    if run_name is None:
        f_conf = os.path.join(dir_set_this, f'conf')
        f_report = os.path.join(dir_tmp_this, f'report')
    else:
        f_conf = os.path.join(dir_set_this, f'conf.{run_name}')
        f_report = os.path.join(dir_tmp_this, f'report.{run_name}')

    return f_conf, f_report, dir_set_this, dir_tmp_this


def adjust_config(cnf):
    cnf['RM']['dir'] = lconst.dir_tmp[istep('make_idxmap_RM')]['self']
    cnf['LSM']['dir'] = lconst.dir_tmp[istep('define_LSM')]['self']


def istep(name):
    for key in lconst.job.keys():
        if lconst.job[key]['self'] == name:
            return key
    raise Exception(f'Invalid value in $name: {name}')

