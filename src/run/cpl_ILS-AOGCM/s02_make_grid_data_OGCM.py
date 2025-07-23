import os
import sys
import subprocess
import json

import const, util, conf

import s00_const as lconst
import s00_util as lutil
import s00_conf as lconf
from s01_make_grid_data_AGCM import make_grid_data


def run():
    step = int(__name__.split('.')[-1][1:3])

    util.job.put_job(lconst.job)

    cnf = json.load(open(lconst.f_cnf,'r'))
    lutil.adjust_config(cnf)

    os.makedirs(lconst.dir_set[step], exist_ok=True)
    os.makedirs(lconst.dir_tmp[step], exist_ok=True)
    os.makedirs(lconst.dir_log[step], exist_ok=True)

    make_grid_data(cnf, step, 'OGCM_ocean')
