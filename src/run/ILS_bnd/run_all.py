import os
import sys
import argparse

sys.path.append('../../../src/run/common')
sys.path.append('../../../src/run/ILS_bnd')

import const, util, conf
from util import istep

import s___const as lconst
import s00_preprocess, \
       s01_make_cmf_mat, \
       s02_make_rt


parser = argparse.ArgumentParser()
parser.add_argument('-s', '--step', help='step number', type=int)
parser.add_argument('-x', help='not update data', action='store_true')
parser.add_argument('-d', '--data', help='data name')
parser.add_argument('-l', '--land', help='land type')
parser.add_argument('-t', '--tile', help='tile name')
parser.add_argument('-f', '--config', help='config file', default='conf.json')
args = parser.parse_args()

update_data = not args.x

util.env.put_job(lconst.job)
util.env.put_f_cnf(args.config)

if args.step is None:
    for step in range(lconst.step_max+1):
        if step == istep('make_rt'):
            scripts[step].run(update_data, args.data, args.land, args.tile)
        else:
            scripts[step].run(update_data)

else:
    if args.step > lconst.step_max:
        raise Exception(f'Step must be less than or equal to {lconst.step_max}.')

    for step in range(args.step):
        if step == istep('make_rt'):
            scripts[step].run(False, args.data, args.land, args.tile)
        else:
            scripts[step].run(False)
    if args.step == istep('make_rt'):
        scripts[args.step].run(update_data, args.data, args.land, args.tile)
    else:
        scripts[args.step].run(update_data)

