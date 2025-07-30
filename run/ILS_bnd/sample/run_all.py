import os
import sys
import argparse

sys.path.append('../../../src/run/common')
sys.path.append('../../../src/run/ILS_bnd')
import const, util, conf
import s00_const as lconst
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

scripts = {
  0: s00_preprocess, 
  1: s01_make_cmf_mat,
  2: s02_make_rt,
}

if args.step is None:
    for step in range(3):
        if step == 2:
            scripts[step].run(update_data, args.data, args.land, args.tile)
        else:
            scripts[step].run(update_data)

else:
    for step in range(args.step):
        if step == 2:
            scripts[step].run(False, args.data, args.land, args.tile)
        else:
            scripts[step].run(False)
    if args.step == 2:
        scripts[args.step].run(update_data, args.data, args.land, args.tile)
    else:
        scripts[args.step].run(update_data)

