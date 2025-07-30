import os
import sys
import argparse

sys.path.append('../../../src/run/common')
sys.path.append('../../../src/run/rt_ILS_default')
import const, util, conf
import s00_const as lconst
import s00_preprocess    , \
       s01_make_cmf_mat  , \
       s02_make_grid_data, \
       s03_make_rt       , \
       s04_merge_rt


parser = argparse.ArgumentParser()
parser.add_argument('-s', '--step', help='step number', type=int)
parser.add_argument('-x', help='not update data', action='store_true')
parser.add_argument('-f', '--config', help='config file', default='conf.json')
args = parser.parse_args()

update_data = not args.x

util.env.put_job(lconst.job)
util.env.put_f_cnf(args.config)

scripts = {
  0: s00_preprocess,
  1: s01_make_cmf_mat,
  2: s02_make_grid_data,
  3: s03_make_rt,
  4: s04_merge_rt,
}

if args.step is None:
    for step in range(5):
        scripts[step].run(update_data)

else:
    for step in range(args.step):
        scripts[step].run(False)
    scripts[args.step].run(update_data)

