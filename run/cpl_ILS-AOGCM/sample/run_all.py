import os
import sys
import argparse

sys.path.append('../../../src/run/common')
sys.path.append('../../../src/run/cpl_ILS-AOGCM')
import const, util, conf
from script import s00_const as lconst, \
                   s00_preprocess, \
                   s01_make_grid_data_GCM, \
                   s02_rasterize_OGCM    , \
                   s03_run_FLOW          , \
                   s04_make_idxmap_RM    , \
                   s05_make_grid_data_RM , \
                   s06_make_rt_standard  , \
                   s07_define_LSM        , \
                   s08_make_grid_data_LSM, \
                   s09_make_rt_standard  , \
                   s10_make_rt_special   , \
                   s11_merge_rt


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
  1: s01_make_grid_data_GCM,
  2: s02_rasterize_OGCM,
  3: s03_run_FLOW,
  4: s04_make_idxmap_RM,
  5: s05_make_grid_data_RM,
  6: s06_make_rt_standard,
  7: s07_define_LSM,
  8: s08_make_grid_data_LSM,
  9: s09_make_rt_standard,
  10: s10_make_rt_special,
  11: s11_merge_rt,
}

if args.step is None:
    for step in range(12):
        scripts[step].run(update_data)

else:
    for step in range(args.step):
        scripts[step].run(False)
    scripts[args.step].run(update_data)

