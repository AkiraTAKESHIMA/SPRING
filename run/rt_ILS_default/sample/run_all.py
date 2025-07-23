import os
import sys
import argparse

sys.path.append('../../../src/run/common')
sys.path.append('../../../src/run/rt_ILS_default')
import const, util, conf
import s00_const as lconst
import s01_make_grid_data, \
       s02_make_rt       , \
       s03_merge_rt


parser = argparse.ArgumentParser()
parser.add_argument('-s', '--step', help='step number', type=int)
args = parser.parse_args()

util.job.put_job(lconst.job)

if args.step is None:
    s01_make_grid_data.run()

    s02_make_rt.run()

    s03_merge_rt.run()

else:
    if args.step == 1:
        s01_make_grid_data.run()

    elif args.step == 2:
        s02_make_rt.run()

    elif args.step == 3:
        s03_merge_rt.run()

