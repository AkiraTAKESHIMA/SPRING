import os
import sys
import argparse

sys.path.append('../../../src/run/common')
sys.path.append('script')
import const, util, conf
from script import s00_const as lconst, \
                   s01_make_grid_data_AGCM, \
                   s02_make_grid_data_OGCM, \
                   s03_rasterize_OGCM     , \
                   s04_run_FLOW           , \
                   s05_make_idxmap_RM     , \
                   s06_make_grid_data_RM  , \
                   s07_make_rt_standard   , \
                   s08_define_LSM         , \
                   s09_make_grid_data_LSM , \
                   s10_make_rt_standard   , \
                   s11_make_rt_special    , \
                   s12_merge_rt


parser = argparse.ArgumentParser()
parser.add_argument('-s', '--step', help='step number', type=int)
parser.add_argument('-c', '--conf', help='only update config without executing program', action='store_false')
args = parser.parse_args()

util.job.put_job(lconst.job)

if args.step is None:
    s01_make_grid_data_AGCM.run(args.conf)

    s02_make_grid_data_OGCM.run(args.conf)

    s03_rasterize_OGCM.run(args.conf)

    s04_run_FLOW.run(args.conf)

    s05_make_idxmap_RM.run(args.conf)

    s06_make_grid_data_RM.run(args.conf)

    s07_make_rt_standard.run(args.conf)

    s08_define_LSM.run(args.conf)

    s09_make_grid_data_LSM.run(args.conf)

    s10_make_rt_standard.run(args.conf)

    s11_make_rt_special.run(args.conf)

    s12_merge_rt.run(args.conf)

else:
    if args.step == 1:
        s01_make_grid_data_AGCM.run(args.conf)

    elif args.step == 2:
        s02_make_grid_data_OGCM.run(args.conf)

    elif args.step == 3:
        s03_rasterize_OGCM.run(args.conf)

    elif args.step == 4:
        s04_run_FLOW.run(args.conf)

    elif args.step == 5:
        s05_make_idxmap_RM.run(args.conf)

    elif args.step == 6:
        s06_make_grid_data_RM.run(args.conf)

    elif args.step == 7:
        s07_make_rt_standard.run(args.conf)

    elif args.step == 8:
        s08_define_LSM.run(args.conf)

    elif args.step == 9:
        s09_make_grid_data_LSM.run(args.conf)

    elif args.step == 10:
        s10_make_rt_standard.run(args.conf)

    elif args.step == 11:
        s11_make_rt_special.run(args.conf)

    elif args.step == 12:
        s12_merge_rt.run(args.conf)

