import os
import sys
import argparse

sys.path.append('../../../src/run/common')
sys.path.append('script')
from script import s01_make_grid_data_AGCM, \
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
args = parser.parse_args()

if args.step is None:
    s01_make_grid_data_AGCM.run()

    s02_make_grid_data_OGCM.run()

    s03_rasterize_OGCM.run()

    s04_run_FLOW.run()

    s05_make_idxmap_RM.run()

    s06_make_grid_data_RM.run()

    s07_make_rt_standard.run()

    s08_define_LSM.run()

    s09_make_grid_data_LSM.run()

    s10_make_rt_standard.run()

    s11_make_rt_special.run()

    s12_merge_rt.run()

else:
    if args.step == 1:
        s01_make_grid_data_AGCM.run()

    elif args.step == 2:
        s02_make_grid_data_OGCM.run()

    elif args.step == 3:
        s03_rasterize_OGCM.run()

    elif args.step == 4:
        s04_run_FLOW.run()

    elif args.step == 5:
        s05_make_idxmap_RM.run()

    elif args.step == 6:
        s06_make_grid_data_RM.run()

    elif args.step == 7:
        s07_make_rt_standard.run()

    elif args.step == 8:
        s08_define_LSM.run()

    elif args.step == 9:
        s09_make_grid_data_LSM.run()

    elif args.step == 10:
        s10_make_rt_standard.run()

    elif args.step == 11:
        s11_make_rt_special.run()

    elif args.step == 12:
        s12_merge_rt.run()

