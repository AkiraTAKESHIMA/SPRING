import os
import sys

sys.path.append('../../../src/run/common')
sys.path.append('script')
from script import s01_make_grid_data_AGCM, \
                   s02_make_grid_data_OGCM, \
                   s03_rasterize_OGCM, \
                   s04_run_FLOW, \
                   s05_make_idxmap_RM, \
                   s06_make_grid_data_RM, \
                   s07_make_rt_standard, \
                   s08_define_LSM, \
                   s09_make_grid_data_LSM


#s01_make_grid_data_AGCM.run()

#s02_make_grid_data_OGCM.run()

#s03_rasterize_OGCM.run()

#s04_run_FLOW.run()

#s05_make_idxmap_RM.run()

#s06_make_grid_data_RM.run()

#s07_make_rt_standard.run()

s08_define_LSM.run()

#s09_make_grid_data_LSM.run()
