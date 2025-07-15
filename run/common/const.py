import os



dir_set = '../set'
dir_tmp = '../tmp'
dir_out = '../out'
dir_log = '../log'

dir_bin = '/data7/akira/SPRING/SPRING/bin'
prog_make_grid_data = f'{dir_bin}/main_std/make_grid_data.exe'
prog_rasterize      = f'{dir_bin}/main_std/rasterize.exe'
prog_remap          = f'{dir_bin}/main_std/remap.exe'
prog_merge_rt       = f'{dir_bin}/main_std/merge_remapping_tables.exe'
prog_make_cmf_mat   = f'{dir_bin}/main_ext/make_cmf_mat.exe'
prog_cpl_define_mat = f'{dir_bin}/main_ext/cpl_aogcm-ils_define_mat.exe'
prog_cpl_make_rt    = f'{dir_bin}/main_ext/cpl_aogcm-ils_make_rt_for_ogcm.exe'

str_dtype_int1 = 'int1'
str_dtype_int2 = 'int2'
str_dtype_int4 = 'int4'
str_dtype_int8 = 'int8'
str_dtype_real = 'real'
str_dtype_dble = 'dble'

endian_default = 'little'
idx_miss_default = -9999
