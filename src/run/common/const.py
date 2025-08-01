import os

dir_bin = '../../bin'
prog_make_grid_data = f'{dir_bin}/main_std/make_grid_data.exe'
prog_rasterize      = f'{dir_bin}/main_std/rasterize.exe'
prog_remap          = f'{dir_bin}/main_std/remap.exe'
prog_merge_rt       = f'{dir_bin}/main_std/merge_remapping_tables.exe'
prog_make_cmf_mat   = f'{dir_bin}/main_ext/make_cmf_mat.exe'
prog_cpl_define_mat = f'{dir_bin}/main_ext/cpl_aogcm-ils_define_mat.exe'
prog_cpl_make_rt    = f'{dir_bin}/main_ext/cpl_aogcm-ils_make_rt_for_ogcm.exe'

dir_FLOW_src = '../../etc/FLOW_free/src'

command_ln = 'ln'
options_ln = '-s'


class Key():
    def __init__(self):
        self.run = 'runName'
        self.lt = 'landTypes'
        self.m = 'meshes'
        self.rt = 'remappingTables'
        self.rtc = 'remappingTables_common'
        self.irt = 'intermediateRemappingTables'
        self.opt = 'options'

k = Key()
