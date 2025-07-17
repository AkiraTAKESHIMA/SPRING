import conf_make_grid_data as make_grid_data
import conf_remap as remap
import conf_merge_rt as merge_rt
import conf_make_cmf_mat as make_cmf_mat


def head(dir_tmp):
    s = f'\
#\n\
path_report: "{dir_tmp}/report.txt"\n'

    return s

