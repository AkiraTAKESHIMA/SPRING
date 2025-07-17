import conf_remap as remap
import conf_makeGridData as makeGridData


def head(dir_tmp):
    s = f'\
#\n\
path_report: "{dir_tmp}/report.txt"\n'

    return s

