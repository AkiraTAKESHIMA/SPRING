import os
import sys


def wconf(srcMeshName, tgtMeshName):
    f_report_river = '../tmp/01_make_rt/{}_to_{}-river/report.txt'\
    .format(srcMeshName, tgtMeshName)
    f_report_noriv = '../tmp/01_make_rt/{}_to_{}-noriv/report.txt'\
    .format(srcMeshName, tgtMeshName)

    with open(f_report_river,'r') as fp:
        for i in range(2):
            fp.readline()
        length_rt_river = int(fp.readline().strip().split()[1])

    with open(f_report_noriv,'r') as fp:
        for i in range(2):
            fp.readline()
        length_rt_noriv = int(fp.readline().strip().split()[1])

    with open(f_conf,'w') as fp:
        fp.write('\
#\n\
path_report: "../tmp/02_merge_rt/{srcMeshName}_to_{tgtMeshName}/report.txt"\n\
\n\
[input]\n\
  # river\n\
  legth_rt: {length_rt_river}\n\
  dir: "../tmp/01_make_rt/{srcMeshName}_to_{tgtMeshName}-river"\n\
  f_rt_sidx: "grid.bin", int4, 1, big\n\
  f_rt_tidx: "grid.bin", int4, 2, big\n\
  f_rt_area: "area.bin", dble, 1, big\n\
  f_rt_coef: "coef.bin", dble, 1, big\n\
\n\
  # noriv\n\
  legth_rt: {length_rt_noriv}\n\
  dir: "../tmp/01_make_rt/{srcMeshName}_to_{tgtMeshName}-noriv"\n\
  f_rt_sidx: "grid.bin", int4, 1, big\n\
  f_rt_tidx: "grid.bin", int4, 2, big\n\
  f_rt_area: "area.bin", dble, 1, big\n\
  f_rt_coef: "coef.bin", dble, 1, big\n\
\n\
  opt_idx_duplication: stop\n\
[end]\n\
\n\
[output]\n\
  grid_coef: target\n\
  grid_sort: target\n\
  opt_coef_sum_modify: 1.d0\n\
\n\
  dir: "../tmp/02_merge_rt/{srcMeshName}_to_[tgtMeshName}"\n\
  f_rt_sidx: "grid.bin", int4, 1, big\n\
  f_rt_tidx: "grid.bin", int4, 2, big\n\
  f_rt_area: "area.bin", dble, 1, big\n\
  f_rt_coef: "coef.bin", dble, 1, big\n\
[end]\n\
\n\
[options]\n\
  old_files: remove\n\
[end]\n\
'.format(srcMeshName=srcMeshName, tgtMeshName=tgtMeshName,
         length_rt_river=length_rt_river, length_rt_noriv=length_rt_noriv)


def run():
    wconf('io-bnd', 'mat')
    wconf('io-met', 'mat')
    wconf('io-metnc', 'mat')
    wconf('mat', 'io-latlon')
    wconf('mat', 'io-row')


if __name__ == '__main__':
    run()
