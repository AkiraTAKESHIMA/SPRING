import os
import sys
import subprocess

sys.path.append('../../../common')
import const, util

import s00_const as lconst


def merge_rt(srcMeshName, tgtMeshName, landType_is_for_src):
    if landType_is_for_src:
        rtName_river = f'{srcMeshName}_river_to_{tgtMeshName}'
        rtName_noriv = f'{srcMeshName}_noriv_to_{tgtMeshName}'
    else:
        rtName_river = f'{srcMeshName}_to_{tgtMeshName}_river'
        rtName_noriv = f'{srcMeshName}_to_{tgtMeshName}_noriv'
    f_report_river = f'{lconst.dir_tmp[1]}/{rtName_river}/report.txt'
    f_report_noriv = f'{lconst.dir_tmp[1]}/{rtName_noriv}/report.txt'

    with open(f_report_river,'r') as fp:
        for i in range(1):
            fp.readline()
        length_rt_river = int(fp.readline().strip().split()[1])

    with open(f_report_noriv,'r') as fp:
        for i in range(1):
            fp.readline()
        length_rt_noriv = int(fp.readline().strip().split()[1])

    f_conf = f'{lconst.dir_set[2]}/{srcMeshName}_to_{tgtMeshName}.conf'

    with open(f_conf,'w') as fp:
        fp.write(f'\
#\n\
path_report: "{lconst.dir_tmp[2]}/{srcMeshName}_to_{tgtMeshName}/report.txt"\n\
\n\
[input]\n\
  # river\n\
  length_rt: {length_rt_river}\n\
  dir: "{lconst.dir_tmp[1]}/{rtName_river}"\n\
  f_rt_sidx: "grid.bin", int4, 1, big\n\
  f_rt_tidx: "grid.bin", int4, 2, big\n\
  f_rt_area: "area.bin", dble, 1, big\n\
  f_rt_coef: "coef.bin", dble, 1, big\n\
\n\
  # noriv\n\
  length_rt: {length_rt_noriv}\n\
  dir: "{lconst.dir_tmp[1]}/{rtName_noriv}"\n\
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
  dir: "{lconst.dir_tmp[2]}/{srcMeshName}_to_{tgtMeshName}"\n\
  f_rt_sidx: "grid.bin", int4, 1, big\n\
  f_rt_tidx: "grid.bin", int4, 2, big\n\
  f_rt_area: "area.bin", dble, 1, big\n\
  f_rt_coef: "coef.bin", dble, 1, big\n\
[end]\n\
\n\
[options]\n\
  old_files: remove\n\
[end]\n\
')

    pc = subprocess.run([const.prog_merge_rt, f_conf],
                        stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                        encoding='utf-8')
    print('stdout:')
    print(pc.stdout.strip())
    print('stderr:')
    print(pc.stderr.strip())


def run():
    os.makedirs(f'{lconst.dir_set[2]}', exist_ok=True)

    merge_rt('in_MATSIRO_bnd', 'MATSIRO', False)
    merge_rt('in_met', 'MATSIRO', False)
    merge_rt('in_metnc', 'MATSIRO', False)
    merge_rt('MATSIRO', 'out_MATSIRO_latlon', True)
    merge_rt('MATSIRO', 'out_MATSIRO_row', True)


if __name__ == '__main__':
    run()
