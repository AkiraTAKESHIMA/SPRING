import os
import sys
import subprocess
import json

sys.path.append('../../../common')
import const, util

import s00_const as lconst
import s00_util as lutil
import s00_conf as lconf


def blocks_body(cnf, srcMeshName, runname, dir_lsm, landType):
    dir_rt = f'{lconst.dir_tmp[lutil.istep("make_rt_standard-1")]}/rt_AGCM_to_OGCM'
    nij_rt = util.get_nij(f'{dir_rt}/grid.bin', cnf['remapping']['dtype_idx'])

    s = f'\
\n\
[input_rt_agcm_to_ogcm]\n\
  length: {nij_rt}\n\
  dir: "{dir_rt}"\n\
  f_sidx: "grid.bin", int4, 1, big\n\
  f_tidx: "grid.bin", int4, 2, big\n\
  f_area: "area.bin", dble, 1, big\n\
  f_coef: "coef.bin", dble, 1, big\n\
[end]\n\
\n\
[input_agcm]\n\
  nij: {cnf["AGCM"]["nij"]}\n\
  dir: "{lconst.dir_tmp[lutil.istep("make_grid_data_AGCM")]}"\n\
  f_grdidx: "grdidx.bin"\n\
  f_grdara: "grdara.bin"\n\
  f_grdlon: "grdlonlat.bin", rec=1\n\
  f_grdlat: "grdlonlat.bin", rec=2\n\
  idx_miss: {cnf["AGCM"]["idx_miss"]}\n\
[end]\n\
\n\
[input_lsm]\n\
  nij: {cnf[srcMeshName]["nij"]}\n\
  dir: "{dir_lsm}"\n\
  f_grdidx: {util.str_file_bin(cnf[srcMeshName]["fin_grdidx"])}\n\
  f_grdara: {util.str_file_bin(cnf[srcMeshName]["fin_grdara"])}\n\
  f_grdlon: {util.str_file_bin(cnf[srcMeshName]["fin_grdlonlat"])}, rec=1\n\
  f_grdlat: {util.str_file_bin(cnf[srcMeshName]["fin_grdlonlat"])}, rec=2\n\
  idx_miss: {cnf[srcMeshName]["idx_miss"]}\n\
[end]\n\
\n\
[output_rt_lsm_to_agcm]\n\
  grid_coef: none\n\
  grid_sort: target\n\
\n\
  dir: "{lconst.dir_tmp[step]}/{runname}"\n\
  fout_rt_sidx: "grid.bin", int4, 1, big\n\
  fout_rt_tidx: "grid.bin", int4, 2, big\n\
  fout_rt_area: "area.bin", dble, 1, big\n\
  fout_rt_coef: "coef.bin", dble, 1, big\n\
\n\
  vrf_target_form: index\n\
  fout_vrf_grdnum: "vrf/tgt_grdnum.bin"\n\
[end]\n\
\n\
[options]\n\
  old_files: remove\n\
  method_rivwat: weighted_dist\n\
[end]\n\
'

    return s



def get_srcMeshName(component, landType):
    return f'{component}_latlon_{landType}'


def get_runname(component, landType):
    srcMeshName = get_srcMeshName(component, landType)
    return f'rt_{srcMeshName}_to_OGCM_via_AGCM'



if __name__ == '__main__':
    step = int(sys.argv[0][1:3])

    cnf = json.load(open(lconst.f_cnf,'r'))
    lutil.adjust_config(cnf)

    os.makedirs(lconst.dir_set[step], exist_ok=True)
    os.makedirs(lconst.dir_tmp[step], exist_ok=True)
    os.makedirs(lconst.dir_log[step], exist_ok=True)
    os.makedirs(lconst.dir_out[step], exist_ok=True)

    for component in ['LSM', 'RM']:
        if component == 'LSM':
            dir_lsm = lconst.dir_tmp[lutil.istep('make_grid_data_LSM')]
            lst_landType = ['river', 'noriv_real', 'noriv_virt']
        else:
            dir_lsm = lconst.dir_tmp[lutil.istep('make_grid_data_RM')]
            lst_landType = ['river_end']

        for landType in lst_landType:
            srcMeshName = get_srcMeshName(component, landType)
            runname = get_runname(component, landType)

            f_conf = f'{lconst.dir_set[step]}/{srcMeshName}.conf'
            print(f_conf)
            fp = open(f_conf, 'w')
            fp.write(lconf.head(lconst.dir_tmp[step]))
            fp.write(blocks_body(cnf, srcMeshName, runname, dir_lsm, landType))
            fp.close()


            f_log = f'{lconst.dir_log[step]}/{runname}.out'
            f_err = f'{lconst.dir_log[step]}/{runname}.err'
            util.exec_program(const.prog_cpl_make_rt, f_conf, f_log, f_err)
