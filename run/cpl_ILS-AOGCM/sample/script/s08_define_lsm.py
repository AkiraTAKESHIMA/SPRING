import os
import sys
import subprocess
import json

sys.path.append('../../../common')
import const, util

import s00_const as lconst
import s00_util as lutil
import s00_conf as lconf


def blocks_body(cnf):
    dir_rt = lconst.dir_tmp[lutil.istep('make_rt_standard-1')]
    dtype_idx = cnf['remapping_table']['dtype_idx']
    endian = cnf['remapping_table']['endian']

    nij_rt_OGCM_ocean_to_AGCM = util.get_nij(f'{dir_rt}/OGCM_ocean_to_AGCM/grid.bin', dtype_idx)
    nij_rt_RM_river_to_AGCM   = util.get_nij(f'{dir_rt}/RM_river_to_AGCM/grid.bin', dtype_idx)
    nij_rt_RM_noriv_to_AGCM   = util.get_nij(f'{dir_rt}/RM_noriv_to_AGCM/grid.bin', dtype_idx)
    nij_rt_RM_ocean_to_AGCM   = util.get_nij(f'{dir_rt}/RM_ocean_to_AGCM/grid.bin', dtype_idx)

    s = f'\
\n\
[input_rt_ogcm_ocean_to_agcm]\n\
  length: {nij_rt_OGCM_ocean_to_AGCM}\n\
  dir: "{dir_rt}/OGCM_ocean_to_AGCM"\n\
  f_sidx: "grid.bin", {dtype_idx}, 1, {endian}\n\
  f_tidx: "grid.bin", {dtype_idx}, 2, {endian}\n\
  f_area: "area.bin", dble, 1, {endian}\n\
[end]\n\
\n\
[input_rt_rm_river_to_agcm]\n\
  length: {nij_rt_RM_river_to_AGCM}\n\
  dir: "{dir_rt}/RM_river_to_AGCM"\n\
  f_sidx: "grid.bin", {dtype_idx}, 1, {endian}\n\
  f_tidx: "grid.bin", {dtype_idx}, 2, {endian}\n\
  f_area: "area.bin", dble, 1, {endian}\n\
[end]\n\
\n\
[input_rt_rm_noriv_to_agcm]\n\
  length: {nij_rt_RM_noriv_to_AGCM}\n\
  dir: "{dir_rt}/RM_noriv_to_AGCM"\n\
  f_sidx: "grid.bin", {dtype_idx}, 1, {endian}\n\
  f_tidx: "grid.bin", {dtype_idx}, 2, {endian}\n\
  f_area: "area.bin", dble, 1, {endian}\n\
[end]\n\
\n\
[input_rt_rm_ocean_to_agcm]\n\
  length: {nij_rt_RM_ocean_to_AGCM}\n\
  dir: "{dir_rt}/RM_ocean_to_AGCM"\n\
  f_sidx: "grid.bin", {dtype_idx}, 1, {endian}\n\
  f_tidx: "grid.bin", {dtype_idx}, 2, {endian}\n\
  f_area: "area.bin", dble, 1, {endian}\n\
[end]\n\
\n\
[input_agcm]\n\
  nij: {cnf["AGCM"]["nx"]*cnf["AGCM"]["ny"]}\n\
  dir: "{lconst.dir_tmp[lutil.istep("make_grid_data_AGCM")]}"\n\
  f_grdidx: "grdidx.bin"\n\
  f_grdara: "grdara.bin"\n\
[end]\n\
\n\
[input_rm]\n\
  nx_raster: {cnf["RM"]["nx"]}\n\
  ny_raster: {cnf["RM"]["ny"]}\n\
  nx_grid: {cnf["RM"]["ncx"]}\n\
  ny_grid: {cnf["RM"]["ncy"]}\n\
\n\
  dir: "{lconst.dir_tmp[lutil.istep("make_idxmap_RM")]}"\n\
  f_grdidx_river: "grdidx_river.bin"\n\
  f_grdidx_noriv: "grdidx_noriv.bin"\n\
  f_grdidx_ocean: "grdidx_ocean.bin"\n\
\n\
  dir: "{lconst.dir_tmp[lutil.istep("make_idxmap_RM")]}"\n\
  f_rstidx_river: "rstidx_river.bin"\n\
  f_rstidx_noriv: "rstidx_noriv.bin"\n\
  f_rstidx_ocean: "rstidx_ocean.bin"\n\
\n\
  dir: "{lconst.dir_tmp[lutil.istep("make_grid_data_RM")]}"\n\
  f_grdara_river: "grdara_river.bin"\n\
  f_grdara_noriv: "grdara_noriv.bin"\n\
  f_grdara_ocean: "grdara_ocean.bin"\n\
\n\
  idx_miss: {cnf["RM"]["idx_miss"]}\n\
[end]\n\
\n\
[output_rt_lsm_river_to_agcm]\n\
  dir: "{lconst.dir_tmp[step]}/rt_LSM_river_to_AGCM"\n\
  f_sidx: "grid.bin", {dtype_idx}, 1, {endian}\n\
  f_tidx: "grid.bin", {dtype_idx}, 2, {endian}\n\
  f_area: "area.bin", dble, 1, {endian}\n\
  f_coef: "coef.bin", dble, 1, {endian}\n\
[end]\n\
\n\
[output_rt_lsm_noriv_to_agcm]\n\
  dir: "{lconst.dir_tmp[step]}/rt_LSM_noriv_to_AGCM"\n\
  f_sidx: "grid.bin", {dtype_idx}, 1, {endian}\n\
  f_tidx: "grid.bin", {dtype_idx}, 2, {endian}\n\
  f_area: "area.bin", dble, 1, {endian}\n\
  f_coef: "coef.bin", dble, 1, {endian}\n\
[end]\n\
\n\
[output_rt_agcm_to_lsm_river]\n\
  dir: "{lconst.dir_tmp[step]}/rt_AGCM_to_LSM_river"\n\
  f_sidx: "grid.bin", {dtype_idx}, 1, {endian}\n\
  f_tidx: "grid.bin", {dtype_idx}, 2, {endian}\n\
  f_area: "area.bin", dble, 1, {endian}\n\
  f_coef: "coef.bin", dble, 1, {endian}\n\
[end]\n\
\n\
[output_rt_agcm_to_lsm_noriv]\n\
  dir: "{lconst.dir_tmp[step]}/rt_AGCM_to_LSM_noriv"\n\
  f_sidx: "grid.bin", {dtype_idx}, 1, {endian}\n\
  f_tidx: "grid.bin", {dtype_idx}, 2, {endian}\n\
  f_area: "area.bin", dble, 1, {endian}\n\
  f_coef: "coef.bin", dble, 1, {endian}\n\
[end]\n\
\n\
[output_agcm]\n\
  dir: "{lconst.dir_tmp[step]}/AGCM"\n\
  f_lndara_ogcm      : "lndara_ogcm.bin"\n\
  f_lndara_river     : "lndara_river.bin"\n\
  f_lndara_noriv_real: "lndara_noriv_real.bin"\n\
  f_lndara_noriv_virt: "lndara_noriv_virt.bin"\n\
  f_lndara_noriv     : "lndara_noriv.bin"\n\
[end]\n\
\n\
[output_lsm]\n\
  dir: "{lconst.dir_tmp[step]}/LSM"\n\
\n\
  f_grdmsk_river     : "grdmsk_river.bin", real, 1, {endian}\n\
  f_grdmsk_noriv     : "grdmsk_noriv.bin", real, 1, {endian}\n\
  f_grdmsk_noriv_real: "grdmsk_noriv-real.bin", real, 1, {endian}\n\
  f_grdmsk_noriv_virt: "grdmsk_noriv-virt.bin", real, 1, {endian}\n\
\n\
  f_grdidx_bnd_river     : "grdidx_bnd_river.bin"\n\
  f_grdidx_bnd_noriv     : "grdidx_bnd_noriv.bin"\n\
  f_grdidx_bnd_noriv_real: "grdidx_bnd_noriv-real.bin"\n\
  f_grdidx_bnd_noriv_virt: "grdidx_bnd_noriv-virt.bin"\n\
\n\
  f_grdidx_river     : "grdidx_river.bin"\n\
  f_grdidx_noriv     : "grdidx_noriv.bin"\n\
  f_grdidx_noriv_real: "grdidx_noriv-real.bin"\n\
  f_grdidx_noriv_virt: "grdidx_noriv-virt.bin"\n\
\n\
  f_grdara_river     : "grdara_river.bin"\n\
  f_grdara_noriv     : "grdara_noriv.bin"\n\
  f_grdara_noriv_real: "grdara_noriv-real.bin"\n\
  f_grdara_noriv_virt: "grdara_noriv-virt.bin"\n\
\n\
  f_grdwgt_river     : "grdwgt_river.bin"\n\
  f_grdwgt_noriv     : "grdwgt_noriv.bin"\n\
  f_grdwgt_noriv_virt: "grdwgt_noriv-virt.bin"\n\
  f_grdwgt_noriv_real: "grdwgt_noriv-real.bin"\n\
\n\
  f_rstidx_river     : "rstidx_river.bin"\n\
  f_rstidx_noriv     : "rstidx_noriv.bin"\n\
  f_rstidx_noriv_real: "rstidx_noriv-real.bin"\n\
  f_rstidx_noriv_virt: "rstidx_noriv-virt.bin"\n\
\n\
  f_rstidx_bnd_river     : "rstidx_bnd_river.bin"\n\
  f_rstidx_bnd_noriv     : "rstidx_bnd_noriv.bin"\n\
  f_rstidx_bnd_noriv_real: "rstidx_bnd_noriv-real.bin"\n\
  f_rstidx_bnd_noriv_virt: "rstidx_bnd_noriv-virt.bin"\n\
[end]\n'

    return s


def block_options(opt):
    s = f'\
\n\
[options]\n\
  old_files: remove\n\
[end]\n'

    return s


if __name__ == '__main__':
    step = int(sys.argv[0][1:3])

    cnf = json.load(open(lconst.f_cnf, 'r'))
    lutil.adjust_config(cnf)

    os.makedirs(lconst.dir_set[step], exist_ok=True)
    os.makedirs(lconst.dir_tmp[step], exist_ok=True)
    os.makedirs(lconst.dir_log[step], exist_ok=True)
    os.makedirs(lconst.dir_out[step], exist_ok=True)

    f_conf = f'{lconst.dir_set[step]}/a.conf'

    print(f_conf)
    fp = open(f_conf,'w')
    fp.write(lconf.head(lconst.dir_tmp[step]))
    fp.write(blocks_body(cnf))
    fp.write(block_options(cnf))
    fp.close()

    f_log = f'{lconst.dir_log[step]}/a.out'
    f_err = f'{lconst.dir_log[step]}/a.err'
    util.exec_program(const.prog_cpl_define_mat, f_conf, f_log, f_err)
