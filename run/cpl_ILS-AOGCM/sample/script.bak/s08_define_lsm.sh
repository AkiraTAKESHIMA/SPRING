#!/bin/bash
set -e
#===============================================================
#
#===============================================================
dir_tmp_this="${dir_tmp}/${name_step_08}"
dir_set_this="${dir_set}/${name_step_08}"
f_conf="${dir_set_this}/define_lsm.conf"
#---------------------------------------------------------------
# Get length of rt
#---------------------------------------------------------------
dir_in_agcm_to_ogcm="${dir_tmp}/${name_step_07}/rt_agcm_to_ogcm"
dir_in_rm_river_to_agcm="${dir_tmp}/${name_step_07}/rt_rm-river_to_agcm"
dir_in_rm_noriv_to_agcm="${dir_tmp}/${name_step_07}/rt_rm-noriv_to_agcm"
dir_in_rm_ocean_to_agcm="${dir_tmp}/${name_step_07}/rt_rm-ocean_to_agcm"

for dir_in in \
    "${dir_in_agcm_to_ogcm}"\
    "${dir_in_rm_river_to_agcm}"\
    "${dir_in_rm_noriv_to_agcm}"\
    "${dir_in_rm_ocean_to_agcm}"; do
  if [ ! -f "${dir_in}/report.txt" ]; then
    echo "****** File not found ******"
    echo "report: ${dir_in}/report.txt"
    exit 1
  fi
done

nij_rt_agcm_to_ogcm=`sed -n 3p ${dir_in_agcm_to_ogcm}/report.txt | cut -d " " -f 2`
nij_rt_rm_river_to_agcm=`sed -n 3p ${dir_in_rm_river_to_agcm}/report.txt | cut -d ":" -f 2`
nij_rt_rm_noriv_to_agcm=`sed -n 3p ${dir_in_rm_noriv_to_agcm}/report.txt | cut -d ":" -f 2`
nij_rt_rm_ocean_to_agcm=`sed -n 3p ${dir_in_rm_ocean_to_agcm}/report.txt | cut -d ":" -f 2`
#===============================================================
#
#===============================================================
mkdir -p ${dir_set_this}

cat << EOF > ${f_conf}
#
path_report: "${dir_tmp_this}/report.txt"

[input_rt_agcm_to_ogcm]
  length: ${nij_rt_agcm_to_ogcm}
  dir: "${dir_in_agcm_to_ogcm}"
  f_sidx: "grid.bin", int4, 1, big
  f_tidx: "grid.bin", int4, 2, big
  f_area: "area.bin", dble, 1, big
[end]

[input_rt_rm_river_to_agcm]
  length: ${nij_rt_rm_river_to_agcm}
  dir: "${dir_in_rm_river_to_agcm}"
  f_sidx: "grid.bin", int4, 1, big
  f_tidx: "grid.bin", int4, 2, big
  f_area: "area.bin", dble, 1, big
[end]

[input_rt_rm_noriv_to_agcm]
  length: ${nij_rt_rm_noriv_to_agcm}
  dir: "${dir_in_rm_noriv_to_agcm}"
  f_sidx: "grid.bin", int4, 1, big
  f_tidx: "grid.bin", int4, 2, big
  f_area: "area.bin", dble, 1, big
[end]

[input_rt_rm_ocean_to_agcm]
  length: ${nij_rt_rm_ocean_to_agcm}
  dir: "${dir_in_rm_ocean_to_agcm}"
  f_sidx: "grid.bin", int4, 1, big
  f_tidx: "grid.bin", int4, 2, big
  f_area: "area.bin", dble, 1, big
[end]

[input_agcm]
  nij: 32768
  dir: "${dir_tmp}/${name_step_01}"
  f_grdidx: "grdidx.bin"
  f_grdara: "grdara.bin"
[end]

[input_rm]
  nx_raster: 21600
  ny_raster: 10800
  nx_grid: 720
  ny_grid: 360

  dir: "${dir_tmp}/${name_step_05}"
  f_grdidx_river: "grdidx_river.bin"
  f_grdidx_noriv: "grdidx_noriv.bin"
  f_grdidx_ocean: "grdidx_ocean.bin"

  dir: "${dir_tmp}/${name_step_05}"
  f_rstidx_river: "rstidx_river.bin"
  f_rstidx_noriv: "rstidx_noriv.bin"
  f_rstidx_ocean: "rstidx_ocean.bin"

  dir: "${dir_tmp}/${name_step_06}"
  f_grdara_river: "grdara_river.bin"
  f_grdara_noriv: "grdara_noriv.bin"
  f_grdara_ocean: "grdara_ocean.bin"

  idx_miss: -9999
[end]

[output_rt_lsm_river_to_agcm]
  dir: "${dir_tmp_this}/rt_lsm-river_to_agcm"
  f_sidx: "grid.bin", int4, 1, big
  f_tidx: "grid.bin", int4, 2, big
  f_area: "area.bin", dble, 1, big
  f_coef: "coef.bin", dble, 1, big
[end]

[output_rt_lsm_noriv_to_agcm]
  dir: "${dir_tmp_this}/rt_lsm-noriv_to_agcm"
  f_sidx: "grid.bin", int4, 1, big
  f_tidx: "grid.bin", int4, 2, big
  f_area: "area.bin", dble, 1, big
  f_coef: "coef.bin", dble, 1, big
[end]

[output_rt_agcm_to_lsm_river]
  dir: "${dir_tmp_this}/rt_agcm_to_lsm-river"
  f_sidx: "grid.bin", int4, 1, big
  f_tidx: "grid.bin", int4, 2, big
  f_area: "area.bin", dble, 1, big
  f_coef: "coef.bin", dble, 1, big
[end]

[output_rt_agcm_to_lsm_noriv]
  dir: "${dir_tmp_this}/rt_agcm_to_lsm-noriv"
  f_sidx: "grid.bin", int4, 1, big
  f_tidx: "grid.bin", int4, 2, big
  f_area: "area.bin", dble, 1, big
  f_coef: "coef.bin", dble, 1, big
[end]

[output_agcm]
  dir: "${dir_tmp_this}/agcm"
  f_lndara_ogcm      : "lndara_ogcm.bin"
  f_lndara_river     : "lndara_river.bin"
  f_lndara_noriv_real: "lndara_noriv_real.bin"
  f_lndara_noriv_virt: "lndara_noriv_virt.bin"
  f_lndara_noriv     : "lndara_noriv.bin"
[end]

[output_lsm]
  dir: "${dir_tmp_this}/lsm"

  f_grdmsk_river     : "grdmsk_river.bin", real, endian=big
  f_grdmsk_noriv     : "grdmsk_noriv.bin", real, endian=big
  f_grdmsk_noriv_real: "grdmsk_noriv-real.bin", real, endian=big
  f_grdmsk_noriv_virt: "grdmsk_noriv-virt.bin", real, endian=big

  f_grdidx_bnd_river     : "grdidx_bnd_river.bin"
  f_grdidx_bnd_noriv     : "grdidx_bnd_noriv.bin"
  f_grdidx_bnd_noriv_real: "grdidx_bnd_noriv-real.bin"
  f_grdidx_bnd_noriv_virt: "grdidx_bnd_noriv-virt.bin"

  f_grdidx_river     : "grdidx_river.bin"
  f_grdidx_noriv     : "grdidx_noriv.bin"
  f_grdidx_noriv_real: "grdidx_noriv-real.bin"
  f_grdidx_noriv_virt: "grdidx_noriv-virt.bin"

  f_grdara_river     : "grdara_river.bin"
  f_grdara_noriv     : "grdara_noriv.bin"
  f_grdara_noriv_real: "grdara_noriv-real.bin"
  f_grdara_noriv_virt: "grdara_noriv-virt.bin"

  f_grdwgt_river     : "grdwgt_river.bin"
  f_grdwgt_noriv     : "grdwgt_noriv.bin"
  f_grdwgt_noriv_virt: "grdwgt_noriv-virt.bin"
  f_grdwgt_noriv_real: "grdwgt_noriv-real.bin"

  f_rstidx_river     : "rstidx_river.bin"
  f_rstidx_noriv     : "rstidx_noriv.bin"
  f_rstidx_noriv_real: "rstidx_noriv-real.bin"
  f_rstidx_noriv_virt: "rstidx_noriv-virt.bin"

  f_rstidx_bnd_river     : "rstidx_bnd_river.bin"
  f_rstidx_bnd_noriv     : "rstidx_bnd_noriv.bin"
  f_rstidx_bnd_noriv_real: "rstidx_bnd_noriv-real.bin"
  f_rstidx_bnd_noriv_virt: "rstidx_bnd_noriv-virt.bin"
[end]

[options]
  old_files: remove
[end]
EOF

${exec_main_ext_cpl_aogcm_ils_define_mat} ${f_conf}

cp -f ../tmp/${name_step_01}/*.bin ${dir_tmp_this}/agcm/.
