#!/bin/bash
set -e
set -x
#===============================================================
#
#===============================================================
dir_tmp_step="${dir_tmp}/${name_step_11}"
dir_set_step="${dir_set}/${name_step_11}"

mkdir -p ${dir_set_step}
#===============================================================
#
#===============================================================
dir_in="${dir_tmp}/${name_step_07}/rt_agcm_to_ogcm"
if [ ! -f "${dir_in}/report.txt" ]; then
  echo "****** File not found ******"
  echo "report: ${dir_in}/report.txt"
  exit 1
fi

nij_rt_agcm_to_ogcm=`sed -n 3p ${dir_in}/report.txt | cut -d " " -f 2`
#===============================================================
#
#===============================================================
for component in "lsm" "rm"; do
  if [ ${component} == "lsm" ]; then
    dir_lsm="${dir_tmp}/${name_step_09}"
    list_landType="river noriv-real noriv-virt"
  elif [ ${component} == "rm" ]; then
    dir_lsm="${dir_tmp}/${name_step_06}"
    list_landType="river-end"
  fi

  for landType in ${list_landType}; do
    srcMeshName="${component}-${landType}"

    f_conf="${dir_set_step}/make_rt_${srcMeshName}_to_ogcm_via_agcm.conf"
    dir_tmp_this="${dir_tmp_step}/rt_${srcMeshName}_to_ogcm_via_agcm"

    cat << EOF > ${f_conf}
#
path_report: "${dir_tmp_this}/report.txt"

[input_rt_agcm_to_ogcm]
  length: ${nij_rt_agcm_to_ogcm}
  dir: "${dir_in}"
  f_sidx: "grid.bin", int4, 1, big
  f_tidx: "grid.bin", int4, 2, big
  f_area: "area.bin", dble, 1, big
  f_coef: "coef.bin", dble, 1, big
[end]

[input_agcm]
  nij: 32768
  dir: "${dir_tmp}/${name_step_01}"
  f_grdidx: "grdidx.bin"
  f_grdara: "grdara.bin"
  f_grdlon: "grdlonlat.bin", rec=1
  f_grdlat: "grdlonlat.bin", rec=2
  idx_miss: 0
[end]

[input_lsm]
  nij: 259200
  dir: "${dir_lsm}"
  f_grdidx: "grdidx_${landType}.bin"
  f_grdara: "grdara_${landType}.bin"
  f_grdlon: "grdlonlat_${landType}.bin", rec=1
  f_grdlat: "grdlonlat_${landType}.bin", rec=2
  idx_miss: -9999
[end]

[output_rt_lsm_to_agcm]
  grid_coef: none
  grid_sort: target

  dir: "${dir_tmp_this}"
  fout_rt_sidx: "grid.bin", int4, 1, big
  fout_rt_tidx: "grid.bin", int4, 2, big
  fout_rt_area: "area.bin", dble, 1, big
  fout_rt_coef: "coef.bin", dble, 1, big

  vrf_target_form: index
  fout_vrf_grdnum: "vrf/tgt_grdnum.bin"
[end]

[options]
  old_files: remove
  use_weighted_dist: .true.
[end]
EOF

    ${exec_main_ext_cpl_aogcm_ils_make_rt_for_ogcm} ${f_conf}
  done
done
