#!/bin/bash
set -e
#set -x
#===============================================================
#
#===============================================================
f_conf="${dir_set_step}/merge_rt_lsm_to_io-row.conf"
dir_tmp_this="${dir_tmp_step}/rt_lsm_to_io-row"

dir_in_river="${dir_tmp}/${name_step_10}/rt_lsm-river_to_io-row"
dir_in_noriv_real="${dir_tmp}/${name_step_10}/rt_lsm-noriv-real_to_io-row"
dir_in_noriv_virt="${dir_tmp}/${name_step_10}/rt_lsm-noriv-virt_to_io-row"

for dir_in in "${dir_in_river}" "${dir_in_noriv_real}" "${dir_in_noriv_virt}"; do
  if [ ! -f "${dir_in}/report.txt" ]; then
    echo "****** File not found ******"
    echo "report: ${dir_in}/report.txt"
    exit 1
  fi
done

nij_river=`sed -n 3p ${dir_in_river}/report.txt | cut -d " " -f 2`
nij_noriv_real=`sed -n 3p ${dir_in_noriv_real}/report.txt | cut -d " " -f 2`
nij_noriv_virt=`sed -n 3p ${dir_in_noriv_virt}/report.txt | cut -d " " -f 2`
#===============================================================
#
#===============================================================
cat << EOF > ${f_conf}
#
path_report: "${dir_tmp_this}/report.txt"

[input]
  # river
  length_rt: ${nij_river}
  dir: "${dir_in_river}"
  f_rt_sidx: "grid.bin", int4, 1, big
  f_rt_tidx: "grid.bin", int4, 2, big
  f_rt_area: "area.bin", dble, 1, big
  f_rt_coef: "coef.bin", dble, 1, big

  # noriv-real
  length_rt: ${nij_noriv_real}
  dir: "${dir_in_noriv_real}"
  f_rt_sidx: "grid.bin", int4, 1, big
  f_rt_tidx: "grid.bin", int4, 2, big
  f_rt_area: "area.bin", dble, 1, big
  f_rt_coef: "coef.bin", dble, 1, big

  # noriv-virt
  length_rt: ${nij_noriv_virt}
  dir: "${dir_in_noriv_virt}"
  f_rt_sidx: "grid.bin", int4, 1, big
  f_rt_tidx: "grid.bin", int4, 2, big
  f_rt_area: "area.bin", dble, 1, big
  f_rt_coef: "coef.bin", dble, 1, big

  opt_idx_duplication: stop
[end]

[output]
  grid_coef: target
  grid_sort: target
  opt_coef_sum_modify: 1.d0

  dir: "${dir_tmp_this}"
  f_rt_sidx: "grid.bin", int4, 1, big
  f_rt_tidx: "grid.bin", int4, 2, big
  f_rt_area: "area.bin", dble, 1, big
  f_rt_coef: "coef.bin", dble, 1, big
[end]

[options]
  old_files: remove
[end]
EOF
#===============================================================
#
#===============================================================
${exec_main_std_merge_regridding_tables} ${f_conf}
