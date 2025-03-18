#!/bin/bash
set -e
#===============================================================
#
#===============================================================
srcGridName="mat"
tgtGridName="io-RLL"

f_report_river="../tmp/01_make_rt/${srcGridName}-river_to_${tgtGridName}/report.txt"
f_report_noriv="../tmp/01_make_rt/${srcGridName}-noriv_to_${tgtGridName}/report.txt"

if [ ! -f ${f_report_river} ]; then
  echo "****** ERROR @ $0 ******" >&2
  echo "File not found. \$f_report_river: ${f_report_river}" >&2
  exit 1
fi
if [ ! -f ${f_report_noriv} ]; then
  echo "****** ERROR @ $0 ******" >&2
  echo "File not found. \$f_report_noriv: ${f_report_noriv}" >&2
  exit 1
fi
length_rt_river=`sed -n 3p ${f_report_river} | cut -d " " -f 2`
length_rt_noriv=`sed -n 3p ${f_report_noriv} | cut -d " " -f 2`

f_conf="../set/02_merge_rt/${srcGridName}_to_${tgtGridName}.conf"

cat << EOF > ${f_conf}
#
path_report: "../tmp/02_merge_rt/${srcGridName}_to_${tgtGridName}/report.txt"

[input]
  # river
  length_rt: "${length_rt_river}"
  dir: "../tmp/01_make_rt/${srcGridName}-river_to_${tgtGridName}"
  f_rt_sidx: "grid.bin", int4, 1, big
  f_rt_tidx: "grid.bin", int4, 2, big
  f_rt_area: "area.bin", dble, 1, big
  f_rt_coef: "coef.bin", dble, 1, big

  # noriv
  length_rt: "${length_rt_noriv}"
  dir: "../tmp/01_make_rt/${srcGridName}-noriv_to_${tgtGridName}"
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

  dir: "../tmp/02_merge_rt/${srcGridName}_to_${tgtGridName}"
  f_rt_sidx: "grid.bin", int4, 1, big
  f_rt_tidx: "grid.bin", int4, 2, big
  f_rt_area: "area.bin", dble, 1, big
  f_rt_coef: "coef.bin", dble, 1, big
[end]

[options]
  old_files: remove
[end]
EOF

${exec_main_std_merge_remapping_tables} ${f_conf}
