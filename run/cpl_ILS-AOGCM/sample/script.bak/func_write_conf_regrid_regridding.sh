#!/bin/bash
set -e

function write_conf_regrid_regridding () {
  if [ $# -ne 5 ]; then
    echo "****** ERROR @ ${FUNCNAME[0]} ******" >&2
    echo "Invalid number of arguments" >&2
    exit 1
  fi

  local grid_coef=${1}
  local opt_coef_sum_modify=${2}
  local dir_tmp_this=${3}
  local vrfSrcForm=${4}
  local vrfTgtForm=${5}

  cat << EOF >> ${f_conf}

[regridding]
  grid_coef: ${grid_coef}
  grid_sort: target

  dir: "${dir_tmp_this}"
  fout_rt_sidx: "grid.bin", int4, 1, big
  fout_rt_tidx: "grid.bin", int4, 2, big
  fout_rt_area: "area.bin", dble, 1, big
  fout_rt_coef: "coef.bin", dble, 1, big
EOF

  if [ ${opt_coef_sum_modify} != "undef" ]; then
    cat << EOF >> ${f_conf}

    opt_coef_sum_modify: ${opt_coef_sum_modify}
EOF
  fi

  if [ ${vrfSrcForm} != "none" ]; then
    cat << EOF >> ${f_conf}

    vrf_source_form: ${vrfSrcForm}
    fout_vrf_grdidx     : "vrf/src_idx.bin"
    fout_vrf_grdara_true: "vrf/src_val.bin", rec=1
    fout_vrf_grdara_rt  : "vrf/src_val.bin", rec=2
    fout_vrf_rerr_grdara: "vrf/src_val.bin", rec=3
    fout_vrf_grdnum     : "vrf/src_num.bin"
EOF
  fi

  if [ ${vrfTgtForm} != "none" ]; then
    cat << EOF >> ${f_conf}

    vrf_target_form: ${vrfTgtForm}
    fout_vrf_grdidx     : "vrf/tgt_idx.bin"
    fout_vrf_grdara_true: "vrf/tgt_val.bin", rec=1
    fout_vrf_grdara_rt  : "vrf/tgt_val.bin", rec=2
    fout_vrf_rerr_grdara: "vrf/tgt_val.bin", rec=3
    fout_vrf_grdnum     : "vrf/tgt_num.bin"
EOF
  fi

  cat << EOF >> ${f_conf}
[end]
EOF
}
