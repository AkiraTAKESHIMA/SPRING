#!/bin/bash
set -e
#===============================================================
#
#===============================================================
function write_conf_remap_remapping () {
  f_conf=${1}
  srcMeshName=${2}
  tgtMeshName=${3}
  vrfSrcForm=${4}
  vrfTgtForm=${5}

cat << EOF >> ${f_conf}

[remapping]
  dir: "../tmp/01_make_rt/${srcMeshName}_to_${tgtMeshName}"
  fout_rt_sidx: "grid.bin", int4, 1, big
  fout_rt_tidx: "grid.bin", int4, 2, big
  fout_rt_area: "area.bin", dble, 1, big
  fout_rt_coef: "coef.bin", dble, 1, big

  vrf_source_form: ${vrfSrcForm}
  fout_vrf_grdidx     : "vrf/src_idx.bin", int4
  fout_vrf_grdara_true: "vrf/src_val.bin", dble, 1
  fout_vrf_grdara_rt  : "vrf/src_val.bin", dble, 2
  fout_vrf_rerr_grdara: "vrf/src_val.bin", dble, 3
  fout_vrf_grdnum     : "vrf/src_num.bin", int4

  vrf_target_form: ${vrfTgtForm}
  fout_vrf_grdidx     : "vrf/tgt_idx.bin", int4
  fout_vrf_grdara_true: "vrf/tgt_val.bin", dble, 1
  fout_vrf_grdara_rt  : "vrf/tgt_val.bin", dble, 2
  fout_vrf_rerr_grdara: "vrf/tgt_val.bin", dble, 3
  fout_vrf_grdnum     : "vrf/tgt_num.bin", int4
[end]
EOF
}
