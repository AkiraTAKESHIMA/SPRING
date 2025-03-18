#!/bin/bash
set -e
#================================================================
#
#================================================================
. ./func_get_raster_bounds_from_bbox.sh
#================================================================
#
#================================================================
function make_conf_remap_modis () {
  f_conf="${dir_set}/${tileName}_${landType}.conf"

  get_raster_bounds_from_bbox ${west_tile} ${east_tile} ${south_tile} ${north_tile}

  if [ ${dxxi} -eq 0 ]; then
    stat=1
    return
  fi

  stat=0
  #echo "f_conf: ${f_conf}"

  cat << EOF > ${f_conf}
#
path_report: "${dir_out}/report/${tileName}_${landType}.txt"

[grid_system_polygon]
  np: 4
  nij: 1440000
  dir: "${dir_coords_modis}"
  f_lon_vertex: "lon_${tileName}.bin"
  f_lat_vertex: "lat_${tileName}.bin"
  coord_unit: degree
  coord_miss: -999.d0
  arc_parallel: .true.
[end]

[grid_system_raster]
  nx: ${ndx}
  ny: ${ndy}
  west: ${west}
  east: ${east}
  south: ${south}
  north: ${north}
  xi: ${dxxi}
  xf: ${dxxf}
  yi: ${dyyi}
  yf: ${dyyf}
  dir: "../tmp/01_make_idx/matsiro"
  fin_rstidx: "1min/index_mkbnd_${landType}.bin"
  fin_grdidx: "30min/index_mkbnd_${landType}.bin"
  in_grid_sz: ${ncx}, ${ncy}
  idx_miss: -9999
  is_south_to_north: .false.
[end]

[remapping]
  dir: "${dir_out}/rt"
  fout_rt_sidx: "mapping_table_idx_${tileName}_${landType}.bin", int4, 1, big
  fout_rt_tidx: "mapping_table_idx_${tileName}_${landType}.bin", int4, 2, big
  fout_rt_area: "mapping_table_area_${tileName}_${landType}.bin", dble, 1, big
  fout_rt_coef: "mapping_table_coef_${tileName}_${landType}.bin", dble, 1, big

  allow_empty: .true.
EOF

  if [ ${make_verification_data} -eq 0 ]; then
    cat << EOF >> ${f_conf}

  vrf_source_form: auto
  fout_vrf_grdidx     : "vrf/src_idx_${tileName}_${landType}.bin", int4, 1, little
  fout_vrf_grdara_true: "vrf/src_val_${tileName}_${landType}.bin", dble, 1, little
  fout_vrf_grdara_rt  : "vrf/src_val_${tileName}_${landType}.bin", dble, 2, little
  fout_vrf_rerr_grdara: "vrf/src_val_${tileName}_${landType}.bin", dble, 3, little

  vrf_target_form: index
  fout_vrf_grdidx     : "vrf/tgt_idx_${tileName}_${landType}.bin", int4, 1, little
  fout_vrf_grdara_true: "vrf/tgt_val_${tileName}_${landType}.bin", dble, 1, little
  fout_vrf_grdara_rt  : "vrf/tgt_val_${tileName}_${landType}.bin", dble, 2, little
  fout_vrf_rerr_grdara: "vrf/tgt_val_${tileName}_${landType}.bin", dble, 3, little
EOF
  fi

  cat << EOF >> ${f_conf}
[end]
EOF

  if [ ${make_verification_data} -eq 0 ]; then
    cat << EOF >> ${f_conf}

[figures]
  dir: "${dir_out}/rt/vrf"
  cmap: bwr
  val_miss: -1e20

  path_fig: ""
  fin_grdval: "src_val.bin", rec=3
  vmin: -1e-7
  vmax:  1e-7

  path_fig: ""
  fin_grdval: "tgt_val.bin", rec=3
  vmin: -1e-13
  vmax:  1e-13
[end]
EOF
  fi

  cat << EOF >> ${f_conf}

[options]
  old_files: remove

  earth_shape: ${earth_shape}
  earth_r: ${earth_r}
[end]
EOF

  return 0
}
