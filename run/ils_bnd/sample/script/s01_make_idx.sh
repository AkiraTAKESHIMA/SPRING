#!/bin/bash
set -e
#===============================================================
#
#===============================================================
dir_set="../set/01_make_idx"
dir_out="../tmp/01_make_idx"
f_conf="${dir_set}/make_cmf_mat.conf"
mkdir -p ${dir_set}

cat << EOF > ${f_conf}
#
[common]
  nx_grid: ${ncx}
  ny_grid: ${ncy}
  nx_raster: ${ndx}
  ny_raster: ${ndy}
  west: ${west}
  east: ${east}
  south: ${south}
  north: ${north}
[end]

[cama-flood]
  fin_nextxy: "${f_nextxy}", ${dtype_nextxy}, 1, ${endian_nextxy}
  fin_catmxy: "${f_catmxy}", ${dtype_catmxy}, 1, ${endian_catmxy}

  nextxy_river_mouth  : ${nextxy_river_mouth}
  nextxy_river_inland : ${nextxy_river_inland}
  nextxy_ocean        : ${nextxy_ocean}
  catmxy_noriv_coastal: ${catmxy_noriv_coastal}
  catmxy_noriv_inland : ${catmxy_noriv_inland}
  catmxy_ocean        : ${catmxy_ocean}

  dir: "${dir_out}/cmf"
  fout_grdidx_river: "30min/index_river.bin"
  fout_grdidx_noriv: "30min/index_noriv.bin"
  fout_rstidx_river: "1min/index_river.bin"
  fout_rstidx_noriv: "1min/index_noriv.bin"
  idx_miss: -9999
[end]

[matsiro]
  dir: "${dir_out}/matsiro"

  fout_grdmsk_river: "30min/land_mask_river.bin", real, endian=big
  fout_grdmsk_noriv: "30min/land_mask_noriv.bin", real, endian=big

  fout_grdidx_river: "30min/index_river.bin"
  fout_grdidx_noriv: "30min/index_noriv.bin"
  fout_rstidx_river: "1min/index_river.bin"
  fout_rstidx_noriv: "1min/index_noriv.bin"

  fout_grdidx_bnd_river: "30min/index_bnd_river.bin"
  fout_grdidx_bnd_noriv: "30min/index_bnd_noriv.bin"

  fout_grdidx_mkbnd_river: "30min/index_mkbnd_river.bin"
  fout_grdidx_mkbnd_noriv: "30min/index_mkbnd_noriv.bin"
  fout_rstidx_mkbnd_river: "1min/index_mkbnd_river.bin"
  fout_rstidx_mkbnd_noriv: "1min/index_mkbnd_noriv.bin"

  idx_miss: -9999
[end]

[options]
  old_files: remove

  earth_shape: ${earth_shape}
  earth_r: ${earth_r}

  save_memory: .false.
[end]
EOF

${exec_main_make_cmf_mat} ${f_conf}
#----------------------------------------------------------------
# Make link for landmask
#----------------------------------------------------------------
dir_landmask="../out/land_mask"
dir_matsiro="../out/matsiro"
dir_cmf="../out/cmf"
f_list_tiles="${dir_landmask}/all_tiles.txt"

mkdir -p ${dir_landmask}
mkdir -p ${dir_matsiro}
mkdir -p ${dir_matsiro}/30min
mkdir -p ${dir_matsiro}/1min
mkdir -p ${dir_cmf}
mkdir -p ${dir_cmf}/30min
mkdir -p ${dir_cmf}/1min
rm -f ${f_list_tiles}

for landType in ${list_landType}; do

  echo ${landType} >> ${f_list_tiles}
  ln -nsf ../../tmp/01_make_idx/matsiro/30min/land_mask_${landType}.bin    ${dir_landmask}/.

  ln -nsf ../../../tmp/01_make_idx/matsiro/30min/index_${landType}.bin       ${dir_matsiro}/30min/.
  ln -nsf ../../../tmp/01_make_idx/matsiro/30min/index_bnd_${landType}.bin   ${dir_matsiro}/30min/.
  ln -nsf ../../../tmp/01_make_idx/matsiro/30min/index_mkbnd_${landType}.bin ${dir_matsiro}/30min/.

  ln -nsf ../../../tmp/01_make_idx/matsiro/1min/index_${landType}.bin       ${dir_matsiro}/1min/.
  ln -nsf ../../../tmp/01_make_idx/matsiro/1min/index_mkbnd_${landType}.bin ${dir_matsiro}/1min/.
done


