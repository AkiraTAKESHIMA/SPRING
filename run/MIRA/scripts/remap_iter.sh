#!/bin/bash
set -e

tbl_res="4"
tbl_mesh="CS ICOD RLL"
var="TPW"

itermax=1
display_log=false

exe_remap="../../bin/std/remap.exe"

export var

# back is source to target

for srcMesh in $tbl_mesh; do
  for tgtMesh in $tbl_mesh; do
    #echo $srcMesh $tgtMesh

    [ $srcMesh == $tgtMesh ] && continue

    [ $srcMesh != CS -o $tgtMesh != ICOD ] && continue

    srcRes="4"
    tgtRes="4"

    srcMeshFull="MIRAu-${srcMesh}-${srcRes}"
    tgtMeshFull="MIRAu-${tgtMesh}-${tgtRes}"

    dir_conf="set/remap/${srcMeshFull}_to_${tgtMeshFull}"
    path_conf_back_template="${dir_conf}/tmpl_iter_back.conf"
    path_conf_forth_template="${dir_conf}/tmpl_iter_forth.conf"

    #dir_rt_in="../../out/remap/${srcMeshFull}_to_${tgtMeshFull}"
    #export length_rt=

    dir_remap="out/remap/${srcMeshFull}_to_${tgtMeshFull}"

    dir_log="${dir_remap}/log"
    mkdir -p ${dir_log}

    dir_field="${dir_remap}/field"
    mkdir -p ${dir_field}

    path_srcField="${dir_field}/${var}_src_iter0000.bin"
    path_srcField_org="../../dat/mesh/MIRA/UniformlyRefined/${srcMesh}/r${srcRes}/val_${var}.bin"
    cp ${path_srcField_org} ${path_srcField}
    echo "Copied field data."

    for i in $(seq 1 ${itermax}); do
      # Variables in template files
      export i0Full=`printf "%04d" $((i-1))`
      export i1Full=`printf "%04d" ${i}`

      path_conf_back="${dir_conf}/iter${i1Full}_back.conf"
      path_conf_forth="${dir_conf}/iter${i1Full}_forth.conf"
      envsubst < ${path_conf_back_template} > ${path_conf_back}
      envsubst < ${path_conf_forth_template} > ${path_conf_forth}

      path_log_back="${dir_log}/iter${i1Full}_back.txt"
      path_log_forth="${dir_log}/iter${i1Full}_forth.txt"

      echo "iter ${i1Full} backward: ${path_log_back}"
      if ${display_log}; then
        ${exe_remap} ${path_conf_back} 2>&1 | tee ${path_log_back}
      else
        ${exe_remap} ${path_conf_back} >${path_log_back} 2>&1
      fi

      echo "iter ${i1Full} forward : ${path_log_forth}"
      if ${display_log}; then
        ${exe_remap} ${path_conf_forth} 2>&1 | tee ${path_log_forth}
      else
        ${exe_remap} ${path_conf_forth} >${path_log_forth} 2>&1
      fi
    done

  done
done
  
