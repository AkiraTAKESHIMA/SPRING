#/bin/bash
set -e
#===============================================================
#
#===============================================================
function get_bbox_latlon_from_tileName () {
  local tileName=${1}

  #-------------------------------------------------------------
  # Case: Global
  if [ ${tileName} == "global" ]; then
    west_tile=-180
    east_tile=180
    south_tile=-90
    north_tile=90
  #-------------------------------------------------------------
  # Case: Tiled; GLCNMO, GTOPO30, HWSD, JRA55 (e.g. E130N40)
  else
    c1=`echo ${tileName} | cut -c 1`
    if [ ${c1} == 'W' -o ${c1} == 'E' ]; then
      if [ ${c1} == 'W' ]; then
        west_tile=$(( -10#`echo ${tileName} | cut -c 2-4` ))
      else
        west_tile=$(( 10#`echo ${tileName} | cut -c 2-4` ))
      fi
      east_tile=$(( west_tile + 10 ))

      c2=`echo ${tileName} | cut -c 5`
      if [ ${c2} == 'N' ]; then
        north_tile=$(( 10#`echo ${tileName} | cut -c 6-7` ))
      else
        north_tile=$(( -10#`echo ${tileName} | cut -c 6-7` ))
      fi
      south_tile=$(( north_tile - 10 ))
    #-----------------------------------------------------------
    # Case:ERROR
    else
      echo "****** ERROR @ ${FUNCNAME[0]} ******" &>2
      echo "Invalid value in \$tileName: ${tileName}" &>2
      exit 1
    fi
  fi
}
