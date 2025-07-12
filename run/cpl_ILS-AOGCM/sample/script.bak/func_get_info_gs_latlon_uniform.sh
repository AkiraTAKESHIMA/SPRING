#!/bin/bash
set -e

function get_info_gs_latlon_uniform () {
  if [ $# -ne 1 ]; then
    echo "****** ERROR @ ${FUNCNAME[0]} ******" >&2
    echo "Invalid number of arguments" >&2
    exit 1
  fi

  local gsName=${1}

  if [ ${gsName} == ${io_bnd_name} ]; then
    nx=${io_bnd_nx}
    ny=${io_bnd_ny}
    west=${io_bnd_west}
    east=${io_bnd_east}
    south=${io_bnd_south}
    north=${io_bnd_north}
    is_south_to_north=${io_bnd_is_south_to_north}
  elif [ ${gsName} == ${io_met_name} ]; then
    nx=${io_met_nx}
    ny=${io_met_ny}
    west=${io_met_west}
    east=${io_met_east}
    south=${io_met_south}
    north=${io_met_north}
    is_south_to_north=${io_met_is_south_to_north}
  elif [ ${gsName} == ${io_metnc_name} ]; then
    nx=${io_metnc_nx}
    ny=${io_metnc_ny}
    west=${io_metnc_west}
    east=${io_metnc_east}
    south=${io_metnc_south}
    north=${io_metnc_north}
    is_south_to_north=${io_metnc_is_south_to_north}
  elif [ ${gsName} == ${io_row_name} ]; then
    nx=${io_row_nx}
    ny=${io_row_ny}
    west=${io_row_west}
    east=${io_row_east}
    south=${io_row_south}
    north=${io_row_north}
    is_south_to_north=${io_row_is_south_to_north}
  elif [ ${gsName} == ${io_rect_name} ]; then
    nx=${io_rect_nx}
    ny=${io_rect_ny}
    west=${io_rect_west}
    east=${io_rect_east}
    south=${io_rect_south}
    north=${io_rect_north}
    is_south_to_north=${io_rect_is_south_to_north}
  elif [ ${gsName} == ${io_dummy_name} ]; then
    nx=${io_dummy_nx}
    ny=${io_dummy_ny}
    west=${io_dummy_west}
    east=${io_dummy_east}
    south=${io_dummy_south}
    north=${io_dummy_north}
    is_south_to_north=${io_met_is_south_to_north}
  else
    echo "****** ERROR @ ${FUNCNAME[0]} ******" >&2
    echo "Invalid value in \$gsName: ${gsName}" >&2
    exit 1
  fi
}
