#!/bin/bash
set -e

file_1min_flwdir="../../../../dat/GlobalFlowDir_20220420/flwdir.bin"
file_1min_elevtn="../../../../dat/GlobalFlowDir_20220420/elevtn.bin"
file_1min_lndmsk="../../../../out/cpl_aogcm-ils/sample/01_rasterize_ogcm/mask.bin"
#---------------------------------------------------------------
#
#---------------------------------------------------------------
if [ ! -f $file_1min_flwdir ]; then
  echo "File not found (file_1min_flwdir): $file_1min_flwdir"
  exit 1
fi

if [ ! -f $file_1min_elevtn ]; then
  echo "File not found (file_1min_elevtn): $file_1min_elevtn"
  exit 1
fi

if [ ! -f $file_1min_lndmsk ]; then
  echo "File not found (file_1min_lndmsk): $file_1min_lndmsk"
  exit 1
fi

rm -rf 1min_flwdir
mkdir 1min_flwdir

ln -s ../${file_1min_flwdir} 1min_flwdir/flwdir.bin
ln -s ../${file_1min_elevtn} 1min_flwdir/elevtn.bin
ln -s ../${file_1min_lndmsk} 1min_flwdir/lndmsk.bin

rm -rf gcmmap
mkdir gcmmap

rm -rf tmp
mkdir tmp
mkdir tmp/1min
mkdir tmp/map
mkdir tmp/map/1min

rm -rf map
mkdir map

rm -f src
ln -s ../../src .
#---------------------------------------------------------------
#
#---------------------------------------------------------------
./src/make_gcmmap.exe

./src/modify_hires.exe
./src/calc_uparea.exe

./src/const_network.exe
./src/define_catchment.exe
./src/visual_check.exe
./src/gcm_rivermap.exe
./src/set_map.exe
./src/calc_inpmat.exe
#---------------------------------------------------------------
#
#---------------------------------------------------------------
cp -r tmp/map/1min map/.
