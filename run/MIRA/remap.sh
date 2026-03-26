#!/bin/bash

ITERMAX=3

# Make remapping tables
#for isForth in True False; do
#  python3 remap.py rt True CS 0 True ICOD 0 ${isForth}
#done

# Remap iteratively
for iter in $(seq 0 $ITERMAX); do
  for isForth in True False; do
    python3 remap.py remap True CS 0 True ICOD 0 ${isForth} ${iter}
  done
done
