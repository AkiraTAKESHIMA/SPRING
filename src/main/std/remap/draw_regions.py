import os
import sys
import numpy as np
import matplotlib.pyplot as plt

f = sys.argv[1]

def read_dat(f):
    regions = []

    for line in open(f,'r').readlines()[2:]:
        dat = line.strip().split()
        rank = int(dat[0])
        west, east, south, north = map(float, dat[1:5])
        msij, mtij = map(int, dat[5:7])
        nRegions = int(dat[7])

        lon = [west, west, east, east, west]
        lat = [north, south, south, north, north]

        region = {
          'rank': rank,
          'lon': lon,
          'lat': lat,
          'msij': msij,
          'mtij': mtij,
          'nRegions': nRegions,
        }

        regions.append(region)

    return regions

dat = open(f,'r').readline().strip().split()
nsij, ntij = int(dat[1]), int(dat[3])

regions = read_dat(f)

mij_sum = sum([r['msij']*r['mtij'] for r in regions])
mij_max = max([r['msij']*r['mtij'] for r in regions])

print('mij_sum {} ({:.3e} %))'\
      .format(mij_sum, mij_sum/float(nsij*ntij)*1e2))

for r in regions:
    plt.fill(r['lon'], r['lat'], linewidth=0.5, edgecolor='gray',
             facecolor=plt.cm.jet(int(float(r['msij']*r['mtij'])/(mij_max)*255)))
plt.show()
