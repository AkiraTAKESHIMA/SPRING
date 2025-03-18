import os
import sys
import warnings
import argparse
from netCDF4 import Dataset
import numpy as np
import math as mt
from numba import jit
import matplotlib.pyplot as plt



def computeGlobalWeightedIntegral(varF, areas):
    INT = np.dot(areas, varF)

    return INT


def computeGlobalConservation(sample, remap, area):
    L_sample = computeGlobalWeightedIntegral(sample, area)

    L_remap = computeGlobalWeightedIntegral(remap, area)

    Labs_sample = computeGlobalWeightedIntegral(np.abs(sample), area)

    L_g = (L_sample - L_remap) / Labs_sample

    return L_g


def computeStandardNorms(sample, remap, area):
    diff = np.abs(np.subtract(sample, remap))
    diff2 = np.power(diff, 2)
    sample2 = np.power(sample, 2)

    L1Num = computeGlobalWeightedIntegral(diff, area)
    L2Num = computeGlobalWeightedIntegral(diff2, area)
    LinfNum = np.amax(diff)

    L1Den = computeGlobalWeightedIntegral(np.abs(sample), area)
    L2Den = computeGlobalWeightedIntegral(sample2, area)
    LinfDen = np.amax(np.abs(sample))

    L_1 = L1Num / L1Den
    L_2 = mt.sqrt(L2Num / L2Den)
    L_inf = LinfNum / LinfDen

    return L_1, L_2, L_inf


def computeGlobalExtremaMetrics(sample, remap):
    Lden = np.max(abs(sample)) - np.min(abs(sample))
    L_min = min(np.amin(remap) - np.amin(sample), 0.0) / Lden
    L_max = max(np.amax(remap) - np.amax(sample), 0.0) / Lden

    return L_min, L_max



@jit(nopython=True, parallel=False)
def loopComputeLocalPatchExtrema(
        minDiff, maxDiff, varConStenDex, varST, varS2T):

    for jj in range(varST.shape[0]):

        # Compute the patch extrema using the sampled target data and adjacency
        # stencil
        lPmin, lPmax = computeLocalPatchExtrema(
            jj, varConStenDex, varST)
        lPminST, lPmaxST = computeLocalPatchExtrema(
            jj, varConStenDex, varS2T)

        # Compute the min and max difference arrays
        minDiff[jj] = min(lPminST - lPmin, 0)  # < 0 indicates failure
        maxDiff[jj] = max(lPmaxST - lPmax, 0)  # > 0 indicates failure


@jit(nopython=True, parallel=False)
def computeLocalPatchExtrema(jj, varConStenDexT, varST):

    # Fetch the stencil of neighboring elements/cells
    sdex = varConStenDexT[jj, :] - 1

    # Fetch the cell values
    pmin = np.amin(varST[sdex])
    pmax = np.amax(varST[sdex])

    return pmin, pmax



def computeLocalExtremaMetrics(
        sample, remap, area, varConStenDex):

    sample2 = np.power(sample, 2)
    minDiff = np.zeros(sample.shape)
    maxDiff = np.zeros(sample.shape)

    loopComputeLocalPatchExtrema(
        minDiff, maxDiff, varConStenDex, sample, remap)

    L1Den = computeGlobalWeightedIntegral(np.abs(sample), area)
    L2Den = computeGlobalWeightedIntegral(sample2, area)
    LinfDen = np.amax(sample) - np.amin(sample)
    if LinfDen < 1e-14:
        Linf = 1.0  # max and min values are same

    L1Num = computeGlobalWeightedIntegral(minDiff, area)
    L2Num = computeGlobalWeightedIntegral(np.power(minDiff,2), area)
    LinfNum = np.amin(minDiff)

    Lmin_1 = L1Num / L1Den
    Lmin_2 = mt.sqrt(L2Num / L2Den)
    Lmin_inf = LinfNum / LinfDen

    L1Num = computeGlobalWeightedIntegral(maxDiff, area)
    L2Num = computeGlobalWeightedIntegral(np.power(maxDiff,2), area)
    LinfNum = np.amax(maxDiff)

    Lmax_1 = L1Num / L1Den
    Lmax_2 = mt.sqrt(L2Num / L2Den)
    Lmax_inf = LinfNum / LinfDen

    return Lmin_1, Lmin_2, Lmin_inf, Lmax_1, Lmax_2, Lmax_inf


if __name__ == '__main__':
    sourceSampledFile = ''
    sourceFieldFile = ''
    sourceMaskFile = ''

    parser = argparse.ArgumentParser()
    parser.add_argument('--meshFile', required=True, metavar='meshFile')
    parser.add_argument('--sampleFile', required=True, metavar='sampleFieldFile')
    parser.add_argument('--remapFile', required=True, metavar='remappedFieldFile')
    parser.add_argument('--maskFile', required=False, metavar='maskFile')
    parser.add_argument('--outFile', required=True, metavar='output')

    args = parser.parse_args()

    meshFile = args.meshFile
    sampleFieldFile = args.sampleFile
    remappedFieldFile = args.remapFile
    maskFile = args.maskFile
    outFile = args.outFile

    varAreaName = 'cell_area'
    varAdjaName = 'cell_edge_adjacency'
    # CS
    #connCell = 'connect1'
    #-----------------------------------------------------------
    # Read mesh, sample field and mask
    #-----------------------------------------------------------
    print('Reading {}'.format(meshFile))
    nc = Dataset(meshFile)
    #print(nc.variables[varAreaName])

    with warnings.catch_warnings():
        warnings.simplefilter('ignore', DeprecationWarning)
        area = nc.variables[varAreaName][:]
        varConStenDex = nc.variables[varAdjaName][:]

    nc.close()

    sampleField = np.fromfile(sampleFieldFile)

    if maskFile is None:
        mask = np.zeros((sampleField.size)) == 0
    else:
        mask = np.fromfile(maskFile,dtype=np.int8) == 1

    print('area min: {:10.3e}, max: {:10.3e}'.format(area.min(), area.max()))
    print('adjcacenty:')
    print(varConStenDex)
    #-----------------------------------------------------------
    # Calc. metrics
    #-----------------------------------------------------------
    globalINT = np.dot(area[mask], sampleField[mask])
    denom_L_g = np.dot(area[mask], np.abs(sampleField[mask]))
    denom_L_1 = denom_L_g
    denom_L_2 = np.dot(area[mask], np.power(sampleField[mask],2))

    list_metricName = [
        'GC', 
        'GL1', 'GL2', 'GLinf',
        'GminE', 'GmaxE',
        'LminL1', 'LminL2', 'LminLm',
        'LmaxL1', 'LmaxL2', 'LmaxLm',
    ]

    list_iIter = []
    
    metrics = {}
    for metricName in list_metricName:
        metrics[metricName] = []

    nIter = 1000
    for iIter in range(1,nIter+1):
    #for iIter in range(1,2):
        fname = os.path.basename(remappedFieldFile)
        extension = fname[::-1][:fname[::-1].index('.')][::-1]
        fname = '{}_{:04d}.{}'.format(fname[:len(fname)-len(extension)-1],iIter,extension)
        path_remap = os.path.join(os.path.dirname(remappedFieldFile), fname)
        if not os.path.isfile(path_remap):
            continue
        print(fname)

        list_iIter.append(iIter)

        remappedField = np.fromfile(path_remap)

        L_g = computeGlobalConservation(
            sampleField, remappedField, area)

        L_1, L_2, L_inf = computeStandardNorms(
            sampleField, remappedField, area)

        Lmin, Lmax = computeGlobalExtremaMetrics(
            sampleField, remappedField)

        Lmin_1, Lmin_2, Lmin_inf, Lmax_1, Lmax_2, Lmax_inf = \
            computeLocalExtremaMetrics(
                sampleField, remappedField, area, varConStenDex)


        metrics['GC'].append(L_g)
        metrics['GL1'].append(L_1)
        metrics['GL2'].append(L_2)
        metrics['GLinf'].append(L_inf)
        metrics['GminE'].append(Lmin)
        metrics['GmaxE'].append(Lmax)
        metrics['LminL1'].append(Lmin_1)
        metrics['LminL2'].append(Lmin_2)
        metrics['LminLm'].append(Lmin_inf)
        metrics['LmaxL1'].append(Lmax_1)
        metrics['LmaxL2'].append(Lmax_2)
        metrics['LmaxLm'].append(Lmax_inf)

    for metricName in list_metricName:
        metrics[metricName] = np.array(metrics[metricName])


    wfmt = '{0:4d}'
    for i in range(len(list_metricName)):
        wfmt += ',{{1[{}]:12.5e}}'.format(i)
    wfmt += '\n'

    print('Writing {}'.format(outFile))
    dir_out = os.path.dirname(outFile)
    if not os.path.isdir(dir_out):
        os.makedirs(dir_out)

    fp = open(outFile,'w')

    fp.write('iIter')
    for metricName in list_metricName:
        fp.write(', '+metricName)
    fp.write('\n')

    for i, iIter in enumerate(list_iIter):
        fp.write(wfmt.format(
          iIter, [metrics[metricName][i] for metricName in list_metricName]))
    fp.close()

