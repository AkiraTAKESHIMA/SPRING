import os
import sys
import argparse
import numpy as np
import matplotlib.pyplot as plt


if __name__ == '__main__':
    #-----------------------------------------------------------
    # Read arguments
    #-----------------------------------------------------------
    parser = argparse.ArgumentParser()
    parser.add_argument('--metricsFile', required=True, metavar='metricsFile')
    parser.add_argument('--dirout', required=True, metavar='dirout')

    args = parser.parse_args()

    metricsFile = args.metricsFile
    dirout = args.dirout
    #-----------------------------------------------------------
    # Read metrics
    #-----------------------------------------------------------
    fp = open(metricsFile,'r')
    list_metricName = [s.strip() for s in fp.readline().strip().split(',')]

    metrics = {}
    for metricName in list_metricName:
        metrics[metricName] = []

    list_iIter = []
    for line in fp.readlines():
        list_iIter.append(line.strip().split(',')[0])
        d = list(map(float,line.strip().split(',')[1:]))
        for i in range(len(list_metricName)):
            metrics[list_metricName[i]].append(d[i])

    for metricName in list_metricName:
        metrics[metricName] = np.array(metrics[metricName])
    #-----------------------------------------------------------
    # Plot
    #-----------------------------------------------------------
    print('Plotting L_g')
    plt.figure(figsize=(4,6))
    plt.plot(list_iIter, np.log10(abs(metrics['L_g'])), linewidth=2, color='red')
    plt.title(title)
    plt.ylabel('log10|L_g|')
    plt.xlabel('Nr')
    plt.savefig(os.path.join(dirout,'L_g.png'),
                bbox_inches='tight', pad_inches=0.1)
    plt.show()
    quit()

    print('Plotting L_1')
    plt.figure(figsize=(4,6))
    plt.plot(list_iIter, np.log10(np.abs(metrics['L_1'])), linewidth=2, color='red')
    plt.title(title)
    plt.ylabel('log10|L_1|')
    plt.xlabel('Nr')
    plt.savefig(os.path.join(dirout,'L_1.png'))

    print('Plotting L_2')
    plt.figure(figsize=(4,6))
    plt.plot(list_iIter, np.log10(metrics['L_2']), linewidth=2, color='red')
    plt.title(title)
    plt.ylabel('log10(L_2)')
    plt.xlabel('Nr')

    print('Plotting L_inf')
    plt.figure(figsize=(4,6))
    plt.plot(list_iIter, metrics['L_inf'], linewidth=2, color='red')
    plt.title(title)
    plt.ylabel('L_inf')
    plt.xlabel('Nr')

    print('Plotting Lmin')
    plt.figure(figsize=(4,6))
    plt.plot(list_iIter, metrics['Lmin'], linewidth=2, color='red')
    plt.title(title)
    plt.ylabel('Lmin')
    plt.xlabel('Nr')

    print('Plotting Lmax')
    plt.figure(figsize=(4,6))
    plt.plot(list_iIter, metrics['Lmax'], linewidth=2, color='red')
    plt.title(title)
    plt.ylabel('Lmax')
    plt.xlabel('Nr')

    print('Plotting Lmin_1')
    plt.figure(figsize=(4,6))
    plt.plot(list_iIter, metrics['Lmin_1'], linewidth=2, color='red')
    plt.title(title)
    plt.ylabel('Lmin_1')
    plt.xlabel('Nr')

    print('Plotting Lmin_2')
    plt.figure(figsize=(4,6))
    plt.plot(list_iIter, metrics['Lmin_2'], linewidth=2, color='red')
    plt.title(title)
    plt.ylabel('Lmin_2')
    plt.xlabel('Nr')

    print('Plotting Lmin_inf')
    plt.figure(figsize=(4,6))
    plt.plot(list_iIter, metrics['Lmin_inf'], linewidth=2, color='red')
    plt.title(title)
    plt.ylabel('Lmin_inf')
    plt.xlabel('Nr')

