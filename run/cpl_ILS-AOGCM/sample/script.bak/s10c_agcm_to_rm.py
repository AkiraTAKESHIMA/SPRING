import os
import sys
import numpy as np
import matplotlib.pyplot as plt


def make_rt(sidx, tidx):

    def make_rt_core(naij, nbij):
        mij_max = int((naij-1) / nbij) + 1
        mij_min = mij_max - 1

        nij_short = naij - mij_min * nbij
        list_ij_add = np.linspace(0, nbij, nij_short+2)[1:-1].astype(np.int32) - 1
        print('mij_min: {}, mij_max: {}, nij_short: {}'.format(mij_min, mij_max, nij_short))
        print('ij_add: {}, ..., {}'.format(list_ij_add[:3],list_ij_add[-3:]))

        list_mij = np.empty((nbij),dtype=np.int32)
        list_mij[:] = mij_min
        list_mij[list_ij_add] += 1
        nij = int(list_mij.sum())
        print('nij: {}'.format(nij))

        if nij < naij:
            print('*** Warning ***')
            print('  nij < naij')
            print('  nij: {}'.format(nij))
            print('  naij: {}'.format(naij))
            list_ij_add = np.where(list_mij == mij_min)
            for i in range(naij-nij):
                list_mij[list_ij_add[i]] += 1
        elif nij > naij:
            print('*** Warning ***')
            print('  nij > naij')
            print('  nij: {}'.format(nij))
            print('  naij: {}'.format(naij))
            list_ij_sub = np.where(list_mij == mij_max)
            for i in range(naij-nij):
                list_mij[list_ij_sub[i]] -= 1

        rt_aij = np.arange(naij,dtype=np.int32)

        rt_bij = np.empty((naij),dtype=np.int32)
        nij = 0
        bij = 0
        for mij in list_mij:
            rt_bij[nij:nij+mij] = bij
            nij += mij
            bij += 1

        print('aij: {}, ..., {}'.format(rt_aij[:3], rt_aij[-3:]))
        print('bij: {}, ..., {}'.format(rt_bij[:3], rt_bij[-3:]))

        return rt_aij, rt_bij


    nsij = sidx[sidx > 0].size
    ntij = tidx[tidx > 0].size
    print('nsij: {}, ntij: {}'.format(nsij, ntij))

    if nsij > ntij:
        rt_sij, rt_tij = make_rt_core(nsij, ntij)
    else:
        rt_tij, rt_sij = make_rt_core(ntij, nsij)

    rt_sidx = sidx[sidx > 0][rt_sij]
    rt_tidx = tidx[tidx > 0][rt_tij]
    print('sidx: {}, ..., {}'.format(rt_sidx[:3], rt_sidx[-3:]))
    print('tidx: {}, ..., {}'.format(rt_tidx[:3], rt_tidx[-3:]))
    #-----------------------------------------------------------
    # Check results
    #-----------------------------------------------------------
    src_grdnum = np.zeros((nsij),dtype=np.int32)
    for sij in rt_sij:
        src_grdnum[sij] += 1

    tgt_grdnum = np.zeros((ntij),dtype=np.int32)
    for tij in rt_tij:
        tgt_grdnum[tij] += 1

    print('src grdnum min: {}, max: {}'.format(src_grdnum.min(),src_grdnum.max()))
    print('tgt grdnum min: {}, max: {}'.format(tgt_grdnum.min(),tgt_grdnum.max()))

    #if src_grdnum.min() != src_grdnum.max():
    #    plt.plot(range(nsij), src_grdnum, linewidth=2, color='red')
    #    plt.title('src_grdnum')
    #    plt.show()

    #if tgt_grdnum.min() != tgt_grdnum.max():
    #    plt.plot(range(ntij), tgt_grdnum, linewidth=2, color='red')
    #    plt.title('tgt_grdnum')
    #    plt.show()

    return rt_sidx, rt_tidx


if __name__ == '__main__':
    f_sidx = sys.argv[1]
    sidx = np.fromfile(f_sidx,dtype=np.int32)

    f_tidx = sys.argv[2]
    tidx = np.fromfile(f_tidx,dtype=np.int32)

    dirout_rt = sys.argv[3]
    fout_rt_grid = os.path.join(dirout_rt,'grid.bin')
    fout_rt_coef = os.path.join(dirout_rt,'coef.bin')
    fout_report = os.path.join(dirout_rt,'report.txt')
    if not os.path.isdir(dirout_rt):
        os.makedirs(dirout_rt)

    rt_sidx, rt_tidx = make_rt(sidx, tidx)
    nij = rt_sidx.size
    rt_coef = np.ones((nij))
    #-----------------------------------------------------------
    # Output
    #-----------------------------------------------------------
    dgt_ij = int(np.log10(nij)) + 1
    dgt_idx = int(np.log10(max(rt_sidx.max(),rt_tidx.max()))) + 1

    print('Writing {}'.format(fout_rt_grid))
    np.c_[rt_sidx,rt_tidx].transpose().astype(np.int32).byteswap().tofile(fout_rt_grid)

    print('Writing {}'.format(fout_rt_coef))
    rt_coef.byteswap().tofile(fout_rt_coef)

    with open(fout_report,'w') as f:
        f.write(('------ Regridding Table ------'+\
               '\nid: '+\
               '\nlength: {}'+\
               '\nsidx min: {:'+str(dgt_idx)+'d} @ ij {:'+str(dgt_ij)+'d}'+\
               '\n     max: {:'+str(dgt_idx)+'d} @ ij {:'+str(dgt_ij)+'d}'+\
               '\ntidx min: {:'+str(dgt_idx)+'d} @ ij {:'+str(dgt_ij)+'d}'+\
               '\n     max: {:'+str(dgt_idx)+'d} @ ij {:'+str(dgt_ij)+'d}'\
                ).format(
                    nij,
                    rt_sidx.min(),np.where(rt_sidx==rt_sidx.min())[0][0]+1,
                    rt_sidx.max(),np.where(rt_sidx==rt_sidx.max())[0][0]+1,
                    rt_tidx.min(),np.where(rt_tidx==rt_tidx.min())[0][0]+1,
                    rt_tidx.max(),np.where(rt_tidx==rt_tidx.max())[0][0]+1,
                ))
