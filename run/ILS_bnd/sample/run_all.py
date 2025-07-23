import os
import sys
import argparse

sys.path.append('../../../src/run/common')
sys.path.append('script')
import const, util, conf
from script import s00_const as lconst
from script import s01_make_cmf_mat, \
                   s02_make_rt


parser = argparse.ArgumentParser()
parser.add_argument('-s', '--step', help='step number', type=int)
parser.add_argument('-d', '--data', help='data name')
parser.add_argument('-l', '--land', help='land type')
parser.add_argument('-t', '--tile', help='tile name')
args = parser.parse_args()

util.job.put_job(lconst.job)

if args.step is None:
    s01_make_cmf_mat.run()

    s02_make_rt.run(args.data, args.land, args.tile)

else:
    if args.step == 1:
        s01_make_cmf_mat.run()

    elif args.step == 2:
        s02_make_rt.run(args.data, args.land, args.tile)

