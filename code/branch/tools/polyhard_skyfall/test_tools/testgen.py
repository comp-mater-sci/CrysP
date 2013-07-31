#!/usr/bin/env python
import os,sys
import numpy
import numpy.linalg
import argparse

if __name__ == '__main__':

    parser = argparse.ArgumentParser()
    parser.add_argument('--eps_min',type=float,default=0.0)
    parser.add_argument('--eps_max',type=float,required=True)
    parser.add_argument('--delta_eps',type=float,required=True)

    args = parser.parse_args()
    # 
    eps_0 = args.eps_min
    eps_max = args.eps_max
    delta_eps = args.delta_eps
    #
    eps = numpy.array([ [2./3., 0., 0],
            [0., -1./3, 0.],
            [0., 0., -1./3.] ])

    eps /= numpy.linalg.norm(eps)
    eps *= delta_eps


    print('Norm of eps:', numpy.linalg.norm(eps,'fro'))

    fmt = 3*'%10.5e ' + '\n'

    out = sys.stdout
    step = 1

    #for step in range(1,10):
    while eps_0 < eps_max:
        seq = step
        eps_0 = eps_0 + delta_eps
        # Write defdata.dat
        
        out = open('defdata.dat','w')

        out.write('%d %d\n' % (step,seq))
        for row in eps:
            out.write(fmt % tuple(row))
        out.write('1\n0\n1\n')
        out.write(2*'%f\n' % (eps_0, eps_0 + delta_eps))
        out.close()
        os.system('hms_texupdate.sh')    
        step += 1
        #out.write

