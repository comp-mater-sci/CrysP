#!/usr/bin/env python
import os
import sys
import string
import argparse
import math
import numpy as np

import pyhms.miscutils.datafile

import pyhms.miscutils.defdata as defdata

def writeGenericPairs(outfile,pairs):
    '''Write iterable of pairs to the outfile. Scientific representation of floats is used.'''
    fmt = ('{0:15.7e} {1:15.7e}' + '\n')
    outfile.write(str(len(pairs))+'\n')
    outfile.writelines((fmt.format(*pair) for pair in pairs))
           

def extrap(x, xp, yp):
    """np.interp function with linear extrapolation"""
    y = np.interp(x, xp, yp)
    y[x < xp[0]] = yp[0] + (x[x<xp[0]]-xp[0]) * (yp[0]-yp[1]) / (xp[0]-xp[1])
    y[x > xp[-1]]= yp[-1] + (x[x>xp[-1]]-xp[-1])*(yp[-1]-yp[-2])/(xp[-1]-xp[-2])
    return y


def main(argv):

    parser = argparse.ArgumentParser()
    parser.add_argument('--deffile',default='defdata.dat')
    parser.add_argument('--oldPPD',required=False)
    parser.add_argument('--PPD',default='PPD')
    parser.add_argument('--config_template',required=True)
    parser.add_argument('--config',required=True)
    parser.add_argument('--crys3D',default='crys3D')
    parser.add_argument('--texture',required=True)
    parser.add_argument('--initial',action='store_true')
    parser.add_argument('--output',required=True)
    args = parser.parse_args(argv)

    try:
        # Open & read defdata.dat into DefData
        inp_defdata = open(args.deffile,mode='r')

        def_data = defdata.DefData().read(inp_defdata)


        # defdata.eps_0 = 0.0
        # defdata.eps_1 = 0.05

        # Open & read the old PPD file
        if not args.initial and args.oldPPD:
            old_ppd = np.loadtxt(args.oldPPD,usecols=[0,2])
        else:
            old_ppd = np.ndarray([0,2])
        #
        # Create crys3d input file from the template file:
        template = string.Template(open(args.config_template,'r').read())

        # Note: crys3d expects magnitude of strain, but hardening models deal with vM strain.
        # For that reason deps has to be multiplied by sqrt(3./2.).
        params = {'rst_mode':  0 if args.initial else 2,
                  'deps': math.sqrt(3./2.)*(def_data.eps_1 - def_data.eps_0), 
                  'vMode': ' '.join([str(x) for x in def_data.vStrainMode]),
                  'f_texture': args.texture
                 }
        with open(args.config,'w') as config_file:
            config_file.write(template.safe_substitute(params))
        #
        # Run the command    
        command = (args.crys3D + ' < ' + args.config)
        info = os.system(command)
        if info != 0:
            raise OSError('Failed to execute : ' + args.crys3D)

        new_ppd = np.loadtxt(args.PPD,usecols=[0,2])
        # Merge old PPD with new PPD
        data = np.vstack((old_ppd,new_ppd))
        if not np.all(np.diff(data[:,0]) > 0):
            # We have to filter out two types of points: non-monotonic and with duplicated x-coordinate.
            mask = np.zeros(np.shape(data)[0],dtype=bool)
            current = data[0,0]
            mask[0] = True
            for idx,x in enumerate(data[1:,0],1):
                if x > current:
                    mask[idx] = True
                    current = x

            data = data[mask]
            #if not np.all(np.diff(data[:,0]) > 0):
            #    raise ValueError('Post-condition failed')
            pass
        
        interpolation_points = np.array([def_data.eps_0, 
                                         def_data.eps_0 + 0.5*(def_data.eps_1 - def_data.eps_0), 
                                         def_data.eps_1])

        interpolated_values = extrap(interpolation_points,data[:,0],data[:,1])

        # Open & write the result file 
        with open(args.output,'w') as out_file:
            writeGenericPairs(out_file,zip(interpolation_points,interpolated_values))
    
        return 0

    except OSError as e:
        sys.stderr.write(str(e))
        return 2

    except Exception as e:
        sys.stderr.write('Unhandled exception:\n')
        sys.stderr.write(str(e))
        return 2


if __name__ == '__main__':
    sys.exit(main(sys.argv[1:]))

