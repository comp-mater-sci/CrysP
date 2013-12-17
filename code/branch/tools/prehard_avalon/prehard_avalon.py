#!/usr/bin/env python
import os
import sys
import errno
import string
import argparse
import math
import numpy as np

import pyhms.miscutils.datafile

import pyhms.miscutils.defdata as defdata

def writePolyHardConfig(outfile,args,pairs):
    
    config_lines = ['{deffile}    # Path to the defdata.dat data file',
                    '{prefix}.hard # Name of output file',
                    '{prefix}.str  # Name of output with strain-stress data',
                    '{order}       # Order of polynomial']
    
    config = {}
    config.update(vars(args))
    config['order'] = len(pairs) - 1
    outfile.writelines(( (line.format(**config) + '\n') for line in config_lines ))
    writeGenericPairs(outfile,pairs)    


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

def makeStrictlyIncreasing(data,column=0):
    '''Alter the array data by eliminating elements that are not strictly increasing with respect to the column.
    
    Post-condition: forall i=0,size(data): data[i+1,column] > data[i,column]
    '''
    if np.all(np.diff(data[:,column]) > 0):
        return data
    else:
        # We have to filter out two types of points: non-monotonic and with duplicated x-coordinate.
        mask = np.zeros(np.shape(data)[column],dtype=bool)
        current = data[0,column]
        mask[0] = True
        for idx,x in enumerate(data[1:,column],1):
            if x > current:
                mask[idx] = True
                current = x

        return data[mask]

def filterOutNegative(data,column):
    mask = data[:,column] > 0
    if np.all(mask):
        return data
    else:
        return data[mask]

        

def main(argv):

    parser = argparse.ArgumentParser()
    parser.add_argument('--deffile',default='defdata.dat',help='deformation data file (in the format of Skyfall defdata.dat)')
    parser.add_argument('--oldPPD',required=False,help='path to the PPD file that shoud be prepended to the PPD resulting from the current run')
    parser.add_argument('--PPD',default='PPD',help='path to the PPD file')
    parser.add_argument('--config_template',required=True,help='path to the template of crys3d configuration file')
    parser.add_argument('--config',required=True,help='path to the crys3D configuration file (to be created)')
    parser.add_argument('--crys3D',default='crys3D',help='path or name of crys3D executable')
    parser.add_argument('--texture',required=True,help='path to the texture file')
    parser.add_argument('--initial',action='store_true',help='if set, the crys3D program will start without an RST file')
    parser.add_argument('--output',required=True,help='path to the output file')
    parser.add_argument('--prefix',default='elem',help='Prefix for the names of polyHard files')
    parser.add_argument('--stress_scaling',default=1.e6,help='Scaling factor for stresses. Default: conversion from MPa to Pa')
    parser.add_argument('--dryrun', action='store_true',default=False,help='Suppress launching the crys3d and use PPD as it is found in the directory')
    args = parser.parse_args(argv)

    try:
        # Open & read defdata.dat into DefData
        inp_defdata = open(args.deffile,mode='r')

        def_data = defdata.DefData().read(inp_defdata)


        # defdata.eps_0 = 0.0
        # defdata.eps_1 = 0.05

        old_ppd = np.ndarray([0,2])
        # Open & read the old PPD file

        if not args.initial and args.oldPPD:
            try:
                old_ppd = np.loadtxt(args.oldPPD,usecols=[0,2])
                old_ppd = makeStrictlyIncreasing(filterOutNegative(old_ppd,column=1))
            except IOError as e:
                sys.stderr.write('Warning: empty old PPD\n')
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
        if not args.dryrun:
            # Run the command    
            command = (args.crys3D + ' < ' + args.config)
            if sys.platform in [ 'win32', 'win64' ]:
                command += ' > nul'
            else:
                command += ' > /dev/null'
            info = os.system(command)
            if info != 0:
                raise OSError('Failed to execute : ' + args.crys3D)

        new_ppd = np.loadtxt(args.PPD,usecols=[0,2])
        new_ppd = makeStrictlyIncreasing(filterOutNegative(new_ppd,column=1))
        # Conditionally merge the old PPD with the new PPD
        if len(old_ppd) and len(new_ppd) and (old_ppd[-1,0] <  new_ppd[0,0]):
            data = np.vstack((old_ppd,new_ppd))
            data = makeStrictlyIncreasing(data)
        else:
            data = new_ppd
        # Post-condition: linear approximation requires at least two data points,
        # so "data" must contain at least two rows.
        if len(data) < 2:
            raise ValueError('Working PPD set must contain at least two data rows')

        scaling_factor = args.stress_scaling
        interpolation_points = np.array([def_data.eps_0, 
                                         def_data.eps_0 + 0.5*(def_data.eps_1 - def_data.eps_0), 
                                         def_data.eps_1])
        # 
        # Scaling of stresses from the PPD (by default: conversion from MPa to Pa):
        interpolated_values = extrap(interpolation_points,data[:,0],scaling_factor * data[:,1])

        # Open & write the result file 
        with open(args.output,'w') as out_file:
            writePolyHardConfig(out_file,args,zip(interpolation_points,interpolated_values))
    
        return 0

    except (OSError,ValueError) as e:
        sys.stderr.write(str(e) + '\n')
        return 2

    except IOError as e:
        if e.args[0] == errno.ENOENT:
            sys.stderr.write('Cannot find file or directory: {fname}\n'.format(fname=e.filename))
        else:
            sys.stderr.write('IO error: ' + str(e) + '\n') 
        return 2 

    except Exception as e:
        sys.stderr.write('Unhandled exception:\n')
        sys.stderr.write('Type: ' + str(type(e)) + '\n')
        sys.stderr.write(str(e) + '\n')
        return 2


if __name__ == '__main__':
    sys.exit(main(sys.argv[1:]))

