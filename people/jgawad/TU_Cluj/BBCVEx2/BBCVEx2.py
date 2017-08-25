#!/usr/bin/env python
#
# $Id$
#
# Author: Jerzy Gawad
# Email:  Jerzy.Gawad@cs.kuleuven.be
# Organization: Katholieke Universiteit Leuven (KU Levuen)
# Organization unit: Dept.Comp.Sci., TWR Group
#
# Copyright by KU Leuven. All rights reserved.
#
# $Revision$
# $Date$
#

from __future__ import print_function

def formatYld(yld):
    '''Produce list containing YLD data in format suitable for BBC2008ident'''
    yld_format = '%12.6e  '*3 + '\n'
    out = [str(len(yld)) + '\n'] 
    yld_data = [(float(angle), data['sigma_scaled'], data['beta']) \
                for angle, data in yld.items()]
    out += [yld_format % point for point in sorted(yld_data)]

    return out

def readBBC2008params(inp):
    
    # Skip two lines of header, one with banner.
    out = {}
    for _ in range(3):
        inp.readline()
    # Next three lines contain k, s and w
    for _ in range(3):
        k, v = [x.strip() for x in inp.readline().split(':')]
        out[k] = v
    params = out['params'] = []
    labels = out['labels'] = []
    for _ in xrange(int(out['s'])*8):
        k, v = [x.strip() for x in inp.readline().split(':')]
        params.append(float(v))
        labels.append(k)
    return out

NPARAMS = 16

# Map between crystal structures and notation used by BBC2008ident
STRUCTURE_MAP = {'fcc': 'F', 'bcc': 'B'}


def main(args):
    import json
    from collections import namedtuple

    try:

        input_data = json.load(args.input)

        if args.bbc2008init:
            bbc_data = readBBC2008params(args.bbc2008init)
            initial_guess = bbc_data['params']
        else:
            initial_guess = NPARAMS * [0.5]
        #
        # Process the inputs
        #
        meta = input_data['meta']
        data = input_data['data']
        #
        structure = STRUCTURE_MAP[meta['material']['structure']]

        components = [d.keys()[0] for d in data]

        extract = lambda name: data[components.index(name)].get(name, None)

        # Get the uniaxial data
        uniaxial = extract('uniaxial')

        Uni = namedtuple('Uni', ('angle', 'sigma', 'rvalue'))

        # Get uniaxial stresses scaled by s_0
        sigma_0 = uniaxial['0.0']['sigma_xx']
        uni_data = sorted(Uni(float(angle),
                              val['sigma_xx'] / sigma_0,
                              val['rvalue']) \
                           for angle, val in uniaxial.items())


        # Get the equibiaxial data
        equibiaxial = extract('equibiaxial')
        rvalue_bx = equibiaxial['r_bx']
        sigma_bx_scaled = equibiaxial['sigma_bx'] / sigma_0

        try:
            # Get the yld data
            yld = extract('yld')
        except (ValueError, KeyError):
            yld = None

        out = args.output

        # Write BBC input file
        fmt = "%12.6e\n"
        hdr = 'BBCVEx2, source data: {}, source texture: {}\n'
        out.write(args.jobname + '\n')
        out.write(hdr.format(args.input.name, meta['material']['data']))
        for uni in uni_data:
            out.write(fmt % uni.sigma)
        out.write(fmt % sigma_bx_scaled)

        for uni in uni_data:
            out.write(fmt % uni.rvalue)

        out.write(fmt % rvalue_bx)
        #
        out.writelines(formatYld(yld))
        # 
        out.write(structure + '\n')
        # Write the initial guess
        for x in initial_guess:
            out.write(fmt % x)
        # Write 'F' that stands for "no weighting factors, use the defaults"
        out.write('F\n')

    except Exception as e:
        print(e)
        raise e



if __name__ == '__main__':
    import argparse
    import sys
    #
    exitcode = 1
    try:
        formatter = argparse.ArgumentDefaultsHelpFormatter
        parser = argparse.ArgumentParser('BBCVEx2',
                                         formatter_class=formatter)

        #
        parser.add_argument('--jobname', required=True)
        parser.add_argument('--input', required=True, 
                            type=argparse.FileType('rb'),
                            help='Path to vef_datacard JSON file, or - for standard input')
        parser.add_argument('--output', required=True,
                            type=argparse.FileType('wt'),
                            help='Path to the output data file, or - standard output')
        parser.add_argument('--bbc2008init', 
                            help='path to BBC2008 parameter file to be included as the initial guess',
                            required=False, type=argparse.FileType('r'),
                            default=None)
        #
        args = parser.parse_args()
        main(args)

        exitcode = 0

    except SystemExit:
        pass

    except Exception as e:
        print('Unhandled exception: ' + str(e))
        # print dir(e)
        # print str(e.child_traceback)

    finally:
        sys.exit(exitcode)
