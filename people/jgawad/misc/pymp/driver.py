#!/usr/bin/env python
#
# $Id: driver.py 30 2013-05-02 01:45:22Z jgawad $
#
# Author: Jerzy Gawad
# Email:  Jerzy.Gawad@cs.kuleuven.be
# Organization: Katholieke Universiteit Leuven (KU Levuen)
# Organization unit: Dept.Comp.Sci., TWR Group
#
# Copyright by KU Leuven. All rights reserved.
#
# $Revision: 30 $
# $Date: 2013-05-01 21:45:22 -0400 (Wed, 01 May 2013) $
#

import FNGGrid
import harvester
import templateWorker



def main_grideval(args):
    import os
    import sys
    try:
        grid_fname = args.grid
        template_fname = args.template
        result_fname = 'outcomm.dat'
        work_increment = args.workinc
        work_tolerance = args.tolerance
        prog_fname = os.path.realpath(os.path.expanduser(args.program))
        out_fname = args.output

        mapping = {}
        mapping['scaling_type'] = 1
        mapping['strain_increment'] = args.straininc
        mapping['work_increment'] = args.workinc
        mapping['time_increment'] = args.timeinc


        # open the grid file
        grid = FNGGrid.FNGGrid(grid_fname)
    
        # open and pre-process template, leave only the grid data unresolved
        template = preprocessTemplate(template_fname,mapping)
    
        # create tasks
        tasks = makeTasksForGrid(template,grid,prog_fname,result_fname,hide_output=True)

        # Create master
        if (args.serial):
            master = templateWorker.SerialMaster()
        else:
            master = templateWorker.PoolMaster(timeout=(args.cputime * grid.npoints))
        #master = templateWorker.SerialMaster()

        master.run(tasks)

        output_grid = FNGGrid.FNGGrid()

        for task in tasks:
            try:
                # Diagnostic -->
                # print task.result
                # <--
                result = task.result[result_fname]
                if not result:
                    continue
                output_checked = verifyResult(work_increment,work_tolerance,result[0])
                if (output_checked):
                    output_grid.addPoint(output_checked)
            except KeyError:
                pass
        del tasks

        # Evaluation of stresses is finished, lets output the results
        try:
            output_grid.write(out_fname)
        except IOError:
            print('Cannot write the output file')
            exit(1)
    except Exception as e:
        print 'Unhandled exception in main_grideval'
        exit(1)



def preprocessTemplate(template_fname,mapping):
    """ Open the template file and substitute as much as possible"""
    import sys
    import string
    import os
    
    try: 
        inp_templ = open(template_fname,'r')
        input = inp_templ.readlines()
        # Make sure there is new line at the end of the template file.
        # Othewise crys3d will not process it correctly (it will fail in INCTRL)
        if input[-1][-1] != '\n':
            input.append('\n')
        config_template = ''.join(input)
    except IOError as e:
        print('Cannot open template file')
        raise e
    # diagnostics -->
    print('Template:')
    print(config_template)
    # <--

    template = string.Template(config_template)

    output_template = template.safe_substitute(mapping)
    # diagnostics -->
    print('Pre-processed template:')
    print(output_template)
    # <--
    return output_template


def verifyResult(work_increment,work_tolerance,output):
    #
    # Check post-conditions
    #
    
    work_tolerable_max = (1.0 + work_tolerance) * work_increment

    output_filtered = None

    # interpret the result: check if the work level substantially exceeds the requested one.
    fields = output.split()
    field_work = float(fields[10]) # the 11th column
    if (field_work <= work_tolerable_max):
        output_filtered = output
    else:
        print('Warning: excessive overshoot of work level. Dropping the point...')

    return output_filtered



def makeTasksForGrid(template,grid_data,prog_fname,result_fname,hide_output=True):
    # Create tasks
    tasks = []
    config = {}
    config['program_path'] = prog_fname

    for k, gridline in enumerate(grid_data,1):
        # print ('Making task for grid point ' + str(k))
        mapping = {}
        mapping['vPlDeps'] = gridline.strip()
        task = templateWorker.Task(template, mapping,config,harvesters=[harvester.TextFileHarvester(result_fname)])
        # diagnostics -->
        # print task
        # <--
        tasks.append(task)
    return tasks


def main_yldCalculations(args):
    import datafile
    import yldPoints
    import os
    import sys
    try:
        
        template_fname = args.template
        out_fname = args.output
        work_increment = args.workinc
        deps3dt = args.deps3dt
        npoints = args.npoints

        result_fname = 'PPD'

        mapping = {}

        config = {}
        config['program_path'] = os.path.realpath(os.path.expanduser(args.program))

        mapping_list = yldPoints.makeSamplingPoints(deps3dt,work_increment,nrhos=npoints)

        # open and pre-process template, leave only the grid data unresolved
        template = preprocessTemplate(template_fname,mapping)
    
        # create tasks
        tasks = []
        for k, mapping in enumerate(mapping_list,1):
            print ('Making task for point ' + str(k))
            task = templateWorker.Task(template, mapping,config,harvesters=[harvester.TextFileHarvester(result_fname)])
            # diagnostics -->
            # print task
            # <--
            tasks.append(task)

        # Create master
        if (args.serial):
            master = templateWorker.SerialMaster()
        else:
            master = templateWorker.PoolMaster(timeout=(args.cputime * npoints))

        master.run(tasks)

        # Extract the results
        output = []
        # selection of fields from the line of PPD:
        # S11: the second field; S22: the fifth field
        ppdfields = [1,4]
        for task in tasks:
            try:
                # Diagnostic -->
                # print task.result
                # <--
                result = task.result[result_fname]
                if not result:
                    continue
                 # The last line of PPD, the second and the fifth field
                result_list = result[-1].strip().split()
                output.append(' '.join([result_list[i] for i in ppdfields]) + '\n')
            except KeyError:
                pass
            except IndexError:
                pass
        del tasks

        # Let's output the results
        try:
            outfile = open(args.output,'w')
            outfile.writelines(output)
            outfile.close()
        except IOError:
            print('Cannot write the output file')
            exit(1)
    except Exception as e:
        print 'Unhandled exception in main_yldCalculations'
        exit(1)
    
    
    

    pass


# Start-up dispatcher    
if __name__ == "__main__":
    #import re
    import argparse
    import sys
    import os
    #
    if sys.platform == 'win32':
        default_program_path = 'C:\\work\\UnivWaterloo_KaanInal\\crys3d\\trunk\\driver\\runsim.cmd' 
    else:
        default_program_path = '~/jgprojects/crys3d/trunk/driver/runsim'

    parser = argparse.ArgumentParser(prog='driver')
    subparsers = parser.add_subparsers()

    common_options = argparse.ArgumentParser(add_help=False)
    common_options.add_argument('--template',help='file name of configuration template',required=True)
    common_options.add_argument('--workinc',help='increment of plastic work',required=True,type=float)
    common_options.add_argument('--cputime',help='maximal time needed for one run of simulation (sec)',default=60)
    common_options.add_argument('--output',help='output file',required=True)
    common_options.add_argument('--program',help='path to the program that has to be executed by the driver',default=default_program_path)
    common_options.add_argument('--serial',help='Disable parallelization',action='store_true')


    grid_options = subparsers.add_parser('grid', parents=[common_options],description='Calculation of stresses for a given increment of plastic work on a grid of strain rates.')
    grid_options.add_argument('--grid',help='grid file of strain rate modes',required=True)
    grid_options.add_argument('--straininc',help='nominal increment of plastic strain',default=0.05,type=float)
    grid_options.add_argument('--timeinc',help='default time increment',default=0.02,type=float)
    grid_options.add_argument('--tolerance',help='tolerance for overshooting in work level (0. - 1.)',default=0.05,type=float)
    grid_options.set_defaults(func=main_grideval)

    yld_options = subparsers.add_parser('yld',parents=[common_options],description='Calculation of S_11 - S_22 yield locus section')
    yld_options.add_argument('--deps3dt',type=float,default=0.001)
    yld_options.add_argument('--npoints',help='Number of points per segment', type=int,default=11)
    # Note: the calculations of yld take time, so higher cputime overrides the default of --cputime.
    yld_options.set_defaults(func=main_yldCalculations,cputime=120)
    
    try:
        args = parser.parse_args()
        args.func(args)
    except SystemExit:
        pass
