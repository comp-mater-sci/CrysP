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
from pyhms.batchprocessing import pymp, harvester
import pyhms.miscutils.datafile as datafile
import numpy

def readTemplate(template_path):
    try:
        contents = open(template_path,'r').readlines()
        return ''.join(contents)
    except IOError as e:
        print(__name__ + ': cannot process template ' + template_path)
        raise e

class QRSHarvester(harvester.DataFileHarvester):
    def __init__(self,**kwargs):
        return super(QRSHarvester,self).__init__(**kwargs)

    def harvest(self,workdir = '.'):
        parent_key,parent_result = super(QRSHarvester,self).harvest(workdir).items()[0]
        view = (datafile.DataView(parent_result,zip(['angle','r-value','s-value'],3*[float])))
        return {parent_key: view}
        
class ASRHarvester(harvester.DataFileHarvester):
    def __init__(self,**kwargs):
        return super(ASRHarvester,self).__init__(**kwargs)

    def harvest(self, workdir = '.'):
        parent_key,parent_result = super(ASRHarvester, self).harvest(workdir).items()[0]
        view = (datafile.DataView(parent_result,zip(['||SonA||','A_11','A_22'],3*[float])))
        return {parent_key: view}

class YldHarvester(harvester.DataFileHarvester):
    def __init__(self,**kwargs):
        return super(YldHarvester,self).__init__(**kwargs)

    def harvest(self, workdir = '.'):
        parent_key, parent_result = super(YldHarvester, self).harvest(workdir).items()[0]
        view = (datafile.DataView(parent_result,zip(['theta','S/S_0','beta'],3*[float])))
        return {parent_key: view}


def subdict(dictionary,keylist):
    subdict_gen = ((key,dictionary[key]) for key in keylist if key in dictionary)
    return dict(subdict_gen)

def mergedict(dict_dst,dict_src):
    dict_dst.update(dict_src)
    return dict_dst

def processYld(dataview):
    # We assume that central difference scheme is used.
    # Thus we consider triplets, in which only the central point is relevant
    yld_data = numpy.array(list(dataview))[1::3,:]
    # convert the first and the third column to degrees
    yld_data[:,(0,2)] = numpy.degrees(yld_data[:,(0,2)])
    return yld_data

def outputYldData(out, yld_data):

    yld_format = '%12.6e  '*3 + '\n'
    # Output yld data
    out.write(str(len(yld_data)) + '\n')
    for x in yld_data:
        out.write(yld_format % tuple(x))

def readBBC2008params(inp):
    
    # Skip two lines of header, one with banner. 
    out ={}
    for _ in range(3):
        inp.readline()
    # Next three lines contain k, s and w
    for _ in range(3):
        k,v = [x.strip() for x in inp.readline().split(':')]
        out[k] = v
    params = out['params'] = []
    labels = out['labels'] = []
    for _ in xrange(int(out['s'])*8):
        k,v = [x.strip() for x in inp.readline().split(':')]
        params.append(float(v))
        labels.append(k)
    return out


def main(args):
    import os
    try:
        model_id_map = {'ALAMEL':0, 'FCTaylor':1}

        arg_keys = ['texture_file', 'model', 'slipsystem_file',
                    'microstructure_file','stress_file']
        # Recover known keys from namespace of args object
        mapping = subdict(vars(args),arg_keys) # Essencially, vars(args) is more Pythonish args.__dict__

        use_yld = False

        try:
            mapping['modelid'] = model_id_map[mapping['model']]
        except KeyError:
            pass

        if args.bbc2008init:
            bbc_data = readBBC2008params(args.bbc2008init)
            initial_guess = bbc_data['params']
        else:
            initial_guess = 16 * [0.5]
        
        executable = os.path.expanduser(args.executable)
        common_config ={'executable':executable}

        name_modifier = '' if args.steps == 1 else '_%d'

        task_types = [('QRS', '.xqrs', readTemplate(args.qrstemplate), QRSHarvester),
                      ('ASR', '.asr',  readTemplate(args.asrtemplate), ASRHarvester)]
        if args.yldtemplate:
            task_types.append(('Yld', '.xyld', readTemplate(args.yldtemplate), YldHarvester))
            use_yld = True

        # use the guess point if provided

        taskgroups = []
        tasks = []
        for step in range(0,args.steps):
            
            tasklist = []    

            if name_modifier:
                jobname = (args.jobname + name_modifier) % step
            else:
                jobname = args.jobname
                
            config_stemname =  '%s_'+ jobname + '.cfg'

            for mode, extension, template, harvester_class in task_types:
                config_fname = config_stemname % (mode)
            
                t = pymp.ExternalProgramTask(templates = {config_fname: template},
                                                harvesters = [harvester_class(data_fname=(jobname+extension))],
                                                config=mergedict({'cmdline_args': [mode, config_fname]},common_config),
                                                keywords=mergedict({'step': step, 'jobname': jobname},mapping),
                                                use_tempdir = False)
                tasks.append(t)
                tasklist.append(t)
            #
            taskgroups.append((jobname,tasklist))
        # Create pool master
        if (args.serial):
            pool_master = pymp.SerialMaster()
        else:
            pool_master = pymp.PoolMaster(timeout = 4800000)
        #
        pool_master.run(tasks)

        for task in tasks:
            if not task.result:
                msg = 'One of tasks did not deliver results.'
                print(msg + ' alamDMC output:')
                print task.command_output
                raise Exception('Error while running alamDMC:' + msg)

        for jobname,tasklist in taskgroups:

            #    
            # Interpret the results:
            #
            # Stage 1: uniaxial test
            dv = tasklist[0].result.values()[0]
            ars = numpy.array(list(dv))
            S0 = ars[0,2]
            #
            rvalues = ars[:,[1]]
            ars[:,[2]] = ars[:,[2]] / S0
            scaled_uni_s = ars[:,[2]] 
            #
            dv = tasklist[1].result.values()[0]
            bx = numpy.array(list(dv))
            rvalue_bx = bx[0,2] / bx[0,1] # r_bx = A_22 / A_11
            scaled_bx_s = bx[0,0] / S0
            #
            if use_yld:
                yld_data = processYld(dataview=tasklist[2].result.values()[0])
            else:
                yld_data = [] # this means no yld data is actually provided.

            # Write BBC input file
            fmt = "%12.6e\n"
            out = open(jobname + '_bbc2008vef.datx','w')
            out.write(jobname + '\n')
            out.write(jobname + ' ' + args.texture_file + ' ' + args.model +  '\n')
            for s in scaled_uni_s:
                out.write(fmt % s[0])
            out.write(fmt % scaled_bx_s)

            for r in rvalues:
                out.write(fmt % r[0])
            out.write(fmt % rvalue_bx)
            #
            outputYldData(out, yld_data)
            # 
            out.write(args.structure + '\n')
            # Write the initial guess
            for x in initial_guess:
                out.write(fmt % x)
            # Write 'F' that stands for "no weighting factors, use the defaults"
            out.write('F\n')
            out.close()

            if args.export:
                ars_file = open((args.export + jobname + '.urs'),'wt')
                # Write output values
                # order of fields: angle normalized_s r-value
                for row in ars:
                    ars_file.write( (3*'%10.6f '+'\n') % (row[0], row[2], row[1]) )
                ars_file.close()
                #
                brs_file = open((args.export + jobname + '.brs'),'wt')
                # Write output values: biaxial r-values and stresses
                # order of fields:  normalized_s_bx r-value_bx
                brs_file.write((2*'%10.6f '+'\n') % (scaled_bx_s, rvalue_bx ) )
                brs_file.close()

    except Exception as e:
        print e
        raise e



if __name__ == '__main__':
    import argparse
    import sys
    if sys.platform in ['win32','win64']:
        alamdmc_prog = 'alamDMC.exe'
    else:
        alamdmc_prog = 'alamDMC'
    #
    exitcode = 1
    try:
        parser = argparse.ArgumentParser('BBCVEx', formatter_class=argparse.ArgumentDefaultsHelpFormatter)
        parser_template_args = parser.add_argument_group('template_keys')

        #
        parser.add_argument('--qrstemplate',help='template of config file for alamDMC QRS',required=True)
        parser.add_argument('--asrtemplate',help='template of config file for alamDMC ASR',required=True)
        parser.add_argument('--yldtemplate',help='template of config file for alamDMC yld',required=False)
        parser.add_argument('--executable',help='path to alamDMC',default=alamdmc_prog,required=False)
        parser.add_argument('--export',help='prefix of filename for the extended output',default=None)
        parser.add_argument('--bbc2008init', 
                            help='path to BBC2008 parameter file to be included as the initial guess',
                            required=False, type=argparse.FileType('r'), default=None)
        parser.add_argument('--serial', help='run the VEF jobs serially', required=False,
                            action='store_true')
        # parser.add_argument('--output',help='Output file or "-"',type=argparse.FileType('w'),default='-')
        # Key/value pairs for template substitution
        parser_template_args.add_argument('--jobname',default='elem',required=True)
        parser_template_args.add_argument('--texture_file',required=True)
        parser_template_args.add_argument('--model',choices=['ALAMEL','FCTaylor'],default='ALAMEL')
        parser_template_args.add_argument('--slipsystem_file',required=True)
        parser_template_args.add_argument('--microstructure_file',required=True)
        parser_template_args.add_argument('--stress_file',required=True)
        parser_template_args.add_argument('--structure',choices=['F','B'],default='F')
        parser_template_args.add_argument('--steps',default=1,type=int)
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
