#!/usr/bin/env python
#
# $Id$
#
import argparse
import os
import pymp
import datafile
import harvester
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


def subdict(dictionary,keylist):
    subdict_gen = ((key,dictionary[key]) for key in keylist if key in dictionary)
    return dict(subdict_gen)

def mergedict(dict_dst,dict_src):
	dict_dst.update(dict_src)
	return dict_dst


def main(args):
    try:
        model_id_map = {'ALAMEL':0, 'FCTaylor':1}

        arg_keys = ['texture_file', 'model', 'slipsystem_file',
                    'microstructure_file','stress_file']
        # Recover known keys from namespace of args object
        mapping = subdict(vars(args),arg_keys) # Essencially, vars(args) is more Pythonish args.__dict__

        try:
            mapping['modelid'] = model_id_map[mapping['model']]
        except KeyError:
            pass

        
        program_path = os.path.expanduser(args.program_path)
        common_config ={'program_path':program_path}

        name_modifier = '' if args.steps == 1 else '_%d'

        task_types = [('QRS', '.xqrs', readTemplate(args.qrstemplate), QRSHarvester),
                      ('ASR', '.asr',  readTemplate(args.asrtemplate), ASRHarvester)]

        if args.yldtemplate:
            task_types.append(('Yld', '.xyld', readTemplate(args.yldtemplate), harvester.FileHarvester))

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
                                                config=mergedict({'cmdline_prologue': (mode + ' '+ config_fname)},common_config),
                                                keywords=mergedict({'step': step, 'jobname': jobname},mapping),
                                                use_tempdir = False)
                tasks.append(t)
                tasklist.append(t)
            #
            taskgroups.append((jobname,tasklist))
        #
        pool_master = pymp.PoolMaster(timeout = 4800000)
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
            # Write BBC input file
            fmt = "%12.6e\n"
            out = open(jobname + '_bbc2008vef.dat','w')
            out.write(jobname + '\n')
            out.write(jobname + ' ' + args.texture_file + ' ' + args.model +  '\n')
            for s in scaled_uni_s:
                out.write(fmt % s[0])
            out.write(fmt % scaled_bx_s)

            for r in rvalues:
                out.write(fmt % r[0])
            out.write(fmt % rvalue_bx)
            # 
            out.write(args.structure)
            out.close()

            if args.export:
                ars_file = open((args.export + jobname + '.ars'),'w')
                # Write output values
                # order of fields: angle normalized_s r-value
                for row in ars:
                    ars_file.write( (3*'%10.6f '+'\n') % (row[0], row[2], row[1]) )


    except Exception as e:
        print e
        raise e



if __name__ == '__main__':
    import sys
    
    if sys.platform in ['win32','win64']:
        alamdmc_prog = 'alamDMC.exe'
    else:
        alamdmc_prog = 'alamDMC'
    #
    exitcode = 1
    try:
        parser = argparse.ArgumentParser('BBCVEx')
        parser_template_args = parser.add_argument_group('template_keys')

        #
        parser.add_argument('--qrstemplate',help='template of config file for alamDMC QRS',required=True)
        parser.add_argument('--asrtemplate',help='template of config file for alamDMC ASR',required=True)
        # parser.add_argument('--output',help='Output file or "-"',type=argparse.FileType('w'),default='-')
        parser.add_argument('--program_path',help='path to alamDMC',default=alamdmc_prog,required=True)
        parser.add_argument('--export',help='prefix of filename for extended output',default=None)
        parser.add_argument('--yldtemplate',help='template of config file for alamDMC yld',required=False)
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
        main(parser.parse_args())

        exitcode = 0

    except SystemExit:
        pass

    except Exception as e:
        print('Unhandled exception: ' + str(e))

    finally:
        sys.exit(exitcode)
