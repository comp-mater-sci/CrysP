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

import multiprocessing
import os 
import sys
import tempfile
import time
import shutil
import string
import harvester


class Task(object):
    def __init__(self):
        pass
    def run(self):
        pass

class ExternalProgramTask(Task):

    def __init__(self,templates=None, keywords=None,config=None,harvesters=None,use_tempdir=True):
        """Initialization of ExternalProgramTask.
        
            Parameters:
            \param templates - dictionary of templates. Each pair is in form: {file_name: template_string}.
            There is a special file_name='-', that causes the substituted template to be send to standard input
            of the back-end program.
            \param mapping - dictionary of keywords to be substituted in templates.
            \param config - configuration 
        """
        self.templates = templates or {}
        self.mapping = keywords or {}
        self.harvesters = harvesters or []
        # configuration flags
        self.config = {}
        self.config['executable'] = ''
        self.config['cmdline_args'] = []
        self.config['hide_output'] = True
        self.setConfig(config or {})
        if not self.config['executable']: 
            raise ValueError('The configuration does not include executable.') 
        # Other members, to be set by run() and co.
        self.cwd = '.'
        if (use_tempdir):
            self.execdir_path = ''
            self.purge_execdir = True
        else:
            self.execdir_path = '.'
            self.purge_execdir = False
        #
        self.command_output = ''
        #
        self.result = None
        #
        super(ExternalProgramTask,self).__init__()

    def removeTempDir(self):
        try:
            if self.purge_execdir and self.execdir_path and os.path.isdir(self.execdir_path):
                shutil.rmtree(self.execdir_path)
        except OSError as e:
            print str(e)
        except:
            pass

    def __del__(self):
        try:
            # Last attempt to do housholding
            self.removeTempDir()
        except:
            pass


    def __str__(self):
        result = ('templates: ' + str(self.templates) + '\n' +
                  'mapping: ' + str(self.mapping) + '\n' + 
                  'config:' + str(self.config))
        return result

    def prepareWorkdir(self):
        # Prepare execution environment
        self.cwd = os.getcwd()
        try:
            #os.mkdir('%d'%os.getpid())
            if (not self.execdir_path):
                self.execdir_path = tempfile.mkdtemp(suffix='_%d'%os.getpid(),dir=self.cwd)
                os.chdir(self.execdir_path)
                print self.execdir_path
        except OSError:
            raise


    def preExecute(self):
        self.prepareWorkdir()


    def setMapping(self,mapping):
        self.mapping.update(mapping)


    def setConfig(self,config):
        self.config.update(config)
        executable = os.path.expanduser(self.config['executable'])
        fpath, fname = os.path.split(executable)
        # Empty fpath means that a command is provided.
        # Non-empty fpath indicates 
        if fpath:
            if os.path.exists(executable) and os.access(executable, os.X_OK):
                self.config['executable'] = os.path.realpath(executable)
            else:
                print('Cannot find ' + executable)
                raise OSError('Executable ' + executable + ' not found.')
        else:
            self.config['executable'] = fname


    def execute(self):
        import os
        import subprocess
        # Put special keywords into mapping:
        # EXEC_DIR
        full_mapping = {}
        full_mapping['WORK_DIR'] = os.path.abspath(self.execdir_path)
        #
        full_mapping.update(self.mapping)
        # Make input files from the template
        stdin_input_string = None
        for template_file,template_string in self.templates.items():
            try:
                template = string.Template(template_string)
                input_data = template.safe_substitute(full_mapping)
                input_string = ''.join(input_data)
                if template_file == '-':
                    stdin_input_string = input_string
                else:
                    outfile = open(template_file,'w')
                    outfile.write(input_string)
                    outfile.close()
            except OSError as e:
                raise e

        # Do actual work: start 
        # Diagnostic -->
        #output = sys.__stdout__
        #output.write(input_string)
        #print(input_string)
        #diagout = open('diag.in','w')
        #diagout.write(input_string)
        #diagout.close()
        # Diagnostic <--

        # OK, the template is ready, let's run the program...
        out_redir = subprocess.PIPE if self.config['hide_output'] else None
        in_redir  = subprocess.PIPE if stdin_input_string else None
        command =[self.config['executable']] + self.config['cmdline_args']
        #
        po = subprocess.Popen(command, stdin=in_redir, stdout=out_redir)
        # and  feed it with the configuration
        self.command_output, stderrdata = po.communicate(stdin_input_string)
        # Diagnostic -->
        #print stdoutdata
        #print stderrdata
        # <-- 
        pass



    def postExecute(self):
        if self.harvesters:
            self.result = {}
            for harvester in self.harvesters:
                self.result.update(harvester.harvest(self.execdir_path))
        

    def finalize(self):
        # Finalize:
        try:
            # Make sure we end up in the initial directory
            os.chdir(self.cwd)
            # Remove the temporary directory
            self.removeTempDir()
        except OSError as e:
            print e

    def run(self):
        try:
            self.preExecute()
            self.execute()
            self.postExecute()
            self.finalize()
            self.is_done = True
            return self.result
        except IOError as e:
            print('In Task::run(): ' + str(e))
            raise e
        except OSError as e:
            print('In Task::run(): ' + str(e))
            raise e 
        except Exception as e:
            print('Unexpected exception in  Task::run(): ' + str(e))
            raise e


class PoolMaster(object):

    def __init__(self, polltime=1, timeout = 60):
        self.timeout = timeout
        self.polltime = polltime

    def run(self,tasks):
        pool = multiprocessing.Pool(processes=multiprocessing.cpu_count(),maxtasksperchild=1)
        result = pool.map_async(dispatcher, tasks)

        # Wait for the results in a semi-active way
        result.wait(self.polltime)
        tot_time = self.polltime
        while (not result.ready()) and (tot_time < self.timeout):
            tot_time += self.polltime
            # Diagnostic -->
            # print 'after ', tot_time, ' is ready:', result.ready()
            # <--
            result.wait(self.polltime)
        #
        if result.ready() and result.successful():
            res = result.get()
            # Diagnostic -->
            # print 'Getting results', res
            # <--
            if len(res) != len(tasks):
                e = Exception('PoolMaster Error: there are fewer results than tasks!')
                raise e
            # unpack the results & put them to the corresponding tasks
            for i in range(0,len(tasks)):
                tasks[i].result = res[i]
        # Destroying the pool
        pool.terminate()
        pool.join()



def dispatcher(task):
    # print ('pid = %d ' % os.getpid())
    return task.run()



class SerialMaster(object):
    def __init__(self):
        pass

    def run(self,tasks):
        for task in tasks:
            try:
                task.run()
            except Exception as e:
                print 'Unexpected exception in SerialMaster::run()'
                raise e



if __name__ == "__main__":

    multiprocessing.freeze_support()

    def prepareTestTasks(config, template):
        tasks = []
        for i in range(0,20):
            mapping = {}
            mapping['ali'] = ('object ' + str(i))
            mapping['kocie'] = 'mysz'
           
            task = ExternalProgramTask(template, mapping,config,harvesters=[harvester.TextFileHarvester('marker.dat')])
            print task
    
            tasks.append(task)
        return tasks


    config = {}
    config['cmdline_args'] = ['marker.dat']
    if sys.platform == 'win32':
        config['executable'] = '.\\runsim.cmd'
    else:
        config['executable'] = 'runsim.sh'
    
    mapping = {}
    mapping['ali'] = 'syfilis'
    mapping['kocie'] = 'mysz'

    templates = {'-': 'Ala ma ${ali}\nKot ma ${kocie}\n', 'datafile.txt': 'ali: ${ali}\nkocie: ${kocie}'}

    single_run = True
    if (single_run):
        task = ExternalProgramTask(templates, mapping,config,harvesters=[harvester.TextFileHarvester('marker.dat')],use_tempdir=False)
        print task
        result = dispatcher(task)
        print('Result: ' + str(result))
        del task

    serial_run = True
    if serial_run:
        tasks = prepareTestTasks(config, templates)

        master = SerialMaster()
        master.run(tasks)    

        for task in tasks:
            print task.result
        del tasks

    parallel_run = True
    # run sequence
    if (parallel_run):
        tasks = prepareTestTasks(config, templates)

        master = PoolMaster()
        master.run(tasks)    

        for task in tasks:
            print task.result
        del tasks

    print "bye"
