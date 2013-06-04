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

    def __init__(self,template=[], mapping={},config={},harvesters=[]):
        self.template = template
        self.mapping = mapping
        self.harvesters = harvesters
        # configuration flags
        self.config = {}
        self.config['program_path'] = ''
        self.config['hide_output'] = True
        self.setConfig(config)

        # Other members, to be set by run() and co.
        self.cwd = '.'
        self.tmpdir = ''
        #
        self.command_output = ''
        #
        self.result = None
        #
        self.purge_tmpdir = True

    def removeTempDir(self):
        try:
            if self.purge_tmpdir and self.tmpdir and os.path.isdir(self.tmpdir):
                shutil.rmtree(self.tmpdir)
        except OSError as e:
            print e
        except:
            pass

    def __del__(self):
        try:
            # Last attempt to do housholding
            self.removeTempDir()
        except:
            pass


    def __str__(self):
        result = ('template: ' + str(self.template) + '\n' +
                  'mapping: ' + str(self.mapping) + '\n' + 
                  'config:' + str(self.config))
        return result

    def prepareWorkdir(self):
        # Prepare execution environment
        self.cwd = os.getcwd()
        try:
            #os.mkdir('%d'%os.getpid())
            self.tmpdir = tempfile.mkdtemp(suffix='_%d'%os.getpid(),dir=self.cwd)
            os.chdir(self.tmpdir)
            print self.tmpdir
        except OSError:
            raise


    def preExecute(self):
        self.prepareWorkdir()


    def setMapping(self,mapping):
        self.mapping.update(mapping)


    def setConfig(self,config):
        self.config.update(config)
        program_path = os.path.expanduser(self.config['program_path'])
        if not os.path.exists(program_path):
            print('Cannot find ' + program_path)
            raise OSError()
        self.config['program_path'] = os.path.realpath(program_path)

    def execute(self):
        import os
        import subprocess
        # Put special keywords into mapping:
        # EXEC_DIR
        full_mapping = {}
        full_mapping['WORK_DIR'] = self.tmpdir
        #
        full_mapping.update(self.mapping)
        template = string.Template(self.template)
        input_data = template.safe_substitute(full_mapping)
        input_string = ''.join(input_data)
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
        po = subprocess.Popen(self.config['program_path'], stdin=subprocess.PIPE, stdout=out_redir)
        # and  feed it with the configuration
        self.command_output, stderrdata = po.communicate(input_string)
        # Diagnostic -->
        #print stdoutdata
        #print stderrdata
        # <-- 
        pass



    def postExecute(self):
        self.result = {}
        for harvester in self.harvesters:
            self.result.update(harvester.harvest(self.tmpdir))
        

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
        except Exception as e:
            print 'Unexpected exception in Task::run()'
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
    print ('pid = %d ' % os.getpid())
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
           
            task = Task(template, mapping,config,harvesters=[harvester.TextFileHarvester('marker.dat')])
            print task
    
            tasks.append(task)
        return tasks


    config = {}
    if sys.platform == 'win32':
        config['program_path'] = 'c:\\work\\TWRMTMProject\\people\\jgawad\\misc\\pymp\\test\\runsim.cmd'
    else:
        config['program_path'] = '~/jgprojects/TWRMTMProject/people/jgawad/misc/pymp/test/runsim.sh'


    mapping = {}
    mapping['ali'] = 'syfilis'
    mapping['kocie'] = 'mysz'

    template = ['Ala ma ${ali}\n', 'Kot ma ${kocie}\n']

    single_run = True
    if (single_run):
        task = Task(template, mapping,config,harvesters=[harvester.TextFileHarvester('marker.dat')])
        print task
        result = dispatcher(task)
        print('Result: ' + str(result))
        del task

    serial_run = True
    if serial_run:
        tasks = prepareTestTasks(config, template)

        master = SerialMaster()
        master.run(tasks)    

        for task in tasks:
            print task.result
        del tasks

    parallel_run = True
    # run sequence
    if (parallel_run):
        tasks = prepareTestTasks(config, template)

        master = PoolMaster()
        master.run(tasks)    

        for task in tasks:
            print task.result
        del tasks

    print "bye"
