import multiprocessing
import os 
import sys
import tempfile
import keysubst
import time
import shutil

class Task(object):

    def __init__(self):
        self.template = []
        self.mapping = {}
        self.config = {}
        # configuration flags
        self.config['program_path'] = ''
        self.config['hide_output'] = True
        self.config['output_path'] = ''
        # Other members, to be set by run() and co.
        self.cwd = '.'
        self.tmpdir = ''
        #
        self.command_output = ''
        #
        self.result = None

    def __del__(self):
        try:
            # Last attempt to do housholding
            if self.tmpdir and os.path.isdir(self.tmpdir):
                shutil.rmtree(self.tmpdir)
        except OSError as e:
            print e
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


    def prepare(self):
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
        input_data = keysubst.substituteVarKeys(self.template,full_mapping)
        input_string = ''.join(input_data)
        # Do actual work: start 
        # Fake -->
        output = sys.__stdout__
        output.write(input_string)
        # Fake <--

        # OK, the template is ready, let's run the program...
        out_redir = subprocess.PIPE if self.config['hide_output'] else None
        po = subprocess.Popen(self.config['program_path'], stdin=subprocess.PIPE, stdout=out_redir)
        # and  feed it with the configuration
        stdoutdata, stderrdata = po.communicate(input_string)

        if not self.config['output_path']:
            self.command_output = stdoutdata
        # Diagnostic -->
        #print stdoutdata
        #print stderrdata
        # <-- 



    def harvest(self):
        output = None
        if self.config['output_path']:
            try:
                inp = open(self.config['output_path'],'r')
                res = inp.readlines()
                inp.close()
                if len(res):
                    output = res
            except:
                print 'Cannot harvest the results'    
        return output
        

    def finalize(self):
        # Finalize:
        try:
            # Make sure we end up in the initial directory
            os.chdir(self.cwd)
        except OSError as e:
            print e

    def run(self):
        try:
            self.prepare()
            self.execute()
            result = self.harvest()
            self.finalize()
            self.is_done = True
            return result
        except Exception as e:
            print 'That is terrible... What a shame...'
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
            print 'after ', tot_time, ' is ready:', result.ready()
            result.wait(self.polltime)
        #
        if result.ready() and result.successful():
            res = result.get()
            print 'Getting results', res
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


if __name__ == "__main__":

    multiprocessing.freeze_support()

    config = {}
    if sys.platform == 'win32':
        config['program_path'] = 'c:\\work\\TWRMTMProject\\people\\jgawad\\misc\\pymp\\test\\runsim.cmd'
    else:
        config['program_path'] = '~/jgprojects/TWRMTMProject/people/jgawad/misc/pymp/test/runsim.sh'
    config['output_path'] = 'marker.dat'

    d = {}
    d['ali'] = 'syfilis'
    d['kocie'] = 'mysz'


    single_run = True
    if (single_run):


    
        task = Task()
        task.template = ['Ala ma ${ali}\n', 'Kot ma ${kocie}\n']
        task.mapping = d
        task.setConfig(config)

        print task
    
        result = dispatcher(task)
        print('Result: ' + str(result))



    parallel_run = True
    # run sequence
    if (parallel_run):
        tasks = []
        for i in range(0,20):
            d = {}
            d['ali'] = ('object ' + str(i))
            d['kocie'] = 'mysz'
               
            task = Task()
            task.template = ['Ala ma ${ali}\n', 'Kot ma ${kocie}\n']
            task.mapping = d
            task.setConfig(config)

            print task

            tasks.append(task)

        master = PoolMaster()

        
        master.run(tasks)    

        for task in tasks:
            print task.result

    print "bye"
