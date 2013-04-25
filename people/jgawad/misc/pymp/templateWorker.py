import multiprocessing
import os 
import sys
import tempfile
import keysubst
import time

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
        self.is_done = False

    def __del__(self):
        try:
            # Last attempt to do housholding
            if self.tmpdir and os.path.isdir(self.tmpdir):
                os.unlink(self.tmpdir)
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
        program_path = self.config['program_path']
        if not os.path.exists(program_path):
            raise OSError()
        self.config['program_path'] = os.path.realpath(os.path.expanduser(program_path))

    def execute(self):
        import os
        import subprocess
        # Put special keywords into mapping:
        # EXEC_DIR
        full_mapping = {}
        full_mapping['EXEC_DIR'] = self.tmpdir
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

        print stdoutdata
        print stderrdata 



    def harvest(self):
        output = None
        if self.config['output_path']:
            try:
                inp = open(self.config['output_path'],'r')
                # Read just one line
                res = inp.readline()
                inp.close()
                output = []
                if (res != ''):
                    output.append(res)
                del res
            except:
                print 'ouups'    
        return output
        

    def finalize(self):
        # Finalize:
        try:
            os.chdir(self.cwd)
            # FIXME: there are problems with unlinking the temporary dir
            #os.unlink(self.tmpdir)
        except Exception as e:
            print e
            raise e
        finally:
            os.chdir(self.cwd) # Make sure we end up in the initial directory

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



def worker(task):
    print ('pid = %d ' % os.getpid())
    print task
    return task.run()




if __name__ == "__main__":
    import multiprocessing
    multiprocessing.freeze_support()

    config = {}
    config['program_path'] = 'c:\\work\\TWRMTMProject\\people\\jgawad\\misc\\pymp\\test\\runsim.cmd'
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
    
        result = worker(task)
        print result

    # run sequence

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

    pool = multiprocessing.Pool(processes=multiprocessing.cpu_count(),maxtasksperchild=1)
    result = pool.map_async(worker, tasks)

    
    timeout = 35
    tot_time = 0
    
    tmp = result.wait(timeout)
    print(type(tmp))
    while (not result.ready()) and (tot_time < 30):
        tot_time += timeout
        print 'after ', tot_time, ' is ready:', result.ready()
        result.wait(timeout)
    #
    if result.ready() and result.successful():
        res = result.get()
        print 'Getting results', res
    pass

    for task in tasks:
        print task.is_done


    print "bye"