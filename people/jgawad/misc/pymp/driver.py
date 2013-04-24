#!/usr/bin/env python

import string

class Master(object):
    def __init__(self,grid,template,commfile,program,output):
        """Initialize the master object."""
        #
        self.comm_fname = commfile
        self.prog_fname = program
        self.output_fname = output
        self.output_file = None


        self.symmetry = 1
        self.template = ''

        try:
            with open(grid,'r') as inp_grid, open(template,'r') as inp_templ:
                # Get the gridfile
                self.grid_data = inp_grid.readlines()
                inp_grid.close()
                npoints,self.symmetry = [int(s) for s in self.grid_data.pop(0).split()]
                #
                config_template = ''.join(inp_templ.readlines())
                self.template = string.Template(config_template)
                inp_templ.close()

            self.output_file = open(self.output_fname,'w')
        except IOError as e:
            print(e)
            raise e

    def configure(self,strain_increment = 0.02,work_increment = 0.0004,stress_scaling = 1.0):
        """Impose configuration parameters to the config template."""
        # 
        self.config = {}
        self.config['stress_scaling'] = stress_scaling
        self.config['strain_increment'] = strain_increment
        self.config['work_increment'] = work_increment
        # Pre-substitute all the parameters available at this moment.
        self.configuration_template = self.template.safe_substitute(self.config)


    def processGrid():
        import subprocess
        import string
        import sys,os
        #
        output = []
        k = 1
        for gridline in self.grid_data:
            print 'Processing grid point', k
            config['vPlDeps'] = gridline.strip()
            config_file = template.substitute(config)
            print template.substitute(config)

            # Start 
        
            out_redir = subprocess.PIPE if hide_output else None
            # OK, the template is ready, let's run the program...
            po = subprocess.Popen(prog_fname,stdin=subprocess.PIPE,
                 stdout=out_redir)
        
            # and  feed it with the configuration
            stdoutdata, stderrdata = po.communicate(config_file)
            print 'Simulation finished, exit code:', po.poll() 
            # Grab the result file
            try:
                inp_res = open(result_fname,'r')
                # Read just one line
                res = inp_res.readline()
                inp_res.close()
                if (res != ''):
                    output.append(res)
                del res
            except IOError:
                print('Cannot open result file for point %d' % k)
            k += 1
        return output




def main(args):
    
    master = Master(grid='grid402.grd',template='in.tmpl',program='runsim',commfile='outcomm.dat', output='elem.mmm')

    master.configure()

    print master.configuration_template
    
   

    output,symmetry = process(grid_fname,template_fname,prog_fname,comm_fname,strain_increment,work_increment)
    # Evaluation of stresses is finished, lets output the results
    try:
        out_output = open(output_fname,'w')
        out_output.writelines(' %d  %d\n' % (len(output),symmetry))
        out_output.writelines(output)
        out_output.close()
    except IOError:
        print('Cannot write the output file')
        exit(1)




def process(config):
    import sys
    import string
    import os
    # 
    output = processGrid(template,grid_data[1:],config,prog_fname,result_fname,True)

    return (output,symmetry)



def processGrid(template,grid_data,config,prog_fname,result_fname,hide_output=True):
    import subprocess
    import string
    import sys,os
    #
    output = []
    k = 1
    for gridline in grid_data:
        print 'Processing grid point', k
        config['vPlDeps'] = gridline.strip()
        
        config_file = template.substitute(config)
        print template.substitute(config)
        
        out_redir = subprocess.PIPE if hide_output else None
        # OK, the template is ready, let's run the program...
        po = subprocess.Popen(prog_fname,stdin=subprocess.PIPE,
             stdout=out_redir)
        
        # and  feed it with the configuration
        stdoutdata, stderrdata = po.communicate(config_file)
        print 'Simulation finished, exit code:', po.poll() 
        # Grab the result file
        try:
            inp_res = open(result_fname,'r')
            # Read just one line
            res = inp_res.readline()
            inp_res.close()
            if (res != ''):
                output.append(res)
            del res
        except IOError:
            print('Cannot open result file for point %d' % k)
        k += 1
    return output


# Start-up dispatcher    
if __name__ == "__main__":
    #import re
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument('--grid',help='grid file of strain rate modes',required=True)
    try:
        args = parser.parse_args()

        print args.grid
        main(args)
    except SystemExit:
        pass





    
