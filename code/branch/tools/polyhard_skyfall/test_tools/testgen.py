#!/usr/bin/env python
import os,sys
import numpy
import numpy.linalg
import argparse
import string
import shutil
import glob
if __name__ == '__main__':

    parser = argparse.ArgumentParser()
    parser.add_argument('--eps_min',type=float,default=0.0)
    parser.add_argument('--eps_max',type=float,required=True)
    parser.add_argument('--delta_eps',type=float,required=True)
    parser.add_argument('--standalone',action='store_true',default=False)
    args = parser.parse_args()
    # 
    eps_0 = args.eps_min
    eps_max = args.eps_max
    delta_eps = args.delta_eps
    #
    eps = numpy.array([ [2./3., 0., 0],
            [0., -1./3, 0.],
            [0., 0., -1./3.] ])

    eps /= numpy.linalg.norm(eps)
    eps *= delta_eps

    run_standalone = args.standalone

    if run_standalone:
        main_first = open('main_ctl.first.tmpl','r').readlines()
        main_next = open('main_ctl.next.tmpl','r').readlines()
        if sys.platform == 'win32':
            executable_altay = os.path.normpath('\\work\\TWRMTMProject\\MTM\\branches\\AlTaySub\\AlTay\\Debug\\AlTay.exe')
            executable_polyhard = os.path.normpath('\\work\\TWRMTMProject\\code\\branch\\tools\\polyhard_skyfall\\Debug\\altayPolyHard.exe')
        else:
            executable_altay = 'altay'
            executable_polyhard = 'altayPolyHard'


    template_main_first = string.Template(''.join(main_first))
    template_main_next  = string.Template(''.join(main_next))
    

    print('Norm of eps:', numpy.linalg.norm(eps,'fro'))

    fmt = 3*'%10.5e ' + '\n'
    fmt_tens = 3*fmt

    out = sys.stdout
    step = 1

    #for step in range(1,10):
    while eps_0 < eps_max:
        seq = step
        eps_0 = eps_0 + delta_eps
        # Write defdata.dat
        out = open('defdata.dat','w')
        out.write('%d %d\n' % (step,seq))
        #for row in eps:
        #    out.write(fmt % tuple(row))
        eps_as_string = fmt_tens % tuple(eps.flatten())
        out.write(eps_as_string)
        out.write('1\n0\n1\n')
        out.write(2*'%f\n' % (eps_0, eps_0 + delta_eps))
        out.close()
        if run_standalone:
            try:
                mapping = {'epsilon_tensor': eps_as_string.strip()}
                if step == 1:
                    for f in glob.glob('texinp*') + glob.glob('texout*') + glob.glob('MAIN.CTL'):
                        os.remove(f)
                    main_ctl = template_main_first.substitute(mapping)
                    
                else:
                    shutil.move('texout.cub', 'texinp.cub')
                    shutil.move('texout.BPM', 'texinp.BPM')
                    os.system('rm texout.*')
                    main_ctl = template_main_next.substitute(mapping)
                #
                try:
                    with open('MAIN.CTL','w') as ctlfile:
                        ctlfile.write(main_ctl)
                        ctlfile.close()
                    os.system(executable_altay)
                except IOError as e:
                    print e
                    exit(1)
            except:
                print 'Unhandled exception'
                exit(1)
            os.system(executable_polyhard + ' altayPolyHard.cfg')    
                          
        else:
            os.system('hms_texupdate.sh')
        shutil.move('elem.hard', 'elem_%d.hard' % step)
        shutil.move('elem.str', 'elem_%d.str' % step)                
        step += 1
       

        #out.write

