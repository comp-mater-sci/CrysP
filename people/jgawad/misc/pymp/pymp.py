#!/usr/bin/env python

import multiprocessing
import os 
import sys
import tempfile

def f(x):
    import tempfile
    import time
    try:
        #os.mkdir('%d'%os.getpid())
        tmpdir = tempfile.mkdtemp(suffix='_%d'%os.getpid(),dir=os.getcwd())
        print tmpdir
        print ('pid = %d ' % os.getpid())
        time.sleep(2)
        os.unlink(tmpdir)
        
    except:
        print 'ooops'

    if (x != 7):
        return x*x
        
        

def fcall(x):
    print 'callback fx:', x

def usePool():
    import multiprocessing
    multiprocessing.freeze_support()
    pool = multiprocessing.Pool(processes=multiprocessing.cpu_count(),maxtaskperchild=1)             
    result = pool.map_async(f, range(0,20))

    
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
    
    #print result.ready()
    #if (result.ready()):
    #    print result.get()
    #    
    #pool = multiprocessing.Pool(processes=4)              # start 4 worker processes
    #result = pool.apply_async(f, [10])    # evaluate "f(10)" asynchronously
    #print result.get(timeout=1)           # prints "100" unless your computer is *very* slow
    #result = pool.map(f, range(10))


def qworker(qtask,qres):
    import time
    import multiprocessing
    while not qtask.empty():
        print 'Getting task', multiprocessing.current_process()
        x = qtask.get()
        time.sleep(1)
        qres.put_nowait((x,x*x))
        print 'Putting result'

    

def useQueue():
    import multiprocessing

    q_tasks = multiprocessing.Queue()
    q_results = multiprocessing.Queue()

    for i in range(1,10): q_tasks.put(i)

    nworkers = multiprocessing.cpu_count()

    workers = [ multiprocessing.Process(target=qworker, args=(q_tasks,q_results)) for i in range(0,nworkers) ]
    # start all
    for p in workers: 
        p.start()
    
    print multiprocessing.active_children()

    for p in workers: 
        p.join(20)
    # while not q_tasks.empty():
    
    
    for k in range(0,q_results.qsize()):
        print q_results.get()
    
    
    
if __name__ == '__main__':    
    useQueue()
