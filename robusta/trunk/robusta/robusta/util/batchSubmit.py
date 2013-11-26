#!/usr/bin/python
#PBS -l nodes=1:8
#PBS -l walltime=10:00:00
#PBS -l mem=4gb
#PBS -m abe
#PBS -M diarmuid.shore@mtm.kuleuven.be

module load abaqus/6.11-2 
cd $PBS_O_WORKDIR
""" script to batch submit a set of abaqus input files matching a given pattern
"""

# import native modules
import sys
import os
from datetime import datetime
import subprocess
import multiprocessing
import shutil

# ---- config ----
allowMultiCPU = True
abaqusExecutablePath = 'abaqus'
struggleOn = True

# get file list
noArgs = len(sys.argv)
if noArgs<2:
    raise Exception('a pattern must be specified.')
pattern = sys.argv[1]

if noArgs>2:
    if sys.argv[2]=='dry':
        dryrun = True
else:
    dryrun = False

folder = os.getcwd()
fileList = [fileName for fileName in os.listdir(folder) \
            if os.path.isfile(fileName) and fileName.endswith('.inp') and \
            pattern in fileName]

if fileList==[]:
    raise Exception('No files found containing < {0} >.'.format(pattern))

# check if old files exist, if so backup
for fileName in fileList:
    prefix = fileName.rstrip('.inp')
    allRelatedFiles = [fileName for fileName in os.listdir(folder) \
                       if prefix in fileName and not fileName.endswith('.inp')]
    
    if not allRelatedFiles==[]:
        # the backup folder has the given pattern removed from the name (to
        # prevent problems re running this script)
        backupPrefix = ''.join(prefix.split(pattern))
        backupPath = os.path.join(folder, '{0}_{1}'.format(backupPrefix,datetime.time(datetime.now())))
        os.mkdir(backupPath)
        
        for fileName in allRelatedFiles:
            oldFile = os.path.join(folder, fileName)
            newFile = os.path.join(backupPath, fileName)
            
            shutil.copyfile(oldFile, newFile)
            os.remove(oldFile)
            
# do a data check (abaqus needs to be on the system execution path)
checkFileName = 'tmp_check.txt'
checkFailed = False
checkFailDetails = []
print 'Checking {0} files'.format(len(fileList))
for fileName in fileList:

    # delete any existing tmp file
    prefix = fileName.rstrip('.inp')
    
    try:
        os.remove(os.path.join(folder, checkFileName))
    except OSError:
        pass
    
    # carry out data check
    print 'checking: {0}'.format(fileName)
    command = '{0} interactive datacheck input={1} job={2} > {3}'.format(
              abaqusExecutablePath, fileName, prefix, checkFileName) 
    subprocess.check_call(command, shell=True)
    
    # check the output for errors
    checkFile = file(checkFileName, 'r')
    data = checkFile.readlines()
    checkFile.close()
    for line in data:
        if 'exited with errors' in line:
            checkFailed = False
            checkFailDetails.append('file:{0} text:{1}\n'.format(fileName, line))
            print checkFailDetails[-1]
    
# raise an exception if something went wrong
if checkFailed:
    errorFileName = 'error.txt'
    errorFile = file(errorFileName, 'w')
    errorFile.writelines(checkFailDetails)
    errorFile.close()
    raise Exception('Datacheck failed: See {0} for details.'.format(errorFileName))

if not dryrun:  
    # otherwise submit the jobs sequentially
    if allowMultiCPU:
        noCPUsToUse = multiprocessing.cpu_count()
    else:
        noCPUsToUse = 1
        
    for fileName in fileList:
        print 'Running Analysis for {0}...'.format(fileName)
        prefix = fileName.rstrip('.inp')
        allRelatedFiles = [fileName for fileName in os.listdir(folder) \
                           if prefix in fileName and not fileName.endswith('.inp')]
        for fileToDel in allRelatedFiles:
            os.remove(os.path.join(folder, fileToDel))
        
        logFileName = prefix + '_batch.log'
        command = '{0} interactive analysis input={1} job={2} cpus={3} > {4}'.format(
                  abaqusExecutablePath, prefix, prefix, noCPUsToUse, logFileName)
        try:
            subprocess.check_call(command, shell=True)
            
        except:
            if struggleOn:
                print ' ---------- Struggling ------------ '
                pass
            else:
                raise
        
