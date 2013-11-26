#!/usr/bin/python
#PBS -l nodes=1
#PBS -l walltime=32:00:00
#PBS -l mem=4gb
#PBS -m abe
#PBS -M diarmuid.shore@mtm.kuleuven.be
#PBS -r n
#module purge
#module load glib/2.31.2
#module load intel_compiler/11.1.072
#module load intel_mkl/10.2.5.035
#module load intel/Compiler_env
#module load abaqus/6.11-2

""" script to batch submit a set of abaqus input files matching a given pattern
"""

# import native modules
import sys
import os
from datetime import datetime
import subprocess
import multiprocessing
import shutil
import time

# ---- config ----
allowMultiCPU = True
#abaqusExecutablePath = 'abaqus'
abaqusExecutablePath = '/home/diarmuid/local/abaqus/abaqus/Commands/abaqus'
struggleOn = False
runningOnCluster = True
checkShell = True
skipCheck = False
waitTime = 30.

pattern = 'simpsh'
#folder = "/user/leuven/305/vsc30540/DATA/simpShearJul1"
folder = os.getcwd()

# cluster commands
if runningOnCluster:
    pass


# start script
os.chdir(folder)
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
if not skipCheck:
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
        subprocess.call(command, shell=checkShell)
        
        # wait for lock file to be deleted
        lockFilePath = os.path.join(folder, '{0}.lck'.format(prefix))
        lockFileExists = True
        while(lockFileExists):
            print 'Waiting on job {0} to finish...'.format(prefix)    
            if lockFileExists: time.sleep(waitTime)
            lockFileExists = os.path.isfile(lockFilePath)        
            
        
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

else: 
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
            subprocess.call(command, shell=checkShell)
            
        except:
            if struggleOn:
                print ' ---------- Struggling ------------ '
                pass
            else:
                raise
                
        # wait for lock file to be deleted
        lockFilePath = os.path.join(folder, '{0}.lck'.format(prefix))
        lockFileExists = True
        while(lockFileExists):
            print 'Waiting on job {0} to finish...'.format(prefix)     
            if lockFileExists: time.sleep(waitTime)
            lockFileExists = os.path.isfile(lockFilePath) 
        
