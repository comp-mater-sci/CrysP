""" module to check (every n seconds) if a lock file exists, and only return
    when the file no longer exists
"""
# import native modules
import time
import os

# config
defaultWaitTime = 5.

def WaitOnLockFile(path, waitTime=defaultWaitTime):

    # loop until the lock file no longer exists
    lockFileExists = True
    while(lockFileExists):
        
        lockFileExists = os.path.isfile(path)        
        if lockFileExists: time.sleep(waitTime)
