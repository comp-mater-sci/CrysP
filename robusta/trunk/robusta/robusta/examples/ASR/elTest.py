""" look at the effect of element size in the wet/dry experiment
    of Jurij
"""
import subprocess
import os

folder = os.getcwd()
elSizeList = [0.5,0.2,0.15,0.1]

for index in range(len(elSizeList)):
    jobName = 'elTest_{0}'.format(index)
    
    # generate the script to run in abaqus CAE    
    scriptFile = open('temp.py', 'w')
    scriptFile.write("import robusta.examples.ASR.buildAsr as asr\n")
    scriptFile.write("from robusta.config import *\n")
    scriptFile.write("import sys\n")
    scriptFile.write("asr.build(topRollRadius=203., botRollRadius=203., sheetThick=1.556,\n")
    scriptFile.write("          reductionPercent=75, rollSpeed=10., jobName='{0}', path='{1}',\n".format(jobName, folder))
    scriptFile.write("          rollSpeedRatio=1.0, topRollFriction=0.4, botRollFriction=0.4,\n")
    scriptFile.write("          rollElementSize=defaultElementSize, sheetElementSize={0},\n".format(elSizeList[index]))
    scriptFile.write("          sheetMoveTime=0.001, contactStepTime=0.1, rollingStepTime=4.0)\n")
    scriptFile.write("sys.exit()\n")
    scriptFile.close()

    # call abaqus
    subprocess.check_call(['/home/diarmuid/local/abaqus/abaqus/Commands/abq6122','cae','-noGUI','temp.py'], shell=False)