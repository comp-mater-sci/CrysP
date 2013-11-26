import numpy as np
import os
import math
from abaqus import *
from abaqusConstants import *

def GetLocalCoordHistOneEl(odb, fileName, folder=os.getcwd()):
    
    stepNames = odb.steps.keys()
    unitXVector = np.array([[1.],[0.],[0.]])
    
    path = os.path.join(folder, fileName)
    currentFile = open(path, 'w')
    currentFile.write('deg\tS11\tS22\tS33\tS12\tLE12\n') 
    line = '{0:6.2f}\t{1:6.2f}\t{2:6.2f}\t{3:6.2f}\t{4:6.2f}\t{5:6.2f}\n'
    
    # go through all steps
    for stepName in stepNames:
        
        step = odb.steps[stepName]
        frames = step.frames
        
        # go through all frames
        for frame in frames:
        
            field = frame.fieldOutputs['S'].values[0]
            
            rotMatrix = np.array(field.localCoordSystem)
            vector = np.dot(rotMatrix, unitXVector)
            angle = np.degrees(np.arctan(vector[1], vector[0]))
            
            stress = field.data
            strain = frame.fieldOutputs['LE'].values[0].data[3]
            currentFile.write(line.format(angle[0], stress[0], stress[1],
                                          stress[2], stress[3], strain))
    
    currentFile.close()
