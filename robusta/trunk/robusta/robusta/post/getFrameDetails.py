""" functions to export a lookup table of data for cross referencing increment
    numbers, strain values, reference frame angles
"""

# Prevent the legacy class type being used
__metaclass__ = type

# import robusta modules
from robusta.config import *
from robusta.util import angleConversion

# import third party modules
try:
    import numpy as np
except ImportError:
    print 'This module requires the following: numpy'
    raise
    

def exportStrains(odbName, outFileName='frameXrefs.csv', elementSetName=None,
           delimiter=',', strainVarName=defaultStrainVariableName, units='degrees'):
    """ export the list of increment numbers, strain values and local coord system
        rotation angles for the elements in the named set. If no name is given
        all elements found will be exported
    """
    
    # open the odb
    odb = session.openOdb(odbName)
    
    stepObj = odb.steps
    stepNameList = stepObj.keys()
    noSteps = len(stepNameList)
    
    instanceObj = odb.rootAssembly.instances
    instanceNameList = instanceObj.keys()
    noInstances = len(instanceNameList)
    
    # open the outout file
    headerValues = ['step_name','increment','elem_label','E11','E22','E33','E12','E13','E23',
                    'phi1','PHI','phi2','maxU1','maxU2']
    formatStr = ''.join(['{'+str(i)+'}'+'{0}'.format(delimiter) \
                         for i in range(len(headerValues))]) + '\n'
        
    outputFile = open(outFileName, 'w')    
    outputFile.write('ODB {0} with {1} steps and {2} instances.\n\n'.format(odbName,
                     noSteps, noInstances))
    
    # for each element in each instance print the data for all steps and all frames
    for instanceName in instanceNameList:
    
        # get the fieldOutput values for this instance
        instance = instanceObj[instanceName]
        strainValues = stepObj[stepNameList[0]].frames[0].fieldOutputs[strainVarName].getSubset(region=instance)
        noElements = len(strainValues.values)
        outputFile.write('Instance name {0}; {1} elements\n'.format(instanceName, noElements))

       
        
        # print the results for each element in all steps/frames
        for elIndex in range(noElements):
            outputFile.write((','.join(headerValues)+'\n'))
            
            for stepName in stepNameList:
                currentStep = stepObj[stepName]
                frameObj = currentStep.frames              
            
                for frameIndex in range(len(frameObj)):
                
                    currentFrame = frameObj[frameIndex]
                    
                    # get the strain component values of interest                
                    strainValues = currentFrame.fieldOutputs[strainVarName].getSubset(region=instance)
                    elLabel = strainValues.values[elIndex].elementLabel
                                        
                    E11 = strainValues.getScalarField(componentLabel=strainVarName+'11').values[elIndex].data
                    E22 = strainValues.getScalarField(componentLabel=strainVarName+'22').values[elIndex].data
                    E33 = strainValues.getScalarField(componentLabel=strainVarName+'33').values[elIndex].data
                    E12 = strainValues.getScalarField(componentLabel=strainVarName+'12').values[elIndex].data
                    E13 = strainValues.getScalarField(componentLabel=strainVarName+'13').values[elIndex].data
                    E23 = strainValues.getScalarField(componentLabel=strainVarName+'23').values[elIndex].data
                    
                    # get the max displacements
                    U1Values = currentFrame.fieldOutputs['U'].getSubset(region=instance).getScalarField(componentLabel='U1').values
                    U2Values = currentFrame.fieldOutputs['U'].getSubset(region=instance).getScalarField(componentLabel='U2').values
                    
                    maxU1 = max([disp.data for disp in U1Values])
                    maxU2 = max([disp.data for disp in U2Values])
                    
                    # calculate the Euler angles for the local coord sys
                    cosMatrix = strainValues.values[elIndex].localCoordSystem
                    if cosMatrix is None:
                        phi1 = 0.
                        PHI = 0.
                        phi2 = 0.
                    else:
                        (phi1, PHI, phi2) = angleConversion.GetEulerAnglesForMatrix(
                                            units=units,cosineMatrix=np.array(cosMatrix))
                                    
                    outputFile.write(formatStr.format(stepName, currentFrame.frameId,
                                     elLabel, E11, E22, E33, E12, E13, E23, phi1,
                                     PHI, phi2, maxU1, maxU2))
                                     
    outputFile.close()
