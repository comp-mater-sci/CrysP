# Prevent the legacy class type being used
__metaclass__ = type

# import native Python classes
import cPickle

# import robusta modules
from robusta.control.GenericControl import *
from robusta.config import *
from robusta.para.Sampling import *
from robusta.simbuild.shear3D import *

import numpy as np


class ParaShear3D(GenericControl):
    """ a base class for creating parametric studies
    """    
        
    def __init__(self, name):
        """ Constructor
        """
        
        # call base class constructor
        GenericControl.__init__(self, name=name)
        
        
    def MakeInputFiles(self, noOfRuns):
        """ create a series of input files using the latin hypercube sampling
            technique
        """
        print 'running'         
        # set some paramters common to all simulations
        constants = {'jobName':'Shear_3D', 'writeCaeFile':True,
                     'Swift_K':494.129e6, 'Swift_n':0.250051,'Swift_eps0':0.008567,
                     'density':2700., 'youngsModulus':70e9, 'poissonRatio':0.35}
        self.SetSimulationConstants(constants)

        # choose parameter values
        minElementSize = 0.3
        maxElementSize = 0.9
        elementSizes = np.linspace(minElementSize, maxElementSize, noOfRuns)
        
        # make the input files
        logFileData = []
        for runNumber in range(noOfRuns):
            runName = 'simpsh_3D_{0}'.format(runNumber)            
            test = shear3D(name=runName)
            test.StoreAllParametersFromDict(self.GetSimulationConstants())
            test.SetValue('caeFileName', runName)
            test.SetValue('modelName', runName)
            
            parameterVals = {'elementSize':elementSizes[runNumber]}
            test.MakeNewTest(parameters=parameterVals, materialClass='SwiftVonMises',
                             meshClass='HomogenousMesh')
            
            # remove the test data (recreating models)
            jobFileName = test.GetValue('jobFileName')        
            test.TearDown()
            pickleFileName = runName+pickleFileExtension
            pickleFile = file(pickleFileName, 'w')
            cPickle.dump(test, pickleFile)
            pickleFile.close()
                                       
            # write a line in the log file
            logFileData.append('job object: {0}\t cae file: {1}\t input file:{2}\n'.format( \
                               pickleFileName, runName+caeFileExtension,
                               jobFileName))
            del test
        
        logFileData.append('parameter studied:elementSize\n')
        self.SetValue('logFileData', logFileData)
                         
if __name__=='__main__':
    caseStudy = ParaShear3D('caseStudy')
    caseStudy.MakeInputFiles(noOfRuns=10)
    
    logFile = file('summary.txt', 'w')
    logFile.writelines(caseStudy.GetValue('logFileData'))
    logFile.close()
