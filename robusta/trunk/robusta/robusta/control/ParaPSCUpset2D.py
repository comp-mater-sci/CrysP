# Prevent the legacy class type being used
__metaclass__ = type

# import native Python classes
import cPickle

# import robusta modules
from robusta.control.GenericControl import *
from robusta.config import *
from robusta.para.Sampling import *
from robusta.simbuild.pscupset2D import *


class ParaPSCUpset2D(GenericControl):
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
        constants = {'jobName':'PSC_up_2D', 'writeCaeFile':True,
                     'Swift_K':494.129e6, 'Swift_n':0.250051,'Swift_eps0':0.008567,
                     'density':7800., 'youngsModulus':70e9, 'poissonRatio':0.3,
                     'dieRadius':1., 'elementSize':0.025}
        self.SetSimulationConstants(constants)

        # define ranges for parametric study parameters
        testGenerator = Sampling('sampleGenerator')
        testGenerator.AddParameter(parameterName='friction', distribParameters={
                                   'distName':'uniform', 'minVal':0., 'maxVal':0.5})                  
        testGenerator.AddParameter(parameterName='dieLength', distribParameters={
                                   'distName':'uniform', 'minVal':5., 'maxVal':20.})
#        testGenerator.AddParameter(parameterName='Swift_K', distribParameters={
#                                   'distName':'uniform', 'minVal':300., 'maxVal':600.})
        testGenerator.AddParameter(parameterName='Swift_n', distribParameters={
                                   'distName':'uniform', 'minVal':0.15, 'maxVal':0.35}) 
#                              
#        testGenerator.AddParameter(parameterName='dieRadius', distribParameters={
#                                   'distName':'uniform', 'minVal':0.1, 'maxVal':2.})
                                   
        # hypercube values
        testGenerator.BuildHyperCubeParmList(noOfRuns)
        
        # make the input files
        logFileData = []
        for runNumber in range(noOfRuns):
            runName = 'PSC_2D_{0}'.format(runNumber)            
            test = pscupset2D(name=runName)
            test.StoreAllParametersFromDict(self.GetSimulationConstants())
            test.SetValue('caeFileName', runName)
            test.SetValue('modelName', runName)
            
            parameterVals = testGenerator.GetNextTestValues()
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
        
        for parameter in testGenerator.GetAddedParametersList():
            logFileData.append('parameter studied:{0}\n'.format(parameter))
        self.SetValue('logFileData', logFileData)
                         
if __name__=='__main__':
    caseStudy = ParaPSCUpset2D('caseStudy')
    caseStudy.MakeInputFiles(noOfRuns=20)
    
    logFile = file('summary.txt', 'w')
    logFile.writelines(caseStudy.GetValue('logFileData'))
    logFile.close()
