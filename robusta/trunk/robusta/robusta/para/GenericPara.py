# Prevent the legacy class type being used
__metaclass__ = type

# import third party modules
try:
    import numpy as np
except ImportError:
    print 'Numpy needs to be installed and accessible.'
    raise

# import robusta modules
from robusta.GenericRobusta import *
from robusta.config import *
from robusta.util import mathTools as mtools

class GenericPara(GenericRobusta):
    """ a base class for creating parametric studies
    """
    
    def __init__(self, name):
        """ Constructor
        """
        
        # call base class constructor
        GenericRobusta.__init__(self, modelName=name, dbaseType='generic')
        
        # set some defaults
        self.SetDefault('distributionList', ['uniform'])
        self.SetDefault('parametersDict', {})
        
        self.SetDefault('distName', 'uniform')
        self.SetDefault('minVal', 0.)
        self.SetDefault('maxVal', 1.)
        
        # other values
        self.SetValue('nextTestNumber',0)
        self.SetValue('parameterSampleValues', None)
        self.SetValue('addedParametersLog',[])
    
    
    def GetAddedParametersList(self):
        """ return the list of parameters that have been added so far
        """
        return self.GetValue('addedParametersLog')
       
        
    def AddParameter(self, parameterName, distribParameters):
        """ add a parameter and set the distribution to use for it. If the parameter
            already exists, it is overwritten.
            
            'distribParameters' is a Python dict with keys for each of the
            parameters the named distribution requires. As a minimum the
            keys 'distName', 'minVal' and 'maxVal' should be given.

        """
        # get copies of the parameter name list and distribution function details
        distributionList = self.GetValue('distributionList')
        parametersDict = self.GetValue('parametersDict')
            
        # create a new parameter if needed
        parametersLog = self.GetValue('addedParametersLog')
        parametersLog.append(parameterName)
        self.SetValue('addedParametersLog', parametersLog)
        if not parameterName in parametersDict.keys():
            parametersDict.update({parameterName:{}})

        # update distribution parameters
        distribSettings = self.GetDistributionSettings(parameterName)
        for distribParameter in distribParameters.keys():
            distribSettings.update({distribParameter:distribParameters.get(distribParameter)})
            
        # check if 'distName', 'minVal' and 'maxVal' are defined, if
        # not use defaults
        distParmsDefined = distribSettings.keys()
        if not 'distName' in distParmsDefined:
            distribSettings.update({'distName':self.GetDefault('distName')})
            
        if not 'minVal' in distParmsDefined:
            distribSettings.update({'minVal':self.GetDefault('minVal')})
            
        if not 'maxVal' in distParmsDefined:
            distribSettings.update({'maxVal':self.GetDefault('maxVal')})
                
        # store the updated values
        parametersDict.update({parameterName:distribSettings})
        self.SetValue('parametersDict', parametersDict)
    
    
    def GetDistributionSettings(self, parameterName):
        """ return the distribution settings for the named parameter
        """
        parametersDict = self.GetValue('parametersDict')
        return parametersDict.get(parameterName)
        
        
    def EvaluateDistribution(self, parameterName, noValues):
        """ return a list of sample values for the given parameter
                   
        """
        parametersDict = self.GetValue('parametersDict')
        distribSettings = self.GetDistributionSettings(parameterName)
        
        # get parameter values common to all distributions
        name = distribSettings.get('distName')
        minVal = distribSettings.get('minVal')
        maxVal = distribSettings.get('maxVal')

        # calculated distributed parameter values
        if name=='uniform':            
            return np.linspace(minVal, maxVal, num=noValues)
            
        else:
            errMsg = 'only the following are implemented:{0}'.format(self.GetValue('distributionList'))
            self.ErrorHandling(errMsg)
            
    
    def RestartTestNumberCounter(self):
        """ reset the counter for getting sets of parameter values for a simulation
        """
        self.SetValue('nextTestNumber',0)
            
            
    def GetNextTestValues(self):
        """ return a dictionary of parameter values from a previously generated
            list of parameter names and parameter sample values
        """
        parametersDict = self.GetValue('parametersDict')
        parameterNames = parametersDict.keys()
        noParms = len(parameterNames)
        
        nextTestNumber = self.GetValue('nextTestNumber')
        
        parameterSampleValues = self.GetValue('parameterSampleValues')
        noSamples = len(parameterSampleValues[0])
        
        if self.Exist(parameterSampleValues) and nextTestNumber<=noSamples and noSamples>0:
             # get the sample parameter values, and build the dictionary
             testRunDict = {}
             for parameterNumber in range(noParms):
                testRunDict.update({parameterNames[parameterNumber]:parameterSampleValues[parameterNumber][nextTestNumber]})
             
             # increment the test number and return the dictionary
             self.SetValue('nextTestNumber', nextTestNumber + 1)
             return testRunDict
             
        else:
            return None
