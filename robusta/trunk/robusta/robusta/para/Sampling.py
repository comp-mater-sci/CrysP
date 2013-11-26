# Prevent the legacy class type being used
__metaclass__ = type

# import robusta modules
from robusta.para.GenericPara import *
from robusta.config import *
from robusta.util import mathTools as mtool

class Sampling(GenericPara):
    """ a class for creating values for parametric studies based on sampling
        techniques
    """
    
    def __init__(self, name):
        """ Constructor
        """
        
        # call base class constructor
        GenericPara.__init__(self, name)
     
        
    def BuildHyperCubeParmList(self, noOfValues):
        """ build a list of combinations of parameter values, based on the
            latin hypercube sampling technique.
            
            The values for each parameter can have a distribution function
            associated with them (use the SetDistribution method - if no function
            is specified the values will be uniformly spread)
            
            Each parameter is divided into values ranging between the limits
            defined in its 'distribution' (see parent class GenericPara). 'noOfValues'
            defines how many sample point to take into account (same number for
            each parameter)  
            
            Combinations are chosen such that each subrange of each parameter is
            used exactly once (see).
            
            This technique better than random sampling when there is very little
            interaction between parameters. It can determine if a single parameter
            dominates the model behaviour.
        """
        # get the list of parameters
        parametersDict = self.GetValue('parametersDict')
        parameterNameList = parametersDict.keys()
        noParms = len(parameterNameList)
        
        # for each parameter get the value ranges
        parameterLevels = np.array([self.EvaluateDistribution(parm, noOfValues) for parm in parameterNameList])

        # get latin hypercube indices, and determine corresponding parameter values
        latinHyperIndices = mtool.LatinHypercubeIndices(noParameters=noParms, noLevels=noOfValues)
        parameterValues = []

        for parameterNumber in range(noParms):
            parameterLevelList = latinHyperIndices[:, parameterNumber]
            parameterValues.append([parameterLevels[parameterNumber, parameterLevel] for parameterLevel in parameterLevelList])
            
        # store the result
        self.SetValue('parameterSampleValues', parameterValues)
        self.RestartTestNumberCounter()
        
