# Prevent the legacy class type being used
__metaclass__ = type

# import robusta modules
from robusta.GenericRobusta import *
from robusta.config import *

class GenericControl(GenericRobusta):
    """ a base class for creating parametric studies
    """
    
    def __init__(self, name):
        """ Constructor
        """
        
        # call base class constructor
        GenericRobusta.__init__(self, modelName=name, dbaseType='generic')
        
        # set up the simulation constants dictionary
        self.SetValue('simConstantsDict', {})
        
    
    def SetSimulationConstants(self, constantsDict):
        """ copy the contents of the dictionary 'constantsDict' to every
            simulation that is run.
            
            this can be used to set up simulations in a standard way, even if
            some of the parameters are subsequently changed, eg. during a
            parameteric study
        """
        
        if type(constantsDict)==dict:
            constantNames = constantsDict.keys()
            simConstantsDict = self.GetValue('simConstantsDict')
            
            # copy the contents of the given dictionary to the simulation constants
            # dictionary
            for constant in constantNames:
                simConstantsDict.update({constant:constantsDict.get(constant)})
                
            # store the result
            self.SetValue('simConstantsDict', simConstantsDict)
            
        else:
            errMsg = 'constantsDict should be a Python dictionary.'
            self.ErrorHandling(errMsg)
            
    def GetSimulationConstants(self):
        """ return the simulation constants dictionary
        """
        return self.GetValue('simConstantsDict')
