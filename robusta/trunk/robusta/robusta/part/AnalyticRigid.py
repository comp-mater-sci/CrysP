# Prevent the legacy class type being used
__metaclass__ = type

# import abaqus modules
try:
    from abaqusConstants import *
    from abaqus import *
except ImportError:
    print ('This module requires the abaqus modules to be available.')
    raise

# import robusta modules
from robusta.part.GenericPart import *
from robusta.config import *


class AnalyticRigid(GenericPart):
    """

    """
    
    def __init__(self, name, modelName, dimensionality='3D'):
        """ Constructor
        """
        
        # call base class constructor
        GenericPart.__init__(self, name, modelName)
        
        # create the Abaqus part object
        modelObj = self.GetModelHandle()
        if dimensionality=='3D':
            partObj = modelObj.Part(name=name, dimensionality=THREE_D,
                                    type=ANALYTIC_RIGID_SURFACE)
        elif dimensionality=='2D':
            partObj = modelObj.Part(name=name, dimensionality=TWO_D_PLANAR,
                                    type=ANALYTIC_RIGID_SURFACE)
        else:
            errMsg = 'Dont know how to make a part with dimensionality <{0}>.'.format(dimensionality)
            self.ErrorHandling(errMsg)
        
        # define a list of required parameters
        parametersRequired = [['extrusionLength', float, 1]]
        self.AppendDefault('parametersRequired',parametersRequired)
        
        # set defaults
        self.SetDefault('partType', 'analytic')
        self.SetDefault('recommendedElementType', None)
        self.SetValue('dimensionality', dimensionality)

      
    def ExtrudeFromGeomObj(self, geometry):
    
        # call inherited method
        GenericPart.ExtrudeFromGeomObj(self, geometry, analytic=True)

