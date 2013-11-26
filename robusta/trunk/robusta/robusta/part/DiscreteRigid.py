# Prevent the legacy class type being used
__metaclass__ = type

# import abaqus modules
try:
    from abaqusConstants import *
    from abaqus import *
    from part import *
except ImportError:
    print ('This module requires the abaqus modules to be available.')
    raise

# import robusta modules
from robusta.part.GenericPart import *
from robusta.config import *


class DiscreteRigid(GenericPart):
    """
    An abstract base class which will contain all template part data and methods.
    
    A part name and abaqus mdb.model object name need to be provided to the
    constructor. example:
    
    myNewGeom = robusta.GenericPart(name='roll1',modelName='Model-1')
    
    
    Data can be loaded from text files by calling the LoadData method.
    """
    
    def __init__(self, name, modelName):
        """ Constructor
        """
        
        # call base class constructor
        GenericPart.__init__(self, name, modelName)
        
        # create the Abaqus part object
        modelObj = self.GetModelHandle()
        partObj = modelObj.Part(name=name, dimensionality=THREE_D, type=DISCRETE_RIGID_SURFACE)
        
        # define a list of required parameters
        parametersRequired = [['extrusionLength', float, 1]]
        self.SetDefault('parametersRequired',parametersRequired)
        
        # set defaults
        self.SetDefault('partType', 'rigid')
        self.SetDefault('recommendedElementType', defaultRigidElement)

        
    def ExtrudeFromGeomObj(self, geometry, analytic=False):
        # call inherited method
        GenericPart.ExtrudeFromGeomObj(self, geometry, analytic)
       
        # convert to shell (so that the part can be placed in the abaqus assembly)
        part = self.GetPartObj()
        
        part.RemoveCells(cellList=part.cells[0:1])
        

    def GetRecommendedMeshControl(self):
        """ return a dictionary of defaults for meshing a deformable part
        """
        # get the generic settings:
        recommendedControls = GenericPart.GetRecommendedMeshControl(self)
        
        # TO DO: add other settings specific to discrete rigid parts
        
        return recommendedControls
