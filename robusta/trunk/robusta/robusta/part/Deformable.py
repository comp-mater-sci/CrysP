# Prevent the legacy class type being used
__metaclass__ = type

# import abaqus modules
try:
    from abaqusConstants import *
    from abaqus import *
    from regionToolset import Region
except ImportError:
    print ('This module requires the abaqus modules to be available.')
    raise

# import robusta modules
from robusta.part.GenericPart import *
from robusta.config import *


class Deformable(GenericPart):
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
                                    type=DEFORMABLE_BODY)
            self.SetDefault('recommendedElementType', defaultDeformable3DElement)
                                    
        elif dimensionality=='2D':
            partObj = modelObj.Part(name=name, dimensionality=TWO_D_PLANAR,
                                    type=DEFORMABLE_BODY)            
            self.SetDefault('recommendedElementType', defaultDeformable2DElement)
            
        else:
            errMsg = 'Dont know how to make a part with dimensionality <{0}>.'.format(dimensionality)
            self.ErrorHandling(errMsg)
                
        # define a list of required parameters
        parametersRequired = [['extrusionLength', float, 1]]
        self.SetDefault('parametersRequired',parametersRequired)
        
        # set defaults
        self.SetValue('partType', 'deformable')
        self.SetValue('dimensionality', dimensionality)
       

    def MakeSections(self, method='default', sectionName=None):
        """ assign material sections based on the named method
        """
        partObj = self.GetPartObj()
        modelObj = self.GetModelHandle()
        is3D = self.GetValue('dimensionality') == '3D'
        
        # default method is to do no partitioning, and assign one section to
        # the whole part
        if method=='default':
            
            # create region to assign section
            if is3D:
                cellList = partObj.cells
                regionRef = Region(cells=cellList)
            else:
                faceList = partObj.faces
                regionRef = Region(faces=faceList)
            
            if not self.Exist(sectionName):
                sectionName = modelObj.sections.keys()[0]
            
            # assign the section
            partObj.SectionAssignment(region=regionRef, sectionName=sectionName, 
                                      offset=0.0, offsetType=MIDDLE_SURFACE,
                                      offsetField='', thicknessAssignment=FROM_SECTION)
                                      
            # store the results
            self.SetValue('sectionList',[sectionName])
        
        else:
            errorMessage = 'Dont know how to make sections using method <{0}>.'.format(method)
            self.ErrorHandling(errorMessage)
 
 
    def GetRecommendedMeshControl(self):
        """ return a dictionary of defaults for meshing a deformable part
        """
        # get the generic settings:
        recommendedControls = GenericPart.GetRecommendedMeshControl(self)
        
        # TO DO: add other settings specific to deformable parts
        
        return recommendedControls
