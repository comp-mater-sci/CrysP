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
from robusta.geom.GenericGeom import *
from robusta.config import *


class Circle(GenericGeom):
    """
        A circular cylinder in 3D or circle in 2D
    """
    
    def __init__(self, name, modelName):
        """ Constructor
        """
        
        # call base class constructor
        GenericGeom.__init__(self, name, modelName)
        
        # set some defaults
        self.SetDefault('centre',(0.0,0.0))
        
        # define a list of required parameters
        parametersRequired = [['radius', float, 1], ['centre', tuple, 2]]
        self.SetDefault('parametersRequired',parametersRequired)
        
        
    def MakeSketch(self, radius=None, centre=None):
        """ Make a sketch from parameter values in the global config file
        """
        
        # check if all parameters given, if not look for defaults        
        parameterValueList = [radius, centre]
        parameterNameList = ['radius', 'centre']
        updatedParmList = self.CheckForNullParameters(parameterValueList, parameterNameList)
        (radius, centre) = updatedParmList
                
        # draw sketch
        sketchObj = self.GetSketchObj()
        sketchObj.CircleByCenterPerimeter(center=centre, point1=(radius+centre[0], 0.0))
        
        # add information on extents of the shape. Note that the extrusion
        # direction in abaqus is the z direction by default, so that only
        # the x and y extents can be defined here
        self.SetValue('xMax', (centre[0] + radius))
        self.SetValue('xMin', (centre[0] - radius))
        self.SetValue('yMax', (centre[1] + radius))
        self.SetValue('yMin', (centre[1] - radius))

        # define a point on the external and internal surface
        self.SetValue('extPoint', (centre[0]+radius, centre[1]))
        self.SetValue('intPoint', centre)
