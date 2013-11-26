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


class Annulus(GenericGeom):
    """
        An annuluar tube in 3D or annulus in 2D
    """
    
    def __init__(self, name, modelName):
        """ Constructor
        """
        
        # call base class constructor
        GenericGeom.__init__(self, name, modelName)
        
        # set some defaults
        self.SetDefault('centre',(0.0,0.0))
        
        # define a list of required parameters
        parametersRequired = [['outRadius', float, 1], ['inRadius', float, 1], ['centre', tuple, 2]]
        self.SetDefault('parametersRequired',parametersRequired)
        
        
    def MakeSketch(self, inRadius=None, outRadius=None, centre=None):
        """ Make a sketch from parameter values in the global config file
        """
        
        # if no values provided check for defaults
        parameterValueList = [inRadius, outRadius, centre]
        parameterNameList = ['inRadius', 'outRadius', 'centre']
        updatedParmList = self.CheckForNullParameters(parameterValueList, parameterNameList)
        (inRadius, outRadius, centre) = updatedParmList
        
        # check the dimensions make sense
        if outRadius <= inRadius:
            self.ErrorHandling('outRadius should be greater than inRadius.')
          
        # draw sketch
        sketchObj = self.GetSketchObj()
        sketchObj.CircleByCenterPerimeter(center=centre, point1=(inRadius+centre[0], 0.0))
        sketchObj.CircleByCenterPerimeter(center=centre, point1=(outRadius+centre[0], 0.0))
        
        # add information on extents of the shape. Note that the extrusion
        # direction in abaqus is the z direction by default, so that only
        # the x and y extents can be defined here
        self.SetValue('xMax', (centre[0] + outRadius))
        self.SetValue('xMin', (centre[0] - outRadius))
        self.SetValue('yMax', (centre[1] + outRadius))
        self.SetValue('yMin', (centre[1] - outRadius))

        # define a point on the external and internal surface
        self.SetValue('extPoint', (centre[0]+outRadius, centre[1]))
        self.SetValue('intPoint', (centre[0]+inRadius, centre[1]))
