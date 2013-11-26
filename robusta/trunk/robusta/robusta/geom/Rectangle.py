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


class Rectangle(GenericGeom):
    """
        A circular cylinder in 3D or circle in 2D
    """
    
    def __init__(self, name, modelName):
        """ Constructor
        """
        
        # call base class constructor
        GenericGeom.__init__(self, name, modelName)

        
        # define a list of required parameters
        parametersRequired = [['width', float, 1], ['height', float, 1]]
        self.SetDefault('parametersRequired',parametersRequired)
        
        
    def MakeSketch(self, width=None, height=None, centre=(0.0,0.0)):
        """ Make a sketch from parameter values in the global config file
        """
        
        # if no values provided check for defaults
        parameterValueList = [width, height, centre]
        parameterNameList = ['width', 'height', 'centre']
        updatedParmList = self.CheckForNullParameters(parameterValueList, parameterNameList)
        (width, height, centre) = updatedParmList
        
        # determine bottom left and top right of rectangle based on
        # given dimensions and the origin
        bottomLeftCoord = (centre[0]-(width/2), centre[1]-(height/2))
        topRightCoord = (centre[0]+(width/2), centre[1]+(height/2))
                
        # draw sketch        
        sketchObj = self.GetSketchObj()
        sketchObj.rectangle(point1=bottomLeftCoord, point2=topRightCoord)
        
        # add information on extents of the shape. Note that the extrusion
        # direction in abaqus is the z direction by default, so that only
        # the x and y extents can be defined here
        xMax = topRightCoord[0]
        xMin = bottomLeftCoord[0]
        yMax = topRightCoord[1]
        yMin = bottomLeftCoord[1]
        self.SetValue('xMax', xMax)
        self.SetValue('xMin', xMin)
        self.SetValue('yMax', yMax)
        self.SetValue('yMin', yMin)
        
        # define a point on the external and internal surface
        self.SetValue('extPoint', bottomLeftCoord)
        self.SetValue('intPoint', centre)
        
        # define the centre of gravity
        self.SetValue('COG',((xMax+xMin)/2.,(yMax+yMin)/2.,0.))
        self.SetValue('COGupdated', True)
