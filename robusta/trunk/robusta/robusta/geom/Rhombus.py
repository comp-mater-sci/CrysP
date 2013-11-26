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


class Rhombus(GenericGeom):
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
        self.SetDefault('angle', 75.)
        
        
    def MakeSketch(self, width=None, height=None, angle=None, centre=(0.0,0.0)):
        """ Make a sketch from parameter values in the global config file
        """
        
        import math
        
        # if no values provided check for defaults
        parameterValueList = [width, height, centre, angle]
        parameterNameList = ['width', 'height', 'centre', 'angle']
        updatedParmList = self.CheckForNullParameters(parameterValueList, parameterNameList)
        (width, height, centre, angle) = updatedParmList
        
        # determine bottom left and top right of rectangle based on
        # given dimensions and the origin
        print angle 
        angleInRad = (angle/180.)*math.pi
        displacement = height/math.tan(angleInRad)
        bottomLeftCoord = (centre[0]-(width/2), centre[1]-(height/2))
        topLeftCoord = (centre[0]-(width/2)+displacement, centre[1]+(height/2))
        bottomRightCoord = (centre[0]+(width/2), centre[1]-(height/2)) 
        topRightCoord = (centre[0]+(width/2)+displacement, centre[1]+(height/2))
                
        # draw sketch        
        sketchObj = self.GetSketchObj()
        sketchObj.Line(point1=bottomLeftCoord, point2=bottomRightCoord)
        sketchObj.Line(point1=bottomRightCoord, point2=topRightCoord)
        sketchObj.Line(point1=topRightCoord, point2=topLeftCoord)
        sketchObj.Line(point1=topLeftCoord, point2=bottomLeftCoord)
        
        # add information on extents of the shape. Note that the extrusion
        # direction in abaqus is the z direction by default, so that only
        # the x and y extents can be defined here
        self.SetValue('xMax', (topRightCoord[0]))
        self.SetValue('xMin', (bottomLeftCoord[0]))
        self.SetValue('yMax', (topRightCoord[1]))
        self.SetValue('yMin', (bottomLeftCoord[1]))
        
        # define a point on the external and internal surface
        self.SetValue('extPoint', bottomLeftCoord)
        self.SetValue('intPoint', centre)
