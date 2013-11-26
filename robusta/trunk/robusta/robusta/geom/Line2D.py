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


class Line2D(GenericGeom):
    """
        A simple line in 2D
    """
    
    def __init__(self, name, modelName):
        """ Constructor
        """
        
        # call base class constructor
        GenericGeom.__init__(self, name, modelName)

        
        # define a list of required parameters
        parametersRequired = [['height', float, 1]]
        self.SetDefault('parametersRequired',parametersRequired)
        
        
    def MakeSketch(self, centre=(0.0,0.0), height=None, length=None,
                   orientation='vertical', centreLineOnOrigin=True):
        """ Make a sketch of a line
        
            'height' or 'length' mean the same thing
        """
        # if no values provided check for defaults
        if height is None and length is None:
            parameterValueList = [length]
            parameterNameList = ['length']
            updatedParmList = self.CheckForNullParameters(parameterValueList, parameterNameList)
            height = updatedParmList
            
        elif not length is None:
            height = length

        # determine bottom left and top right of the line based on
        # given dimensions and the origin
        if centreLineOnOrigin:
            bottomLeftCoord = centre[1]-(height/2)
            topRightCoord = centre[1]+(height/2)

        else:
            bottomLeftCoord = 0.
            topRightCoord = height
                
        # add information on extents of the shape. Note that the extrusion
        # direction in abaqus is the z direction by default, so that only
        # the x and y extents can be defined here
        if orientation=='vertical':
            xMax = 0.
            xMin = 0.
            yMax = topRightCoord
            yMin = bottomLeftCoord
            
            # define a point on the external and internal surface
            self.SetValue('extPoint', (xMin, bottomLeftCoord))
            self.SetValue('intPoint', centre)
        
        elif orientation=='horizontal':
            yMax = 0.
            yMin = 0.
            xMax = topRightCoord
            xMin = bottomLeftCoord
            
            # define a point on the external and internal surface
            self.SetValue('extPoint', (bottomLeftCoord, yMin))
            self.SetValue('intPoint', centre)
            
        else:
            errMsg = 'Unknown orientation for line <{0}>'.format(orientation)
            self.ErrorHandling(errMsg)
        
        self.SetValue('xMax', xMax)
        self.SetValue('xMin', xMin)
        self.SetValue('yMax', yMax)
        self.SetValue('yMin', yMin)
        
        # define the centre of gravity
        self.SetValue('COG',(centre[0], centre[1], 0.))
        self.SetValue('COGupdated', True)
        
        # draw sketch (the construction line is needed for revolved parts)
        sketchObj = self.GetSketchObj()
        sketchObj.ConstructionLine(point1=(0.0, -100.0), point2=(0.0, 100.0))
        sketchObj.Line(point1=(xMin, yMin), point2=(xMax, yMax))
