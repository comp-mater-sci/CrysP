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

# import native modules
import math


class HptDie(GenericGeom):
    """
        A simple line in 2D
    """
    
    def __init__(self, name, modelName):
        """ Constructor
        """
        
        # call base class constructor
        GenericGeom.__init__(self, name, modelName)
        
        # define a list of required parameters
        parametersRequired = [['sampleWidth', float, 1], ['filletRadius', float, 1],
                              ['wallAngle', float, 1], ['gap', float, 1]]
        self.SetDefault('parametersRequired',parametersRequired)
        
        # a list of default values
        self.SetValue('symmetric', False)
        self.SetDefault('centre', (0.0,0.0))
        self.SetDefault('closed', False)
        self.SetDefault('solid', False)
        self.SetDefault('thickness', 50.)
        
        
    def MakeSketch(self, sampleWidth=None, filletRadius=None, wallAngle=None,
                   gap=None, totalWidth=None, symmetric=None, closed=None,
                   centre=None, thickness=None, oneFilletOnly=False):
        """
        """
        # if no values provided check for defaults
        parameterValueList = [sampleWidth, filletRadius, wallAngle, gap,
                              totalWidth, symmetric, closed, centre, oneFilletOnly]
        parameterNameList = ['sampleWidth', 'filletRadius', 'wallAngle', 'gap',
                              'totalWidth', 'symmetric', 'closed', 'centre',
                              'oneFilletOnly']
        updatedParmList = self.CheckForNullParameters(parameterValueList, parameterNameList)
        (sampleWidth, filletRadius, wallAngle, gap, totalWidth, symmetric,
                      closed, centre, oneFilletOnly) = updatedParmList
                                         
        self.SetValue('symmetric', symmetric)
        if self.GetValue('solid') and thickness is None:
            thickness = gap + self.GetValue('thickness')
        
        # check if the parameter values make sense
        if (sampleWidth + gap*math.cos(math.radians(wallAngle))) > totalWidth:
            self.ErrorHandling('The provided sketch parameters conflict')
            
        # draw sketch, taking into account symmetry and whether it should be a
        # 'closed' object  
        if symmetric:
            self.DrawSymmetricOutline(updatedParmList)
            
        else:
            self.DrawAsymmetricOutline(updatedParmList)
            
        if closed:
            self.MakeClosedSketch(thick, width)
        
        
        # set remaining bounding box reference points    
        self.SetValue('intPoint', centre)
        
    
    def DrawSymmetricOutline(self, parameterList):
        """ draw the symmetric version of the hpt die
        """
        self.ErrorHandling('DrawSymmetricOutline not implemented yet')
        
        
    def MakeClosedSketch(self, parameterList):
        """ draw a version of the die which can be meshed
        """
        self.ErrorHandling('MakeClosedSketch not implemented yet.')
        
        
    def DrawAsymmetricOutline(self, parameterList):
        """ draw the asymmetric version of the hpt die
        """
        # get the sketch parameters
        (sampleWidth, filletRadius, wallAngle, gap, totalWidth, symmetric,
                      closed, centre, oneFilletOnly) = parameterList
                
        # define key points
        sampleYPos = centre[1]
        sampleEdgePoint = [centre[0]+sampleWidth, sampleYPos]
        filletEdgePoint = [sampleEdgePoint[0] + math.cos(math.radians(wallAngle)),
                           sampleYPos + gap]
        dieTopEndPoint = [totalWidth, sampleYPos + gap]
        
        offset = (findAtOffsetPercent/100.)*sampleWidth
        filletFindAtPoint1 = (sampleEdgePoint[0]-offset, sampleEdgePoint[1])
        filletFindAtPoint2 = (sampleEdgePoint[0]+0.5*(filletEdgePoint[0]-sampleEdgePoint[0]),
                              (sampleEdgePoint[1]+0.5*(filletEdgePoint[1]-sampleEdgePoint[1])))
        filletFindAtPoint3 = (filletEdgePoint[0]+offset, filletEdgePoint[1])
        
        # define some bounding box points
        self.SetValue('xMin', centre[0])
        self.SetValue('xMax', dieTopEndPoint[0])
        self.SetValue('yMax', dieTopEndPoint[1])
        self.SetValue('yMin', sampleEdgePoint[1])
        self.SetValue('intPoint', centre)
        self.SetValue('extPoint', filletEdgePoint)
        
        # sketch the geometry (construction line required for revolved parts)
        sketchObj = self.GetSketchObj()
        sketchGeom = sketchObj.geometry
        sketchObj.ConstructionLine(point1=(0.0, -100.0), point2=(0.0, 100.0))
                
        sketchObj.Line(point1=centre, point2=sampleEdgePoint)
        sketchObj.Line(point1=sampleEdgePoint, point2=filletEdgePoint)
        sketchObj.Line(point1=filletEdgePoint, point2=dieTopEndPoint)
        
        if not oneFilletOnly:
            sketchObj.FilletByRadius(radius=filletRadius, nearPoint1=(filletFindAtPoint1),
                      curve1=sketchGeom.findAt(filletFindAtPoint1),
                      nearPoint2=(filletFindAtPoint2),
                      curve2=sketchGeom.findAt(filletFindAtPoint2))
                  
        sketchObj.FilletByRadius(radius=filletRadius, nearPoint1=(filletFindAtPoint2),
                  curve1=sketchGeom.findAt(filletFindAtPoint2),
                  nearPoint2=(filletFindAtPoint3),
                  curve2=sketchGeom.findAt(filletFindAtPoint3))
                  
        # store the key points (useful for surface definitions)
        self.SetValue('sampleEdgePoint', sampleEdgePoint)
        self.SetValue('filletEdgePoint', filletEdgePoint)
        self.SetValue('dieTopEndPoint', dieTopEndPoint)
        self.SetValue('wallAngle', wallAngle)
        
