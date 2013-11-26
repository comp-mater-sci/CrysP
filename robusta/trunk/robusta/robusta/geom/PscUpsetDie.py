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

class PscUpsetDie(GenericGeom):

    def __init__(self, name, modelName):
        """ Constructor
        """
        
        # call base class constructor
        GenericGeom.__init__(self, name, modelName)
        
        # define a list of required parameters
        parametersRequired = [['width', float, 1], ['height', float, 1]]
        self.SetDefault('parametersRequired',parametersRequired)
        
        # a list of default values
        self.SetValue('symmetric', False)
        self.SetDefault('radius', 1.0)
        self.SetDefault('centre', (0.0,0.0))
        self.SetDefault('closed', False)
        self.SetDefault('dieScaleX', 2.)
        self.SetDefault('dieScaleY', 0.5)
        self.SetDefault('solid', False)
        
        
    def MakeSketch(self, width=None, height=None, radius=None, centre=None, symmetric=None, closed=None, thick=None):
        """ Make a sketch from parameter values in the global config file
        """
        
        # if no values provided check for defaults
        if self.GetValue('solid'):
            thick=0.0
        parameterValueList = [width, height, centre, radius, symmetric, closed, thick]
        parameterNameList = ['width', 'height', 'centre', 'radius', 'symmetric', 'closed', 'thick']
        updatedParmList = self.CheckForNullParameters(parameterValueList, parameterNameList)
        
        (width, height, centre, radius, symmetric, closed, thick) = updatedParmList
        self.SetValue('symmetric', symmetric)

        
        # check if the parameter values make sense
        if (radius > height) or (radius > width):
            self.ErrorHandling('The value for radius is too large.')
                       
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
        """ draw the symmetric version of the plane strain upsetting die
        """
        
        sketchObj = self.GetSketchObj()
        sketchGeom = sketchObj.geometry
        
        # get parameter values
        (width, height, centre, radius, symmetric, closed, thick) = parameterList
        dieBodyWidth = width * self.GetValue('dieScaleX')
        dieBodyHeight = height * self.GetValue('dieScaleY')
        platenWidth = width
        platenHeight = height
        
        
        # define some points on the platen
        bottomLeftCoord = (centre[0]-(platenWidth/2), centre[1]-(platenHeight/2))
        topRightCoord = (centre[0]+(platenWidth/2), centre[1]+(platenHeight/2))
        
        midPointOnLeftSide = (bottomLeftCoord[0], topRightCoord[1]/2)
        midPointOnRightSide = (topRightCoord[0], topRightCoord[1]/2)
        midPointOnBottom = (topRightCoord[0]/2, bottomLeftCoord[1])
        midPointOnTop = (topRightCoord[0]/2, topRightCoord[1])
        
        # define some bounding box points
        self.SetValue('yMin', bottomLeftCoord[1])
        self.SetValue('extPoint', midPointOnBottom)
        
        # draw the platen
        sketchObj.rectangle(point1=bottomLeftCoord, point2=topRightCoord)
        sketchObj.FilletByRadius(radius=radius,
                                 curve1=sketchGeom.findAt(midPointOnLeftSide),
                                 nearPoint1=(midPointOnLeftSide),
                                 curve2=sketchGeom.findAt(midPointOnBottom), 
                                 nearPoint2=(midPointOnBottom))
        sketchObj.FilletByRadius(radius=radius,
                                 curve1=sketchGeom.findAt(midPointOnRightSide),
                                 nearPoint1=(midPointOnRightSide),
                                 curve2=sketchGeom.findAt(midPointOnBottom), 
                                 nearPoint2=(midPointOnBottom))
        sketchObj.autoTrimCurve(curve1=sketchGeom.findAt(midPointOnTop),
                                point1=midPointOnTop)
                                
        # define some points on the die body (the geometry of the die
        # body is dictated by the platen geometry)         
        bottomLeftCoord = (centre[0]-(dieBodyWidth/2), centre[1]+(platenHeight/2))
        topRightCoord = (centre[0]+(dieBodyWidth/2), centre[1]+(platenHeight/2)+dieBodyHeight)
        previousPointOnTop = midPointOnTop
        midPointOnTop = (topRightCoord[0]/2, topRightCoord[1])
        
        # draw the die body
        sketchObj.rectangle(point1=bottomLeftCoord, point2=topRightCoord)
        sketchObj.autoTrimCurve(curve1=sketchGeom.findAt(midPointOnTop),
                                point1=midPointOnTop)
        sketchObj.autoTrimCurve(curve1=sketchGeom.findAt(previousPointOnTop),
                                point1=previousPointOnTop)  
                                
        # remaining information on extents of the shape. Note that the extrusion
        # direction in abaqus is the z direction by default, so that only
        # the x and y extents can be defined here
        self.SetValue('xMin', bottomLeftCoord[0])
        self.SetValue('xMax', topRightCoord[0])
        self.SetValue('yMax', topRightCoord[1])
        
        
    def DrawAsymmetricOutline(self, parameterList):
        """ draw the asymmetric version of the plane strain upsetting die
        """
        
        sketchObj = self.GetSketchObj()
        sketchGeom = sketchObj.geometry
        
        # get parameter values
        (width, height, centre, radius, symmetric, closed, thick) = parameterList
        dieBodyWidth = width * self.GetValue('dieScaleX')
        dieBodyHeight = height * self.GetValue('dieScaleY')
        platenWidth = width
        platenHeight = height
                
        
        # define some points on the platen
        bottomLeftCoord = (centre[0], centre[1]-(platenHeight/2))
        topRightCoord = (centre[0]+(platenWidth/2), centre[1]+(platenHeight/2))
        
        midPointOnRightSide = (topRightCoord[0], topRightCoord[1]/2)
        midPointOnLeftSide = (bottomLeftCoord[0], topRightCoord[1]/2)
        midPointOnBottom = (topRightCoord[0]/2, bottomLeftCoord[1])
        midPointOnTop = (topRightCoord[0]/2, topRightCoord[1])
                
        # define some bounding box points
        self.SetValue('yMin', bottomLeftCoord[1])
        self.SetValue('extPoint', midPointOnBottom)
        
        # draw the platen
        sketchObj.rectangle(point1=bottomLeftCoord, point2=topRightCoord)
        sketchObj.FilletByRadius(radius=radius,
                                 curve1=sketchGeom.findAt(midPointOnRightSide),
                                 nearPoint1=midPointOnRightSide,
                                 curve2=sketchGeom.findAt(midPointOnBottom), 
                                 nearPoint2=midPointOnBottom)
        sketchObj.autoTrimCurve(curve1=sketchGeom.findAt(midPointOnLeftSide),
                                point1=midPointOnLeftSide)
        sketchObj.autoTrimCurve(curve1=sketchGeom.findAt(midPointOnTop),
                                point1=midPointOnTop)
                              
        # define some points on the die body (the geometry of the die
        # body is dictated by the platen geometry)         
        bottomLeftCoord = (centre[0], centre[1]+(platenHeight/2))
        topRightCoord = (centre[0]+(dieBodyWidth/2), centre[1]+(platenHeight/2)+dieBodyHeight)
        midPointOnLeftSide = (bottomLeftCoord[0], topRightCoord[1]/2)
        midPointOnBottom = (topRightCoord[0]/2, bottomLeftCoord[1])
        previousPointOnTop = midPointOnTop
        midPointOnTop = (topRightCoord[0]/2, topRightCoord[1])
        
        # draw the die body
        sketchObj.rectangle(point1=bottomLeftCoord, point2=topRightCoord)
        sketchObj.autoTrimCurve(curve1=sketchGeom.findAt(midPointOnTop),
                                point1=midPointOnTop)
        sketchObj.autoTrimCurve(curve1=sketchGeom.findAt(midPointOnLeftSide),
                                point1=midPointOnLeftSide)
        sketchObj.autoTrimCurve(curve1=sketchGeom.findAt(previousPointOnTop),
                                point1=previousPointOnTop)
                                
        # remaining information on extents of the shape. Note that the extrusion
        # direction in abaqus is the z direction by default, so that only
        # the x and y extents can be defined here
        self.SetValue('xMin', bottomLeftCoord[0])
        self.SetValue('xMax', topRightCoord[0])
        self.SetValue('yMax', topRightCoord[1])
             
        
    def MakeClosedSketch(self, thick, width):
        """ add an offset curve so that the sketch has 'thickness'
        """
        
        sketchObj = self.GetSketchObj()
        sketchGeom = sketchObj.geometry
        
        # get stored values
        solid = self.GetValue('solid')
        symmetric = self.GetValue('symmetric')
        xMax = self.GetValue('xMax')
        xMin = self.GetValue('xMin')
        yMax = self.GetValue('yMax')
        yMin = self.GetValue('yMin')
        
        # build list of geometry parts for the sketch
        geometryList = [item[1] for item in sketchGeom.items()]
        
        # check if the given thickness value makes sense
        if thick > width/2:
            print 'Thickness value <{0}> too high. Making solid part.'.format(thick)
            solid = True
            
        if solid:
            # close off the shape to make a 'simple solid part'
            if symmetric:
                sketchObj.Line(point1=(xMin,yMax), point2=(xMax, yMax))
            else:
                sketchObj.Line(point1=(xMin,yMin), point2=(xMin, yMax))
                sketchObj.Line(point1=(xMin,yMax), point2=(xMax, yMax))
            
        else:
            # if the part is symmetric, the top two gaps need to be closed
            if symmetric:
                sketchObj.offset(distance=thick, objectList=geometryList, side=LEFT)
                sketchObj.Line(point1=(xMax,yMax), point2=(xMax-thick, yMax))
                sketchObj.Line(point1=(xMin,yMax), point2=(xMin+thick, yMax))
            
            # else the side (die mid point) and a single gap at the top need it 
            else:
                sketchObj.offset(distance=thick, objectList=geometryList, side=RIGHT)
                sketchObj.Line(point1=(xMin,yMin), point2=(xMin, yMin+thick))
                sketchObj.Line(point1=(xMax,yMax), point2=(xMax-thick, yMax))
            
            
        
