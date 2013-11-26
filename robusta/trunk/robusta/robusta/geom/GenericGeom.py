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
from robusta.GenericRobusta import *
from robusta.config import *


class GenericGeom(GenericRobusta):
    """
    An abstract base class which will contain all template geometry data and methods.
    
    A geometry name and abaqus mdb.model object name need to be provided to the
    constructor. example:
    
    myNewGeom = robusta.GenericGeom(name='roll1',modelName='Model-1')
    
    
    Data can be loaded from text files by calling the LoadData method.
    """
    
    def __init__(self, name, modelName):
        """ Constructor
        """
        
        # call base class constructor
        GenericRobusta.__init__(self, modelName)

        # store some defaults (see config file for values)
        self.SetDefault('sketchPadDims', sketchPadDims)
        self.SetDefault('COG',[0.,0.,0.])
        self.SetValue('COGupdated', False)
        
        # store geometry name and create the Abaqus sketch object
        self.SetDefault('geometryName',name)        
        self.SetDefault('symmetric', True)
        self.SetDefault('withBoundingBox', True)
        
        modelObj = self.GetModelHandle()
        
        if name in modelObj.sketches.keys():
            errorMessage = 'The name {0} is already in the sketches database, and cant be used.'.format(name)
            self.ErrorHandling(errorMessage)
            
        modelObj.ConstrainedSketch(name=name, sheetSize=sketchPadDims)
     
        
    def GetSketchObj(self):
        """ return an alias for the sketch
        """
        modelObj = self.GetModelHandle()
        return modelObj.sketches[self.GetDefault('geometryName')]
        
        
    def MakeSketchFromFile(self, fileName, folder):
        """ Build a sketch using details in a parameter file
        """
        self.LoadParameters(fileName, folder)
        
        
    def GetRequirements(self):
        """ Return a list of parameter requirements
        """
        
        return self.GetValue('parametersRequired')
        
        
    def SetExtrusionLength(self, length, part=None):
        """ set the extrusion length, and update the centre of gravity value
        """
        length = abs(length)        
        self.SetValue('extrusionLength', length)
        
        # get the current centre of gravity value
        COG = self.GetValue('COG')
        
        # check if the COG should be updated now (because of a new sketch)
        if not self.GetValue('COGupdated'):
            try:
                xMin = self.GetValue('xMin')
                yMin = self.GetValue('yMin')
                xMax = self.GetValue('xMax')
                yMax = self.GetValue('yMax')
                
            # a key error is raised if the geometry was not correctly defined
            except KeyError:
                errMsg = 'Cant set extrusion properties before geometry x and y properties are defined.'
                self.ErrorHanding(errMsg)
                
            else:
                COG[0] = (xMax-xMin)/2.
                COG[1] = (yMax-yMin)/2.
        
        # update the COG
        if part is None:
            partType = 'deformable'
        else:
            partType = part.GetValue('partType')
            
        if partType=='rigid' or partType=='analytic':
            # if the part is a rigid (/analytic) surface, the part is extruded in
            # in the z direction, with the extents in the z direction centred at
            # zero, regardless of how the extrusion is done. hence, the COG needs
            # to be handled differently than for deformable parts
            newCOG = [COG[0], COG[1], 0.]
        
        else:
            newCOG = [COG[0], COG[1], (COG[2] + length/2)]
            
        self.SetValue('COG', newCOG)
        

