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
from robusta.part.AnalyticRigid import *
from robusta.config import *


class HptDiePart(AnalyticRigid):
    """

    """
    
    def __init__(self, name, modelName, dimensionality='3D'):
        """ Constructor
        """
        
        # call base class constructor
        AnalyticRigid.__init__(self, name, modelName, dimensionality)
       
       
    def CreateAnalyticSetsAndSurf(self, mainContactSetName, side):
        """ create a single surface on the die part and a geometry
            set containing the reference point accordingly
            
            only applies to analytic surface parts
        """
        part = self.GetPartObj()
        
        # get the points defining the two zones
        geometry = self.GetValue('geometry')
        symmetric = self.GetValue('symmetric')
        
        # define the sets
        if symmetric:
            self.ErrorHandling('Defining sets for non symmetric part not implemented yet.')

        # get key points (from 2D geometry)
        centre2D = geometry.GetValue('centre')
        centre = (centre2D[0], centre2D[1], 0.)
        self.SetValue('centre', centre)
        
        # create a contact surface and a geom set containing the ref point
        if side==1:
            part.Surface(side1Faces=part.faces, name=surfacePrefix+mainContactSetName)
            
        elif side==2:
            part.Surface(side2Faces=part.faces, name=surfacePrefix+mainContactSetName)
        
        else:
            self.ErrorHandling('wrong value for keyword side')
            
        if not 'RP' in part.features.keys():
            errMsg = 'No ref. point exists (yet) for part <{0}>.'.format(part.name)
            self.ErrorHandling(errMsg)
            
        refPointId = part.features['RP'].id
        part.Set(referencePoints=(part.referencePoints[refPointId],),
                 name=geometrySetPrefix+mainContactSetName)
        
    
    def CreateTwoZoneGeomSets(self, mainContactSetName, dieWallSetName):
        """ The idea here was to have two distinct surfaces on the die
            part so that the contact conditions could be specified
            independently.
            
            This is not possible with analytic parts, because they can
            have only one surface defined on each side
        """
        part = self.GetPartObj()
        
        # get the points defining the two zones
        geometry = self.GetValue('geometry')
        symmetric = self.GetValue('symmetric')
        
        # define the sets
        if symmetric:
            self.ErrorHandling('Defining sets for non symmetric part not implemented yet.')
        
        else:
            # get key points (from 2D geometry)
            filletEdgePoint2D = geometry.GetValue('filletEdgePoint')
            sampleEdgePoint2D =  geometry.GetValue('sampleEdgePoint')
            wallAngle = geometry.GetValue('wallAngle')
            centre2D = geometry.GetValue('centre')
            
            # convert these to 3D and store
            centre = (centre2D[0], centre2D[1], 0.)
            pointOnWall = (sampleEdgePoint2D[0] + (filletEdgePoint2D[0] - sampleEdgePoint2D[0])*0.5,
                           sampleEdgePoint2D[1] + (filletEdgePoint2D[1] - sampleEdgePoint2D[1])*0.5,
                           0.)
            self.SetValue('pointOnWall', pointOnWall)
            self.SetValue('centre', centre)
            
            # define surface under the sample
            contactUnderSampleSuffix = 'Main'
            self.CreateSetFromFaceAtPoint(point=centre, name=mainContactSetName,
                                          suffix=contactUnderSampleSuffix,
                                          createElSet=True, createNodeSet=True)
                                          
            # define the surface which is everywhere but under the same
            # i.e. the union set of surfaces of 'ALL' - (the above)
            faces = part.faces
            sampleSurfaceFaceIndex = faces.findAt((centre,))[0].index
            faceIndicesNotUnderSample = [face.index for face in faces \
                                         if not(face.index==sampleSurfaceFaceIndex)]
            
            # create a geom set for each of the faces found above
            for faceIndex in faceIndicesNotUnderSample:
                self.CreateSetsFromFace(face=part.faces[faceIndex], createElSet=True,
                                        suffix='wall_fid{0}'.format(faceIndex),
                                        createNodeSet=True)
                                        
            # create a union set from these
            setsToJoin = tuple([part.sets[key] for key in part.sets.keys() \
                                if part.sets[key].faces[0].index in faceIndicesNotUnderSample])
            part.SetByBoolean(name=dieWallSetName, sets=setsToJoin)
            
            
