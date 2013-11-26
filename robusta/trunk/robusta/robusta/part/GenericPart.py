# Prevent the legacy class type being used
__metaclass__ = type

# import native Python modules
from warnings import warn
import math

# import abaqus modules
try:
    from abaqusConstants import *
    from abaqus import *
    from regionToolset import Region
    import mesh
    import step
except ImportError:
    print ('This module requires the abaqus modules to be available.')
    raise

# import robusta modules
from robusta.GenericRobusta import *
from robusta.config import *
from robusta.geom import alignments


class GenericPart(GenericRobusta):
    """
    An abstract base class which will contain all template part data and methods.
    
    A part name and abaqus mdb.model object name need to be provided to the
    constructor. example:
    
    myNewGeom = robusta.GenericPart(name='roll1',modelName='Model-1')
    
    
    Data can be loaded from text files by calling the LoadData method.
    """
    
    def __init__(self, name, modelName):
        """ Constructor
        """
        
        # call base class constructor
        GenericRobusta.__init__(self, modelName)

        # store geometry name and create the Abaqus part object
        self.SetDefault('partName',name)
        self.SetDefault('meshAlgorithm',defaultMeshAlgorithm)
        self.SetDefault('defaultTechnique', defaultTechnique)
        self.SetValue('partCreated', False)
                
        modelObj = self.GetModelHandle()
        
        if name in modelObj.parts.keys():
            errorMessage = 'The name {0} is already in the parts database, and cant be used.'.format(name)
            self.ErrorHandling(errorMessage)
            
        # set flags
        self.SetValue('isMeshed', False)
        self.SetValue('isSeeded', False)
           
    
    def SetInertia(self, matrixDiag, name=''):
        """ add an rotary interia matrix for the part, with 
        """
        # get a name if no name is specified
        part = self.GetPartObj()
        if name=='':
            name = '{0}_defI'.format(self.GetValue('partName'))
        
        # get the reference point for the part.
        refPointId = part.features['RP'].id
        regionRef = Region(referencePoints=(part.referencePoints[refPointId],))
        
        # add the inertia details
        part.engineeringFeatures.PointMassInertia(name='Inertia-1', region=regionRef,
             mass=1.0, i11=matrixDiag[0], i22=matrixDiag[1], i33=matrixDiag[2], 
             alpha=0.0, composite=0.0)
             
       
    def GetPartObj(self):
        """ return an alias for the part
        """
        modelObj = self.GetModelHandle()
        part = modelObj.parts[self.GetDefault('partName')]
        
        # this is a convenient point to set the display resolution in CAE
        part.setValues(geometryRefinement=defaultDisplayResolution)
        
        return part

        
    def ExtrudeFromRobustaObj(self, geometry, analytic=False):
        """ Create a part using the passed geometry object by extrusion
        """
        # Extrude the part
        self.SetValue('geometry', geometry)
        self.ExtrudeFromGeomObj(geometry, analytic=analytic)
        
        # add a reference point
        part = self.GetPartObj()
        part.ReferencePoint(point=geometry.GetValue('COG'))
        
        
    def RevolveFromRobustaObj(self, geometry, analytic=False):
        """ Create a part using the passed geometry object by revolution

        """
        # generate the part by revolution
        self.SetValue('geometry', geometry)
        self.RevolveFromGeomObj(geometry, analytic=analytic)
        
        # add a reference point
        part = self.GetPartObj()
        part.ReferencePoint(point=geometry.GetValue('COG'))
        

    def Make2DShell(self, geometry, withBoundingBox=True, analytic=False):
        """ 
        """
        # get geometry values and part
        self.SetValue('geometry', geometry)
        sketch = geometry.GetSketchObj()
        part = self.GetPartObj()
        
        # make the part from the sketch
        if analytic:
            part.AnalyticRigidSurf2DPlanar(sketch=sketch)
        
        else:
            part.BaseShell(sketch=sketch)
        
        self.SetValue('partCreated', True)
        self.SetValue('dimensionality', '2D')
            
        # store the dimensions of the box surrounding the part (useful for
        # creating datums and position instances in the assembly)
        self.SetValue('xMax', geometry.GetValue('xMax'))
        self.SetValue('xMin', geometry.GetValue('xMin'))
        self.SetValue('yMax', geometry.GetValue('yMax'))
        self.SetValue('yMin', geometry.GetValue('yMin'))
        
        # make the bounding box
        if withBoundingBox: self.MakeBoundingBox()
        
        # update the external and internal point info (available in the geometry obj)
        extPoint = geometry.GetValue('extPoint')
        intPoint = geometry.GetValue('intPoint')
        
        self.SetValue('extPoint', (extPoint[0], extPoint[1]))
        self.SetValue('intPoint', (intPoint[0], intPoint[1]))
        
        # store symmetry info
        self.SetValue('symmetric', geometry.GetValue('symmetric'))
        
        # add reference point
        COG = geometry.GetValue('COG')        
        part.ReferencePoint(point=(COG[0],COG[1],0.))
            
    
    def RevolveFromGeomObj(self, geometry, analytic=False):
        """ create a part from the given geometry object by revolving
        
            the revolution angle can only be specified in the case of a discrete
            rigid part
        
            if a discrete rigid part is required, the 'extrusionLength' parameter
            needs to be set in the geometry object (it is used to define the
            centre of gravity/centroid)
        """
        
        # get geometry values
        self.SetValue('geometry', geometry)
        sketch = geometry.GetSketchObj()
        COG = geometry.GetValue('COG')
        extPoint = geometry.GetValue('extPoint')
        intPoint = geometry.GetValue('intPoint')

        # Generate the part. Bounding boxes are different for the analytic
        # rigid and discrete rigid cases
        part = self.GetPartObj()
        if analytic:
            part.AnalyticRigidSurfRevolve(sketch=sketch)
            
            self.SetValue('zMax', COG[2])
            self.SetValue('zMin', COG[2])
            self.SetValue('extPoint', (extPoint[0], extPoint[1], COG[2]))
            self.SetValue('intPoint', (intPoint[0], intPoint[1], COG[2]))
            
        else:
            part.BaseSolidExtrude(sketch=sketch, angle=angle,
                                  flipRevolveDirection=OFF)
            
            # the following parameters are only relevant for discrete parts                      
            extrusionLength = geometry.GetValue('extrusionLength')
            angle = geometry.GetValue('angle')

            self.SetValue('zMax', COG[2] + extrusionLength*0.5)
            self.SetValue('zMin', COG[2] - extrusionLength*0.5)
            self.SetValue('extPoint', (extPoint[0], extPoint[1], extrusionLength/2))
            self.SetValue('intPoint', (intPoint[0], intPoint[1], extrusionLength/2))
        
        # flag part existence    
        self.SetValue('partCreated', True)
        self.SetValue('dimensionality', '3D')

        # set up the bounding box
        self.StoreBoundingBoxDetails(geometry=geometry)        

    
    def ExtrudeFromGeomObj(self, geometry, analytic=False):
        """ Extrude a part from the given geometry object
        
            Note that the built in part.cells.getBoundingBox() or
            part.faces.getBoundingBox() methods could be used to get values
            for the extents of the part, but the idea here is to leave some
            flexibility for positioning the part in the assembly. Eg. you
            might want to create a smaller bounding box 'inside' the part which
            helps alignment of some internal surface in the assembly. The size
            is in every case determined directly by the 2D geometry object and 
            the method of forming the part (i.e. extrusion in this case)
        """
                
        # get geometry values
        self.SetValue('geometry', geometry)
        sketch = geometry.GetSketchObj()
        extrusionLength = geometry.GetValue('extrusionLength')
                    
        # Extrude part
        part = self.GetPartObj()
        if analytic:
            part.AnalyticRigidSurfExtrude(sketch=sketch, depth=extrusionLength)
        else:
            part.BaseSolidExtrude(sketch=sketch, depth=extrusionLength)
            
        self.SetValue('partCreated', True)
        self.SetValue('dimensionality', '3D')
        
        # set up the bounding box
        COG = geometry.GetValue('COG')
        self.SetValue('zMax', COG[2] + extrusionLength*0.5)
        self.SetValue('zMin', COG[2] - extrusionLength*0.5)
        self.StoreBoundingBoxDetails(geometry=geometry)
        
        # update the external and internal point info (available in the geometry obj)
        extPoint = geometry.GetValue('extPoint')
        intPoint = geometry.GetValue('intPoint')
        
        self.SetValue('extPoint', (extPoint[0], extPoint[1], extrusionLength/2))
        self.SetValue('intPoint', (intPoint[0], intPoint[1], extrusionLength/2))
        
        
    def StoreBoundingBoxDetails(self, geometry):
        """ Create the bounding box. There should be information stored in the
            2D geometry defining the x and y extents of the box. Z extent is
            defined by the extrusion length.
        """
        
        # store the dimensions of the box surrounding the part (useful for
        # creating datums and position instances in the assembly)
        self.SetValue('xMax', geometry.GetValue('xMax'))
        self.SetValue('xMin', geometry.GetValue('xMin'))
        self.SetValue('yMax', geometry.GetValue('yMax'))
        self.SetValue('yMin', geometry.GetValue('yMin'))
        
        self.SetValue('symmetric', geometry.GetValue('symmetric'))
        
        # make the bounding box
        if geometry.GetValue('withBoundingBox'): self.MakeBoundingBox()
  
    
    def GetLength(self, direction='x'):
        """ return the part 'length' in the x direction (as defined in the part space)
        """
        if self.GetValue('partCreated'):
            try:
                if direction=='x' or direction=='X':
                    return abs(self.GetValue('xMax') - self.GetValue('xMin'))
                elif direction=='y' or direction=='Y':
                    return abs(self.GetValue('yMax') - self.GetValue('yMin'))
                elif direction=='z' or direction=='Z':
                    return self.GetValue('zMax') - self.GetValue('zMin')
                else:
                    self.ErrorHandling('Unknown direction {0}.'.format(direction))
            
            # a key error is raised if the extents of the part have not been properly defined
            except KeyError:
                errorMsg = 'Cant get length in part {0} direction as the bounding box is not defined yet.'.format(direction)
                self.ErrorHandling(errorMsg)
        else:
            errorMsg = 'Part has not been created yet.'
            self.ErrorHandling(errorMsg)
        
    
    def CreateInternalSets(self):
        """ create internal element and node sets
        """
        part = self.GetPartObj()
        
        self.OverwriteSet('allElems')
        part.Set(elements=part.elements, name='allElems')
        
        self.OverwriteSet('allNds')
        part.Set(nodes=part.nodes, name='allNodes')
        
        allFacesName = geometrySetPrefix+allFacesSetName
        self.OverwriteSet(allFacesName)
        part.Set(faces=part.faces, name=allFacesName)
    
    
    def CreateSurfacesFromSet(self, setName, surfName, overWrite=True, side=1):
        """ create a surface (eg for specifying contact interaction) using the faces
            returned by the given set name
            
            setting "overWrite"=False prevents deleting any existing set in for the
            part with the same name
            
            setting "side" to 1 or 2 specifies which side is the 'external' side
        """
        # check if the named set exists for this part
        part = self.GetPartObj()
        
        if not setName in part.sets.keys():
                errorMessage = 'Set with name <{0}> does not exist for this part.'.format(setName)
                self.ErrorHandling(errorMessage)
                
        # check if the surface already exists, overwrite accordingly
        if surfName in part.surfaces.keys():
            if overWrite:
                del part.surfaces[surfName]
            else:
                errorMessage = 'Surface with name <{0}> already exists for this part.'.format(surfName)
                self.ErrorHandling(errorMessage)
        
        # create the new surface
        is3D = self.GetValue('dimensionality') == '3D'
        
        if side==1:
            if is3D:
                part.Surface(name=surfName, side1Faces=part.sets[setName].faces)
            else:
                part.Surface(name=surfName, side1Edges=part.sets[setName].edges)
                
        elif side==2:
            if is3D:
                part.Surface(name=surfName, side2Faces=part.sets[setName].faces)
            else:
                part.Surface(name=surfName, side2Edges=part.sets[setName].edges)
        else:
            errorMessage = '<side> should have the value 1 or 2, not <{0}>'.format(side)
            self.ErrorHandling(errorMessage)
    
    
    def CreateExternalSets(self):
        """ Create sets of geometry, elements and nodes on the external surface
            of the part
        """
        part = self.GetPartObj()
        setPossible = True
        
        # first categorise the part faces or edges
        is3D = self.GetValue('dimensionality') == '3D'
        
        if is3D:
            self.CategoriseExternalFaces()
            categories = self.GetValue('faceCategories')
            polarities = self.GetValue('facePolarities')
            
            entities = part.faces
            noEntities = len(entities) 
            
        else:
            self.CategoriseExternalEdges()
            categories = self.GetValue('edgeCategories')
            polarities = self.GetValue('edgePolarities')
            
            entities = part.edges
            noEntities = len(entities)
        
        # get a name for the set based on the category       
        for index in range(noEntities):
            entity = entities[index]
            axis = categories[index]
            sense = polarities[index]
            
            # create sets on positive X face
            if categories[index]==0 and polarities[index]==1:
                suffix=posXSurfName
                
            # create sets on negative X face
            elif categories[index]==0 and polarities[index]==0:
                suffix=negXSurfName
                
            # create sets on positive Y face
            elif categories[index]==1 and polarities[index]==1:
                suffix=posYSurfName
                
            # create sets on negative Y face
            elif categories[index]==1 and polarities[index]==0:
                suffix=negYSurfName
                
            # create sets on positive Z face
            elif categories[index]==2 and polarities[index]==1 and is3D:
                suffix=posZSurfName
                
            # create sets on negative Z face
            elif categories[index]==2 and polarities[index]==0 and is3D:
                suffix=negZSurfName
                
            else:
                setPossible = False
                if verbose:
                    print 'At least one face has not been categorised.'
            
            # make the set
            if setPossible:
                
                if is3D:  
                    self.CreateSetsFromFace(face=entity, suffix=suffix)
                else:
                    self.CreateSetsFromEdge(edge=entity, suffix=suffix)
    
    
    def CreateSetsFromEdge(self, edge, suffix):
        """ create a new set with the given name based on the given list of edges
        
            if a set already exists by that name it will be overwritten
        """
        # check if name already exists
        part = self.GetPartObj()
        name = geometrySetPrefix+suffix
        self.OverwriteSet(name)
            
        # create the geometry set
        part.Set(edges=part.edges.findAt(edge.pointOn), name=name)
        
        # check if the element set already exists
        name = '{0}{1}-eid{2}'.format(elementSetPrefix, suffix, edge.index)
        self.OverwriteSet(name)

        # create the element set
        part.Set(elements=edge.getElements(), name=name)
        
        # check if the node set already exists
        name = '{0}{1}-eid{2}'.format(nodeSetPrefix, suffix, edge.index)
        self.OverwriteSet(name)
        
        # create the node set
        part.Set(nodes=edge.getNodes(), name=name)
    
    
    def CreateSetsFromFace(self, face, suffix, createElSet=True, createNodeSet=True):
        """ create a new set with the given name based on the given face
        
            if a set already exists by that name it will be overwritten
        """
        # check if name already exists
        part = self.GetPartObj()
        name = geometrySetPrefix+suffix
        self.OverwriteSet(name)
            
        # create the geometry set
        faceRefound = part.faces.findAt(face.pointOn)
        if faceRefound is None:
            part.Set(faces=face, name=name)            
        else:
            part.Set(faces=faceRefound, name=name)
        
        # create the node and element sets
        if createElSet or createNodeSet:
            self.CreateElNodeSetsFromGeomSet(faces=[face], suffix=suffix,
                                             createNodeSet=createNodeSet,
                                             createElSet=createElSet)
        
        
    def CreateSetFromFaceAtPoint(self, point, name, suffix='', createElSet=True,
                                 createNodeSet=True):
        """ create a set using the part face found at the given point
        """
        # check if name already exists
        part = self.GetPartObj()
        self.OverwriteSet(name)
        
        # get the face object nearest the given point
        face = part.faces.findAt((point,))
        if face is None:
            errMsg = 'a geometry face could not be found at the point <{0}>.'.format(point)
            self.ErrorHandling(errMsg)
        
        # create the geometry set
        self.OverwriteSet(name)
        part.Set(faces=face, name=name)
        
        # create the node and element sets
        if createNodeSet or createElSet:
            self.CreateElNodeSetsFromGeomSet(faces=face, createNodeSet=createNodeSet,
                                             suffix=suffix, createElSet=createElSet)
        
        
    def CreateElNodeSetsFromGeomSet(self, faces, suffix, createElSet=True,
                                    createNodeSet=True):
        """ create element and node sets corresponding to the named geometry set
        """
        part = self.GetPartObj()
        
        if createElSet or createNodeSet:
        
            for face in faces:
            
                if createElSet:
                    # check if the element set already exists
                    name = '{0}{1}-fid{2}'.format(elementSetPrefix, suffix, face.index)
                    self.OverwriteSet(name)
                    

                    # create the element set
                    part.Set(elements=face.getElements(), name=name)
                
                if createNodeSet:
                    # check if the node set already exists
                    name = '{0}{1}-fid{2}'.format(nodeSetPrefix, suffix, face.index)
                    self.OverwriteSet(name)
                    
                    # create the node set
                    part.Set(nodes=face.getNodes(), name=name)
                
        else:
            warnings.warn('No node or element set to create for {0}'.format(name))

        
    
    def OverwriteSet(self, name):
        """ check if the named set exists for the part, if so delete it
        """
        
        part = self.GetPartObj()
        if name in part.sets.keys():
            del part.sets[name]
            warn ('Overwriting an existing set by the name <{0}>.'.format(name))
            
            
    def CategoriseExternalEdges(self):
        """ determine which are the external edges of the given part. 
        
            See the doc string for CategoriseExternalFaces for more details,
            this method uses the same approach

        """
        
        # get cell list
        part = self.GetPartObj()
        cells = part.cells
        is3D = self.GetValue('dimensionality') == '3D'
        
        # check if it has been completely meshed
        fullyMeshed = True
        for cell in cells:
            if type(cell.getElements())==NoneType:
                fullyMeshed = False
                print 'Cell <{0}> has not been meshed yet.'.format(cell.index)
        
        if not fullyMeshed:
            self.ErrorHandling('Part must be meshed before sets can be generated.')
        
        
        # loop through each edge in the part and apply the method described above
        noEdge = len(part.edges)
        
        # "catergories" stores which of (X,Y,Z) or (X,Y) the faces normals aligns with
        # "polarity" stores whether the face's centroid is nearer the positive
        # negative limit of the part's geometry (along the direction in "categories")
        categories = []
        polarities = []
        
        box = self.GetBoundingBox()
        if is3D:
            limits = [(box[0], box[1]),(box[2], box[3]), (box[4], box[5])]
        else:
            limits = [(box[0], box[1]),(box[2], box[3])]

        # go through the list of edges
        for edge in part.edges:

            # get the number of edges for the element type used in the part
            noEdgesOnElement = len(part.elements[0].getElemEdges())
            
            # check if any of the elements for this face are external
            for element in edge.getElements():
                
                if len(element.getAdjacentElements()) < noEdgesOnElement:
                    IsExternal = True
                    break
                    
            # determine which category the face belongs to (X,Y or Z)
            vertex1 = np.array(part.vertices[edge.getVertices()[0]].pointOn[0], dtype=defaultPrecision) 
            vertex2 = np.array(part.vertices[edge.getVertices()[1]].pointOn[0], dtype=defaultPrecision)
            midpoint = (vertex2 + vertex1)/2
            edgeVector = vertex2 - vertex1
            normal = np.array([edgeVector[1], edgeVector[0], edgeVector[2]])

            category = alignments.CategoriseVectorXYZ(normal)
            categories.append(category)
            
            # determine the polarity
            polarity = alignments.DetermineClosestPrincipalPlane(midpoint,
                                  limits[category], category)
            polarities.append(polarity)
            
        
        # store the results
        self.SetValue('edgeCategories', categories)
        self.SetValue('edgePolarities', polarities)
        
    
    def CategoriseExternalFaces(self):
        """ determine which are the external faces of the given part. 
        
            This method is generic and should work for more or less
            any geometry. If a part has cavities or channels the free surfaces
            will also be included as 'external' surfaces.
            
            The part needs to be meshed first. However the resulting geometry
            sets will persist as expected after subsequent remeshing, as the
            mesh is just used to identify which are external surfaces (as
            opposed to the internal faces between cells for example)
            
            Also it is assumed that the part is entirely meshed with the same
            basic element type (hex, tetrahedral, etc). This is usually necessary
            anyway.
            
            It works as follows:
            
                - for each face in the part get a list of elements
                - for each of these elements (thus obtained) check if any have
                  less than 'n' neighbouring elements, where n is the total number
                  of faces for that kind of element (eg n=8 for the C3D8R). If
                  such an element is found, the face can be considered external.
                  
                - once a set of external faces has been collected in this way,
                  they are divided into three sets, depending on the closest
                  of the three prinicipal axes their normals are. If a normal
                  happens to be equidistant (in angle) then that face is assigned
                  to the categories X,Y,Z in that order of priority.
                  
                - once all the external faces are divided into the three groups,
                  they are subdivided into positive and negative, by placing a
                  dividing principal plane at the part centre, and considering
                  which side the face's centroid falls on.


        """
        
        # get cell list
        part = self.GetPartObj()
        cells = part.cells
        
        # check if it has been completely meshed
        fullyMeshed = True
        for cell in cells:
            if type(cell.getElements())==NoneType:
                fullyMeshed = False
                print 'Cell <{0}> has not been meshed yet.'.format(cell.index)
        
        if not fullyMeshed:
            self.ErrorHandling('Part must be meshed before sets can be generated.')
        
        
        # loop through each face in the part and apply the method described above
        noFaces = len(part.faces)
        
        # "catergories" stores which of (X,Y,Z) or (X,Y) the faces normals aligns with
        # "polarity" stores whether the face's centroid is nearer the positive
        # negative limit of the part's geometry (along the direction in "categories")
        categories = []
        polarities = []
        
        box = self.GetBoundingBox()
        limits = [(box[0], box[1]),(box[2], box[3]), (box[4], box[5])]

        # go through the list of faces
        for face in part.faces:

            # get the number of faces for the element type used in the part
            noFacesOnElement = len(part.elements[0].getElemFaces())
            
            # check if any of the elements for this face are external
            for element in face.getElements():
                
                if len(element.getAdjacentElements()) < noFacesOnElement:
                    IsExternal = True
                    break
                    
            # determine which category the face belongs to (X,Y or Z)
            category = alignments.CategoriseVectorXYZ(face.getNormal())
            categories.append(category)
            
            # determine the polarity
            polarity = alignments.DetermineClosestPrincipalPlane(face.getCentroid()[0],
                                  limits[category], category)
            polarities.append(polarity)
            
        
        # store the results
        self.SetValue('faceCategories', categories)
        self.SetValue('facePolarities', polarities)
            
    
    def CreateDefaultExternalSurface(self, side=1, overWrite=True, name=DefaultExtSurfName):
        """ create a surface for all external faces of the part
        """
        # get an external point (comes originally from the geometry object used
        # to create the part)
        
        # get a point on the external surface
        part = self.GetPartObj()
        externalPoint = self.GetValue('extPoint')
        
        is3D = self.GetValue('dimensionality') == '3D'
        
        # get a face or edge list associated with this point
        if is3D:
            faces = part.faces.findAt((externalPoint,))
        else:
            edges = part.edges.findAt(([externalPoint[0], externalPoint[1], 0.],))
        
        # check if the surface exists, overwrite accordingly
        if name in part.surfaces.keys():
            if overWrite:
                del part.surfaces[name]
            else:
                errorMessage = 'Cant create default surface, name already exists.'
                self.ErrorHandling(errorMessage)
        
        # create the surface
        if side==1:
            if is3D:
                part.Surface(side1Faces=faces, name=name)
            else:
                part.Surface(side1Edges=edges, name=name)
        elif side==2:
            if is3D:
                part.Surface(side2Faces=faces, name=name)
            else:    
                part.Surface(side2Edges=edges, name=name)
     
    
    def GetBoundingBox(self):
        """ get the bounding box as reported by abaqus for the part, rather than
            the bounding box defined by the method "ExtrudeFromGeomObj"
        """
        if not self.GetValue('partCreated'):
            errorMessage = 'The part has not been created (eg. by extrusion) yet.'
            self.ErrorHandling(errorMessage)
            
        partObj = self.GetPartObj()
        
        # call the bounding box method. rigid parts have no cells, so a slightly
        # different method is used (faces rather than cells)
        cells = partObj.cells
        if len(cells)==0:
            faces = partObj.faces
            box = faces.getBoundingBox()
        else:
            box = cells.getBoundingBox()
        
        xMax = box['high'][0]
        yMax = box['high'][1]
        zMax = box['high'][2]
        xMin = box['low'][0]
        yMin = box['low'][1]
        zMin = box['low'][2]
        
        return (xMax, xMin, yMax, yMin, zMax, zMin)
        
        
    def MakeBoundingBox(self):
        """ create a box of datum planes around the current part
        """
        is3D = self.GetValue('dimensionality')=='3D'
             
        # get extents
        xMax = self.GetValue('xMax')
        xMin = self.GetValue('xMin')
        yMax = self.GetValue('yMax')
        yMin = self.GetValue('yMin')
        if is3D:
            zMax = self.GetValue('zMax')
            zMin = self.GetValue('zMin')
        
        # create datums
        self.MakePrinciplePlaneDatum(plane=YZPLANE, name='upperYZ', offset=xMax)
        self.MakePrinciplePlaneDatum(plane=YZPLANE, name='lowerYZ', offset=xMin)
        self.MakePrinciplePlaneDatum(plane=XZPLANE, name='upperXZ', offset=yMax)
        self.MakePrinciplePlaneDatum(plane=XZPLANE, name='lowerXZ', offset=yMin)
        
        if is3D:
            self.MakePrinciplePlaneDatum(plane=XYPLANE, name='upperXY', offset=zMax)
            self.MakePrinciplePlaneDatum(plane=XYPLANE, name='lowerXY', offset=zMin)
        
        
    def MakePrinciplePlaneDatum(self, plane, offset, name):
        """ create datum planes parallel to the principal planes on the current part
        """
        # create datums
        part = self.GetPartObj()
        newDatum = part.DatumPlaneByPrincipalPlane(principalPlane=plane, offset=offset)
        part.features.changeKey(fromName=newDatum.name, toName=name)
        
        # store references
        self.SetValue(name, newDatum)
        
        
    def GetRecommendedMeshControl(self):
        """ return a dictionary of defaults for the meshing of the part
        """
        recommendedControls = {}
        recommendedControls.update({'meshAlgorithm':self.GetValue('meshAlgorithm')})
        recommendedControls.update({'defaultTechnique': self.GetValue('defaultTechnique')})
        
        elementType = self.GetValue('recommendedElementType')
        recommendedControls.update({'recommendedElementType':elementType})
        
        # the abaqus elemType object is used to specify element types
        elemTypeObj = mesh.ElemType(elemCode=elementType)
        recommendedControls.update({'elemTypeObj':elemTypeObj})
        
        return recommendedControls
        
    
    def GetPartType(self):
        """
        """
        return self.GetValue('partType')


    def AssignPointMassToRP(self, mass=defaultPointMass, name='default_mass'):
        """ associate a mass with the part's reference point
        """
        part = self.GetPartObj()
        
        # check if the part has a reference point
        if 'RP' in part.features.keys():
            refPoints = part.referencePoints.keys()
            refPointTuple = (part.referencePoints[refPoints[0]],)
            regionRef = Region(referencePoints=refPointTuple)
        
        else:
            errMsg = 'The part {0} has no reference point (yet).'.format(part.name)
            self.ErrorHandling(errMsg)
        
        
        # check if an engineering feature with the desired named already exists
        inertias = part.engineeringFeatures.inertias
        if name in inertias.keys():
            errMsg = 'There is already an inertia with name {0} assigned to the part {1}'.format(name, part.name)
            self.ErrorHandling(errMsg)
            
        # assign the mass
        part.engineeringFeatures.PointMassInertia(name=name, region=regionRef, mass=mass, alpha=0.0, composite=0.0)
        
        
    def AssignLocalCoordSys(self, angle=0., axis=AXIS_1, stackDirection=STACK_3,
                            orientation=None):
        """
        """
        part = self.GetPartObj()
        region = Region(cells=part.cells)
        part.MaterialOrientation(region=region, orientationType=SYSTEM,
                                 axis=axis, localCsys=orientation, fieldName='', 
                                 additionalRotationType=ROTATION_ANGLE,
                                 additionalRotationField='', angle=angle,
                                 stackDirection=stackDirection)
