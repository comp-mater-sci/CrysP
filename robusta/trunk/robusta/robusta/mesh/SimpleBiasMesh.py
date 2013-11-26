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
from robusta.mesh.GenericMesh import *
from robusta.config import *


class SimpleBiasMesh(GenericMesh):
    """
    This class represents the simplest form of meshing. The part is not partitioned,
    and a mesh with homogenous element size is made.
    """
    
    def __init__(self, name, modelName, elementSize=defaultElementSize):
        """ Constructor
        """
        
        # call base class constructor
        GenericMesh.__init__(self, name, modelName)
        
        # store data
        self.SetValue('elementSize', elementSize)
        self.SetDefault('deviationFactor', defaultElementDeviationFactor)
        self.SetDefault('meshConstraint', defaultConstraint)
        
        
    def MeshPart(self, robustaPartObj, overWrite=True):
        """ apply a mesh to the given (robusta) part object
        """
        # get geometry details
        part = robustaPartObj.GetPartObj()
        edgeList = part.edges
        cellList = part.cells
        name = robustaPartObj.GetValue('partName')
        
        # check if the part has been meshed already, delete accordingly
        meshFlag = robustaPartObj.GetValue('isMeshed')
        if meshFlag:
            if overWrite:
                part.deleteMesh()
                robustaPartObj.SetValue('isMeshed', False)
            else:
                self.ErrorHandling('Part <{0}> is already meshed. Set overWrite=True or delete the mesh.'.format(name))
        
        
        # check if the part has been seeded already, delete accordingly
        seedFlag = robustaPartObj.GetValue('isSeeded')
        if seedFlag:
            if overWrite:
                part.deleteSeeds()
                part.deleteSeeds(regions=edgeList)
                robustaPartObj.SetValue('isSeeded', False)
            else:
                self.ErrorHandling('Part <{0}> is already seeded. Set overWrite=True or delete seeding.'.format(name))
                
        # seed the part
        for edge in edgeList:
            self.SeedThisEdgeByElSize(part=part, edge=edge)
        robustaPartObj.SetValue('isSeeded', True)
        
        # apply mesh controls
        self.ApplyMeshControls(robustaPartObj)
        
        # mesh the part
        part.generateMesh()
        robustaPartObj.SetValue('isMeshed', True)

