# Prevent the legacy class type being used
__metaclass__ = type

# import abaqus modules
try:
    from abaqusConstants import *
    from abaqus import *
    import mesh
except ImportError:
    print ('This module requires the abaqus modules to be available.')
    raise

# import robusta modules
from robusta.GenericRobusta import *
from robusta.config import *


class GenericMesh(GenericRobusta):
    """
    An abstract base class to hold methods and data for different seeding and
    meshing strategies.
    
       
    The intention is that:
        - all parameters/etc. sufficient to define the meshing strategy will be
          passed to the constructor method, subsequently
        - any robusta part object can be passed to any robusta mesh object derived
          from this class
        - the part will be meshed by derived classes by using the MeshPart method,
          which is not intended to be extended
        - the derived classes should define at least two new methods called SeedThisEdge
          and ApplyMeshControls

    """
    
    def __init__(self, name, modelName):
        """ Constructor
        """
        
        # call base class constructor
        GenericRobusta.__init__(self, modelName)

        # store geometry name and create the Abaqus part object
        self.SetDefault('meshName',name)

 
    def SeedThisEdgeByElSize(self, part, edge, bias=None, elementSize=None):
        """ apply seeding to the given edge. In this case its simply equally spaced
            seeds
        """
        if not self.Exist(elementSize):
            elementSize = self.GetValue('elementSize')
                        
        devFactor = self.GetValue('deviationFactor')
        constraint = self.GetValue('meshConstraint')
        
        # check if biasing is to be applied
        if self.Exist(bias):
            # a bias of > 1 implies....
            pass
        else:
            part.seedEdgeBySize(edges=(edge,), size=elementSize,
                            deviationFactor=0.1, constraint=constraint)                      
                            
    def ApplyMeshControls(self, robustaPartObj):
        """ apply mesh controls to the part. In this case the 'recommended' controls
            for the part are used.
        """
        # check part has been created
        part = robustaPartObj.GetPartObj()
        is3D = robustaPartObj.GetValue('dimensionality') == '3D'
        if not robustaPartObj.GetValue('partCreated'):
            self.ErrorHandling('The part <{0}> must be created first.'.format(robustaPartObj.GetValue('partName')))
        
        # get meshing settings
        recommendedControls = robustaPartObj.GetRecommendedMeshControl()
                
        # apply the controls to each cell (for deformables) or each face (rigids)
        cells = part.cells
        
        # if there are no cells, then the part is rigid
        if len(cells)==0:
            for face in part.faces:
                # get region reference (
                #regionRef = face.findAt((face.pointsOn[0]))
        
                # apply mesh settings
                part.setElementType(regions=(face,), elemTypes=(recommendedControls.get('elemTypeObj'),))
                part.setMeshControls(regions=(face,), algorithm=recommendedControls.get('meshAlgorithm'))
        else:
            for cell in cells:
                
                # apply mesh settings
                part.setElementType(regions=(cell,), elemTypes=(recommendedControls.get('elemTypeObj'),))
                part.setMeshControls(regions=(cell,), algorithm=recommendedControls.get('meshAlgorithm'))

