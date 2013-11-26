""" script to set up a 2D plane strain compression upset simulation using the "robusta" package
"""
# import abaqus modules
from abaqusConstants import *
from abaqus import *

# import robusta classes
from robusta.geom.Rectangle import *                    # geometries
from robusta.geom.PscUpsetDie import *                  # geometries
from robusta.part.AnalyticRigid import *                # parts
from robusta.part.Deformable import *                   # parts
from robusta.assemble.PlaneStrainComp import *          # assembly
from robusta.job.GenericJob import *                    # job details

from robusta.config import *
from robusta.GenericRobusta import *

class pscupset2D(GenericRobusta):
    
    def __init__(self, name):
        """ constructor
        """
        # call base class constructor
        GenericRobusta.__init__(self, modelName=name, dbaseType='generic')
        
        # set defaults
        self.SetDefault('modelName', 'psc_upset')
        self.SetDefault('materialDataFolder', defaultMaterialDataFolder)
        self.SetDefault('materialDataFileName', defaultMaterialFile)
        self.SetDefault('materialName', 'default_material')
        self.SetDefault('sample2DCentre',(0., 0.))
        self.SetDefault('sampleLength', 10.)
        self.SetDefault('sampleWidth', 25.)
        self.SetDefault('sampleThickness', 7.)
        self.SetDefault('dieLength', 10.)
        self.SetDefault('dieHeight', 10.)
        self.SetDefault('dieRadius', 0.5)
        self.SetDefault('dieThickness', defaultElementSize)
        self.SetDefault('dieDepth', self.GetValue('sampleWidth')*1.3)
        self.SetDefault('toolDisplacement', 5.)
        self.SetDefault('symmetric', False)
        self.SetDefault('elementSize', 0.05)
        self.SetDefault('jobName', 'default_job_name')
        self.SetDefault('writeCaeFile', False)
        self.SetDefault('submit', False)
        self.SetDefault('writeInput', True)
        self.SetDefault('caeFileName', 'default_cae_file')
        
        
    def MakeNewTest(self, parameters, materialClass='SwiftVonMises',
                    meshClass='HomogenousMesh'):
        """ build a test with the given parameters
        
            'parameters' is a Python dictionary. If required values are missing,
            will try to work with defaults...
            
            materialClass is the name of a Robusta class which defines
            the material model to use. The material class should take the arguments
            'materialName', 'modelName' and 'dimensionality' for its constructor.
        """
        
        # import the specified material and mesh classes
        try:
            command = 'from robusta.material.{0} import *'.format(materialClass)
            exec(command)
            
            command = 'from robusta.mesh.{0} import *'.format(meshClass)
            exec(command)            
        except ImportError:
            errMsg = 'The classes {0} or {1} appear not to exist?'.format(materialClass, meshClass)
            self.ErrorHandling
        
        # store the given parameter values
        self.StoreAllParametersFromDict(parameters)
        
        # Create a new model
        modelName = self.GetValue('modelName')
        mdb.Model(modelName)

        # create materials and sections: look for new (given) parameters, otherwise
        # create the material from a file:
        
        # create the material object
        materialName = self.GetValue('materialName')
        command = 'specimen = {0}(materialName, modelName, dimensionality="2D")'.format(materialClass)
        exec(command)
        
        # get list of required parameters and check if values are available.
        matParmsList = specimen.ListRequiredMaterialParameters()                
        if self.AreAllTheseParmetersDefined(matParmsList):
            # copy the relevant parameter values
            for parameter in matParmsList:
                specimen.SetValue(parameter, self.GetValue(parameter))
                
            # make the material
            specimen.BuildMaterial()
        
        # else use data from a file
        else:
            materialDataFileName = self.GetValue('materialDataFileName')
            materialDataFolder = self.GetValue('materialDataFolder')
            
            # make the material
            specimen.MakeMaterialFromFile(fileName=materialDataFileName, folder=materialDataFolder)

        # create sample geometry
        sampleThickness = self.GetValue('sampleThickness')
        sampleLength = self.GetValue('sampleLength')
        sample2DCentre = self.GetValue('sample2DCentre')
        
        sampleGeom = Rectangle('flat', modelName)
        sampleGeom.MakeSketch(height=sampleThickness, width=sampleLength, centre=sample2DCentre)

        # create die geometry
        dieLength = self.GetValue('dieLength')
        dieRadius = self.GetValue('dieRadius')
        dieHeight = self.GetValue('dieHeight')
        symmetric = self.GetValue('symmetric')
        dieThickness = self.GetValue('dieThickness')
        
        dieGeom = PscUpsetDie('die', modelName)
        dieGeom.SetValue('solid', False)
        dieGeom.MakeSketch(width=dieLength, height=dieHeight, radius=dieRadius,
                           symmetric=symmetric, closed=False, thick=dieThickness)

        # create 2D parts
        hammer = AnalyticRigid('hammer', modelName, dimensionality='2D')
        hammer.Make2DShell(geometry=dieGeom, analytic=True)

        sample = Deformable('sample', modelName, dimensionality='2D')
        sample.Make2DShell(geometry=sampleGeom)
        sample.MakeSections()

        ## mesh the parts
        elementSize = self.GetValue('elementSize')
        command = 'newMesh = {0}("simpleMesh", modelName, elementSize={1})'.format(meshClass, elementSize)
        exec(command)
        newMesh.MeshPart(sample)

        ## create sets
        sample.CreateInternalSets()
        sample.CreateExternalSets()

        ## create assembly
        assembly = PlaneStrainComp('psc', modelName, tool=hammer, sample=sample)

        # apply initial boundary conditions and define contact
        assembly.DefaultContact(frictionCoeff=self.GetValue('friction'))
        assembly.PSCFixedAnvil()

        # create deformation steps
        toolDisplacement = self.GetValue('toolDisplacement')
        assembly.DisplaceMainToolAlongAxis(axis='Y', displacement=-toolDisplacement,
                                           stepName='move_hammer', rotationAllowed=True)

        # write the input file
        jobName = self.GetValue('jobName')
        caeFileName = self.GetValue('caeFileName')
        job = GenericJob(jobName, modelName)
        job.SetDefaultRequests()
        jobFileName = job.CreateJob(writeCaeFile=self.GetValue('writeCaeFile'),
                                    caeFile=caeFileName, submit=self.GetValue('submit'),
                                    writeInput=self.GetValue('writeInput'))
        self.SetValue('jobFileName', jobFileName)        
