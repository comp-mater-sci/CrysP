""" script to set up a 3D plane strain compression upset simulation using the "robusta" package
"""
# import abaqus modules
from abaqusConstants import *
from abaqus import *

# import robusta classes
from robusta.material.SwiftVonMises import *            # materials
from robusta.geom.Line2D import *                       # geometries
from robusta.geom.Rectangle import *                    # geometries
from robusta.part.AnalyticRigid import *                # parts
from robusta.part.Deformable import *                   # parts
from robusta.mesh.HomogenousMesh import *               # meshes
from robusta.assemble.SimpleShear import *              # assembly
from robusta.job.GenericJob import *                    # job details
from robusta.GenericRobusta import *

# import native python modules
import os

class shear3D(GenericRobusta):
    
    def __init__(self, name):
        """ constructor
        """
        # call base class constructor
        GenericRobusta.__init__(self, modelName=name, dbaseType='generic')
        
        # ----  set defaults  ----
        self.SetDefault('modelName', 'mtm_shear_3D')
        self.SetDefault('caeFileName', 'shear3D')
        self.SetDefault('assemblyName', 'simp_shear')
        
        self.SetDefault('materialName', 'defaultMaterial')
        self.SetDefault('materialDataFolder', '/home/diarmuid/LAPTOP/all/Code_and_Packages/Mine/packages/Copyright_KUL/robusta/robusta/test')
        self.SetDefault('materialDataFileName', 'AA6016_1mm_0deg.txt')

        self.SetDefault('sample2DCentre', (0., 0.))
        sampleWidth = 50.
        self.SetDefault('sampleWidth', sampleWidth)
        sampleHeight = 60.
        self.SetDefault('sampleHeight', sampleHeight)
        self.SetDefault('sampleThickness', 1.)
        self.SetDefault('sampleDeformedZone', 4.)

        self.SetDefault('gripWidth', sampleWidth*1.1)
        self.SetDefault('gripHeight', sampleHeight/2.05)
        self.SetDefault('gripDepth', 1.) # arbitrary, the tools will be 2D rigid
        self.SetDefault('gripForce', -500*sampleWidth*sampleHeight*0.5)
        self.SetDefault('gripStepName', 'apply_grip_load')
        self.SetDefault('gripStepDuration', 1.0)
        self.SetDefault('gripMass', 100.)

        self.SetDefault('frictionCoeff', 1.0)

        self.SetDefault('toolDisplacement', 8.)
        self.SetDefault('shearStepName', 'shear_step')
        self.SetDefault('shearStepDuration', 10.0)

        self.SetDefault('symmetric', True) # this flag should always be set to True in this configuration

        self.SetDefault('elementSize', 1.)
        self.SetDefault('massScaling', 1.)

        self.SetDefault('contactAlg', KINEMATIC)
        self.SetDefault('workDirectory', os.getcwd())

        self.SetDefault('noODBFramesGripStep', 10)
        self.SetDefault('noODBFramesShearStep', 50)


        self.SetDefault('writeCaeFile', True)
        self.SetDefault('submit', False)
        self.SetDefault('writeInput', True)
        self.SetValue('caeFileName', 'default_cae_file')
        

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


        # create geometries
        sampleGeom = Rectangle('flat', modelName)
        sampleGeom.MakeSketch(height=self.GetValue('sampleHeight'),
                              width=self.GetValue('sampleWidth'),
                              centre=self.GetValue('sample2DCentre'))

        gripGeom = Line2D('grip', modelName)
        #gripGeom = Rectangle('grip', modelName)
        surfaceSide = 1 # set this to one for analytic surfaces with no extension into 3 dim
        
        gripGeom.SetValue('solid', False)
        gripGeom.MakeSketch(height=self.GetValue('gripHeight'),
                            centre=self.GetValue('sample2DCentre'))
        #gripGeom.MakeSketch(height=gripHeight, width=gripDepth, centre=sample2DCentre)


        # create 3D parts
        gripJaw = AnalyticRigid('gripJaw', modelName)
        gripGeom.SetExtrusionLength(self.GetValue('gripWidth'), part=gripJaw)
        gripJaw.ExtrudeFromRobustaObj(geometry=gripGeom)
        gripJaw.AssignPointMassToRP(mass=self.GetValue('gripMass'))

        sample = Deformable('sample', modelName)
        sampleGeom.SetExtrusionLength(self.GetValue('sampleThickness'), part=sample)
        sample.ExtrudeFromRobustaObj(geometry=sampleGeom)
        sample.MakeSections()

        # mesh the parts
        elementSize = self.GetValue('elementSize')
        command = 'newMesh = {0}("simpleMesh", modelName, elementSize={1})'.format(meshClass, elementSize)
        exec(command)
        newMesh.MeshPart(sample)

        # create sets
        sample.CreateInternalSets()
        sample.CreateExternalSets()

        # create assembly
        assembly = SimpleShear(self.GetValue('assemblyName'), modelName,
                               tool=gripJaw, sample=sample, surfaceSide=surfaceSide,
                               gap=self.GetValue('sampleDeformedZone'))

        # apply initial boundary conditions and define contact/ grip force
        assembly.DefaultContact(frictionCoeff=self.GetValue('frictionCoeff'),
                                constraint=self.GetValue('contactAlg'))
        assembly.SimpleShearXYSymm()


        # create deformation steps
        gripStepName = self.GetValue('gripStepName')
        shearStepName = self.GetValue('shearStepName')
        assembly.ApplyGripForceAsFirstStep(stepName=gripStepName,
                                           gripForce=self.GetValue('gripForce'),
                                           duration=self.GetValue('gripStepDuration'))
        assembly.DisplaceMainToolAlongAxis(axis='X',
                                           displacement=self.GetValue('toolDisplacement'),
                                           stepName=shearStepName, rotationAllowed=True,
                                           duration=self.GetValue('shearStepDuration'),
                                           massScaling=self.GetValue('massScaling'))
        assembly.ScaleAmplitudesToFitAllSteps()
        
                                   
        # add some controls to prevent the tools moving during/after loading
        assembly.NoXTransDuringNoReboundAfter(stepName=gripStepName)


        # write job (note that the LOOP method is used for parallelisation, not DOMAIN
        #            because of a shared contact zone. This could be eliminated by partitioning)
        job = GenericJob('Job_uno', modelName)

        job.SetDefaultRequests()
        job.ContactRequestsAllInteractions()
        job.SetNoRequestsForStep(stepName=self.GetValue('gripStepName'),
                                 noRequests=self.GetValue('noODBFramesGripStep'))
        job.SetNoRequestsForStep(stepName=shearStepName,
                                 noRequests=self.GetValue('noODBFramesShearStep'))

        jobFileName = job.CreateJob(writeCaeFile=self.GetValue('writeCaeFile'),
                                    caeFile=self.GetValue('caeFileName'),
                                    submit=self.GetValue('submit'),
                                    writeInput=self.GetValue('writeInput'))
        
        self.SetValue('jobFileName', jobFileName)
