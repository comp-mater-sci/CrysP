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

# import native python modules
import os

# ----  config  ----
modelName = 'mtm_shear_3D'
caeFileName = 'shear3D'
materialDataFolder = '/home/diarmuid/LAPTOP/all/Code_and_Packages/Mine/packages/Copyright_KUL/robusta/robusta/test'
#materialDataFolder = '/media/Documents and Settings/Administrator/Documents/all/Code_and_Packages/Mine/packages/Copyright_KUL/robusta/robusta/test'
materialDataFileName = 'AA6016_1mm_0deg.txt'

sample2DCentre = (0., 0.)
sampleWidth = 50.
sampleHeight = 60.
sampleThickness = 1.
sampleDeformedZone = 4.

gripWidth = sampleWidth*1.1
gripHeight = sampleHeight/2.05
gripDepth = 1.              # arbitrary, the tools will be 2D rigid
gripForce = -500*sampleWidth*sampleHeight*0.5
gripStepName = 'apply_grip_load'
gripStepDuration = 1.0
gripMass = 100.

frictionCoeff = 1.0

toolDisplacement = 8.
shearStepName = 'shear_step'
shearStepDuration = 10.0

symmetric=True              # this flag should always be set to True in this configuration

elementSize = 0.25
massScaling = 1.

contactAlg = KINEMATIC
workDirectory = os.getcwd()
noODBFramesGripStep = 10
noODBFramesShearStep = 50
# --- end config ---


# Create a new model
mdb.Model(modelName)

# create materials and sections
alumSpecimen = SwiftVonMises('AA6016_1mm', modelName)
alumSpecimen.MakeMaterialFromFile(fileName=materialDataFileName, folder=materialDataFolder)

# create geometries
sampleGeom = Rectangle('flat', modelName)
sampleGeom.MakeSketch(height=sampleHeight, width=sampleWidth, centre=sample2DCentre)

gripGeom = Line2D('grip', modelName)
surfaceSide = 1 # set this to one for analytic surfaces with no extension into 3 dim
#gripGeom = Rectangle('grip', modelName)
gripGeom.SetValue('solid', False)
gripGeom.MakeSketch(height=gripHeight, centre=sample2DCentre)
#gripGeom.MakeSketch(height=gripHeight, width=gripDepth, centre=sample2DCentre)

# create 3D parts
gripJaw = AnalyticRigid('gripJaw', modelName)
gripGeom.SetExtrusionLength(gripWidth, part=gripJaw)
gripJaw.ExtrudeFromRobustaObj(geometry=gripGeom)
gripJaw.AssignPointMassToRP(mass=gripMass)

sample = Deformable('sample', modelName)
sampleGeom.SetExtrusionLength(sampleThickness, part=sample)
sample.ExtrudeFromRobustaObj(geometry=sampleGeom)
sample.MakeSections()

## mesh the parts
newMesh = HomogenousMesh('simpleMesh', modelName, elementSize=elementSize)
newMesh.MeshPart(sample)

## create sets
sample.CreateInternalSets()
sample.CreateExternalSets()

## create assembly
assembly = SimpleShear('simp_shear', modelName, tool=gripJaw, sample=sample,
                        gap=sampleDeformedZone, surfaceSide=surfaceSide)

# apply initial boundary conditions and define contact/ grip force
assembly.DefaultContact(frictionCoeff=frictionCoeff, constraint=contactAlg)
assembly.SimpleShearXYSymm()

# create deformation steps
assembly.ApplyGripForceAsFirstStep(stepName=gripStepName, gripForce=gripForce,
                                   duration=gripStepDuration)
assembly.DisplaceMainToolAlongAxis(axis='X', displacement=toolDisplacement,
                                   stepName=shearStepName, rotationAllowed=True,
                                   duration=shearStepDuration, massScaling=massScaling)
assembly.ScaleAmplitudesToFitAllSteps()
                                   
# add some controls to prevent the tools moving during/after loading
assembly.NoXTransDuringNoReboundAfter(stepName=gripStepName)

# write job (note that the LOOP method is used for parallelisation, not DOMAIN
#            because of a shared contact zone. This could be eliminated by partitioning)
job = GenericJob('Job_uno', modelName)

job.SetDefaultRequests()
job.ContactRequestsAllInteractions()
job.SetNoRequestsForStep(stepName=gripStepName, noRequests=noODBFramesGripStep)
job.SetNoRequestsForStep(stepName=shearStepName, noRequests=noODBFramesShearStep)

job.CreateJob(writeCaeFile=False, submit=False, writeInput=True,  parallelMethod=LOOP)

# save a cae file
mdb.saveAs(pathName=os.path.join(workDirectory, caeFileName))
