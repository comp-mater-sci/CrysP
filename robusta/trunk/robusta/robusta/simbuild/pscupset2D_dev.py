""" script to set up a 2D plane strain compression upset simulation using the "robusta" package
"""
# import abaqus modules
from abaqusConstants import *
from abaqus import *

# import robusta classes
from robusta.material.SimpleSwiftVonMises import *      # materials
from robusta.geom.Rectangle import *                    # geometries
from robusta.geom.PscUpsetDie import *                  # geometries
from robusta.part.AnalyticRigid import *                # parts
from robusta.part.Deformable import *                   # parts
from robusta.mesh.HomogenousMesh import *               # meshes
from robusta.assemble.PlaneStrainComp import *          # assembly
from robusta.job.GenericJob import *                    # job details

# ----  config  ----
modelName = 'psc_upset'
materialDataFolder = '/home/diarmuid/LAPTOP/all/Code_and_Packages/Mine/packages/Copyright_KUL/robusta/robusta/test'
materialDataFileName = 'AA6016_1mm_0deg.txt'

sample2DCentre = (0., 0.)
sampleLength = 10.
sampleWidth = 25.
sampleThickness = 7.

dieLength = 10.
dieHeight = 10.
dieRadius = 0.5
dieThickness = defaultElementSize
dieDepth = sampleWidth*1.3

toolDisplacement = 5.

symmetric=False

elementSize = 0.05
# --- end config ---


# Create a new model
mdb.Model(modelName)

# create materials and sections
alumSpecimen = SimpleSwiftVonMises('AA6016_1mm', modelName, dimensionality='2D')
alumSpecimen.MakeMaterialFromFile(fileName=materialDataFileName, folder=materialDataFolder)

# create geometries
sampleGeom = Rectangle('flat', modelName)
sampleGeom.MakeSketch(height=sampleThickness, width=sampleLength, centre=sample2DCentre)

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
newMesh = HomogenousMesh('simpleMesh', modelName, elementSize=elementSize)
newMesh.MeshPart(sample)

## create sets
sample.CreateInternalSets()
sample.CreateExternalSets()

## create assembly
assembly = PlaneStrainComp('psc', modelName, tool=hammer, sample=sample)

# apply initial boundary conditions and define contact
assembly.DefaultContact(frictionCoeff=DefaultFrictionCoeff)
assembly.PSCFixedAnvil()

# create deformation steps
assembly.DisplaceMainToolAlongAxis(axis='Y', displacement=-toolDisplacement,
                                   stepName='move_hammer', rotationAllowed=True)

# run job
job = GenericJob('Job_uno', modelName)
job.SetDefaultRequests()
job.CreateJob(writeCaeFile=False, submit=False, writeInput=True)
