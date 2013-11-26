""" script to set up an example HPT simulation using the "robusta" package
"""
# import abaqus modules
from abaqusConstants import *
from abaqus import *

# import robusta classes
from robusta.material.SwiftVonMises import *      # materials
from robusta.geom.Circle import *                 # geometries
from robusta.geom.HptDie import *                 # geometries
from robusta.part.HptDiePart import *             # parts
from robusta.part.Deformable import *             # parts
from robusta.mesh.HomogenousMesh import *         # meshes
from robusta.assemble.SemiHpt import *            # assembly

from robusta.job.GenericJob import *
from robusta.config import *

# import native modules
import math

# ----  config  ----
modelName = 'HPT_sim'

materialDataFolder = '/home/diarmuid/LAPTOP/all/Code_and_Packages/Mine/packages/Copyright_KUL/robusta/robusta/test'
materialDataFolder=defaultMaterialDataFolder
#materialDataFolder = 'C:/Users/Administrator/Documents/all/Code_and_Packages/Mine/packages/Copyright_KUL/robusta/robusta/test'
materialDataFileName = 'AA6016_1mm_0deg.txt'

blankThick = 2.
blankRadius = 5.
filletRadius = 0.2

gap = 0.4
phiDegrees = 30
alpha = 0.

#pressure = 1.e1
toolDisplacement = 0.16
dieWallFricCoeff = 0.05
dieBaseFricCoeff = 0.4

#elementSize = .2
elementSize = 0.05

noODBFramesPerStep = 200
compStepDuration = 0.01
torsionStepDuration = 0.001
torsionAngle = 2.0

offsetToAvoidFillet = 0.008
default2DCentre = (0.0,0.0)

contactStiffness = 1e12
contactPressCutoffDist = 0.005

sampleName = 'blank'
bottomDieName = 'bottom_die'
dieWallContPropName = 'dieWallCont'
dieBaseContPropName = 'dieBaseCont'

# --- end config ---


# calculate dependent parameter values
topDieRadius = 1.5*blankRadius + gap/math.tan(math.radians(phiDegrees))

# Create a new model
mdb.Model(modelName)

# create materials and sections
sheetAlum = SwiftVonMises('AA6016_1mm', modelName)
sheetAlum.SetValue('maxTrueStrain', 10.)
sheetAlum.SetValue('strainStepSize', 0.05)
sheetAlum.MakeMaterialFromFile(fileName=materialDataFileName, folder=materialDataFolder)

# create the top and bottom die geometries            
bottomDieGeom = HptDie(bottomDieName, modelName)
bottomDieGeom.MakeSketch(sampleWidth=blankRadius, filletRadius=filletRadius,
                        wallAngle=90-phiDegrees, gap=gap, totalWidth=topDieRadius,
                        symmetric=False, closed=False, oneFilletOnly=True)

sampleGeom = Circle(sampleName, modelName)
sampleGeom.MakeSketch(radius=blankRadius)
sampleGeom.SetExtrusionLength(blankThick*0.5)

# create 3D parts
bottomDie = HptDiePart(bottomDieName, modelName)
bottomDie.RevolveFromRobustaObj(geometry=bottomDieGeom, analytic=True)

sample = Deformable(sampleName, modelName)
sample.ExtrudeFromRobustaObj(geometry=sampleGeom)
sample.MakeSections()
sample.SetValue('centre', [0.,0.,0.])
sample.AssignLocalCoordSys(angle=alpha)

# mesh the sample
newMesh = HomogenousMesh('simpleMesh', modelName, elementSize=elementSize)
newMesh.MeshPart(sample)

# create sets
sample.CreateInternalSets()
sample.CreateExternalSets()

# create assembly
assembly = SemiHpt(name='semiHpt', modelName=modelName, dieName=bottomDieName,
                   diePartObj=bottomDie, sampleName=sampleName, samplePartObj=sample,
                   mainContactSuffix='mainCont', dieWallSuffix=None,
                   offset=offsetToAvoidFillet)
                   
# apply initial boundary conditions
assembly.SetInitialConditions()

# define contact between the wall of the sample and the die
sampleWallSurf = assembly.GetValue('sampleSideSurf')
dieSurf = assembly.GetValue('mainContactSurf')

assembly.CreateBasicInteractionProp(frictionCoeff=dieWallFricCoeff, stiffness=contactStiffness,
                                    name=dieWallContPropName)
assembly.DefaultContactBySurfs(instMastSurf=dieSurf, instSlaveSurf=sampleWallSurf,
                               name='dieWallCont', propName=dieWallContPropName,
                               frictionCoeff=None, constraint=KINEMATIC, sliding=FINITE,
                               allowSeparationAfterContact=defaultAllowContactSeparation)

# define contact between the base of the sample and the die
sampleBaseSurf = assembly.GetValue('sampleBotSurf')
assembly.CreateBasicInteractionProp(frictionCoeff=dieBaseFricCoeff, stiffness=contactStiffness,
                                    name=dieBaseContPropName,
                                    expCutoffDistance=contactPressCutoffDist)
assembly.DefaultContactBySurfs(instMastSurf=dieSurf, instSlaveSurf=sampleBaseSurf,
                               name='dieBaseCont', propName=dieBaseContPropName,
                               frictionCoeff=None, constraint=PENALTY, sliding=FINITE,
                               allowSeparationAfterContact=defaultAllowContactSeparation)
                                    
# create simulation steps
assembly.CreateCompressionDispStep(displacement=toolDisplacement,
                                   stepName='compression', previousStep='Initial',
                                   duration=compStepDuration)
assembly.CreateTorsionStep(angle=torsionAngle, stepName='torsion',
                           duration=torsionStepDuration)

# create a job and define output
job = GenericJob('Job_uno', modelName)

job.SetDefaultRequests()
job.ContactRequestsAllInteractions()
job.SetNoRequestForAllSteps(noRequests=noODBFramesPerStep)

#jobFileName = job.CreateJob(writeCaeFile=self.GetValue('writeCaeFile'),
#                            caeFile=self.GetValue('caeFileName'),
#                            submit=self.GetValue('submit'),
#                            writeInput=self.GetValue('writeInput'))

assembly.SetAllFramesAllSteps(noODBFramesPerStep)

# regenerate the assembly to avoid bug?
assembly.Regenerate()
