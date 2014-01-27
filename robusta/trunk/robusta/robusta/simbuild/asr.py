""" script to set up an example rolling simulation using the "robusta" package
"""
# import abaqus modules
from abaqusConstants import *
from abaqus import *

# import robusta classes
from robusta.material.VoceVonMises import *             # materials
from robusta.geom.Annulus import *                      # geometries
from robusta.geom.Rectangle import *                      # geometries
from robusta.part.DiscreteRigid import *                # parts
from robusta.part.Deformable import *                   # parts
from robusta.mesh.HomogenousMesh import *               # meshes
from robusta.assemble.FlatTwoRoll import *              # assemblies
from robusta.job.GenericJob import *
from robusta.config import *

def build(modelName='rolling_sim', materialDataFolder=defaultMaterialDataFolder,
          materialDataFileName='AA6016_1mm_0deg.txt', topRollRadius=100.,
          botRollRadius=150., sheetThick=5.0, reductionPercent=25,
          rollMass=defaultPointMass, sheetWidth=defaultPartSizeZ,
          sheetLength=None, rollWidthScale=1.5, rollSpeed=10.,
          rollSpeedRatio=1.0, topRollFriction=0.4, botRollFriction=0.4,
          contactStiffness=2.5e10, rollElementSize = defaultElementSize,
          sheetElementSize=0.4, sheetMoveTime=0.001, contactStepTime=0.1,
          rollingStepTime=4, sheetLengthRatio=1.5, sheetDepth=defaultPartSizeZ):

    # dependent parameter values
    if sheetLength is None:
        sheetLength = max(topRollRadius, botRollRadius) * sheetLengthRatio
    rollVertDisp = (1 - reductionPercent/100.) * sheetThick
    sheet2DCentre = (sheetLength*0.3,0.0)
    topRollVelocity = -rollSpeed
    botRollVelocity = rollSpeed * rollSpeedRatio

    # define other defaults
    default2DCentre = (0.0,0.0)
    topRollName = 'top_roll'
    botRollName = 'bot_roll'

    # Create a new model
    mdb.Model(modelName)

    # create materials and sections
    sheetAlum = VoceVonMises('AA6016_1mm', modelName)
    sheetAlum.MakeMaterialFromFile(fileName=materialDataFileName, folder=materialDataFolder)

    # create geometries (assuming different bottom and top rolls)
    topRollGeometry = Annulus(topRollName, modelName)
    topRollGeometry.MakeSketch(outRadius=topRollRadius, inRadius=topRollRadius-rollElementSize,
                               centre=default2DCentre)
    topRollGeometry.SetExtrusionLength(sheetDepth*rollWidthScale)

    bottomRollGeometry = Annulus(botRollName, modelName)
    bottomRollGeometry.MakeSketch(outRadius=botRollRadius, inRadius=botRollRadius-rollElementSize,
                               centre=default2DCentre)
    bottomRollGeometry.SetExtrusionLength(sheetDepth*rollWidthScale)

    sheetGeometry = Rectangle('sheet', modelName)
    sheetGeometry.MakeSketch(height=sheetThick, width=sheetLength, centre=sheet2DCentre)
    sheetGeometry.SetExtrusionLength(sheetDepth)

    # modify the centre of gravity definition (it should be at the centre of the roll!)
    defaultCOG = topRollGeometry.GetValue('COG')
    topRollGeometry.SetValue('COG', [0.0,0.0,defaultCOG[2]])

    defaultCOG = bottomRollGeometry.GetValue('COG')
    bottomRollGeometry.SetValue('COG', [0.0,0.0,defaultCOG[2]])

    # create 3D parts
    topRoll3D = DiscreteRigid(topRollName, modelName)
    topRoll3D.ExtrudeFromRobustaObj(geometry=topRollGeometry)
    topRoll3D.SetInertia([2.0,2.0,2.0])
    topRoll3D.AssignPointMassToRP(mass=rollMass)

    bottomRoll3D = DiscreteRigid(botRollName, modelName)
    bottomRoll3D.ExtrudeFromRobustaObj(geometry=bottomRollGeometry)
    bottomRoll3D.SetInertia([2.0,2.0,2.0])
    bottomRoll3D.AssignPointMassToRP(mass=rollMass)

    sheet = Deformable('sheet', modelName)
    sheet.ExtrudeFromRobustaObj(geometry=sheetGeometry)
    sheet.MakeSections()

    # mesh the parts
    newMesh = HomogenousMesh('simpleMesh', modelName)
    newMesh.MeshPart(sheet, elementSize=sheetElementSize)
    newMesh.MeshPart(topRoll3D, elementSize=rollElementSize)
    newMesh.MeshPart(bottomRoll3D, elementSize=rollElementSize)

    # create sets
    topRoll3D.CreateInternalSets()
    bottomRoll3D.CreateInternalSets()
    sheet.CreateInternalSets()
    sheet.CreateExternalSets()

    # create assembly
    assembly = FlatTwoRoll('asr', modelName, topRoll=topRoll3D, botRoll=bottomRoll3D, sheet=sheet)

    # apply initial boundary conditions and define contact
    assembly.BCPlaneStrainRolling()
    assembly.DefaultContact(frictionCoeff=topRollFriction,
                            contactStiffness=contactStiffness,
                            frictionRatio=botRollFriction/topRollFriction)
    assembly.CreateInitialisationSteps(sheetVel=20, rollDisp=rollVertDisp, contactInitTime=contactStepTime,
                                       sheetMoveTime=sheetMoveTime)

    # couple the rolls to their reference points, and set rotation speeds
    assembly.CoupleRollsToRP()
    assembly.CreateRollRotationStep(topRollVelocity=topRollVelocity, stepTime=rollingStepTime,
                                    botRollVelocity=botRollVelocity, suppressRotationLocks=True)
                                    
    assembly.ScaleAmplitudesToFitAllSteps()

    # create a job and define output
    job = GenericJob('Job_uno', modelName)

    job.SetDefaultRequests()
    job.ContactRequestsAllInteractions()
    job.SetNoRequestForAllSteps(noRequests=defaultNoODBFrames)

    #jobFileName = job.CreateJob(writeCaeFile=self.GetValue('writeCaeFile'),
    #                            caeFile=self.GetValue('caeFileName'),
    #                            submit=self.GetValue('submit'),
    #                            writeInput=self.GetValue('writeInput'))
    #self.SetValue('jobFileName', jobFileName)
