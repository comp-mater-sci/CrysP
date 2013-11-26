# Prevent the legacy class type being used
__metaclass__ = type

# import native modules
from warnings import warn

# import abaqus modules
try:
    from abaqusConstants import *
    from abaqus import *
    import regionToolset
except ImportError:
    print ('This module requires the abaqus modules to be available.')
    raise

# import robusta modules
from robusta.assemble.GenericAssembly import *
from robusta.config import *


class SemiHpt(GenericAssembly):
    """ assemble a HPT test in which the bottom half of the sample disc and
        bottom die are modelled
    """
    
    def __init__(self, name, modelName, dieName, diePartObj, sampleName,
                 samplePartObj, mainContactSuffix, dieWallSuffix, offset):
        """ Constructor
        """
        # call base class constructor
        GenericAssembly.__init__(self, name, modelName)
        
        # store the instance names and references to part objects
        self.SetValue('dieName', dieName)
        self.SetValue('sampleName', sampleName)
        self.SetValue('diePartObj', diePartObj)
        self.SetValue('samplePartObj', samplePartObj)
        
        # create surfaces on the sample (which will be replicated in the assembly)
        sampleBotSurfSetName = geometrySetPrefix + negZSurfName
        sampleBotSurfName = surfacePrefix + negZSurfName        
        sampleSideSurfSetName = geometrySetPrefix + posXSurfName
        sampleSideSurfName = surfacePrefix + posXSurfName
        
        samplePartObj.CreateSurfacesFromSet(setName=sampleBotSurfSetName,
                                           surfName=sampleBotSurfName, side=2)
        
        centre = samplePartObj.GetValue('geometry').GetValue('COG')
        self.SetValue('centre', centre)
        xMax = samplePartObj.GetValue('xMax')
        samplePartObj.CreateSetFromFaceAtPoint(name=sampleSideSurfSetName,
                                               point=(xMax,0.,centre[2]))
        samplePartObj.CreateSurfacesFromSet(setName=sampleSideSurfSetName,
                                            surfName=sampleSideSurfName, side=2)
                                            
        # create surfaces on the die
        mainContactSetName = geometrySetPrefix + mainContactSuffix
        mainContactSurfName = surfacePrefix + mainContactSuffix
        
        if dieWallSuffix is None:
            diePartObj.CreateAnalyticSetsAndSurf(mainContactSuffix, side=2)
            self.SetValue('toolContactGeomSet',
                           diePartObj.GetPartObj().sets[mainContactSetName])
            
        else:            
            dieWallSetName = geometrySetPrefix + dieWallSuffix
            dieWallSurfName = surfacePrefix + dieWallSuffix
            diePartObj.CreateTwoZoneGeomSets(mainContactSetName, dieWallSetName)
        
            diePartObj.CreateSurfacesFromSet(setName=mainContactSetName,
                                         surfName=mainContactSurfName)
            diePartObj.CreateSurfacesFromSet(setName=dieWallSetName, surfName=dieWallSurfName)
            
            self.SetValue('wallContactSurfName', dieWallSurfName)
        
        # instance the parts
        GenericAssembly.MakeInstance(self, partObj=diePartObj, instanceName=dieName)
        GenericAssembly.MakeInstance(self, partObj=samplePartObj, instanceName=sampleName)
        self.SetMovingToolInstanceName(instanceName=dieName)
        
        # bring the instances parallel
        GenericAssembly.BringDatumsTogether(self, moveableInstanceName=sampleName,
                            moveableDatumName='lowerXY', fixedInstanceName=dieName,
                            fixedDatumName='upperXZ')
                            
        # bring the instances in contact
        dieAbaqusPart = diePartObj.GetPartObj()
        dieDepth = dieAbaqusPart.features['upperXZ'].offset - dieAbaqusPart.features['lowerXZ'].offset
        GenericAssembly.TranslateAlongPrincipDir(self, instanceName=sampleName,
                            distance=-(dieDepth-offset-airGap), direction='y', force=True)
                            
        # store useful sets objects
        assemblyObj = self.GetAssemblyObj()
        self.SetValue('symmPlaneSet', assemblyObj.instances[sampleName].sets[\
                      '{0}{1}'.format(geometrySetPrefix, posZSurfName)])
        
        # store the surface names
        self.SetValue('sampleBotSurfName', sampleBotSurfName)
        self.SetValue('sampleSideSurfName', sampleSideSurfName)        
        self.SetValue('mainContactSurfName', mainContactSurfName)
        
        # store the surface object references
        assemblyObj = self.GetAssemblyObj()
        sampleBotSurf = assemblyObj.instances[sampleName].surfaces[sampleBotSurfName]
        sampleSideSurf = assemblyObj.instances[sampleName].surfaces[sampleSideSurfName]
        mainContactSurf = assemblyObj.instances[dieName].surfaces[mainContactSurfName]
        self.SetValue('sampleBotSurf', sampleBotSurf)
        self.SetValue('sampleSideSurf', sampleSideSurf)        
        self.SetValue('mainContactSurf', mainContactSurf)  
                      

    def SetInitialConditions(self):
        """ create boundary conditions, etc. for the intial step
        """
        modelObj = self.GetModelHandle()
        region = self.GetRPRegionRefForInstance(instanceName=self.GetValue('dieName'))
        
        # prevent rotation and Z/X translation of the die
        modelObj.DisplacementBC(name='no_rot_die', createStepName='Initial', 
                                region=region, u1=SET, u2=UNSET,
                                u3=SET, ur1=SET, ur2=SET, ur3=SET, 
                                amplitude=UNSET, distributionType=UNIFORM,
                                fieldName='', localCsys=None)
                                
        # assign a point mass to the die reference point
        self.GetValue('diePartObj').AssignPointMassToRP()
                      
                      
    def CreateCompressionForceStep(self, load, stepName='compression',
                              analyticTool=False, previousStep='Initial',
                              amplitude=defaultAmpName, duration=1):
        """ create a step for the initial application of pressure
        """
        modelObj = self.GetModelHandle()
        assemblyObj = self.GetAssemblyObj()
        
        
        sampleName = self.GetValue('sampleName')
        dieName = self.GetValue('dieName')
        
        # get the required geometry sets and surfaces
        symmPlaneSet = self.GetValue('symmPlaneSet')
        region = self.GetRPRegionRefForInstance(instanceName=self.GetValue('dieName'))
        
        # create the new step        
        modelObj.ExplicitDynamicsStep(name=stepName, previous=previousStep,
                                      timePeriod=duration)
        
        # add boundary conditions        
        modelObj.YsymmBC(name='compStepXSymm', createStepName=stepName, 
                         region=symmPlaneSet, localCsys=None)
                         
        # add pressure load. If the tool is analytic a concentrated force must be
        # used instead
        quadAmpName = self.CreateQuadraticAmplitude(duration=duration)
        self.SetValue('quadAmplitudeName', quadAmpName)
        if analyticTool:
            modelObj.ConcentratedForce(name='Load_'+stepName, createStepName=stepName,
                     region=region, cf2=load, amplitude=quadAmpName, 
                     distributionType=UNIFORM, field='', localCsys=None)
        
        else:
            modelObj.Pressure(name='Pressure_'+stepName, createStepName=stepName, 
                     region=region, distributionType=UNIFORM, field='',
                     magnitude=load, amplitude=quadAmpName)

             
    def CreateCompressionDispStep(self, displacement, stepName='compression',
                                  previousStep='Initial', duration=1):
        """ create a step for the initial application of pressure
        """
        modelObj = self.GetModelHandle()
        assemblyObj = self.GetAssemblyObj()
        
        
        sampleName = self.GetValue('sampleName')
        dieName = self.GetValue('dieName')
        
        # get the required geometry sets and surfaces
        symmPlaneSet = self.GetValue('symmPlaneSet')
        region = self.GetRPRegionRefForInstance(instanceName=self.GetValue('dieName'))
 
        # create an amplitude and a displacement BC
        quadAmpName = self.CreatePowerLawAmplitude(duration=duration, exponent=2)
        self.SetValue('powerLawAmplitude', quadAmpName)
        
        self.DisplaceMainToolAlongAxis(displacement=displacement, axis='Y',
                                  amplitude=quadAmpName, rotationAllowed=True,
                                  massScaling=defaultMassScaling,
                                  duration=duration, stepName=stepName)
        
        # add boundary conditions        
        modelObj.YsymmBC(name='compStepXSymm', createStepName=stepName, 
                         region=symmPlaneSet, localCsys=None)
        
        
    def CreateTorsionStep(self, angle, stepName='torsion', ampName=defaultAmpName,
                          duration=1.):
        """ step to apply the twist
        """
        # create a new step
        self.CheckNameAndCreateExplicitStep(name=stepName, duration=duration)
        modelObj = self.GetModelHandle()
        
        # deactivate all previous BCs
        BCNameList = modelObj.boundaryConditions.keys()
        for BCName in BCNameList:
            modelObj.boundaryConditions[BCName].deactivate(stepName)
            
        # apply rotation of the die, but lock all other DOFs
        region = self.GetRPRegionRefForInstance(instanceName=self.GetValue('dieName'))
        modelObj.DisplacementBC(name='lock_rot_die', createStepName=stepName,
                 region=region, u1=0.0, u2=0.0, u3=0.0, ur1=0.0, ur2=angle,
                 ur3=0.0, amplitude=ampName, fixed=OFF, distributionType=UNIFORM,
                 fieldName='', localCsys=None)
        
        # encastre the symmetry plane
        symmPlaneSet = self.GetValue('symmPlaneSet')
        modelObj.EncastreBC(name='encastre_symmplane', createStepName=stepName, 
                            region=symmPlaneSet, localCsys=None)


