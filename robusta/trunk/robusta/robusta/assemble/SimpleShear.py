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
from robusta.assemble.GenericAssembly import *
from robusta.config import *


class SimpleShear(GenericAssembly):
    """ assemble a basic flat rolling simulation, given three part objects
        corresponding to top roll, bottom roll, and sheet
    """
    
    def __init__(self, name, modelName, tool, sample, gap,
                 jaw1Name='jaw1', jaw2Name='jaw2', sampleName='sheet',
                 configuration='default', surfaceSide=1):
        """ Constructor
        """
        # call base class constructor
        GenericAssembly.__init__(self, name, modelName)
        
        # store the instance names
        self.SetValue('jaw1Name', jaw1Name)
        self.SetValue('jaw2Name', jaw2Name)
        self.SetValue('sampleName', sampleName)
                
        # determine if symmetry is being used
        symmetric = tool.GetValue('symmetric')
        self.SetValue('symmetric', symmetric)
        
        # determine and store the dimensionality
        dimensionality = sample.GetValue('dimensionality')
        self.SetValue('dimensionality', dimensionality)
        is3D = dimensionality == '3D'

        # create surfaces on the parts (which will be replicated in the assembly)
        FrontSurfSampleSetName = geometrySetPrefix + posZSurfName
        FrontSurfName = surfacePrefix + posZSurfName
        BackSurfSampleSetName = geometrySetPrefix + negZSurfName
        BackSurfName = surfacePrefix + negZSurfName

        sample.CreateSurfacesFromSet(setName=FrontSurfSampleSetName, surfName=FrontSurfName, side=2)
        sample.CreateSurfacesFromSet(setName=BackSurfSampleSetName, surfName=BackSurfName, side=2)
        
        # the surface on the tool needs to be specified slightly differently 
        # depending on whether it is 2D or 3D; also there is a weird behaviour
        # where if you create a surface on an analytic part with the correction
        # orientation (ie the surface is on the 'outside' rather than the 'inside')
        # the orientation gets reversed if the derived instance is flipped in the
        # assembly space.
        # unfortunately the solution seems to be that the surfaces for a 2D
        # part should be created on the instances rather than the parts

        # for 3D parts surfaces can be created on the part (for 2D the surface
        # is created on the instances, see below)
        if is3D:
            tool.CreateDefaultExternalSurface(name=DefaultExtSurfName,
                                              side=surfaceSide)
            
        
        # store the surface names
        self.SetValue('sampleFrontSurfName', FrontSurfName)    
        self.SetValue('sampleBackSurfName', BackSurfName)
        self.SetValue('jaw1SurfName', DefaultExtSurfName)
        self.SetValue('jaw2SurfName', DefaultExtSurfName)

        # instance the parts
        GenericAssembly.MakeInstance(self, partObj=tool, instanceName=jaw1Name)
        GenericAssembly.MakeInstance(self, partObj=tool, instanceName=jaw2Name)
        GenericAssembly.MakeInstance(self, partObj=sample, instanceName=sampleName)
        self.SetMovingToolInstanceName(instanceName=jaw1Name)
        
        # create the tool surfaces on the instances for 2D parts which are flipped
        # or rotated in the assembly space (flipping causes problems with surface
        # orientation)
        
        # note that this surface creation has to be done before the instances are
        # moved, due to the fact that the .findAt method seems to be the only
        # reliable way of creating surfaces (ie. if you move the instances first,
        # then you are faced with trying to calculate a point on each instance's
        # surface suitable for .findAt)
        if not is3D:            
            partObj = tool.GetPartObj()
            assemblyObj = self.GetAssemblyObj()
            externalPoint = tool.GetValue('extPoint')
            
            # the surface is named to seem like it came from a part surface, in
            # order to be consistent with the routines for defining interactions
            jaw1SurfName = '{0}.{1}'.format(jaw1Name, DefaultExtSurfName)
            self.SetValue('jaw1SurfName', jaw1SurfName)
            edges = assemblyObj.instances[jaw1Name].edges.findAt(([externalPoint[0], externalPoint[1], 0.],))
            assemblyObj.Surface(side1Edges=edges, name=jaw1SurfName)

            jaw2SurfName = '{0}.{1}'.format(jaw2Name, DefaultExtSurfName)
            self.SetValue('jaw2SurfName', jaw2SurfName)
            edges = assemblyObj.instances[jaw2Name].edges.findAt(([externalPoint[0], externalPoint[1], 0.],))
            assemblyObj.Surface(side2Edges=edges, name=jaw2SurfName)

        
        # Position the instances using the Abaqus CAE position constraints feature
        # see the class 'GenericPart' for the naming of Datum surfaces
        if configuration=='default' and is3D:
        
            # rotate the jaws so that they are parallel with the sample (this is
            # required because rigid parts are extruded in the z direction- their
            # 'length' or 'width' will be in the 'thickness' direction of the
            # deformable parts)
#            GenericAssembly.RotateInstanceAboutRP(self, instanceName=jaw1Name,
#                                                  axis='Y', angle=90.)
#            GenericAssembly.RotateInstanceAboutRP(self, instanceName=jaw2Name,
#                                                  axis='Y', angle=90.)
#                                                  
#            # rotate and translate grips to upper and lower positions
#            GenericAssembly.RotateInstanceAboutRP(self, instanceName=jaw1Name,
#                                                  axis='Z', angle=90.)
#            GenericAssembly.RotateInstanceAboutRP(self, instanceName=jaw2Name,
#                                                  axis='Z', angle=90.)
        
            # make the jaw1 move infront of the sample
            GenericAssembly.BringDatumsTogether(self, moveableInstanceName=jaw1Name,
                            moveableDatumName='lowerYZ', fixedInstanceName=sampleName,
                            fixedDatumName='upperXY')
                            
            # make the jaw2 move infront of the sample                                               
            GenericAssembly.BringDatumsTogether(self, moveableInstanceName=jaw2Name,
                            moveableDatumName='lowerYZ', fixedInstanceName=sampleName,
                            fixedDatumName='upperXY')
               
            # separate the grips
            toolHeight = abs(tool.GetValue('yMax') - tool.GetValue('yMin'))
            offSet = (gap + toolHeight)*0.5
            GenericAssembly.TranslateAlongPrincipDir(self, instanceName=jaw1Name,
                            distance=offSet, direction='y')                            
            GenericAssembly.TranslateAlongPrincipDir(self, instanceName=jaw2Name,
                            distance=-offSet, direction='y')
                            
            # add clearance (helps with contact)
            GenericAssembly.TranslateAlongPrincipDir(self, instanceName=jaw1Name,
                            distance=airGap, direction='z')
                            
            GenericAssembly.TranslateAlongPrincipDir(self, instanceName=jaw2Name,
                            distance=airGap, direction='z')

           
        elif configuration=='default' and not is3D:    
            # not implemented yet for 2D
            raise Exception('Assembly not implemented for 2D case yet.')
        
        else:
            errorMessage = 'Dont know how to construct the configuration {0}.'.format(configuration)
            GenericAssembly.ErrorHandling(self, errorMessage)
        
        
    def DefaultContact(self, frictionCoeff, constraint=PENALTY, sliding=FINITE):
        """ uses the method in GenericAssembly to identify which surfaces should
            be paired with which for contact in a two tool plane strain set up
            
            Note there is something strange about the abaqus object model here:
            even though surfaces appear in a common surfaces repository in the GUI,
            they are not found in rootAssembly.surfaces but actually in
            rootAssembly.instance[instanceName].surfaces !
        """
        
        assemblyObj = self.GetAssemblyObj()
        
        # interaction between jaw1 and sample
        jaw1Name = self.GetValue('jaw1Name')
        jaw1SurfName = self.GetValue('jaw1SurfName')
        masterSurf = self.FindSurface(jaw1SurfName, instanceName=jaw1Name)
        
        sampleName = self.GetValue('sampleName')
        sampleFrontSurfName = self.GetValue('sampleFrontSurfName')
        slaveSurf = self.FindSurface(sampleFrontSurfName)
        
        self.DefaultContactBySurfs(frictionCoeff=frictionCoeff, constraint=constraint,
                                   instMastSurf=masterSurf, instSlaveSurf=slaveSurf,
                                   sliding=sliding, name='jaw1_sample')
        
        # interaction between bottom jaw2 and sample
        jaw2Name = self.GetValue('jaw2Name')
        jaw2SurfName = self.GetValue('jaw2SurfName')
        masterSurf = self.FindSurface(jaw2SurfName, instanceName=jaw2Name)
        
        slaveSurf = self.FindSurface(sampleFrontSurfName)
        
        self.DefaultContactBySurfs(frictionCoeff=frictionCoeff, constraint=constraint,
                                   instMastSurf=masterSurf, instSlaveSurf=slaveSurf,
                                   sliding=sliding, name='jaw2_sample')
                                   
                   
    def ApplyGripForceAsFirstStep(self, stepName, gripForce,
                                  amplitude=defaultAmpName, rotationAllowed=False,
                                  massScaling=defaultMassScaling, duration=defaultDuration):
        """ apply a load to the grips to the first step (will propagate if not
            specifically deactivated in later steps)
        """
        # check the step name is free
        modelHandle = self.GetModelHandle()
        steps = modelHandle.steps
                         
        if stepName in steps.keys():
            errMsg = 'There is already a step with name {0}'.format(stepName)
            self.ErrorHandling(errMsg)
            
        # create the new step (NOTE: This has to be consolidated with the same
        #                            routines in GenericAssembly, e.g. see
        #                            DisplaceMainToolAlongAxis. the main difference
        #                            is hardcoding of previous='Initial')
        if simulationType=='explicit':
            modelHandle.ExplicitDynamicsStep(name=stepName, previous='Initial', 
                        massScaling=((SEMI_AUTOMATIC, MODEL, AT_BEGINNING, massScaling,
                        0.0, None, 0, 0, 0.0, 0.0, 0, None), ), timePeriod=duration)
        else:
            errMsg = 'Dont know how to set up a simulation step of type <{0}>.'.format(simulationType)
            self.ErrorHandling(errMsg)
        
        # get the instance names
        jaw1Name = self.GetValue('jaw1Name')
        sampleName = self.GetValue('sampleName')
        jaw2Name = self.GetValue('jaw2Name')
        
        # apply the load for the first non default step
        
        # apply to grip 1
        regionRef = self.GetRPRegionRefForInstance(jaw1Name)        
        modelHandle.ConcentratedForce(name='Load_{0}'.format(jaw1Name), 
                    createStepName=stepName, region=regionRef, cf1=0., cf2=0.,
                    cf3=gripForce, distributionType=UNIFORM, field='',
                    localCsys=None, amplitude=amplitude)
                    
        # apply to grip 2
        regionRef = self.GetRPRegionRefForInstance(jaw2Name)        
        modelHandle.ConcentratedForce(name='Load_{0}'.format(jaw2Name), 
                    createStepName=stepName, region=regionRef, cf1=0., cf2=0.,
                    cf3=gripForce, distributionType=UNIFORM, field='',
                    localCsys=None, amplitude=amplitude)
                                 
                                   
    def SimpleShearXYSymm(self):
        """ Apply boundary conditions and constraints for simple shear with a
            symmetry plane in the XY plane
        """
        # get the instance names
        jaw1Name = self.GetValue('jaw1Name')
        sampleName = self.GetValue('sampleName')
        jaw2Name = self.GetValue('jaw2Name')
        
        # BCs for the tools
        self.ApplyBC(instanceName=jaw2Name, kind='disp', label='jaw2_fixed',
                     data=[SET, SET, UNSET, SET, SET, SET], stepName='Initial')
        self.ApplyBC(instanceName=jaw1Name, kind='disp', label='jaw1_horz_only',
                     data=[UNSET, SET, UNSET, SET, SET, SET], stepName='Initial')
        
        # BCs for the sheet are different for 2D and 3D
        is3D = self.GetValue('dimensionality') == '3D'
        
        if is3D:
            self.ApplyBC(instanceName=sampleName, kind='zsymm', label='sheet_symm',
                     setName=geometrySetPrefix+negZSurfName)
            
        else:
            raise Exception('default 2D boundary conditions not implemented yet')
            
            
    def NoXTransDuringNoReboundAfter(self, stepName):
        """ prevent x translation of the tools during gripping, and prevent movement
            of the tools in the z direction after the named step
        """
        # get the instance names
        jaw1Name = self.GetValue('jaw1Name')
        jaw2Name = self.GetValue('jaw2Name')
        
        # check the step exists
        modelObj = self.GetModelHandle()
        stepNameList = modelObj.steps.keys()
        if not stepName in stepNameList:
            errMsg = 'No step with name {0} exists.'.format(stepName)
            self.ErrorHandling(errMsg)
            
        # get the name of the subsequent step
        subsequentStepName = [name for name in stepNameList \
                              if modelObj.steps[name].previous==stepName]
        if subsequentStepName==[]:
            errMsg = 'No step defined subsequent to the loading step {0}'.format(stepName)
            self.ErrorHandling(errMsg)
        
        subsequentStepName = subsequentStepName[0]
        
        # prevent x translation during the step, and deactivate in subsequent step
        region = self.GetRPRegionRefForInstance(instanceName=jaw1Name)
        modelObj.DisplacementBC(name='jaw1_no_x_trans', createStepName=stepName,
                                region=region, u1=0.0, u2=UNSET, u3=UNSET, 
                                ur1=UNSET, ur2=UNSET, ur3=UNSET, amplitude=UNSET,
                                fixed=OFF, distributionType=UNIFORM, fieldName='',
                                localCsys=None)
        modelObj.boundaryConditions['jaw1_no_x_trans'].deactivate(subsequentStepName)

        
        # prevent z translation in subsequent steps                      
        modelObj.DisplacementBC(name='jaw1_no_rebound', createStepName=subsequentStepName,
                                region=region, u1=UNSET, u2=UNSET, u3=0.0, 
                                ur1=UNSET, ur2=UNSET, ur3=UNSET, amplitude=UNSET,
                                fixed=OFF, distributionType=UNIFORM, fieldName='',
                                localCsys=None)
                                
        region = self.GetRPRegionRefForInstance(instanceName=jaw2Name)
        modelObj.DisplacementBC(name='jaw2_no_rebound', createStepName=subsequentStepName,
                                region=region, u1=UNSET, u2=UNSET, u3=0.0, 
                                ur1=UNSET, ur2=UNSET, ur3=UNSET, amplitude=UNSET,
                                fixed=OFF, distributionType=UNIFORM, fieldName='',
                                localCsys=None)
        
