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


class FlatTwoRoll(GenericAssembly):
    """ assemble a basic flat rolling simulation, given three part objects
        corresponding to top roll, bottom roll, and sheet
    """
    
    def __init__(self, name, modelName, topRoll, botRoll, sheet,
                 topRollName='top_roll', botRollName='bot_roll', sheetName='sheet',
                 configuration='default'):
        """ Constructor
        """
        # call base class constructor
        GenericAssembly.__init__(self, name, modelName)
        
        # store the instance names
        self.SetValue('topRollName', topRollName)
        self.SetValue('botRollName', botRollName)
        self.SetValue('sheetName', sheetName)
        
        # create surfaces on the parts (which will be replicated in the assembly)
        TopSurfSheetSetName = geometrySetPrefix + posYSurfName
        TopSurfName = surfacePrefix + posYSurfName
        BotSurfSheetSetName = geometrySetPrefix + negYSurfName
        BotSurfName = surfacePrefix + negYSurfName

        sheet.CreateSurfacesFromSet(setName=TopSurfSheetSetName, surfName=TopSurfName, side=2)
        sheet.CreateSurfacesFromSet(setName=BotSurfSheetSetName, surfName=BotSurfName, side=2)

        topRoll.CreateDefaultExternalSurface(name=DefaultExtSurfName)
        botRoll.CreateDefaultExternalSurface(name=DefaultExtSurfName)
        
        # store the surface names
        self.SetValue('topRollSurfName', DefaultExtSurfName)
        self.SetValue('botRollSurfName', DefaultExtSurfName)
        self.SetValue('sheetTopSurfName', TopSurfName)
        self.SetValue('sheetBotSurfName', BotSurfName)

        # instance the parts
        GenericAssembly.MakeInstance(self, partObj=topRoll, instanceName=topRollName)
        GenericAssembly.MakeInstance(self, partObj=botRoll, instanceName=botRollName)
        GenericAssembly.MakeInstance(self, partObj=sheet, instanceName=sheetName)
        self.SetMovingToolInstanceName(instanceName=topRollName)
                
        # Position the instances using the Abaqus CAE position constraints feature
        if configuration=='default':
        
            # make the rop roll move above the sheet
            GenericAssembly.BringDatumsTogether(self, moveableInstanceName=topRollName,
                            moveableDatumName='lowerXZ', fixedInstanceName=sheetName,
                            fixedDatumName='upperXZ')
                                     
            # make the bottom roll move below the sheet
            GenericAssembly.BringDatumsTogether(self, moveableInstanceName=botRollName,
                            moveableDatumName='upperXZ', fixedInstanceName=sheetName,
                            fixedDatumName='lowerXZ')
        else:
            errorMessage = 'Dont know how to construct the configuration {0}.'.format(configuration)
            GenericAssembly.ErrorHandling(self, errorMessage)
        
        # check if the rolls are wider than the sheet
        topRollWidth = topRoll.GetLength(direction='z')
        botRollWidth = botRoll.GetLength(direction='z')
        sheetWidth = sheet.GetLength(direction='z')
        if (topRollWidth < sheetWidth) or (botRollWidth < sheetWidth):
            warn('The rolls are more narrow than the sheet, this can cause contact problems.')
        
        # centre the sheet between the rolls in the z (extrusion/ roll axis) direction
        self.AlignInstanceByRP(moveInstanceName=topRollName,
                               refInstanceName=sheetName, direction='z')
        self.AlignInstanceByRP(moveInstanceName=botRollName,
                               refInstanceName=sheetName, direction='z')
            
        # set some defaults
        self.SetDefault('rollContactStepName','make_roll_contact')
        self.SetDefault('sheetMoveStepName','feed_in_sheet')
            
            
    def BCPlaneStrainRolling(self):
        """ Apply boundary conditions and constraints for plane strain rolling
        """
        # get the instance names
        topRollName = self.GetValue('topRollName')
        botRollName = self.GetValue('botRollName')
        sheetName = self.GetValue('sheetName')
        
        # pin the two rolls
        self.ApplyBC(instanceName=topRollName, kind='ySlotted', label='slotted_top_roll')
        self.ApplyBC(instanceName=botRollName, kind='zpinned', label='pin_bot_roll')
        
        # enforce plane strain on the sheet
        self.ApplyBC(instanceName=sheetName, kind='zsymm', label='plane_strain_a', setName=geometrySetPrefix+negZSurfName)
        self.ApplyBC(instanceName=sheetName, kind='zsymm', label='plane_strain_b', setName=geometrySetPrefix+posZSurfName)
        
        
    def CreateInitialisationSteps(self, sheetVel, rollDisp, contactInitTime=1.0,
                                  previousStep='Initial', massScaling=1.0,
                                  timeScaling=1.0, sheetMoveTime=0.001):
        """ Defines steps to give the sheet some initial velocity, and to bring
            the rolls into contact with the sheet
        """
        # create a step to give the sheet some intial velocity
        modelObj = self.GetModelHandle()
        assemblyObj = self.GetAssemblyObj()
        sheetMoveStepName = self.GetValue('sheetMoveStepName')
                
        modelObj.ExplicitDynamicsStep(name=sheetMoveStepName, previous=previousStep,
                                      scaleFactor=timeScaling)
        modelObj.steps[sheetMoveStepName].setValues(timePeriod=sheetMoveTime)
                
        # boundary condition for sheet movement
        sheetName = self.GetValue('sheetName')
        data = [UNSET]*7
        data[6] = defaultAmpName
        data[0]=-sheetVel
        self.ApplyBC(instanceName=sheetName, kind='vel', stepName=sheetMoveStepName,
                     setName='allNodes',label='sheet_move', data=data)
                                              
        # boundary conditions for roll movement
        data = [UNSET]*7
        data[5]=0.0
        
        # prevent top roll from rotating during initial contact
        topRollName = self.GetValue('topRollName')
        self.ApplyBC(instanceName=topRollName, kind='disp', data=data,
                     label='top_roll_rotation_lock', stepName=sheetMoveStepName)
        
        # prevent bottom roll from rotating during initial contact
        botRollName = self.GetValue('botRollName')
        self.ApplyBC(instanceName=botRollName, kind='disp', data=data,
                 label='bot_roll_rotation_lock', stepName=sheetMoveStepName)
        
        
        # create a step to bring the rolls in contact with the sheet by
        # moving the top roll downwards
        rollContactStepName = self.GetValue('rollContactStepName')
        modelObj.ExplicitDynamicsStep(name=rollContactStepName,
                                      previous=sheetMoveStepName, scaleFactor=timeScaling)
        modelObj.steps[rollContactStepName].setValues(timePeriod=contactInitTime)
             
        # bring top roll in contact with sheet
        data = [UNSET]*7
        data[1] = -rollDisp
        data[6] = defaultAmpName
        self.ApplyBC(instanceName=topRollName, kind='disp', data=data,
                         label='top_roll_init_contact', stepName=rollContactStepName)
                         
        # deactivate the sheet velocity boundary condition
        modelObj.boundaryConditions['sheet_move'].deactivate(rollContactStepName)
        
        
    def DefaultContact(self, frictionCoeff, contactStiffness, name='rollContact',
                        frictionRatio=1.):
        """ uses the method in GenericAssembly to identify which surfaces should
            be paired with which for contact in a two roll flat rolling set up
            
            Note there is something strange about the abaqus object model here:
            even though surfaces appear in a common surfaces repository in the GUI,
            they are not found in rootAssembly.surfaces but actually in
            rootAssembly.instance[instanceName].surfaces !
        """
        
        assemblyObj = self.GetAssemblyObj()
        
        # define an interaction property
        self.CreateBasicInteractionProp(name=name, stiffness=contactStiffness,
                                   frictionCoeff=frictionCoeff, separation=OFF)
                                   
        # define a second interaction if the two rolls have different friction
        differentRollFriction = not(frictionRatio==1.)
        if differentRollFriction:
            secondContactPropName = '{0}_bot'.format(name)
            self.CreateBasicInteractionProp(name=secondContactPropName,
                                   stiffness=contactStiffness, separation=OFF,
                                   frictionCoeff=frictionCoeff*frictionRatio)
        
        # interaction between top roll and sheet
        topRollName = self.GetValue('topRollName')
        topRollSurfName = self.GetValue('topRollSurfName')
        masterSurf = assemblyObj.instances[topRollName].surfaces[topRollSurfName]
        
        sheetName = self.GetValue('sheetName')
        sheetTopSurfName = self.GetValue('sheetTopSurfName')
        slaveSurf = assemblyObj.instances[sheetName].surfaces[sheetTopSurfName]
        
        self.DefaultContactBySurfs(frictionCoeff=frictionCoeff, propName=name,
                                   instMastSurf=masterSurf, instSlaveSurf=slaveSurf,
                                   name='top_roll_sheet')
        
        # interaction between bottom roll and sheet
        botRollName = self.GetValue('botRollName')
        botRollSurfName = self.GetValue('botRollSurfName')
        masterSurf = assemblyObj.instances[botRollName].surfaces[botRollSurfName]
        
        sheetBotSurfName = self.GetValue('sheetBotSurfName')
        slaveSurf = assemblyObj.instances[sheetName].surfaces[sheetBotSurfName]
        
        if differentRollFriction:
            self.DefaultContactBySurfs(frictionCoeff=frictionCoeff*frictionRatio,
                                   instMastSurf=masterSurf, instSlaveSurf=slaveSurf,
                                   name='bot_roll_sheet', propName=secondContactPropName)
        else:
            self.DefaultContactBySurfs(frictionCoeff=frictionCoeff, propName=name,
                                   instMastSurf=masterSurf, instSlaveSurf=slaveSurf,
                                   name='bot_roll_sheet')
                                   
    def CoupleRollsToRP(self):
        """ add a "coupling" constraint to constrain the DOF of the roll bodies
            to its reference point. This only makes sense if the roll is an
            analytic surface or is meshed with rigid elements
        """
        modelObj = self.GetModelHandle()
        assemblyObj = self.GetAssemblyObj()
        
        # get the reference point objects in the root assembly for the top roll
        topRollName = self.GetValue('topRollName')
        topRollInstance = assemblyObj.instances[topRollName]
        topRPRegion = self.GetRPRegionRefForInstance(instanceName=topRollName)
        
        # likewise for the bottom roll
        botRollName = self.GetValue('botRollName')
        botRollInstance = assemblyObj.instances[botRollName]
        botRPRegion = self.GetRPRegionRefForInstance(instanceName=botRollName)
        
        # define region object for the top roll body
        topRollFaces = topRollInstance.faces
        topRollEdges = topRollInstance.edges
        topRollBodyRegion = regionToolset.Region(edges=topRollEdges, faces=topRollFaces)
        
        # define region object for the bottom roll body
        botRollFaces = botRollInstance.faces
        botRollEdges = botRollInstance.edges
        botRollBodyRegion = regionToolset.Region(edges=botRollEdges, faces=botRollFaces)
        
        # define the coupling constraints
        modelObj.Coupling(name='top_roll_to_RP', controlPoint=topRPRegion, 
                          surface=topRollBodyRegion, influenceRadius=WHOLE_SURFACE,
                          couplingType=KINEMATIC, localCsys=None, u1=ON, u2=ON,
                          u3=ON, ur1=ON, ur2=ON, ur3=ON)
                          
        modelObj.Coupling(name='bot_roll_to_RP', controlPoint=botRPRegion, 
                          surface=botRollBodyRegion, influenceRadius=WHOLE_SURFACE,
                          couplingType=KINEMATIC, localCsys=None, u1=ON, u2=ON,
                          u3=ON, ur1=ON, ur2=ON, ur3=ON)


    def CreateRollRotationStep(self, topRollVelocity, botRollVelocity, stepTime,
                               suppressRotationLocks=False, rotationStepName='roll_rotation',
                               amplitudeName='default_amp', timeScaling=1.0):
        """ create a new step (as a final step after any exisiting steps) and set
            velocity boundary conditions for the rolls
        """
        # get shortcuts
        modelObj = self.GetModelHandle()
        assemblyObj = self.GetAssemblyObj()        
        stepObj = modelObj.steps
        
        # find the previous step name and create the new step
        previousStepLinks = [stepObj[stepName].previous for stepName in stepObj.keys()]
        lastStepName = [stepName for stepName in stepObj.keys() if not stepName in previousStepLinks][0]

        modelObj.ExplicitDynamicsStep(name=rotationStepName, previous=lastStepName,
                                      scaleFactor=timeScaling)
        modelObj.steps[rotationStepName].setValues(timePeriod=stepTime)
        
        # get region objects for the roll reference points
        topRPRegion = self.GetRPRegionRefForInstance(instanceName=self.GetValue('topRollName'))
        botRPRegion = self.GetRPRegionRefForInstance(instanceName=self.GetValue('botRollName'))
        
        # create the velocity boundary conditions
        modelObj.VelocityBC(name='top_roll_rotate', createStepName=rotationStepName,
                            region=topRPRegion, v1=0.0, v2=0.0, v3=0.0, 
                            vr1=0.0, vr2=0.0, vr3=topRollVelocity,
                            amplitude=amplitudeName, localCsys=None,
                            distributionType=UNIFORM, fieldName='')
                            
        modelObj.VelocityBC(name='bot_roll_rotate', createStepName=rotationStepName,
                            region=botRPRegion, v1=0.0, v2=0.0, v3=0.0, 
                            vr1=0.0, vr2=0.0, vr3=botRollVelocity,
                            amplitude=amplitudeName, localCsys=None,
                            distributionType=UNIFORM, fieldName='')
                            
                            
        # remove any rotation locks if present
        if suppressRotationLocks:
            
            # find boundary conditions applying to the rolls with the work
            # 'rotation' and 'lock' in them, or the word 'init'
            bcNameList = [bcName for bcName in modelObj.boundaryConditions.keys() \
                          if 'rotation' in bcName and 'lock' in bcName\
                          or 'init' in bcName]
                          
            # deactivate these conditions in the roll rotation step
            for bcName in bcNameList:
                modelObj.boundaryConditions[bcName].deactivate(rotationStepName)
                
        # replace the slotted condition on the top roll with a pinned condition
        modelObj.boundaryConditions['slotted_top_roll'].deactivate(rotationStepName)
        modelObj.boundaryConditions['pin_bot_roll'].deactivate(rotationStepName)
        
        # prevent oscillation of the sheet on the output side by preventing vertical
        # movement of the start and end of the sheet
        region = assemblyObj.instances[self.GetValue('sheetName')].sets['{0}{1}'.format(geometrySetPrefix, negXSurfName)]
        modelObj.DisplacementBC(name='out_table', createStepName='Initial', region=region,
                                u1=UNSET, u2=0.0, u3=UNSET, ur1=UNSET, ur2=UNSET,
                                ur3=UNSET, amplitude=UNSET, fixed=OFF,
                                distributionType=UNIFORM, fieldName='', localCsys=None)
                                
        region = assemblyObj.instances[self.GetValue('sheetName')].sets['{0}{1}'.format(geometrySetPrefix, posXSurfName)]
        modelObj.DisplacementBC(name='in_table', createStepName='Initial', region=region,
                                u1=UNSET, u2=0.0, u3=UNSET, ur1=UNSET, ur2=UNSET,
                                ur3=UNSET, amplitude=UNSET, fixed=OFF,
                                distributionType=UNIFORM, fieldName='', localCsys=None)
            
