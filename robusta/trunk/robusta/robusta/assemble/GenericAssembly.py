# Prevent the legacy class type being used
__metaclass__ = type

# import abaqus modules
try:
    from abaqusConstants import *
    from abaqus import *
    from regionToolset import Region
    from interaction import *
    import numpy as np
except ImportError:
    print ('This module requires the abaqus modules to be available.')
    raise

# import robusta modules
from robusta.GenericRobusta import *
from robusta.config import *

# import native modules
from warnings import warn
import math

class GenericAssembly(GenericRobusta):
    """
    An abstract base class which will contain all template assembly data and methods.
    
    An assembly name and abaqus mdb.model object name need to be provided to the
    constructor. example:
    
    myNewAssem = robusta.GenericAssembly(name='roll1',modelName='Model-1')
    
    
    Data can be loaded from text files by calling the LoadData method.
    """
    
    def __init__(self, name, modelName):
        """ Constructor
        """
        # call base class constructor
        GenericRobusta.__init__(self, modelName)

        # store assembly name
        self.SetDefault('assemblyName',name)
        
        # set default global coordinate system
        assembly = self.GetAssemblyObj()
        assembly.DatumCsysByDefault(CARTESIAN)
        
        # create instance data dictionary
        self.SetValue('instanceDict',{})
        
        # create default amplitude
        modelHandle = self.GetModelHandle()
        modelHandle.SmoothStepAmplitude(name=defaultAmpName, timeSpan=STEP, 
                                        data=((0.0, 0.0), (1.0, 1.0)))
                                        
        # create a default interaction property
        self.CreateBasicInteractionProp()

       
    def GetAssemblyObj(self):
        """ return an alias for the assembly
        """
        modelObj = self.GetModelHandle()
        
        return modelObj.rootAssembly
        
        
    def FindSurface(self, surfName, instanceName=None):
        """ return an Abaqus surface object from the assembly repositories
        
            the first surface with matching name is returned, starting in the
            main rootAssembly surface repository
        
            this method is necessary because the surface object could be stored
            in one of the instance surface repositories:
            mdb.models[modelName].rootAssembly.instances[instanceName].surfaces
            or the main surface repository:
            mdb.models[modelName].rootAssembly.surfaces
            
        """
        assemblyObj = self.GetAssemblyObj()
        
        # if no instance name is given
        if instanceName is None:
            # first check the main rootAssembly repository:
            surfNameList = assemblyObj.surfaces.keys()
            instanceNameList = assemblyObj.instances.keys()
            
            if surfName in surfNameList:
                return assemblyObj.surfaces[surfName]
            
            # else try the instance surface repositories, returning
            # the first one found
            for instanceName in instanceNameList:
                surfNameList = assemblyObj.instances[instanceName].surfaces.keys()
            
                if surfName in surfNameList:
                    return assemblyObj.instances[instanceName].surfaces[surfName]
        
        else:
            # if an instance name is given
            surfNameList = assemblyObj.instances[instanceName].surfaces.keys()
            
            if surfName in surfNameList:
                    return assemblyObj.instances[instanceName].surfaces[surfName]
        

        # if the surface is not found, raise an exception
        errMsg = 'A surface with name <{0}> was not found in the rootAssembly or '+\
                 'part instance surface repositories.'.format(surfName)
        self.ErrorHandling(errMsg)
        
    
    def DisplaceMainToolAlongAxis(self, axis, displacement, stepName=None,
                                  amplitude=defaultAmpName, rotationAllowed=False,
                                  massScaling=defaultMassScaling,
                                  duration=defaultDuration):
        """ linear displacement along one of the principal axes ('X', 'Y', 'Z)

            'amplitude' is the name of an amplitude object
        """
        
        modelHandle = self.GetModelHandle()
        toolInstanceName = self.GetMovingToolInstanceName()
        
        # determine the step name
        allStepNames = modelHandle.steps.keys()
        stepName = self.GetNewStepName(tryToUseName=stepName)
        
        # set up the new step
        if simulationType=='explicit':
            modelHandle.ExplicitDynamicsStep(name=stepName, previous=allStepNames[-1], 
                        massScaling=((SEMI_AUTOMATIC, MODEL, AT_BEGINNING, massScaling,
                        0.0, None, 0, 0, 0.0, 0.0, 0, None), ), timePeriod=duration)
        else:
            errMsg = 'Dont know how to set up a simulation step of type <{0}>.'.format(simulationType)
            self.ErrorHandling(errMsg)
        
        # set displacement boundary condition
        data = [UNSET]*7
        data[6] = amplitude
        
        directionIndex = self.GetIndexForDirection(axis)
        data[directionIndex] = displacement
        
        if not rotationAllowed:
            data[3] = 0.
            data[4] = 0.
            data[5] = 0.
        
        # apply the BC
        self.ApplyBC(instanceName=toolInstanceName, kind='disp',
                     label='Disp_<{0}>'.format(stepName), stepName=stepName,
                     setName='', data=data)
    
    def GetRPRegionRefForInstance(self, instanceName):
        """ return a region object for reference point (RP) associated with
            the named instance. If the object has no RP, an error is raised
        """
        # get instance data
        modelObj = self.GetModelHandle()
        instanceObj = self.GetInstanceData(instanceName, 'instanceObj')
        partObj = self.GetInstanceData(instanceName, 'partObj')
        
        # return a region for the RP if the part actually has an RP
        if 'RP' in partObj.features.keys():
            refPointId = partObj.features['RP'].id
            return Region(referencePoints=(instanceObj.referencePoints[refPointId],))
            
        else:
            errMsg = 'Part with name {0} has no reference point (yet).'.format(partObj.name)
            self.ErrorHandling(errMsg)
            

    def ApplyBC(self, instanceName, kind, label, stepName='Initial', setName='', data=[]):
        """ create boundary conditions for the given based on the keyword 'kind'
        """
        modelObj = self.GetModelHandle()
        
        # get the reference point, if the part actually has one
        if setName=='':
            regionRef = self.GetRPRegionRefForInstance(instanceName)
            
        else:
            regionRef = modelObj.rootAssembly.instances[instanceName].sets[setName]

        
        # apply the boundary condition described by 'kind'
        
        # can only rotate about z axis
        if kind=='zpinned':
            modelObj.DisplacementBC(name=label, createStepName=stepName, 
                                    region=regionRef, u1=SET, u2=SET,
                                    u3=SET, ur1=SET, ur2=SET, ur3=UNSET, 
                                    amplitude=UNSET, distributionType=UNIFORM,
                                    fieldName='', localCsys=None)

        # part can displace in the vertical (y) direction only, with rotation about z           
        elif kind=='ySlotted':
            modelObj.DisplacementBC(name=label, createStepName=stepName, 
                                    region=regionRef, u1=SET, u2=UNSET,
                                    u3=SET, ur1=SET, ur2=SET, ur3=UNSET, 
                                    amplitude=UNSET, distributionType=UNIFORM,
                                    fieldName='', localCsys=None)
                                    
        # part can displace in the vertical (y) direction only, with no rotation
        elif kind=='ySlottedLocked':
            modelObj.DisplacementBC(name=label, createStepName=stepName, 
                                    region=regionRef, u1=SET, u2=UNSET,
                                    u3=SET, ur1=SET, ur2=SET, ur3=SET, 
                                    amplitude=UNSET, distributionType=UNIFORM,
                                    fieldName='', localCsys=None)

        # generic velocity BC
        elif kind=='vel':
            modelObj.VelocityBC(name=label, createStepName=stepName,
                    region=regionRef, v1=data[0], v2=data[1], v3=data[2], 
                    vr1=data[3], vr2=data[4], vr3=data[5], amplitude=data[6],
                    localCsys=None, distributionType=UNIFORM, fieldName='')

        # generic displacement BC                    
        elif kind=='disp' and stepName=='Initial':
            modelObj.DisplacementBC(name=label, createStepName=stepName,
                     region=regionRef, u1=data[0], u2=data[1], u3=data[2], 
                     ur1=data[3], ur2=data[4], ur3=data[5],
                     fixed=OFF, distributionType=UNIFORM, fieldName='', localCsys=None)
                     
        elif kind=='disp' and not stepName=='Initial':
            modelObj.DisplacementBC(name=label, createStepName=stepName,
                     region=regionRef, u1=data[0], u2=data[1], u3=data[2], 
                     ur1=data[3], ur2=data[4], ur3=data[5], amplitude=data[6],
                     fixed=OFF, distributionType=UNIFORM, fieldName='', localCsys=None)
        
        # z symm
        elif kind=='zsymm':
            modelObj.ZsymmBC(name=label, createStepName=stepName, 
                             region=regionRef, localCsys=None)
                             
        # x symm
        elif kind=='xsymm':
            modelObj.XsymmBC(name=label, createStepName=stepName, 
                             region=regionRef, localCsys=None)
        
        # encastre
        elif kind=='encastre':
            modelObj.EncastreBC(name=label, createStepName=stepName, 
                                region=regionRef, localCsys=None)
    
        else:
            errorMessage = 'Dont know how to apply BC of type <{0}>.'.format(kind)
            self.ErrorHandling(errorMessage)
      
        
    def BringDatumsTogether(self, moveableInstanceName, moveableDatumName,
                            fixedInstanceName, fixedDatumName, clearance=airGap,
                            parallel=True, faced=True, flip=OFF):
        """ Using the CAE position constraints tools, bring the two assembly instances
            together face to face via associated datum planes.
            
            Note that the referencing of datums in the abaqus assembly is a bit
            convoluted. It appears to work as follows:
                - a datum is created on a part, and stored in the features list (in
                  fact a datum is an attribute of a feature, not an object itself)
                - an instance is created in the assembly. The datums (and possibly
                  other aspects of the parent part's features) are copied to a
                  dictionary (eg. ...rootAssembly.instances[instance].datums).
                - the datums in the instance.datums list have the same id's as the
                  corresponding datums in the parent part
                - So, you can find the instance datum you want if you know the name
                  of the feature in the part features list:
                  
                  datumId = ...parts[part].features.get(datumName)
                  instanceDatumRef = ...rootAssembly.instances[instance].datums[datumId]
        """
        
        assemblyObj = self.GetAssemblyObj()
        
        # get the moveable instance's datum reference
        moveableInstance = self.GetInstanceData(moveableInstanceName, 'instanceObj')
        moveablePart = self.GetInstanceData(moveableInstanceName, 'partObj')
        moveableFeatureId = moveablePart.features[moveableDatumName].id
        moveableDatum = moveableInstance.datums[moveableFeatureId]
        
        # get the fixed instance's datum reference
        fixedInstance = self.GetInstanceData(fixedInstanceName, 'instanceObj')
        fixedPart = self.GetInstanceData(fixedInstanceName, 'partObj')
        fixedFeatureId = fixedPart.features[fixedDatumName].id
        fixedDatum = fixedInstance.datums[fixedFeatureId]
        
        # make the two datum planes parallel
        if parallel:
            assemblyObj.ParallelFace(movablePlane=moveableDatum,
                        fixedPlane=fixedDatum, flip=flip)
        
        # make the two datum planes face to face, with required clearance
        if faced:
            assemblyObj.FaceToFace(movablePlane=moveableDatum,
                        fixedPlane=fixedDatum, flip=flip, clearance=clearance)
                               
    
    def RotateInstanceAboutRP(self, instanceName, axis, angle=180.):
        """ rotate the given instance about its reference point in the X,Y or Z
            axis direction
        """
        
        # get coord of reference point for the instance (rather than the part)
        assemblyObj = self.GetAssemblyObj()
        instanceRPCoord = self.GetInstanceRPCoords(instanceName)

        # define rotation direction
        index = self.GetIndexForDirection(direction=axis)
        rotDirection = np.array([0.0, 0.0, 0.0])
        rotDirection[index] = 1.0
        
        # rotate instance
        assemblyObj.rotate(instanceList=(instanceName, ), axisPoint=instanceRPCoord,
                           axisDirection=rotDirection, angle=angle)

     
     
    def GetInstanceRPCoords(self, instanceName):
        """ return the coords of the reference point for the given instance
        
            not to be confused with the coords of the reference point in 
            the (parent) part  object
        """
        instanceObj = self.GetInstanceData(instanceName, 'instanceObj')
        partObj = self.GetInstanceData(instanceName, 'partObj')
        
        # get the reference point, if the part actually has one
        if 'RP' in partObj.features.keys():
        
            # get coords of ref point in part space
            refPointCoords = []
            refPointCoords.append(partObj.features['RP'].xValue)
            refPointCoords.append(partObj.features['RP'].yValue)
            refPointCoords.append(partObj.features['RP'].zValue)
            
            # return coords of ref point for instance in assembly space
            instanceTranslation = instanceObj.getTranslation()
            return (np.array(refPointCoords) + np.array(instanceTranslation))
                       
        # if no ref point is defined, raise an exception
        else:
            errorMsg = 'The parent part for instance <{0}> appears to have no reference point (yet?).'.format(instanceName)
            self.ErrorHandling(errorMsg)
        
        
    def AlignInstanceByRP(self, moveInstanceName, refInstanceName, direction='x'):
        """ translate the instance in the given direction so that the two instance's
            reference points line up on a plane perpendicular to that direction
        """                
        
        # get the reference point coords
        moveInstanceRPCoord = self.GetInstanceRPCoords(instanceName=moveInstanceName)
        refInstanceRPCoord = self.GetInstanceRPCoords(instanceName=refInstanceName)
        
        # calculate start and end points for translation
        index = self.GetIndexForDirection(direction=direction)
        vector = np.array([0., 0., 0.])
        vector[index] = refInstanceRPCoord[index] - moveInstanceRPCoord[index]
        
        # translate the moveable instance
        assemblyObj = self.GetAssemblyObj()
        assemblyObj.translate(instanceList=(moveInstanceName,), vector=vector)
        
    
    def TranslateAlongPrincipDir(self, instanceName, distance, direction='x',
                                 force=False):
        """ translate the given instance along the positive or negative
            principal direction
            
            setting 'force' to True breaks any existing position contraints
        """
        
        # calculate the translation vector
        vector = np.array([0., 0., 0.])
        index = self.GetIndexForDirection(direction=direction)
        vector[index] = distance
        
        # translate the instance, breaking position constraints if required
        assemblyObj = self.GetAssemblyObj()
        if force: assemblyObj.instances[instanceName].ConvertConstraints()
        returnValue = assemblyObj.translate(instanceList=(instanceName,), vector=vector)
        
        if not(returnValue==0):
            warn('The instance {0} may not have been translated due to' +\
                          ' conflict with position constraints.'.format(instanceName))
        

    def GetInstanceData(self, instanceName, key):
        """ return instance data with the given key
        """
        allInstancesDictionary = self.GetValue('instanceDict')
        
        # check if the named instance has been made already
        if instanceName not in allInstancesDictionary.keys():
            errorMessage = 'instance <{0}> not found.'.format(instanceName)
            self.ErrorHandling(errorMessage)
            
        instanceDictionary = allInstancesDictionary.get(instanceName)

        # check if the requested key exists
        if key not in instanceDictionary.keys():
            errorMessage = 'Data for <{0}> not found for instance {1}.'.format(key, instanceName)
            self.ErrorHandling(errorMessage)
            
        return instanceDictionary.get(key)


    def AddInstanceData(self, instanceObj, data=None):
        """ store the given instance in this object
        
            data should be a python dictionary
        """
        # get a copy of the current instance dictionary
        name = instanceObj.name
        currentInstanceDictionary = self.GetValue('instanceDict')
        
        # check if there is an instance with the same name already
        if name in currentInstanceDictionary.keys():
        
            errorMessage = 'There is already an instance <{0}> in this object.'.format(name)
            self.ErrorHandling(errorMessage)

        # add the instance object
        newEntry = {'instanceObj':instanceObj}
        
        # if data was provided, add it also
        if self.Exist(data):
            if not type(data)==dict: raise TypeError('<data> must be a python dictionary.')
            
            dataItems = data.keys()
            for item in dataItems:
                newEntry.update({item:data.get(item)})
        
        # update the dictionary    
        currentInstanceDictionary.update({instanceObj.name:newEntry})
        self.SetValue('instanceDict', currentInstanceDictionary)
        
        
    def MakeInstance(self, partObj, instanceName):
        """ 
            if required, construct a box around the part with datum planes which
            are paralell to the principle planes
            
            apologies for the naming here; partObj is the robusta object
            and abaqusPart is the native abaqus part object
        """
        
        assemblyObj = self.GetAssemblyObj()
        abaqusPart = partObj.GetPartObj()
        
        instance = assemblyObj.Instance(name=instanceName, part=abaqusPart, dependent=ON)       
        
        instanceData = {'robustaObj':partObj}
        instanceData.update({'partObj':abaqusPart})

        # store the instance data
        self.AddInstanceData(instanceObj=instance, data=instanceData)
        
    
    def DefaultContactBySurfs(self, instMastSurf, instSlaveSurf, name,
                              frictionCoeff=None, constraint=KINEMATIC, sliding=FINITE,
                              allowSeparationAfterContact=defaultAllowContactSeparation,
                              propName=DefaultIntPropName):
        """ apply the simplest contact interation rules
        
            will use the first interaction property found, if any exists,
            otherwise will create a basic interaction
            
            "partMasterSurf" and "partSlaveSurf" specify the surface names in the original
            part context to be used to define contact in the assembly. Eg a surface called
            'external' in a part will be called '<instancename>.external' in the assembly
        """
        
        # check if an interaction property exists, if not create
        # in any case get an int. property name to work with (default is preferred) 
        modelHandle = self.GetModelHandle()
        assemblyObj = self.GetAssemblyObj()
        
        existingProps = modelHandle.interactionProperties.keys()
        if propName in existingProps:
            intPropName = propName
        
        # else just take the first in the list    
        else:
            intPropName = existingProps[0]

        # set the flag for allowing/disallowing separation of surfaces
        modelHandle.interactionProperties[intPropName].normalBehavior.setValues(
                    allowSeparation=allowSeparationAfterContact)
        
        # check if an interaction has already been defined with the given name
        if name in modelHandle.interactions.keys():
            errorMessage = 'An interaction rule with the name <{0}> already exists.'.format(name)
            self.ErrorHandling(errorMessage)
        
        # check if the sliding algorithm is compatible with the choice of constraint
        if constraint==PENALTY and not sliding==FINITE:
            
            warn('Changed sliding from {0} to FINITE for compatibility with PENALTY constraint'.format(sliding))
            sliding=FINITE
       
        # create an interaction between the two surfaces
        modelHandle.SurfaceToSurfaceContactExp(name=name, createStepName='Initial',
                    master=instMastSurf, slave=instSlaveSurf, 
                    mechanicalConstraint=constraint, sliding=sliding, 
                    interactionProperty=intPropName, initialClearance=OMIT,
                    datumAxis=None, clearanceRegion=None)
        
            
    def CreateBasicInteractionProp(self, name=DefaultIntPropName, stiffness=defaultContactStiffness,
                                   frictionCoeff=DefaultFrictionCoeff, separation=OFF,
                                   expCutoffDistance=0.):
        """ create a default simple interaction property
        """
        # check if the name is already being used
        modelHandle = self.GetModelHandle()
        if name in modelHandle.interactionProperties.keys():
            errorMessage = 'Cant create the default interaction property, as the name <{0}> is in use.'.format(name)
            self.ErrorHandling(errorMessage)
            
        # if not create the new property        
        modelHandle.ContactProperty(name)
        modelHandle.interactionProperties[name].TangentialBehavior(formulation=PENALTY,
                    directionality=ISOTROPIC, slipRateDependency=OFF, 
                    pressureDependency=OFF, temperatureDependency=OFF,
                    dependencies=0, table=(( frictionCoeff, ), ),
                    shearStressLimit=None, maximumElasticSlip=FRACTION, 
                    fraction=0.005, elasticSlipStiffness=None)

        modelHandle.interactionProperties[name].NormalBehavior(
                    pressureOverclosure=defaultOverClosureBehaviour,
                    allowSeparation=separation, 
                    constraintEnforcementMethod=DEFAULT,
                    contactStiffness=stiffness)
                    
        # if the 'simple exponential' type contact is desired, alter the normal
        # behaviour accordingly
        if expCutoffDistance > 0.:
            modelHandle.interactionProperties[name].normalBehavior.setValues(
                        constraintEnforcementMethod=DEFAULT, maxStiffness=None, 
                        pressureOverclosure=EXPONENTIAL,
                        table=((stiffness, 0.0), (0.0, expCutoffDistance)))

                    
    def GetMovingToolInstanceName(self):
        """ return the instance name for the (main) tool which moves
        """
        
        return self.GetValue('movingToolInstanceName')
        
        
    def GetMovingToolInstanceObj(self):
        """ return a reference to the Abaqus instance object which represents
            the (main) moving tool
        """
        assemblyObj = self.GetAssemblyObj()
        
        return assemblyObj.instances[self.GetMovingToolInstanceName()]
        
        
    def SetMovingToolInstanceName(self, instanceName):
        """ elect an instance as the main moving tool
        """
        modelHandle = self.GetModelHandle()
        
        if instanceName in modelHandle.rootAssembly.instances.keys():
            self.SetValue('movingToolInstanceName', instanceName)
        else:
            errMsg = 'An instance with name <{0}> does not exist (yet?).'.format(instanceName)
            self.ErrorHandling(errMsg)
       
            
    def GetNewStepName(self, tryToUseName=defaultStepName):
        """ return a feasible name for a step, based on the name supplied
        
            ie. if a supplied step name already exists, a name with a digit
            appended will be returned
        """
        # get existing step names
        modelHandle = self.GetModelHandle()
        stepNames = modelHandle.steps.keys()
        suitableNameNotFoundYet = True
        suffixNumber = 1
        
        # if the name is already in use, find a new one
        if tryToUseName in stepNames:
        
            while(suitableNameNotFoundYet):
                
                # generate a new name
                suffixNumber += 1
                newName = '{0}_{1}'.format(tryToUseName, suffixNumber)
                
                # check if the name isn't already in use
                suitableNameNotFoundYet = newName in stepNames
                
                # check iteration limit
                if suffixNumber > defaultIterationLimit:
                    errMsg = 'Couldnt find a suitable step name based on <{0}>.'.format(tryToUseName)
                    self.ErrorHandling(errMsg)
                    
            return newName
             
        else:
            return tryToUseName
            
            
    def SetStepTimePeriod(self, stepName, time):
        """ edit the time period for the named step
        """        
        modelObj = self.GetModelHandle()
        
        # check the step name is valid
        if not stepName in modelObj.steps.keys() or stepName=='Initial':
            errMsg = 'step with name {0} does not exist or is an editable step'.format(stepName)
            
        # adjust the time period
        stepObj = modelObj.steps[stepName]
        stepObj.setValues(timePeriod=abs(time))
        
        
    def ScaleAmplitudesToFitAllSteps(self):
        """ go through the list of steps and make/assign scaled copies of the
            default amplitude to each, so that the duration of the amplitude
            is equal to the duration of each step
        """
        modelObj = self.GetModelHandle()
        stepObj = modelObj.steps
        stepNameList = [name for name in stepObj.keys() if not name=='Initial']
                        
        for stepName in stepNameList:
            step = stepObj[stepName]
            
            # get the duration and active BC list for the current step
            duration = step.timePeriod
            bcStateObj = step.boundaryConditionStates
            bcNameList = bcStateObj.keys()
            filteredBCList = [bcName for bcName in bcNameList \
                            if bcStateObj[bcName].status in [CREATED, PROPAGATED]]
                            
            # filter out the bcs which use 'instantaenous'                
            activeBCList = [bcName for bcName in filteredBCList \
                            if not bcStateObj[bcName].amplitude=='']
                            
            # for each active BC, determine the amplitude in use, and make a list
            ampList = [bcStateObj[bcName].amplitude for bcName in activeBCList]
            
            # for each amplitude, check if the step duration differs from the amp duration
            ampObj = modelObj.amplitudes
            modifiedAmpList = [ampName for ampName in ampList \
                               if not ampObj[ampName].data[-1][0]==duration \
                               and ampObj[ampName].timeSpan==STEP]
            modifiedAmpIndices = [ampList.index(name) for name in modifiedAmpList]
                               
            copyNameList = {}                   
            for copyAmpName in modifiedAmpList:            
                # for each amplitude that has been identified above, make a new amp
                newAmpName = '{0}:cpy_{1}'.format(stepName, copyAmpName)
                modelObj.Amplitude(name=newAmpName, objectToCopy=ampObj[copyAmpName])
                copyNameList.update({copyAmpName: newAmpName})
                
                # and modify its duration to match that of the current step
                ampData = list(ampObj[copyAmpName].data)
                ampData[-1] = (duration, ampData[-1][1])
                ampObj[newAmpName].setValues(data=ampData)
                
            # for each BC apply the new (modified) amplitudes
            bcObj = modelObj.boundaryConditions
            for bcIndex in modifiedAmpIndices:
            
                bc = bcObj[activeBCList[bcIndex]]
                ampName = modifiedAmpList[bcIndex]
                
                bc.setValuesInStep(amplitude=copyNameList[ampName], stepName=stepName)
                
    def Regenerate(self):
        """ for some unknown reason some analyses will not run without
            'regenerating the assembly'
        """
        self.GetAssemblyObj().regenerate()
        
    
    def CreatePowerLawAmplitude(self, name='quadratic_amp', duration=1.,
                                 noPoints=11, smoothing=0.01, span=STEP,
                                 exponent=2):
        """ create a quadratic amplitude curve
        
            returns the name of the ampltidue object in the mdb structure
        """
        modelObj = self.GetModelHandle()
        amplitudeObj = modelObj.amplitudes
        
        # check if given name already exists
        if name in amplitudeObj.keys():
            if verbose: warn('amplitude <{0}> already exists, using existing...'.format(name))
            return name
        
        else:
            # create the data
            data = tuple([(tabValue, (tabValue/duration)**exponent) for tabValue in np.linspace(0,duration,noPoints)])
            
            # create the amplitude      
            mdb.models['HPT_sim'].TabularAmplitude(name=name, timeSpan=span, 
                                  smooth=smoothing, data=data)
            return name

        """ set the same number of output requests for all steps
        """
        # get shortcuts
        modelObj = self.GetModelHandle()
        stepObj = modelObj.steps
        fieldOutputObj = modelObj.fieldOutputRequests
        historyOutputObj = modelObj.historyOutputRequests
        
        # get list of steps, field and history requests
        stepNameList = stepObj.keys()
        stepNameList.remove('Initial')
        fieldOutputNameList = fieldOutputObj.keys()
        historyOutputNameList = historyOutputObj.keys()
        
        # for each step make sure the number of requests will be the same
        for stepName in stepNameList:
            
            # calculate the size of the request interval
            step = stepObj[stepName]
            stepDuration = step.timePeriod
            timeInterval = stepDuration/noFrames
            
            # field requests
            fieldRequestsInThisStep = step.fieldOutputRequestStates
            activeFieldRequestsInThisStep = [request \
                                             for request in fieldRequestsInThisStep.keys() \
                                             if not(fieldRequestsInThisStep[request].status==DEACTIVATED)]
            
            for fieldRequestName in fieldOutputNameList:                
                if fieldRequestName in activeFieldRequestsInThisStep:
                    fieldOutputObj[fieldRequestName].setValuesInStep(
                                                     stepName=stepName,
                                                     timeInterval=timeInterval)
            
            # history requests
            historyRequestsInThisStep = step.historyOutputRequestStates
            activeHistoryRequestsInThisStep = [request \
                                               for request in historyRequestsInThisStep.keys() \
                                               if not(historyRequestsInThisStep[request].status==DEACTIVATED)]
            
            for historyRequestName in activeHistoryRequestsInThisStep:
                if historyRequestName in activeHistoryRequestsInThisStep:
                    historyOutputObj[historyRequestName].setValuesInStep(
                                                         stepName=stepName,
                                                         timeInterval=timeInterval)
                                                         
