# Prevent the legacy class type being used
__metaclass__ = type

# import abaqus modules
try:
    from abaqusConstants import *
    from abaqus import *
    import numpy as np
except ImportError:
    print ('This module requires the abaqus modules to be available.')
    raise

# import native modules
from multiprocessing import cpu_count

# import robusta modules
from robusta.GenericRobusta import *
from robusta.config import *


class GenericJob(GenericRobusta):


    def __init__(self, name, modelName):
        """ Constructor
        """
        # call base class constructor
        GenericRobusta.__init__(self, modelName)

        # clear any existing history and field output requests
        mdb = self.GetModelHandle()
        
        defaultHistories = mdb.historyOutputRequests.keys()
        for request in defaultHistories:
            del mdb.historyOutputRequests[request]
            
        defaultFields = mdb.fieldOutputRequests.keys()
        for request in defaultFields:
            del mdb.fieldOutputRequests[request]
            
            
    def SetDefaultRequests(self, noOfRequests=defaultNoODBFrames):
        """ the basic variable requests at a time interval which will give
            evenly spaced outputs across all steps
        """
        mdb = self.GetModelHandle()
        
        # get the total simulation time
        stepNameList = [stepName for stepName in mdb.steps.keys() if not stepName=='Initial']
        simTimeTotal = np.sum([mdb.steps[stepName].timePeriod for stepName in stepNameList])
        
        # get time interval for requests
        interval = simTimeTotal/noOfRequests
        
        # add the field request
        if defaultFieldName in mdb.fieldOutputRequests.keys():
            del mdb.fieldOutputRequests[defaultFieldName]
        
        mdb.FieldOutputRequest(name=defaultFieldName, createStepName=stepNameList[0],
                               variables=defaultFieldVars, timeInterval=interval)
                               
        # add the history request
        if defaultHistoryName in mdb.historyOutputRequests.keys():
            del mdb.historyOutputRequests[defaultHistoryName]
            
        mdb.HistoryOutputRequest(name=defaultHistoryName, createStepName=stepNameList[0],
                                 variables=defaultHistoryVars, timeInterval=interval)


    def ContactRequestsAllInteractions(self, stepName=None,
                                        noOfRequests=defaultNoODBFrames,
                                        contactVars=defaultContactVars):
        """ request general contact related variables
        """
        
        modelObj = self.GetModelHandle()
        
        # get the step name (if none provided)
        steps = modelObj.steps
        if stepName is None:            
            stepName = [name for name in steps.keys() \
                        if steps[name].previous=='Initial'][0]
                        
        elif not stepName in steps.keys():
            errMsg = 'Step with name {0} does not exist (yet?)'.format(stepName)
            self.ErrorHandling(errMsg)
            
        # get the time interval to use from existing history output requests
        reqInThisStep = steps[stepName].historyOutputRequestStates
        existingRequestNames = [name for name in reqInThisStep.keys() \
                               if reqInThisStep[name].status in [CREATED, PROPAGATED]]
        if existingRequestNames==[]:
            errMsg = 'This method needs at least one active history request in step {0}'.format(stepName)
            self.ErrorHandling(errMsg)
        
        timeInterval = reqInThisStep[existingRequestNames[0]].timeInterval
   
        # add the history request for each defined interaction
        for interactionName in modelObj.interactions.keys():
        
            requestName = 'Cont:int_{0}'.format(interactionName)
            modelObj.HistoryOutputRequest(name=requestName,
                     createStepName=stepName, variables=contactVars,
                     timeInterval=timeInterval, interactions=(interactionName, ), 
                     sectionPoints=DEFAULT, rebar=EXCLUDE)
  
    
    def SetNoRequestsForStep(self, stepName, noRequests)                            :
        """ set the number of output requests (history and field) for the named
            step
        """
        modelObj = self.GetModelHandle()
        steps = modelObj.steps
        
        # check the step name exists
        if not stepName in steps.keys():
            errMsg = 'No step named {0} exists (yet?)'.format(stepName)
            self.ErrorHandling(errMsg)
        
        # get the duration for this step
        stepDuration = steps[stepName].timePeriod
        timeInterval = stepDuration/noRequests
        
        # adjust the number of frames for each request in the given step
        fieldObj = modelObj.fieldOutputRequests        
        historyObj = modelObj.historyOutputRequests
        
        for fieldReqName in fieldObj.keys():
            fieldObj[fieldReqName].setValuesInStep(stepName=stepName,
                                                   timeInterval=timeInterval)
                                                   
        for histReqName in historyObj.keys():
            historyObj[histReqName].setValuesInStep(stepName=stepName,
                                                    timeInterval=timeInterval)
                                                    
                                                    
    def SetNoRequestForAllSteps(self, noRequests):
        """ set the number of history and field output requests for all steps
        """
        modelObj = self.GetModelHandle()
        stepNames = modelObj.steps.keys()
        
        # discard the 'initial' step from the list of names - the initial
        # step does not have output!
        stepNames.remove('Initial')
        
        for stepName in stepNames:
            self.SetNoRequestsForStep(stepName=stepName, noRequests=noRequests)
        
    
    
    def CreateJob(self, writeCaeFile=True, submit=False, caeFile=None,
                  homeFolder=None, scratchFolder='', userSubFile='',
                  overwrite=False, writeInput=True, parallelMethod=DOMAIN):
        """ create and submit a job
        """
        
        mod = self.GetModelHandle()
        noCpus = cpu_count()
        
        # get a folder name
        if not self.Exist(homeFolder):
            homeFolder = os.getcwd()
              
        # write the project file
        if writeCaeFile:
           
            # get a file name for the cae file
            if not self.Exist(caeFile):
                caeFile = mod.name + caeFileExtension
            
            if not caeFile.endswith(caeFileExtension):
                caeFile = caeFile + caeFileExtension
                
            # check if the cae file exists, overwrite as required
            caeFilePath = os.path.join(homeFolder, caeFile)
            if os.path.isfile(caeFilePath):
            
                if overwrite:
                    os.remove(caeFilePath)
                
                else:
                    errMsg = 'A file <{0}> already exists.'.format(caeFilePath)
                    self.ErrorHandling(errMsg)
        
            mdb.saveAs(pathName=caeFilePath)
        
        # get a name for the job/input file
        fileNameNotFound = True
        runNumber = 0
        
        while(fileNameNotFound):
            # create a job/input name
            jobFileName = 'Job_{0}_run{1}'.format(mod.name, runNumber)
            jobFilePath = os.path.join(homeFolder, jobFileName+'.'+odbFileExtension)
            inpFilePath = os.path.join(homeFolder, jobFileName+'.inp')
            
            # check if it exists
            if os.path.isfile(jobFilePath) or os.path.isfile(inpFilePath):
                runNumber += 1
            else:
                fileNameNotFound = False
        
        # work out how many domains to decompose into
        if parallelMethod==DOMAIN:
            numDomains = noCpus
        else:
            numDomains = 1
         
        # create the job
        command = 'mdb.Job(name=\'{0}\', model=\'{1}\', description=\'\', type=ANALYSIS,'+\
                  'atTime=None, waitMinutes=0, waitHours=0, queue=None,'+\
                  'explicitPrecision=explicitPrecision, nodalOutputPrecision=nodalOutputPrecision,'+\
                  'echoPrint=OFF, modelPrint=OFF, contactPrint=OFF, historyPrint=OFF,'+\
                  'userSubroutine=\'{2}\',scratch=\'{3}\',parallelizationMethodExplicit=parallelMethod,'+\
                  'numDomains=numDomains, activateLoadBalancing=False, multiprocessingMode=DEFAULT,'+\
                  'numCpus=noCpus)'

        exec(command.format(jobFileName, mod.name, userSubFile, scratchFolder))
        
        # write input if required
        if writeInput:
            mdb.jobs[jobFileName].writeInput(consistencyChecking=OFF)
        
        # submit if required
        if submit:
            mdb.jobs[jobFileName].submit(consistencyChecking=OFF)
            
        return jobFileName
        
                               
                               
        
