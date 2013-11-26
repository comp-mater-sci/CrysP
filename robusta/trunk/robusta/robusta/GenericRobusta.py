# Prevent the legacy class type being used
__metaclass__ = type

# import configuration
from robusta.config import *

# import the Abaqus classes and modules
try:
    from abaqusConstants import *
    from abaqus import *
    
except ImportError:
    if verbose: print ('The abaqus modules could not be imported.')
    
    
# import native libraries
from warnings import warn
import os
import sys

# import third part libraries
try:
    import numpy as np
except ImportError:
    print 'Numpy needs to be available.'
    raise


class GenericRobusta():
        
    """
    An abstract base class which will contain all template model data and accessor
    methods. Data is loaded from text files by calling the LoadData method.
    """
    
    # global objects available to all objects derived from this class:
    thisClassData = {}

    
    def __init__(self, modelName, dbaseType='model', folder=os.getcwd()):
        """ Constructor

            All the parameters and data are stored in a python dictionary called
            "thisObjectsData", with a corresponding dictionary called
            "thisObjectsDefaults" for corresponding default data all native
            Abaqus objects (eg. part objects) are stored in a dictionary called
            "thisClassData"
        """
        self.thisObjectsData = {'myName':modelName}
        self.thisObjectsDefaults = {}
        
        # if the inherited class is associated with models, check if the model exists
        if dbaseType=='model':
            if modelName in mdb.models.keys():
                # store model details
                self.SetValue('modelName', modelName)
                
            else:
                # if the model object has not been found try to open it
                filePath = os.path.join(folder, modelName+caeFileExtension)
                try:
                    openMdb(filePath)
                except IOError:
                    errorMessage = 'Model name {0} is not found in the current database, nor could a cae file be found.'.format(modelName)
                    self.ErrorHandling(errorMessage, errType='key')
                else:
                    self.SetValue('modelName', modelName)
                    
        
        # else if the class is associated with odbs, check if the odb exists, or can be opened
        elif dbaseType=='odb':
            filePath = os.path.join(folder, modelName+odbFileExtension)
            
            # if the odb is already open, associate with that
            if filePath in session.odbs.keys():
                self.SetValue('odbName', modelName)
                self.SetValue('odbPath', filePath)
            
            # otherwise try to open it looking in the given folder (or cwd if no
            # folder is given)
            elif os.path.isfile(filePath):
                session.openOdb(filePath)
                self.SetValue('odbName', modelName)
                self.SetValue('odbPath', filePath)
                
            else:
                errMsg = 'No odb was found on path <{0}>.'.format(filePath)
                self.ErrorHandling(errMsg)
                
        elif dbaseType=='hdf':
            # this type of object is not associated with an odb or mdb 
            self.SetValue('hdfFileName', modelName)
        
        elif dbaseType=='generic':
            # this type is not a database per se
            self.SetValue('name', modelName)
        
        else:
            errMsg = '<{0}> is an unknown type for Robusta classes.'.format(dbaseType)
            self.ErrorHandling(errMsg)
                
    
    def GetOdbHandle(self):
        """ return an alias to the odb
        """
        return session.odbs[self.GetValue('odbPath')]
    
    
    def GetModelHandle(self):
        """ return an alias to the model in the abaqus mdb to which this object
            is related
        """
        return mdb.models[self.GetValue('modelName')]
            
        
    def ErrorHandling(self, message, errType='general', warningOnly=False):
        """ check if errors should be suppressed
        """
        if suppressErrors or warningOnly:
            warn(message)
        else:
            if errType=='general':
                raise Exception(message)
            elif errType=='name':
                raise NameError(message)
            elif errType=='key':
                raise KeyError(message)
            elif errType=='type':
                raise TypeError(message)
            elif errType=='value':
                raise ValueError(message)
            elif errType=='io':
                raise IOError(message)                
                
            # default back to general exception
            else:
                print 'Unknown error type <{0}>.'.format(errType)
                raise Exception(message)
            
            
    def IsGlobal(self, parameterName):
        """ return True if the given name exists in the list of global parameters
        """
        
        return parameterName in self.thisClassData.keys()
        
    
    def IsDefault(self, parameterName):
        """ return True if the given name exists in the list of default parameters
        """
        
        return parameterName in self.thisObjectsDefaults.keys()
       
       
    def AreAllTheseParmetersDefined(self, trialList):
        """ check if each of the parameters named in the list are defined
        """
        if type(trialList)==list:
            allDefined = True
            
            for parameter in trialList:
                allDefined = allDefined and self.IsParameter(parameter)
            
            return allDefined
            
        else:
            errMsg = 'trialList should be a Python list.'
            self.ErrorHandling(errMsg)
     
        
    def IsParameter(self, parameterName):
        """ return True if the given name exists in the list of object parameters
        """
        
        return parameterName in self.thisObjectsData.keys()
        
        
    def GetGlobal(self, parameterName):
        """
        Return a value from the "thisClassData" - this is a globally accessible
        dictionary, but it is intended to be used only for storing references
        to class-wide accessible data
        """
        
        globalValue = self.thisClassData.get(parameterName)
        
        if not self.Exist(globalValue):
        
            errorMessage = 'A value was not found for the global parameter <{0}>'.format(parameterName)
            self.ErrorHandling(errorMessage, errType='key')
        else:
            return globalValue


    def SetGlobal(self, parameterName, parameterValue):
        """
        Store a value in the "thisClassData" - this is a globally accessible
        dictionary, but it is intended to be used only for storing references to
        class-wide accessible data
        """
        self.thisClassData.update({parameterName:parameterValue})

    
    def GetValue(self, parameterName, checkConfigFile=True, suppressError=False):
        """
        returns the parameter value specified by "parameterName". If the parameter
        doesn't exist a default value is used. If no identically named default
        has been defined, a default is looked for in the config file, based
        on the an exception is raised.
        """
        
        # check if parameter exists
        if not self.Exist(self.thisObjectsData.get(parameterName)):
        
            # it not, check if it has a default value
            if not self.Exist(self.thisObjectsDefaults.get(parameterName)):
                
                # if not check if it has a default defined in the config file
                if checkConfigFile:
                    self.CheckConfigFileForParameters(requirementList=[parameterName], typeCheck=False)
            
            # check again if it has a default value
            if not self.Exist(self.thisObjectsDefaults.get(parameterName)) and not suppressError:                
                errorMessage = 'A default or user defined value was not found for <{0}>'.format(parameterName)
                self.ErrorHandling(errorMessage, errType='key')
                
            # if it has a default value now, use this
            else:
                self.thisObjectsData.update({parameterName:(self.thisObjectsDefaults[parameterName])})
        
        return self.thisObjectsData.get(parameterName)
        
        
    def SetValue(self, parameterName, parameterValue):
        """
        sets the parameter value specified by "parameterName". If such parameter
        name does not exist, it creates one and appends it to the object data
        dictionary (a warning may be given when this happens)
        """

        if not self.Exist(self.thisObjectsData.get(parameterName)) and verbose:
            print '{0}: parameter <{1}> created.'.format(self.thisObjectsData['myName'], parameterName)
    
        self.thisObjectsData.update({parameterName:parameterValue})


    def GetDefault(self, parameterName):
        """
        Specifically gets the original default value (rather than the value from
        the current object data)
        """
        defaultValue = self.thisObjectsDefaults.get(parameterName)
        if not self.Exist(defaultValue):
            errorMessage = 'A default or user defined value was not found for <{0}>'.format(parameterName)
            self.ErrorHandling(errorMessage, errType='key')
        else:    
            return defaultValue


    def SetDefault(self, parameterName, parameterValue):
        """
        Add the given data to the object 'defaults' dictionary. The idea is that
        parameters only get added to this dictionary once. So constructor data
        goes here, along with defaults for other parameters.
        
        Currently this method only handles one parameter at a time!
        """
        
        if self.IsDefault(parameterName):
            errorMessage = 'Cant overwrite existing default value <{0}> for parameter <{1}>.'.format(self.GetDefault(parameterName),parameterName)
            self.ErrorHandling(errorMessage, errType='value')
        
        self.thisObjectsDefaults.update({parameterName:parameterValue})
    
    
    def AppendDefault(self, parameterName, parameterValue):
        """ Add the data to the existing data rather than overwrite
        """
        exisitingData = self.thisObjectsDefaults.get(parameterName)
        exisitingType = type(exisitingData)
        newType = type(parameterValue)
        
        # if the parameter doesn't already exist simply add it
        if not self.Exist(exisitingData):
        
            self.SetDefault(parameterName, parameterValue)
            if verbose: print 'AppendDefault was used to add a new parameter <{0}>. Should SetDefault not have been used?'.format(parameterName)
        
        # if it does exist, but is of a different type, raise an exception    
        elif not newType==exisitingType:
            
            errorMessage = 'You cant add this data to {0} because they have different types ({1} & {2}).'.format(parameterName, exisitingType, newType)
            self.ErrorHandling(errorMessage, errType='type')
            
        # if it exists, and is of the same type, attempt to append it, depending
        # on the basic type it is (list, int, str, etc.)
        else:
            if exisitingType==list:
                exisitingData.append(parameterValue)

            elif exisitingType==str:
            
                exisitingData += parameterValue
                
            else:
                errorMessage = 'Dont know how to append type {0} to {1} for parameter {2}.'.format(newType, exisitingType, parameterName)
                self.ErrorHandling(errorMessage, errType='type')
            
        
        
    def LoadParameters(self, fileName, folder=''):
        """
        Load the contents of a given text file, interpret the lines starting
        with a "#" as comments and "!" as global variables.
        
        TO DO: make this loading routine more secure. It uses exec, which is
        "dangerous"!!
        """
        
        if verbose: print('reading from file: '+fileName)
        self.SetValue('DataFileName',fileName)
        self.SetValue('DataFolder',folder)

        currentFile = open(os.path.join(folder,fileName),'r')
        currentLine = ''
        EOF = False
        counter = 0
        loadedData = {}
        updateCommand = 'pass'

        # read a line of text from the file until the EOF is reached
        while not(EOF):
            currentLine = currentFile.readline()
            currentLine = currentLine.strip()
            counter+=1

            if (currentLine):
                # if the data line begins with '&' the variable is flagged as a global
                if (currentLine.startswith('&')):
                    currentLine = currentLine[1:len(currentLine)]
                    updateCommand = 'self.SetGlobal('+currentLine+')'

                # if the data line begins with '#' it is a comment line
                elif ((len(currentLine)<5) or (currentLine.startswith('#'))):
                    if verbose:
                        print('ignored line {0}: empty or comment line'.format(counter))
                    else:
                        sys.stdout.write('X')
                    updateCommand = 'pass'

                else:
                    sys.stdout.write('.')
                    updateCommand = 'self.SetValue('+currentLine+')'

                # update the user or global data accordingly
                try:
                    exec(updateCommand)
                except():
                    warnings.warn('problem reading file <'+fileName+'> line '+counter)
            else:
                EOF = True
    
        currentFile.close()
    
            
    def Exist(self, parameter):
        """
        returns true if the parameter is not equal to 'None' (the python null type)
        """
        
        if parameter == None:
            return False
        else:
            return True
            
            
    def CheckForNullParameters(self, parameterValueList, parameterNameList):
        """ check if any of the objects in the given list are == 'None'. If so,
            check if any default values exist for this parameter with the same name
            and type (as defined by the parent objects 'parametersRequired' variable).
            
            The (possibly) updated parameter list is returned.
            
            If all else fails, raise an exception.
        """
        
        updatedParameterList = []
        noParms = len(parameterValueList)
        
        for index in range(noParms):
            # get next parameter name and value
            parameterValue = parameterValueList[index]
            parameterName = parameterNameList[index]
            
            # if the parameter has a value of 'None', search for a default
            if not self.Exist(parameterValue):
                if verbose: print 'No value found for <{0}>, looking for default.'.format(parameterName)
                self.CheckConfigFileForParameters()
                updatedParameterList.append(self.GetDefault(parameterName))
            
            # else just copy the given value
            else:
                updatedParameterList.append(parameterValue)
            
        return updatedParameterList       
            
            
    def CheckConfigFileForParameters(self, requirementList=None, overwriteExisting=False, typeCheck=True):
        """ search the config file for suitable defaults (ie. the parameter will
            already exist as a variable in the current namespace
        """
        # get the requirements list
        if not self.Exist(requirementList):
            requirementList = self.GetValue('parametersRequired')

        # check for each named parameter if a variable exists in the config file
        for requirement in requirementList:
            
            # get the current requirements
            parameterName = requirement[0]
            parameterType = requirement[1]
            parameterSize = requirement[2]
            warnMessage = 'No default value for {0} in config file.'.format(parameterName)
            
            checkIfDefinedAndGetType = 'definedType = type({0})'.format(parameterName)
            getSize = 'definedSize = len({0})'.format(parameterType)
            getValue = 'definedValue = {0}'.format(parameterSize)
            
            try:
                exec(checkIfDefinedAndGetType)
                exec(getSize)
                exec(getValue)
                
            except NameError:
                # a NameError exception will be raised if the parameter is not yet defined
                if verbose:
                    print warnMessage
                else:
                    pass
                
            else:
                # store any parameter, found in the config file, if:
                #     - the parameter is of the required type and size (OR type
                #       checking is not required)
                # AND - the parameter doesn't have some value already (OR overwriting
                #       is allowed)
                
                if ((definedType==parameterType and definedSize==parameterSize) \
                    or not typeCheck)  and  ((self.IsDefault(parameterName) and \
                    overwriteExisting) or not (self.IsDefault(parameterName))):
                    self.SetDefault(parameterName, definedValue)
                    
                elif verbose:
                    print warnMessage
                    
    def GetIndexForDirection(self, direction):
        """ return 0,1,2 corresponding to x, y, z
        """
        
        if direction=='X' or direction=='x':
            return 0
        elif direction=='Y' or direction=='y':
            return 1
        elif direction=='Z' or direction=='z':
            return 2
        else:
            self.ErrorHandling('axis or direction <{0}> is unknown.'.format(direction))
        
    
    def CheckParListSizeType(self, parList, parNameList, parSizeList, parTypeList):
        
        noPars = len(parList)
        
        for index in range(noPars):
            
            par = parList[index]
            parName = parNameList[index]
            parSize = parSizeList[index]
            parType = parTypeList[index]
            self.CheckParameterSizeType(par, parName, parSize, parType)
        
        
    def CheckParameterSizeType(self, par, parName, parSize, parType):
        """ check the given parameter is of the correct size and type
        """
        errMsg = None
        
        # check the type
        if not type(par)==parType:
            errMsg = 'parameter <{0}> needs to be <{1}> not <{2}>.'.format(parName, parType, type(par))
            
        # check the size
        elif self.Exist(parSize):
            # if the size is given as a tuple, assume the parameter is a numpy type
            if type(parSize)==tuple:
                    if self.Exist(parSize[0]):
                        try:
                            if not par.shape==parSize:
                                errMsg = 'parameter <{0}> needs to have shape <{1}>, not <{2}>.'.format(parName, par.shape, parSize)
                        except AttributeError:
                            errMsg = 'If parSize is given as a tuple, par needs to be a numpy type.'

            # otherwise just use len() to check the size
            elif not len(par)==parSize and self.Exist(parSize):
                errMsg = 'parameter <{0}> needs to have a length of <{1}>, not <{2}>.'.format(parName, parSize, len(par))
                
        if self.Exist(errMsg):
            self.ErrorHandling(errMsg)
            
            
    def StoreAllParametersFromDict(self, parameterDict):
        """ internally store all the values in the given dictionary
        """
                
        if not type(parameterDict)==dict:
            errMsg = 'parameterDict must be a Python dictionary.'
            self.ErrorHandling(errMsg)
            
        parameterNames = parameterDict.keys()
        
        for parameter in parameterNames:
            self.SetValue(parameter, parameterDict[parameter])
            
    
                  
    def TearDown(self):
        """ delete the abaqus model object
        """
        del mdb.models[self.GetValue('modelName')]

        del mdb.jobs[self.GetValue('jobFileName')]
        
        
    def CheckNameAndCreateExplicitStep(self, name, duration=1.):
        """ check if the given name can be used for a new step, and slot it
            in after the latest step
        """
        
        # see if step name is ok and create a new step
        modelObj = self.GetModelHandle()
        stepObj = modelObj.steps                
        stepNameList = stepObj.keys()
        stepNameList.remove('Initial')
        
        if stepNameList is None or stepNameList==[]:
            modelObj.ExplicitDynamicsStep(name=name, previous='Initial',
                                          timePeriod=duration)
        
        else:
            if name in stepNameList:
                errMsg = 'Step with name {0} already exists'.format(stepName)
                self.ErrorHandling(errMsg)
            
            # find the name of the last step
            previousStepLinks = [stepObj[stepName].previous for stepName in stepNameList]
            lastStepName = [stepName for stepName in stepObj.keys() if not stepName in previousStepLinks][0]
                
            modelObj.ExplicitDynamicsStep(name=name, previous=lastStepName,
                                          timePeriod=duration)

