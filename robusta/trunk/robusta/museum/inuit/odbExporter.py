# Prevent the legacy class type being used
__metaclass__ = type

# import the Abaqus classes and modules
try:
    from abaqusConstants import *
    from abaqus import *
except ImportError:
    print 'The abaqus modules need to be accessible for this module.'
    raise
 
# import native libraries
import os
import sys

# import robusta classes
from robusta.GenericRobusta import *
from robusta.config import *
from robusta.inuit.FlatODB import *

# import third party modules
try:
    import h5py
except ImportError:
    print 'This module requires the H5py and HDF5 libraries to be accessible.'
    raise


class odbExporter(GenericRobusta):
 
    """ class to export data from the Abaqus ODB to a HDF file whose structure is
        defined by the FlatODB class
    """
    
    def __init__(self, odbName, folder=os.getcwd(), hdfFileName=None):
        """ Constructor
        """
        
        # call the inherited init method
        GenericRobusta.__init__(self, modelName=odbName, folder=folder, dbaseType='odb')
        
        # create the "FlatODB" version of the hdf file
        if not self.Exist(hdfFileName):
	        hdfFileName = odbName
        
        hdfObj = FlatODB(hdfFileName)
        self.SetValue('hdfObj', hdfObj)


    def ExportMaterialFieldOutputs(self, matInstanceName, jobID, ignoreVars=[], ignoreSteps=[], autoDiscard=True):
        """ export the variables listed
        
            as detailed in the FlatODB doc strings, the FlatODB expects only a single
            instance for the material data. Infact, in the interest of simplicity, it
            doesn't explictly distinguish between instances at all.
            
            the list 'ignoreVars' specifies which variables not to export
            
            Rather than using the Abaqus step[..].frame[..] the FlatOdb just works with
            an absolute (simulation) time value. Again, simplicity is the goal.
            
            Steps can be ignored by specifying 'ignoreSteps=['Initial',...etc]'. This is useful
            for eliminating steps which are present for numerical or convenience purposes
            rather than being 'true' simulation purposes. If this option is used, time is
            counted from zero for the first non-ignored step, and continues incrementally
            for each unignored step, as if the ignored steps did not exist.
            
            Otherwise, if a step name starts with '_setup_' it will be ignored by
            default. This behaviour can be disabled by setting autoDiscard=False
        """
        # aliases for hdf file and Abaqus odb
        flatOdb = self.GetValue('hdfObj')
        odb = self.GetOdbHandle()
        instanceList = odb.rootAssembly.instances
        stepObj = odb.steps
                
        # check if the given instance name exists in this odb
        if not matInstanceName in instanceList.keys():
            errMsg = 'There is no instance <{0}> in odb <{1}>.'.format(matInstanceName, self.GetValue('odbName'))
            self.ErrorHandling(errMsg)
            
        instanceObj = odb.rootAssembly.instances[matInstanceName]
        
        # Get the step list
        allStepList = [stepName for stepName in odb.steps.keys() if not stepName in ignoreSteps]
        if autoDiscard:
            stepList = [stepName for stepName in allStepList if not stepName.startswith('_setup_')]
        else:
            stepList = allStepList
            
        # go through each unignored step and export
        currentTime = 0.
        for stepName in stepList:
            
            # Get the variable list
            stepObj = odb.steps[stepName]
            fieldObj = stepObj.frames[0].fieldOutputs
            allVars = fieldObj.keys()
            fullVarList = [var for var in allVars if not var in ignoreVars]
            
            # remove variables which arent relevant for the given instance
            varList = []            
            for var in fullVarList:
                varObj = fieldObj[var].values
                instanceList = [value.instance.name for value in varObj]
                if matInstanceName in instanceList: varList.append(var)

            
            # for each var determine if it is a nodal or element quantity
            # values[x].locations[y].position returns an Abaqus symbolic constant
            # which describes the source (NODAL, INTEGRATION_POINT, etc.)
            sourceType = {}
            varTypes = {}
            noVars = len(varList)
            
            for var in varList:
                varObj = fieldObj[var].values[0]
                sourceType.update({var:str(fieldObj[var].locations[0].position)})
                varTypes.update({var:str(varObj.type)})
                
            
            # for each frame
            noFrames = len(stepObj.frames)
            for index in range(noFrames):
                
                # get the time value for this frame
                currentFrame = stepObj.frames[index]
                currentTime = currentTime + currentFrame.frameValue
                
                # export the fields
                if verbose: print '\nExporting frame {0}/{1}, for time = {2}'.format(index, noFrames, currentTime)
                sys.stdout.write('['+('-'*noVars)+']\r[')
                
                for var in varList:
                    sys.stdout.write('+')
                
                    # aliases
                    varObj = currentFrame.fieldOutputs[var]
                    varValues = varObj.values
                    source = sourceType[var]
                    
                    # get the variable data and names for the data components
                    varComponentData = np.array([value.data for value in varValues])
                    varComponentNames = self.GetVarComponents(varName=var, varType=varTypes[var])
                    noComponents = len(varComponentNames)
                    
                    # determine node or element labels
                    if source=='NODAL':
                        idList = np.array([value.nodeLabel for value in varValues])
                    elif source=='INTEGRATION_POINT':
                        idList = np.array([value.elementLabel for value in varValues])
                    else:
                        errMsg = 'Dont know how to get id values for variable of type <{0}>.'.format(source)
                        self.ErrorHandling(errMsg)
                    
                    # create a boolean mask to allow data not associated with
                    # the given instance name to be ignored
                    instanceList = np.array([value.instance.name for value in varValues])
                    instanceMask = instanceList==matInstanceName
                    ids = idList[instanceMask]
                    
                    # write the data to the HDF file
                    # scalars have a slightly different data structure than vectors
                    # or tensors in abaqus, hence this check for numbers of components
                    
                    # SCALAR
                    if noComponents == 1:
                        # get the component data
                        dataCol = varComponentData[instanceMask]
                        dataCol.reshape(len(dataCol),1)
                                               
                        # write it
                        flatOdb.BulkStoreFieldVariable(varName=varComponentNames[0],
                                time=currentTime, jobID=jobID, sourceType=source,
                                nativeIDList=ids,  dataColumn=dataCol)
                    
                    # VECTOR, TENSOR, etc.
                    else:
                        for componentIndex in range(noComponents):
                        
                            # get the component data
                            dataCol = varComponentData[instanceMask,componentIndex]
                            componentName = varComponentNames[componentIndex]
                            
                            # write it
                            flatOdb.BulkStoreFieldVariable(varName=componentName,
                                    time=currentTime, jobID=jobID, sourceType=source,
                                    nativeIDList=ids,  dataColumn=dataCol)
    

    def GetVarComponents(self, varName, varType):
        """ expand the named variable to individually named components, depending
            on the given type (types are the Abaqus constants)
        
            eg. U is 
        """
        
        # create the list
        if varType==TENSOR_3D_FULL or varType=='TENSOR_3D_FULL':
            nameList = [varName+'11', varName+'22', varName+'33',
                        varName+'12', varName+'13', varName+'23']
                     
        elif varType==VECTOR or varType=='VECTOR':
            nameList = [varName+'1', varName+'2', varName+'3']
            
        elif varType==SCALAR or varType=='SCALAR':
            nameList = [varName]
            
        else:
            errMsg = 'variable <{0}>: Dont know how to name the components for variable type <{1}>.'.format(varName, varType)
            self.ErrorHandling(errMsg)

        # check if there are any forward slashes in the names, these need to be
        # replaced because they are interpreted as subgroup references in hdf5
        return [name.replace('/','-') for name in nameList]
