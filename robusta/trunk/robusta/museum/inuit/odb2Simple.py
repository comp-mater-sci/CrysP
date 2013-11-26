# Prevent the legacy class type being used
__metaclass__ = type

# import the Abaqus classes and modules
try:
    from abaqusConstants import *
    from abaqus import *
    import numpy as np
except ImportError:
    print 'The abaqus modules need to be accessible for this module.'
    raise
 
# import native libraries
import os
import sys
import time

# import robusta classes
from robusta.GenericRobusta import *
from robusta.config import *
from robusta.inuit.simpleODB import *

# import third party modules
try:
    import h5py
except ImportError:
    print 'This module requires the H5py and HDF5 libraries to be accessible.'
    raise


class odb2Simple(GenericRobusta):
 
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
        
        hdfObj = simpleODB(hdfFileName)
        self.SetValue('hdfObj', hdfObj)
        
        
    def ExportNodeSets(self, ignoreSets=[], ignorePattern=None):
        """ export the named sets from the odb, ignore those listed in ignoreSets
        
            if ignorePattern is specified, any set containing that pattern in its
            name is ignored
        """
        # get abaqus and hdf file aliases
        odb = self.GetOdbHandle()
        hdfObj = self.GetValue('hdfObj')
        instanceObj = odb.rootAssembly.instances
        
        # get the node sets associated with instances
        for instanceName in instanceObj.keys():
        
            instance = instanceObj[instanceName]
            
            # get the node set names
            if self.Exist(ignorePattern):
                setNames = [setName for setName in instance.nodeSets.keys() \
                            if not setName in ignoreSets and\
                            not ignorePattern in setName]
            else:
                setNames = [setName for setName in instance.nodeSets.keys() \
                            if not setName in ignoreSets]
            
            if not setNames==[]:                
                # build a list of numpy arrays containing the node labels corresponding
                # to each set name
                nodeSetLabelList = []
                for setName in setNames:
                    nodeLabels = [node.label for node in instance.nodeSets[setName].nodes]
                    nodeSetLabelList.append(np.array(nodeLabels).reshape(len(nodeLabels),1))
                                
                # add the sets to the instance data in the hdf file
                hdfObj.AddInstanceNodeSets(instanceName=instance.name,
                       nodeSetNameList=setNames, nodeSetLabelList=nodeSetLabelList)
        
        
        
#    def ExportNodalConnectivities(self, instanceName, ignoreSteps=[]):
#        """ exports the mesh in terms of nodal connectivies.
#        
#            The number of the array row corresponds to the node label, and the 8
#            positions in the row correspond to the node labels connected to this
#            node in the mesh
#            
#            abaqus represents the mesh in the ODB from the point of view of
#            elements and the nodes they contain. The list of nodes to which a
#            given node is connected has to be deduced by ???
#            
#        """
#        # get abaqus and hdf file aliases
#        odb = self.GetOdbHandle()
#        hdfObj = self.GetValue('hdfObj')
#        instanceObj = odb.rootAssembly.instances[instanceName]
#        
#        # ????
    
    def ExportNodeCoords(self, instanceName):
        """ call the node coord export routine in simpleODB
        """
        odb = self.GetOdbHandle()
        hdfObj = self.GetValue('hdfObj')
        
        # get node coords
        instanceObj = odb.rootAssembly.instances[instanceName]
        nodeCoords = [node.coordinates for node in instanceObj.nodes]
        
        # store them in the hdf file
        hdfObj.AddInstanceNodalCoors(instanceName, nodeCoords)
       

    def ExportFieldVarsForInstance(self, instanceName, ignoreSteps=[], ignoreVars=[]):
        """ exports all of the field variables in the odb, for the given
            instance
        
            ignoreSteps gives the names of steps not to export
            ignoreVars gives the names of variables not to export
        """
        # define variable types
        odb = self.GetOdbHandle()
        hdfObj = self.GetValue('hdfObj')
        varTypes = ['unknown', SCALAR, 'unknown', VECTOR, 'unknown', 'unknown', TENSOR_3D_FULL]
        
        # get the list of variables and stepNames
        varNameList = self.GetFieldVarList(ignoreVars, ignoreSteps)
        stepNameList = self.GetStepList(ignoreSteps)

        # for each variable
        for varName in varNameList:

            # get an aggregated array for the data
            [dataArray, timeLabels] = self.GetAggregateDataArray(instanceName, varName, stepNameList)
            
            # split the data into components
            noComponents = dataArray.shape[2]     
            componentNames = self.GetVarComponentNames(varName, varTypes[noComponents])
            
            # write to the hdf file
            for index in range(noComponents):
                hdfObj.AddInstanceScalarData(instanceName=instanceName,
                       dataName=componentNames[index], scalarDataArray=dataArray[:,:,index],
                       timeLabels=timeLabels, overwrite=False)
            
            
    def GetAggregateDataArray(self, instanceName, varName, stepNameList):
        """ return a 3D numpy array for the given data, and a list of time values
            corresponding to the steps/frames from which the data came.
            
            the rows of this array correspond to node labels, ie. row X corresponds
            to node label X
            
            the columns of this array correspond to absolute values of simulation
            time, as detailed in the returned list timesVsCols
        """
        odb = self.GetOdbHandle()
        instanceObj = odb.rootAssembly.instances[instanceName]
        
        # expand the var name list to scalar values only, eg. U gets
        # expanded to U1, U2, U3. S gets expanded to S11, S22, S33,
        # S12, S13, S23, etc.
        varComponentNames = self.ExpandVar(varName, stepNameList)
        
        # determine if data is not nodal data (ie extrapolation to nodes is needed)
        dataType = odb.steps[stepNameList[0]].frames[0].fieldOutputs[varName].values[0].position
        if dataType==INTEGRATION_POINT:
            dataPosition = ELEMENT_NODAL
            trueNodalData = False
            
        elif dataType==NODAL:
            dataPosition = NODAL
            trueNodalData = True
            
        else:
            errMsg = 'Dont know how to handle data which is available for position <{0}>.'.format(dataType)
            self.ErrorHandling(errMsg)


        # reserve an array for the returned data. Node 1 goes in row one, node 2
        # in row 2, etc.
        noRows = len(instanceObj.nodes) + 1
        
        exampleColData = odb.steps[stepNameList[0]].frames[0].fieldOutputs[varName].values[0].data
        noColsInValueData = self.GetDataLength(exampleColData)
                    
        noFramesTotal = np.sum([len(odb.steps[stepName].frames) for stepName in stepNameList])
        dataArray = np.zeros((noRows, noFramesTotal, noColsInValueData))
        timeList = []
        frameIndex = 0
                
        # go through each step in which the variable is found
        print '\nExporting <{0}> for {1} frames.'.format(varName, noFramesTotal)
        for stepName in stepNameList:
            
            # go through each frame
            frameObj = odb.steps[stepName].frames
            
            sys.stdout.write('\n['+('-'*noFramesTotal)+']\r[')
            
            for frame in frameObj:
                sys.stdout.write('+')
                
                # get the data and node labels (the list comprehension is slow!!)
                fieldObj = frame.fieldOutputs[varName].getSubset(position=dataPosition, region=instanceObj)
                values = [[value.nodeLabel, value.data] for value in fieldObj.values]

                                
                # if the data is originally nodal data, then each value is unique
                # and can be taken as is
                if trueNodalData:
                    nodeLabels = [val[0] for val in values]                    
                    data = np.array([val[1] for val in values])
                    
                    # 1D arrays should be made into nx1 2D arrays
                    if len(data.shape)==1:
                        data = data.reshape(len(data),1)
                                        
                # otherwise the data was projected to the nodes, and is given on
                # an element by element basis, with no expectation that the values
                # for the shared nodes are the same from one element to the next
                else:
                    # get the unique node label list, and the sets of indices where
                    # each given node value occurs in the data
                    
                    # ----------- first attempt, very very slow for loop (for row in ...)
#                    allLabels = np.array([val[0] for val in values])
#                    nodeLabels = np.unique(allLabels)
#                    noNodes = len(nodeLabels)
#                    
#                    rawData = np.vstack([val[1] for val in values])
#                    
#                    if len(rawData.shape)==1:
#                        data = np.zeros((noNodes, 1))
#                    else:
#                        data = np.zeros((noNodes, rawData.shape[1]))
#                    
#                    for row in range(noNodes):
#                        # get a boolean mask for the data rows corresponding
#                        # to a given node label (eg. there will be between 3 and
#                        # 8 shared elements for each node with C3D8R elements)
#                        label = nodeLabels[row]
#                        mask = allLabels==label
#                        
#                        # add the averaged data to the main data array
#                        data[row,:] = np.average(rawData[mask,:], axis=0)

                    # ----------- second attempt
                    # get sorted node labels and list of unique node labels
                    allLabels = np.array([val[0] for val in values])
                    uniqueLabels = np.unique(allLabels)
                    sortIndices = np.argsort(allLabels)
                    
                    # get the data projected to the nodes, without resorting to
                    # a python for loop
                    rawData = np.array([val[1] for val in values])
                    sortedData = rawData[sortIndices, :]
                    
                    
                        
                        
                # copy the data to the main array, with row number = node number
                dataArray[nodeLabels, frameIndex, :] = data
                
                # loop over
                if timeList==[]:
                    timeList = [0.]
                else:
                    timeList.append(timeList[-1] + frame.frameValue)
                frameIndex += 1
                
                
        return (dataArray, timeList)
        
                
            
    def GetVarObjWithinSteps(self, varName, stepNameList):
        """ return a reference to a fieldOutput object for the given variable name
        """
        
        odb = self.GetOdbHandle()
        stepNo = 0
        notFound = True
        
        # search through the steps until an example of the variable is found
        while(notFound and stepNo<=len(stepNameList)):
            stepObj = odb.steps[stepNameList[stepNo]]
            fieldObj = stepObj.frames[0].fieldOutputs
            
            if varName in fieldObj.keys():
                notFound = False
                varObj = fieldObj[varName]
            else:
                stepNo += 1
        
        # if the variable was not found, raise an exception
        if notFound:
            errMsg = 'variable <{0}> not found in the list of steps <{1}>.'.format(varName, stepNameList)
            self.ErrorHandling(errMsg)
            
        # otherwise return the object reference
        else:    
            return varObj

    
    def ExpandVar(self, varName, stepNameList):
        """ expand a non-scalar variable name into a list of names of its
            scalar components
        """
        varObj = self.GetVarObjWithinSteps(varName, stepNameList)

        return self.GetVarComponentNames(varName, varObj.values[0].type)
    
    
    def GetStepList(self, ignoreSteps):
        """ return a list of steps
        """
        odb = self.GetOdbHandle()
        
        return [stepName for stepName in odb.steps.keys() if not stepName in ignoreSteps]
        
    
    def GetFieldVarList(self, ignoreVars, ignoreSteps):
        """ return the list of field vars present in the odb
        
            its possible that different steps have different field vars stored
        """   
        odb = self.GetOdbHandle()
        fullVarList = []
        
        # get a list of step names
        stepNameList = self.GetStepList(ignoreSteps)
        
        # for each step check the variables available
        for stepName in stepNameList:
            
            varList = odb.steps[stepName].frames[0].fieldOutputs.keys()
            
            for var in varList:
                if not var in fullVarList and not var in ignoreVars:
                    fullVarList.append(var)
                    
        return fullVarList
                
    

    def GetVarComponentNames(self, varName, varType):
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
        
        
    def GetDataLength(self, data):
        """ returns the length of the data, for both the case where the data is
            just a single float/str/etc or a list or numpy array
        """
        typesWithNoLen = [int, float, bool]
        
        if type(data) in typesWithNoLen:
            return 1
        else:
            return len(data)
