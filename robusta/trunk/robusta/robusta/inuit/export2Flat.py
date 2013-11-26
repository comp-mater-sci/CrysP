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

# import robusta classes
from robusta.GenericRobusta import *
from robusta.inuit.FlatODB import *
from robusta.config import *

# import third party modules
try:
    import numpy as np
except ImportError:
    print 'This module requires numpy to be accessible.'
    raise


class export2Flat(GenericRobusta):

    def __init__(self, odbName, runName, entityName, mode='append', hdfFileName=None, folder=os.getcwd()):
        """ constructor
        """
        
        GenericRobusta.__init__(self, modelName=odbName, dbaseType='odb', folder=folder)
        
        # set up the hdf database
        self.SetValue('runName', runName)
        self.SetValue('entityName', entityName)
        
        if hdfFileName is None:
            hdfFileName = odbName

        hdfObj = FlatODB(hdfFileName=hdfFileName, folder=folder, mode=mode)
        self.SetValue('hdfFileName', hdfFileName)
        self.SetValue('hdfObj', hdfObj)
        
    
    def Close(self):
        """
        """
        hdfObj = self.GetValue('hdfObj')
        hdfObj.Close()
        
        odb = self.GetOdbHandle()
        odb.close()
        
        
    def ExportAllNodeSetsForInstance(self, instanceName, excludeSetNameList=[]):
        """
        """
        # get node set name list
        odb = self.GetOdbHandle()
        nodeSetNameList = [setName for setName in odb.rootAssembly.instances[instanceName].nodeSets.keys() \
                           if not setName in excludeSetNameList]
        
        # export each set
        for nodeSetName in nodeSetNameList:
            self.ExportNodeSet(nodeSetName, instanceName)
    
    
    def ExportRootAssemblyNodeSets(self, excludeSetNameList=[]):
        """
        """
        # get node set name list
        odb = self.GetOdbHandle()
        nodeSetNameList = [setName for setName in odb.rootAssembly.nodeSets.keys() \
                           if not setName in excludeSetNameList]
        
        # export each set
        for nodeSetName in nodeSetNameList:
            self.ExportNodeSet(nodeSetName)          
                   
    
    def ExportElementConnectivtyAsNodeSet(self, mdbName, modelName, partName, folder=os.getcwd()):
        """ exports the element connectivity as a node set (2D array). The cae
            file associated with the odb is required, and the model and part
            names need to be given
        
            The format is:
            
            col0 = element label, col1 = node label1 on edge1, col2 = node
            label2 on edge1, col3 = node label1 on edge2, etc..
        """
        # open the cae file
        mdbFileName = mdbName+caeFileExtension
        filePath = os.path.join(folder, mdbFileName)
        try:
            mdb = openMdb(filePath)
        except IOError:
            errMsg = 'File < {0} > not found in folder < {1} >.'.format(mdbFileName, folder)
            self.ErrorHandling(errMsg)
        
        # check if the model and part name are correct
        if modelName in mdb.models.keys():
            if partName in mdb.models[modelName].parts.keys():
                part = mdb.models[modelName].parts[partName]
            else:
                errMsg = 'no part with name < {0} > in cae file < {1} >.'.format(partName, mdbName)
                self.ErrorHandling(errMsg)
        else:
            errMsg = 'no model with name < {0} > in cae file < {1} >.'.format(modelName, mdbName)
            self.ErrorHandling(errMsg)
        
        # get the list of element edge lists
        elements = part.elements
        edgeLists = [element.getElemEdges() for element in elements]
    
        # get a seperate list of element labels corresponding to the edgeLists
        elementLabels = [element.label for element in elements]
    
        # get the list of node label lists (nesting makes this hard to read but
        # quicker to execute. Apologies!) Should take about 2 mins for 100k elements
        nodeLabelLists = [[[node.label for node in edge.getNodes()] \
                            for edge in edgeList] for edgeList in edgeLists]
        
        # convert the nested Python lists to a single flattened numpy array
        # the shape of this array will be (noElements, noEdgesPerElement, noNodesPerEdge(=2) )
        nodeLabelArray = np.array(nodeLabelLists, dtype=np.int)
        noElements = nodeLabelArray.shape[0]
        noEdgesPerElement = nodeLabelArray.shape[1]
        noNodesPerEdge = nodeLabelArray.shape[2]
        
        # the 2D export array will have element labels as first column, followed
        # by the pairs of node labels corresponding to edges, as described in the
        # the doc string above
        noLabelsPerElement = noEdgesPerElement*noNodesPerEdge
        exportArray = np.zeros((noElements, noLabelsPerElement+1), dtype=np.int)
        exportArray[:,0] = elementLabels
        exportArray[:,1::] = nodeLabelArray.reshape(noElements, noLabelsPerElement)
            
        # write the output and close the cae file
        hdfObj = self.GetValue('hdfObj')
        runName = self.GetValue('runName')
        hdfObj.AddNodeSet(runName=runName, nodeSetName='ElementEdgeNodeLabels', nodeLabels=exportArray)
        mdb.close()
        
        
    
    def ExportNodeSet(self, nodeSetName, instanceName=None, excludeInstanceNames=None):
        """
        """
        odb = self.GetOdbHandle()
        hdfObj = self.GetValue('hdfObj')
        runName = self.GetValue('runName')
        
        # get the list of all node sets
        if instanceName is None:
            allNodeSets = odb.rootAssembly.nodeSets
        else:
            allNodeSets = odb.rootAssembly.instances[instanceName].nodeSets
        
        # check if the node set exists
        if not nodeSetName in allNodeSets:
            errMsg = 'node set with name < {0} > not found in odb root assembly.'.format(nodeSetName)
            self.ErrorHandling(errMsg)
        
        # add the labels to the hdf file
        # note the Abaqus weirdness here (the reason for this next if clause):
        # if the node set is in the rootAssembly
        # you get the nodes using   - nodeSet[nodeSetName].nodes[0][...]
        # rather than the expected  - nodeSet[nodeSetName].nodes[...]
        # in effect you get a node set inside a (single 'node') node set
        #
        # on top of this each node set in the root assembly can have a list of
        # instances associated with it (nodeSet[nodeSetName].node[x].instances
        
        nodeSet = allNodeSets[nodeSetName]
        somethingToExport = False
        
        if instanceName is None:
            # if no instance name is given, check the root assembly for node sets
            if excludeInstanceNames is None:
                nodeLabels = [node.label for node in nodeSet.nodes[0]]
                somethingToExport = True
            
            else:
                # check if any of the instances to be excluded are associated
                # with the current node set (even in the root assembly).
                # NOTE: a node set spread across more than one instance wont be
                #       exported!
                instanceNamesInNSet = [inst.name for inst in nodeSet.instances \
                                       if not inst.name in excludeInstanceNames]
                                       
                if not instanceNamesInNSet==[]:
                    nodeLabels = [node.label for node in nodeSet.nodes[0]]
                    somethingToExport = True
            
        else:
            # if an instance name is given, export the associated node set
            nodeLabels = [node.label for node in nodeSet.nodes]
            somethingToExport = True
        
        if somethingToExport:
            hdfObj.AddNodeSet(runName, nodeSetName, nodeLabels)
    
        
    def SetStepsToExtract(self, excludeStepNameList=[]):
        """ set the step names to extract
        """
        
        odb = self.GetOdbHandle()
        
        # get step names
        stepNameList = [stepName for stepName in odb.steps.keys() if not stepName in excludeStepNameList]
        self.SetValue('stepNameList', stepNameList)
        
        # get the time values associated with the step names
        times = []
        cumulativeTimeForLastFrames = 0.
        
        for stepName in stepNameList:
        
            frameObj = odb.steps[stepName].frames
            noFrames = len(frameObj)
            
            newTimes = [frame.frameValue + cumulativeTimeForLastFrames for frame in frameObj]
            times += newTimes                
            cumulativeTimeForLastFrames += sum(newTimes)
        
         
        self.SetValue('timeStamps', times)
        
        
    def EntityFromInstance(self, instanceName):
        """
        """
        hdfObj = self.GetValue('hdfObj')        
        entityName = self.GetValue('entityName')        
        
        hdfObj.CreateEntity(entityName)
        self.SetValue('instanceName', instanceName)
        
        
    def DefineXrefs(self, contiguousNodeLabels=True):
        """
        """
        odb = self.GetOdbHandle()
        hdfObj = self.GetValue('hdfObj')

        timeStamps = self.GetValue('timeStamps')
        stepNameList = self.GetValue('stepNameList')
        entityName = self.GetValue('entityName')
        instanceName = self.GetValue('instanceName')
        runName = self.GetValue('runName')
        
        # if the cross references have already been defined, this process can be skipped
        xRefsAlreadyDefined = hdfObj.CheckXrefsDefinedForRunName(runName)
        
        # get the node labels (using displacement, which is assumed to be in the ODB)
        # this step could be skipped, but needs some extra code to compensate..
        instanceObj = odb.rootAssembly.instances[instanceName]
        values = odb.steps[stepNameList[0]].frames[0].fieldOutputs['U'].getSubset(region=instanceObj).values
        noValues = len(values)
        
        # detemine element numbers for later reference
        exampleField = odb.steps[stepNameList[0]].frames[0].fieldOutputs['S'].getSubset(region=instanceObj)
        noElements = len(exampleField.values)
        noNodesPerElement = len(instanceObj.elements[0].connectivity)
        self.SetValue('noElements', noElements)
        self.SetValue('noNodesPerElement', noNodesPerElement)
        
        # likewise get info on the number of values for IP variabels projected to nodes
        extrapolatedFieldVals = exampleField.getSubset(position=ELEMENT_NODAL).values
        noNodesForExtrapField = len(extrapolatedFieldVals)
        self.SetValue('noNodesForExtrapField', noNodesForExtrapField)

        nodeLabelList = [value.nodeLabel for value in values]
        self.SetValue('nodeLabelList', nodeLabelList)
        self.SetValue('noNodes', noValues)
        
        if xRefsAlreadyDefined:
            print 'Skipping Xref creation: xrefs are already defined for < {0} >.'.format(runName)
            return
            
        else:
            
            # get the node labels for element variables projected to the nodes by Abaqus
            projectedNodeLabels = [value.nodeLabel for value in extrapolatedFieldVals]
            projectedElemLabels = [value.elementLabel for value in extrapolatedFieldVals]
            
            # get the coordinates. coordinates are stored in a different part of the odb
            # under a different order (usually contiguous)
            instanceNodeLabels = [node.label for node in instanceObj.nodes]
            # note : the following code works in the general case that
            #        node labels are not a contiguous series of numbers. Its slow.
            if not contiguousNodeLabels:
                lookupIndicies = [instanceNodeLabels.index(nodeLabel) for nodeLabel in nodeLabelList]
                
            else:
                # the code below assumes the node labels begin at one and run
                # contiguously, so that finding the indices of a simple sort will
                # do the job
                lookupIndicies = np.argsort(instanceNodeLabels)
            
            # in the following, coordArray is the node coordinate array rearranged
            # according to the order of nodes
            coordList = np.vstack([node.coordinates for node in instanceObj.nodes])
            coordArray = coordList[lookupIndicies, :]
            
            hdfObj.AddNodeLabelXRef(labelList=nodeLabelList, rowList=range(noValues),
                           entityName=entityName, runName=runName, coordArray=coordArray)                      
            hdfObj.AddColValueRef(columnValueList=timeStamps, runName=runName)

            hdfObj.AddProjIPLabelXRef(nodeLabelList=projectedNodeLabels, elementLabelList=projectedElemLabels,
                                      rowList=range(noNodesForExtrapField), entityName=entityName,
                                      runName=runName)
        
    def ExportNodalVarList(self, varList, sourceType='nodal'):
        """ export a list of nodal variables
        """
        odb = self.GetOdbHandle()
        hdfObj = self.GetValue('hdfObj')
        runName = self.GetValue('runName')
        entityName = self.GetValue('entityName')
        stepNameList = self.GetValue('stepNameList')
        
        for varName in varList:
            # get the name of the variables components
            exampleValue = odb.steps[stepNameList[0]].frames[0].fieldOutputs[varName].values[0]
            componentNames = self.GetComponentNames(exampleValue, varName)
            checkList = []
            
            for componentName in componentNames:
                checkList.append(hdfObj.CheckRunNameExistsForVarComponent(runName, componentName, entityName))
        
            # export the variable if its not already in the hdf file
            if all(checkList):
                print 'Data for variable < {0} > already exists for run < {1} >. Skipping.'.format(varName, runName)
            
            elif not all(checkList) and any(checkList):
                print 'Skipping export of variable < {0} > - Warning! some components have already been exported.'.format(varN)
            
            else:
                if sourceType=='nodal':
                    self.ExportNodalVar(varName)
                    
                elif sourceType=='IP':
                    self.ExportIPVarAsNodal(varName)
                    
                else:
                    errMsg = 'Dont know what variable source type < {0} > is.'.format(sourceType)
                    self.ErrorHandling(errMsg)
                
        
    def ExportNodalVar(self, varName):
        """ export the given variable, which should be a nodal variable
        """
        odb = self.GetOdbHandle()
        hdfObj = self.GetValue('hdfObj')
        
        stepNameList = self.GetValue('stepNameList')
        entityName = self.GetValue('entityName')
        instanceName = self.GetValue('instanceName')
        runName = self.GetValue('runName')
        instanceObj = odb.rootAssembly.instances[instanceName]
        
        # determine the array size needed
        noRows = self.GetValue('noNodes')
        noCols = 0
        for stepName in stepNameList:
            noCols += len(odb.steps[stepName].frames)


        # determine the data type
        exampleValue = odb.steps[stepNameList[0]].frames[0].fieldOutputs[varName].values[0]
        componentNames = self.GetComponentNames(exampleValue, varName)
            
        noComponents = len(componentNames)

        # copy the data in to a temporary array
        currentColNo = 0
        allData = np.zeros((noRows, noCols, noComponents))
        
        for stepName in stepNameList:
        
            stepObj = odb.steps[stepName]
            
            for frame in stepObj.frames:
            
                valueObj = frame.fieldOutputs[varName].getSubset(region=instanceObj).values
                allData[:,currentColNo,:] = np.vstack([value.data for value in valueObj])
                
                currentColNo += 1
                
        # write the data to the hdf file
        for componentNum in range(noComponents):
            
            componentName = componentNames[componentNum]
            hdfObj.AddFieldVarData(runName=runName, varName=componentName,
                                   entityName=entityName, dataTable=allData[:,:,componentNum])

    
    def ExportIPVarAsNodal(self, varName):
        """ export the given variable, which should be a nodal variable
        """
        odb = self.GetOdbHandle()
        hdfObj = self.GetValue('hdfObj')
        
        stepNameList = self.GetValue('stepNameList')
        entityName = self.GetValue('entityName')
        instanceName = self.GetValue('instanceName')
        runName = self.GetValue('runName')
        instanceObj = odb.rootAssembly.instances[instanceName]
        
        # determine the array size needed (for a nodal variable)
        noRows = self.GetValue('noNodes')
        noCols = 0
        for stepName in stepNameList:
            noCols += len(odb.steps[stepName].frames)


        # determine the data type
        exampleValue = odb.steps[stepNameList[0]].frames[0].fieldOutputs[varName].values[0]
        componentNames = self.GetComponentNames(exampleValue, varName)
        noComponents = len(componentNames)

        # copy the data in to a temporary array (for the element/IP variable)
        allData = np.zeros((noRows, noCols, noComponents))
        currentColNo = 0
        noTempRows = self.GetValue('noNodesForExtrapField')
        tempData = np.zeros((noTempRows, noCols, noComponents))

        for stepName in stepNameList:
        
            stepObj = odb.steps[stepName]
            
            for frame in stepObj.frames:
            
                # valueObj = frame.fieldOutputs[varName].getSubset(region=instanceObj).values
                valueObj = frame.fieldOutputs[varName].getSubset(position=ELEMENT_NODAL, region=instanceObj).values
                tempData[:,currentColNo,:] = np.vstack([value.data for value in valueObj])
                
                currentColNo += 1
                
        # write the data to the hdf file
        for componentNum in range(noComponents):
            
            componentName = componentNames[componentNum]
            hdfObj.AddFieldVarData(runName=runName, varName=componentName,
                                   entityName=entityName, dataTable=tempData[:,:,componentNum])

           
                
    def GetComponentNames(self, exampleValue, varName):
        """
        """
        dataType = exampleValue.type
        noComponents = len(exampleValue.data)
        knownTypes = [TENSOR_2D_PLANAR, TENSOR_3D_FULL, VECTOR, SCALAR]
        
        if not dataType in knownTypes:
            errMsg = 'Dont know how to handle type < {0} >'.format(dataType)
            self.ErrorHandling(errMsg)
            
        if dataType==TENSOR_2D_PLANAR or dataType==TENSOR_3D_PLANAR:
            return '{0}11,{0}22,{0}33,{0}12'.format(varName).split(',')
        
        elif dataType==TENSOR_3D_SURFACE:
            return '{0}11,{0}22,{0}12'.format(varName).split(',')

        elif dataType==TENSOR_3D_FULL:
            return '{0}11,{0}22,{0}33,{0}12,{0}13,{0}23'.format(varName).split(',')
            
        elif dataType==VECTOR:
            if noComponents==2:
                return '{0}1,{0}2'.format(varName).split(',')
            else:
                return '{0}1,{0}2,{0}3'.format(varName).split(',')
                
        elif dataType==SCALAR:
            return [varName]
            
        else:
            self.ErrorHandling('problem identifying data type')

