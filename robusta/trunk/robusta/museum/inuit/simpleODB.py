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
from robusta.inuit.GenericHDF5 import *
from robusta.config import *

# import third party modules
try:
    import h5py
    import numpy as np
except ImportError:
    print 'This module requires the H5py and HDF5 libraries to be accessible.'
    raise
    
    
class simpleODB(GenericHDF5):

    def __init__(self, hdfFileName, folder=os.getcwd(), mode='write'):
        """ Constructor
        
            Just creates a new HDF file with minimal structure
        """
        
        # call the inherited init method
        GenericHDF5.__init__(self, hdfFileName, folder, mode)
        
        # build a new data structure in that file
        hdfObj = self.GetHdfObj()
        
        # main groups
        self.SetValue('instances', hdfObj.create_group('instances'))
        self.SetValue('xrefs', hdfObj.create_group('xrefs'))
        self.SetValue('config', hdfObj.create_group('config'))
        
        
    def GetInstanceContainer(self, name, createIfRequired=True):
        """ return a hdf5 data group for instance data
        
            set up the default data structure for instance data if it doesn't
            already exist
        """
        instanceContainer = self.GetValue('instances')
        
        # if the instance doesnt have a container yet, create it
        if not name in instanceContainer.keys():
            if createIfRequired:
                newContainer = instanceContainer.create_group(name)
         
                newContainer.create_group('sets')
                newContainer.create_group('node_data')
                newContainer.create_group('lookup_tables')
            
                return newContainer
                
            else:
                errMsg = 'No instance with name <{0}> in this file.'.format(name)
                self.ErrorHandling(errMsg)

        # else return the existing one
        else:
            return instanceContainer[name]
            
        
    def AddInstanceNodeSets(self, instanceName, nodeSetNameList, nodeSetLabelList, overwrite=False):
        """ same as AddInstanceNodeSet, but that nodeSetNameList is a list of node set names, and
            nodeSetLabelList is a list of numpy arrays of node labels
        """
        
        noSets = len(nodeSetNameList)
        
        for index in range(noSets):
            
            nodeSetName = nodeSetNameList[index]
            nodeSetLabels = nodeSetLabelList[index]
        
            self.AddInstanceNodeSet(instanceName=instanceName, nodeSetName=nodeSetName,
                                    nodeSetLabels=nodeSetLabels, overwrite=overwrite)
            
            
    def AddInstanceNodeSet(self, instanceName, nodeSetName, nodeSetLabels, overwrite=False):
        """ add an array of node sets for the named instance
        """
        # check input types are ok
        self.CheckParListSizeType(
             parList=[instanceName, nodeSetName, nodeSetLabels, overwrite],
             parNameList=['instanceName', 'nodeSetName', 'nodeSetLabels','overwrite'],
             parSizeList=[None, None, None, None],
             parTypeList=[str, str, np.ndarray, bool])
             
        # get shortcuts to the data containers
        dataContainer = self.GetInstanceContainer(name=instanceName)
        setsData = dataContainer['sets']
        
        # if a node set by that name already exists....
        if nodeSetName in setsData.keys():
            
            if overwrite:
                del setsData[nodeSetName]
                
            else:
                errMsg = 'Set with name <{0}> already in data for instance <{1}>.'.format(nodeSetName, instanceName)
                self.ErrorHandling(errMsg)
            
        # create the new node set
        nodeLabels = nodeSetLabels.reshape(len(nodeSetLabels),1)
        newSet = setsData.create_dataset(nodeSetName, nodeLabels.shape, nodeLabels.dtype)
        newSet[...] = nodeLabels[...]


    def AddInstanceNodalCoors(self, instanceName, nodeCoords):
        """ add the node coordinates to the hdf file
        """
       
        # get shortcuts to the data containers
        dataContainer = self.GetInstanceContainer(name=instanceName)
        nodeData = dataContainer['node_data']
        
        # get node coordinates
        noNodes = len(nodeCoords)
        nodeXCoords = np.array([coord[0] for coord in nodeCoords]).reshape(noNodes, 1)
        nodeYCoords = np.array([coord[1] for coord in nodeCoords]).reshape(noNodes, 1)
        nodeZCoords = np.array([coord[2] for coord in nodeCoords]).reshape(noNodes, 1)
        
        # write to the hdf file
        xCoords = nodeData.create_dataset('Coord_X', (noNodes,1), defaultPrecision)
        yCoords = nodeData.create_dataset('Coord_Y', (noNodes,1), defaultPrecision)
        zCoords = nodeData.create_dataset('Coord_Z', (noNodes,1), defaultPrecision)
        
        xCoords[...] = nodeXCoords[...]
        yCoords[...] = nodeYCoords[...]
        zCoords[...] = nodeZCoords[...]
        
    
        
    def AddInstanceScalarData(self, instanceName, dataName, scalarDataArray, timeLabels, overwrite=False):
        """ add an array of data to the named instance
            
            instanceName   the name to file the data under.
            dataName       scalar variable to add data for
           scalarDataArray a 2D array with row numbers corresponding to nodeLabels,
                           and column numbers corresponding to instants in time
                           as given by timeLabels
            timeLabels     values of time corresponding to each column in scalarDataArray
            
            
        """
        
        # check input types are ok
        self.CheckParListSizeType(
             parList=[instanceName, dataName, scalarDataArray,
             timeLabels, overwrite], parNameList=['instanceName', 'dataName',
             'scalarDataArray', 'timeLabels', 'overwrite'],
             parSizeList=[None, None, (None,None), None, None],
             parTypeList=[str, str, np.ndarray, list, bool])
        
        # get shortcuts to the data containers
        dataContainer = self.GetInstanceContainer(name=instanceName)
        
        nodeData = dataContainer['node_data']
        lookupTables = dataContainer['lookup_tables']
        
        # check if data already exists
        if dataName in nodeData.keys():
            if overwrite:
                del nodeData[dataName]
                
            else:
                errMsg = 'Data for variable <{0}> already exists. Set overwrite=True to replace it.'.format(dataName)
                self.ErrorHandling(errMsg)
        
        # add the data
        newData = nodeData.create_dataset(dataName, scalarDataArray.shape, scalarDataArray.dtype)
        newData[...] = scalarDataArray[...]
        
        # build and add the lookup table for column number vs time
        noCols = len(timeLabels)
        colLookupData = np.hstack((np.array(range(noCols)).reshape(noCols,1),
                                   np.array(timeLabels).reshape(noCols,1)))
        self.AddLookupTable(container=lookupTables, table=colLookupData,
                            name=dataName, entity='Cols', attributes={'unit':'time [s]'})
                            
        
    def AddLookupTable(self, container, table, name, entity, attributes=None):
    
        # create the table and copy data
        label = '{0}:{1}'.format(entity, name)
        newTable = container.create_dataset(label, table.shape, table.dtype)
        newTable[...] = table[...]
        
        # add the attribute data if present
        if self.Exist(attributes):
            for name in attributes.keys():
                 newTable.attrs[name] = attributes[name]
                 
                 
