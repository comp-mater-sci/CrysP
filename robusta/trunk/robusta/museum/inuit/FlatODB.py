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


class FlatODB(GenericHDF5):
 
    """ class to export data from the Abaqus ODB to a generic HDF file
    
    The goal is to have an FE simulation described from the point of
    view of a single material being deformed, with as little nesting of data
    as possible. This is because the kind of work I/we do focuses on a single
    material sample's behaviour, rather than complex shapes or the specifics of
    tool design, so that the highly abstracted generic Abaqus ODB object model
    is definitely more of a hindrance than a help for post processing.
    
    Another major motivation for moving outside of the Abaqus ODB object model
    per se is the fact that it is very very difficult to do post processing with
    third party modules (even plotting) within the Abaqus Python environment,
    and at the same time the ODB is not accesible with standard Python outside
    of the Abaqus Python environment. I suspect it is in the interest of
    software suppliers to maintain a 'licensed proprietry code only' approach
    to the high level accessing of result databases. ie. one needs to check out
    a cae or abaqus/viewer license every time one wants to access an odb.
    
    The traditional alternative, exporting raw data either to .fil files (builtin
    facility) or by python scripts, is slow, prone to mistakes, produces no
    audit trail, consumes lots of storage (because the odb needs to be kept),
    and tends to promote repeated reinvention of the wheel from one simulation
    design to the next. It is quicker to set up, but does not offer the kind
    of control and repeatability suitable for a long term project.
    
    Finally, regarding use of the HDF5 format itself, the motivation is simple:
        - HDF5 is open source and distributed under a liberal BSD-style license,
          which should pose little or no obstruction for future commercial codes
        - it is a standardised and the format is well known to scientific communities
        - it is designed for handling very large arrays of numerical data. It is
          fast and it is claimed to outperform many numerical packages for 'out of
          core' data processing.
        - sharing is easy. many tools exist for viewing and manipulating data
        - bindings exist in many popular languages for scientific and numerical
          work: C, C++, Fortran, Python, Ruby, others..
        - the Python binding H5Py exposes the data in a way very similar to the
          numpy package used by abaqus does
        
        and more specific to MTM:
        - the HMS will eventually use the HDF5 format for data handling.
    
    """
    
    def __init__(self, hdfFileName, folder=os.getcwd(), mode='write'):
        """ Constructor
        
        This sets up a new hdf file with the following structure. 
            
            /---mat_data
              |     |
              |     |--field_vars
              |     |
              |     ---constants
              |
              |-tool_data
              |     |
              |     |--field_vars
              |     |
              |     |--ref_point_vars
              |     |
              |     ---constants
              |
              --sets
              |
              --config
              |
              --xrefs
              
              
            (xrefs) contains lookup tables linking the Abaqus node, element,
                    integration point numbers and job/mdb names with the internal
                    material point reference system for the hdf file
           (config) is intended to hold house keeping information for the file
                    eg. details on compression, data chunking, etc.
             (sets) are pretty much the same idea as in Abaqus, but using the
                    internal material point reference system rather than node,
                    element numbers or assembly instance names
         (mat_data) contains nearly all the data from the point of view of
                    material 'points'
        (tool_data) contains the tool data, which is expected to be fairly minimal,
                    mainly variable data associated with reference points.

        Notes:
                  - this structure expects a single instance for the 'material' or
                    sample, and one or more instances for the 'tools'. This is one
                    of the main distinctions between these two categories.
                  - a second distinction between the 'material' and 'tool' groups
                    is the fact that the tools might have large rotations. Data in
                    this group is not expected to need to be compared closely, in
                    the sense that direct qunatitative comparisons of 2D fields
                    is expected in the material category
                    
        """
        
        # call the inherited init method
        GenericHDF5.__init__(self, hdfFileName, folder, mode)
        
        # build a new data structure in that file
        hdfObj = self.GetHdfObj()
        
        # main groups
        materialData = hdfObj.create_group('mat_data')
        sets = hdfObj.create_group('sets')
        config = hdfObj.create_group('config')
        xrefs = hdfObj.create_group('xrefs')
        toolData = hdfObj.create_group('tool_data')

        # subgroups
        matFieldVariables = materialData.create_group('field_vars')
        matConstants = materialData.create_group('constants')
        
        toolFieldVariables = toolData.create_group('field_vars')
        toolRPVariables = toolData.create_group('ref_point_vars')
        toolConstants = toolData.create_group('constants')
        
        # store all of the above references for convenience
        self.SetValue('materialData', materialData)
        self.SetValue('sets', sets)
        self.SetValue('config', config)
        self.SetValue('xrefs', xrefs)
        
        self.SetValue('matFieldVariables', matFieldVariables)
        self.SetValue('matConstants', matConstants)
        
        self.SetValue('toolFieldVariables', toolFieldVariables)
        self.SetValue('toolRPVariables', toolRPVariables)
        self.SetValue('toolConstants', toolConstants)
        
        
    def BulkStoreFieldVariable(self, varName, time, jobID, nativeIDList, sourceType, dataColumn, overwrite=False, objectType='material'):
        """ store an array of data values for a field variable for a number of 'points' (eg nodes, IPs)
        
            inputs:
                "varName" - the field variable name as a string. Eg. 'U1' for displacement in the 1 direction
                "time"    - a scalar float which gives the instant in time the data belongs to
                "jobID"   - a string which uniquely identifys which model AND simulation run of that model
                            the data belongs to. A seperate data set is created for each simulation run.
           "nativeIDList" - a list or nx1 numpy array which gives the (Abaqus) labels to which each row of
                            the given data corresponds
             "sourceType" - a string which uniquely identifies what the data source is. Eg. node, element,
                            DIC, etc.
             "dataColumn" - an nx1 numpy array which gives scalar values of the field variable. Each row
                            corresponds to the entity label in the same row in "nativeIDList"
              "overwrite" - set to True if you want to delete any existing data set for the same
                            variable/simulation-run/model combination.
             "objectType" - can be "material" or "tool". Field
        
            data is stored in one of the two fieldVariables subgroups (see the doc string at
            the start of this file). A data set is created, or data is appended to an existing
            set as follows:
            
                - the data set name is generated from the
        """
        # get aliases for the group objects that contain field variable data
        if objectType == 'material':
            fieldVariables = self.GetValue('matFieldVariables')
        elif objectType == 'tool':
            fieldVariables = self.GetValue('toolFieldVariables')
        else:
            errMsg = '<{0}> is an unknown object type for storing field variable data.'.format(objectType)
            self.ErrorHandling(errMsg)
            
        existingVars = fieldVariables.keys()
        makeNewDataSet = True
        
        # check if the given variable already has a group, if not create 
        if not varName in existingVars:
            fieldVariables.create_group(varName)

        # check the type given as data (should be an nx1 numpy array, but this
        # check just confirms it is some kind of numpy array)
        noItems = len(dataColumn)
        try:
            dataType = dataColumn.dtype
        except AttributeError:
            errMsg = 'dataColumn must be a nx1 numpy type array'
            self.ErrorHanding(errMsg)

        # create an alias for the variable group, and get the dataset name
        varGroup = fieldVariables[varName]
        dataSetName = self.CheckDataSetName(jobID=jobID, sourceType=sourceType)
            
        # check if a data set already exists for the given simulation run
        if dataSetName in varGroup.keys():
            
            # get existing metadata
            makeNewDataSet = False
            dataSet = varGroup[dataSetName]
            colLookup = self.GetLookupTable(tableName='colLookup', varName=varName, jobID=jobID)
            rowLookup = self.GetLookupTable(tableName='rowLookup', varName=varName, jobID=jobID)
                    
            # overwrite existing data if requested
            if overwrite:
                print 'Overwriting existing data for jobID <{0}> for variable <{1}>.'.format(jobID, varName)
                del varGroup[jobID]
            
            # else check if data for the given value of 'time' already exists
            elif time in colLookup[:,1]:
                errMsg = 'A value for var <{0}> at time <{1}> already exists.'.format(varName, time)
                self.ErrorHandling(errMsg)
                
            # else check if the id list is different from that used for the existing data
            elif not np.all(nativeIDList==rowLookup[:,1]):
                errMsg = ('The given id list contains different points, or points in a different order,'+
                         'than in the existing data for this job.')
                self.ErrorHandling(errMsg)
       
            # check that no data is missing
            elif not noItems==len(colLookup):
                errMsg = 'The data for this time value has a different number of points than previous time values.'
                self.ErrorHandling(errMsg)
       
            # else resize the data to add a new column
            else:
                newColNumber = dataSet.shape[1] + 1               
                dataSet.resize((noItems, newColNumber))
                
                
        # else create a new dataset and meta data
        if makeNewDataSet:
            dataSet = varGroup.create_dataset(dataSetName,(noItems,1), dataType, maxshape=(None, None))
            
            self.Make2DLookupTable(idList=nativeIDList, tableName='rowLookup', varName=varName, jobID=jobID)
            self.Make2DLookupTable(idList=np.array([[time]]), tableName='colLookup', varName=varName, jobID=jobID)
            
            dataSet.attrs['sourceType'] = sourceType
            #dataSet.attrs['colHeaders'] = []

        
        # add the new data and update metadata
        dataSet[:,-1] = dataColumn
        metaData = dataSet.attrs
        
        if 'colHeaders' in metaData.keys():
            existingHeaders = list(dataSet.attrs['colHeaders'])
        else:
            existingHeaders = []
            
        existingHeaders.append('time={0}'.format(time))
        dataSet.attrs['colHeaders'] = existingHeaders
        
          

    def CheckDataSetName(self, jobID, sourceType):
        """ return the unique name to be used for a dataset based on the job id and source type
        """
        
        return '{0}:{1}'.format(jobID, sourceType)
        
        
    def Make2DLookupTable(self, idList, tableName, varName, jobID):
        """ return a nx2 lookup table, n is the number of rows in nativeIDList
        
            col1 = integers 0 to n-1, representing the row or column index in the data array
            col2 = nativeID (should be integers)
        """
        
        noRows = len(idList)
        rowNumbers = np.array([range(noRows)], dtype=DefaultIntegerType).transpose()
        idNumbers = np.array(idList).reshape(noRows,1)

        newTable = np.hstack((rowNumbers, idNumbers))
        
        self.SetLookupTable(tableName, newTable, varName, jobID)
       
        
    def SetLookupTable(self, tableName, tableData, varName, jobID):
        """
        """
        xrefObj = self.GetXrefGroupObj(varName)
        fullTableName = self.CheckDataSetName(jobID=jobID, sourceType=tableName)

        if fullTableName in xrefObj.keys():
            # if the lookup table already exists, do nothing ?
            pass
            
        else:
            # else create a new lookup table
            newTable = xrefObj.create_dataset(fullTableName, tableData.shape, tableData.dtype)
            newTable[...] = tableData[...]        
            newTable.attrs['colHeaders'] = ['dataSetRowNumber','sourceLabel']
        
        
    def GetLookupTable(self, tableName, varName, jobID):
        """
        """
        
        fullTableName = self.CheckDataSetName(jobID=jobID, sourceType=tableName)
        xrefObj = self.GetXrefGroupObj(varName)
        
        if not fullTableName in xrefObj.keys():
            errMsg = 'No lookup table found for <{0}>'.format(fullTableName)
            self.ErrorHandling(errMsg)
            
        else:
            return xrefObj[fullTableName]
        
        
    def GetXrefGroupObj(self, varName):
        """ returns the data group for cross references and lookup tables for
            the given variable
        """
        xrefObj = self.GetValue('xrefs')
                
        if not varName in xrefObj.keys():
            return xrefObj.create_group(varName)
           
        else:
            return xrefObj[varName]    
