# Prevent the legacy class type being used
__metaclass__ = type

# import the Abaqus classes and modules
try:
    from abaqusConstants import *
    from abaqus import *
except ImportError:
    print 'Warning: The abaqus modules may need to be accessible for this module.'

 
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
    
    Motivation:
    
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
    
    def __init__(self, hdfFileName, folder=os.getcwd(), mode='append'):
        """ Constructor
        
        This sets up a new hdf file with the following structure. Note that
        references to "Abaqus" here equally apply to other software such as
        corresponding data provided by DIC, etc.
            
        /-|
          --[ entities ]
          |   |
          |   --[ entity_1 ]
          |   |     |
          |   |     --[calculated_values]
          |   |     |
          |   |     --[ xrefs ]
          |   |     |
          |   |     --[ sets ]
          |   |     |
          |   |     --[ field_variables ]
          |   |     |     |
          |   |     |     |--< run_name_1>
          |   |     :     :
          |   |     
          |   --[ entity_2 ]
          |   |     |
          |   :     :
          |   
          --[ config ]
          |
          |     
          :
              
        Sqare brackets denote data groups, angle brackets denote data sets
          
       [ entities ] Contains a data group for each object being modelled. Eg. an
                    individual tool or a material specimen in an FE model (ie.
                    it corresponds to an 'instance' in an Abaqus ODB)
                    Each distinct object gets its own entity data group
          [ xrefs ] contains lookup tables linking the original (source) material
                    point numbers and job/run names with the internal
                    material point numbering system for the hdf file     
           [ sets ] sets of material point numbers for a given entity for one of
                    the following groupings: (i) field_variable + run_name,
                    (ii)   
     [ field_vars ] field data which varies with time.
       < run_name > Dataset with a name indicating the data source and job name.
                    Data is stored as scalars
                    in 2D arrays where each row corresponds to a material point,
                    and each column to a given moment in absolute time. It is
                    expected that this data is nodal data, so data typically
                    available at integration points should be projected to the
                    nodes and averaged before importing to this database.
                    The correspondance between row number and the label in the
                    source software (eg. node label in Abaqus) is found in the
                    lookup table (pointer) given in the dataset's attributes                  
         [ config ] is intended to hold house keeping information for the file
                    eg. details on compression, data chunking, etc.

                    
        """
        
        # call the inherited init method
        GenericHDF5.__init__(self, hdfFileName, folder, mode)
        self.SetValue('mode', mode)
        
        # build a new data structure (only a 'config' group at this point!)
        hdfObj = self.GetHdfObj()
        if not mode in ['append','read']:
            config = hdfObj.create_group('config')
                      
        # set defaults
        self.SetValue('currentEntityName', None)        
        self.SetValue('xRefNameList', ['labels', 'column_values', 'coordinates',
                      'projected_labels'])

    
    
    def GetPath(self, pathItemList):
        """ return a reference string for a HDF group or data set (ie. a *nix style
            path)
        """
        pathString = []
        
        for item in pathItemList:
            if '/' in item:
                errMsg = ' the character < / > cannot be used in HDF data or group names.'
                self.ErrorHandling(errMsg)
                
            pathString.append(item)
            pathString.append('/')
            
        return ''.join(pathString)
    
    
    def CheckEntityExists(self, entityName):
        """ return True if the entity name exists
        """
        hdfObj = self.GetHdfObj()
        existingEntityNames = hdfObj.keys()
        
        return entityName in existingEntityNames
        
        
    def CreateEntity(self, entityName):
        """ add a new entity and data structure to the database
        """
        # check if the name is free
        hdfObj = self.GetHdfObj()
        mode = self.GetValue('mode')
        
        alreadyExists = self.CheckEntityExists(entityName)
        appendMode = mode=='append'
        
        if alreadyExists:
            errMsg = 'Entity with name < {0} > already exists.'.format(entityName)
            self.ErrorHandling(errMsg, warningOnly=appendMode)
            self.SetValue('entityName', entityName)
            return
        
        # build structure
        newEntity = hdfObj.create_group(entityName)
        fieldVars = newEntity.create_group('field_variables')
        fieldVars.attrs['lastVariableNumber'] = 0
        
        xrefs = newEntity.create_group('xrefs')
        xrefs.create_group('labels')
        xrefs.create_group('column_values')
        xrefs.create_group('projected_labels')
        xrefs.create_group('coordinates')
        
        sets = newEntity.create_group('sets')
        
        calcVals = newEntity.create_group('calculated_values')
        
        self.SetValue('entityName', entityName)
        
    
    def DeleteEntity(self, entityName):
        """ delete the named entity
        """
        hdfObj = self.GetHdfObj()
        if self.CheckEntityExists(entityName):
            del hdfObj[entityName]
            self.SetValue('entityName', None)
               
            
    def CheckFieldVarExists(self, varName, entityName):
        """ return True if the field variable exists for the given entity
        """
        hdfObj = self.GetHdfObj()
        
        if self.CheckEntityExists(entityName):

            fieldVarNames = hdfObj[self.GetPath([entityName, 'field_variables'])].keys()
            return varName in fieldVarNames
            
        else:
            errMsg = 'No entity with name < {0} > exists.'.format(entityName)
            self.ErrorHandling(errMsg)
        
    
    def CreateFieldVar(self, varName, entityName):
        """ add a new field variable
        """
        hdfObj = self.GetHdfObj()
        if not self.CheckFieldVarExists(varName, entityName):
            # create the new variable
            fieldVarGroup = hdfObj[self.GetPath([entityName, 'field_variables'])]
            newFieldVar = fieldVarGroup.create_group(varName)

            # add attribute data
            newVariableNumber = fieldVarGroup.attrs['lastVariableNumber'] + 1
            newFieldVar.attrs['variableNumber'] = newVariableNumber
            fieldVarGroup.attrs['lastVariableNumber'] = newVariableNumber
            
            
    def DeleteFieldVar(self, varName, entityName):
        """ remove the given variable. note that associated sets and xref data will
            not be deleted
        """
        hdfObj = self.GetHdfObj()
        if self.CheckFieldVarExists(varName, entityName):
            del hdfObj[self.GetPath([entityName, 'field_variables', varName])]
       
    
    def SetCurrentEntity(self, entityName):
        """
        """
        if self.CheckEntityExists(entityName) and not entityName=='config':        
            self.SetValue('currentEntityName', entityName)
            
        else:            
            print 'No entity with name < {0} > !'.format(entityName)
            
    
    def SetCurrentRun(self, runName):
        """
        """
        try:
            entityName = self.GetValue('currentEntityName')
            
        except KeyError:
            raise KeyError('Set an entity name first (eg use SetCurrentEntity).')
                        
        else:
            self.SetValue('currentRunName', runName)
            hdfObj = self.GetHdfObj()
            xrefs = hdfObj[self.GetPath([entityName, 'xrefs'])]

            col_vals = xrefs['column_values'].keys()
            coordinates = xrefs['coordinates'].keys()
            labels = xrefs['labels'].keys()
            projected_labels = xrefs['projected_labels'].keys()
            
            if not (runName in col_vals and runName in coordinates and \
                    runName in labels and runName in projected_labels):
                
                print 'Warning: the cross references for run < {0} > are incomplete.'.format(runName)
       
       
    def GetFieldVarComponentData(self, varComponentName):
        """ return a reference for the data
        """
        hdfObj = self.GetHdfObj()
        (entityName, runName) = self.GetCurrentEntityAndRun()            
        
        # check if there is data for the named variable
        if self.CheckRunNameExistsForVarComponent(runName=runName, varComponentName=varComponentName,
                                                  entityName=entityName):
            return hdfObj[self.GetPath([entityName,'field_variables', varComponentName,
                                         runName])]

        else:
            print 'No data found for run name < {0} > for variable < {1} >'.format(runName, varComponentName)
            
    
    def GetCurrentEntityAndRun(self):
        """
        """
        try:
            entityName = self.GetValue('currentEntityName')
        except KeyError:
            raise KeyError('No entity have been selected yet. Specify with SetCurrentEntity(entityName)')
            
        try:
            runName = self.GetValue('currentRunName')
        except KeyError:
            raise KeyError('No run name has been selected yet. Specify with SetCurrentRun(runName)')
            
        return (entityName, runName)
        
            
    def GetXref(self, ref='coordinates'):
        """
        """
        xrefNames = self.GetValue('xRefNameList')
        
        if not ref in xrefNames:
            errMsg = '< ref > should be one of < {0} >.'.format(xrefNames)
            self.ErrorHandling(errMsg)
        
        hdfObj = self.GetHdfObj()
        (entityName, runName) = self.GetCurrentEntityAndRun()
        
        return hdfObj[self.GetPath([entityName, 'xrefs', ref, runName])]
        
    
    def AddFieldVarData(self, runName, varName, entityName, dataTable):
        """ add the data and xref info for the given variable. Both should be 2D
            numpy arrays (see doc string of this class for description of data
            format)
        
        """
        # check names are ok    
        if not self.CheckFieldVarExists(varName, entityName):
            self.CreateFieldVar(varName, entityName)
        
        # check if data for this variable already has been added
        appendMode = self.GetValue('mode') == 'append'
        if self.CheckRunNameExistsForVarComponent(runName, varName, entityName):
            errMsg = 'A Run name < {0} > already exists for variable < {1} >.'.format(runName, varName)
            self.ErrorHandling(errMsg, warningOnly=appendMode)
            return
        
        # check data is in the right form
        if not self.Is2DNumpyArray(dataTable):
            errMsg = 'dataTable should be a 2D numpy array, with max shape {0}.'.format(maxArrayShape)
            self.ErrorHandling(errMsg)
        
        # add the data
        hdfObj = self.GetHdfObj()
        fieldVarGroup = hdfObj[self.GetPath([entityName, 'field_variables', varName])]
        
        dataShape = dataTable.shape
        dataType = dataTable.dtype
        
        runDataSet = fieldVarGroup.create_dataset(runName, dataShape, dataType,
                                   maxshape=maxArrayShape, compression=hdfCompressionMethod)
        runDataSet[:,:] = dataTable[:,:]
 
    
    def CheckRunNameExistsForVarComponent(self, runName, varComponentName, entityName):
        """
        """
        hdfObj = self.GetHdfObj()
        
        if varComponentName in hdfObj[self.GetPath([entityName, 'field_variables'])]:
        
            fieldVarGroup = hdfObj[self.GetPath([entityName, 'field_variables', varComponentName])]
            return runName in fieldVarGroup.keys()
            
        else:
            return False
    
    
    def CheckXrefsDefinedForRunName(self, runName):
        """
        """
        hdfObj = self.GetHdfObj()
        checkNames = self.GetValue('xRefNameList')
        entityName = self.GetValue('entityName')
        
        checkList = []        
        for checkName in checkNames:
            checkList.append(runName in hdfObj[self.GetPath([entityName, 'xrefs', checkName])].keys())
        
        if all(checkList):
            return True
            
        elif any(checkList) and not all(checkList):
            errMsg = 'Incomplete Xrefs exist for runName < {0} >.'.format(runName)
            self.ErrorHandling(errMsg)
            
        else:
            return False

    
    def RemoveXRef(self, entityName, runName):
        """ remove the xref table for the given run name/entity
        """
        # check if the entityName exists
        if not self.CheckEntityExists(entityName):
            errMsg = 'No entity with name < {0} > exists.'.format(entityName)
            self.ErrorHandling(errMsg)
            
        # check if the given runName already has an xref table
        hdfObj = self.GetHdfObj()
        xref = hdfObj[self.GetPath([entityName, 'xrefs'])]
        
        if runName in xref.keys():
            del xref[runName]
            
        else:
            errMsg = 'no xref table exists for runName < {0} >.'.format(runName)
            self.ErrorHandling(errMsg)

    
    def AddNodeLabelXRef(self, labelList, rowList, entityName, runName, coordArray):
        """ add a cross reference table for the given entity for matching
            labels with row numbers in the data
        
            labelList is a list of unique integer values which identify the
            material point in question in the source system (eg node labels from
            a finite element model)
            
            rowList is a list (equal in length to labelList) which describes the
            correspondance between rows in the labelist list the runName data arrays
            
            coordArray is a 2D numpy array of 2 or 3D coordinates
        """
        appendMode = self.GetValue('mode')=='append'
        
        # check if the entityName exists
        if not self.CheckEntityExists(entityName):
            errMsg = 'No entity with name < {0} > exists.'.format(entityName)
            self.ErrorHandling(errMsg)
            
        # check if the given runName already has an xref table
        hdfObj = self.GetHdfObj()
        xref = hdfObj[self.GetPath([entityName, 'xrefs', 'labels'])]
        
        if runName in xref.keys():
            errMsg = 'Source/run with name < {0} > already has an xref table.'.format(runName)
            self.ErrorHandling(errMsg, warningOnly=appendMode)
            return
        
        # check data is in the right form
        if not self.Is2DNumpyArray(coordArray):
            errMsg = 'coordArray should be a 2D numpy array, with max shape {0}.'.format(maxArrayShape)
            self.ErrorHandling(errMsg)
        
        noLabels = len(labelList)
        if not noLabels==len(rowList):
            errMsg = 'rowList and labelList must have the same number of rows'
            self.ErrorHandling(errMsg)
        
        # add the table
        data = np.hstack([np.array(labelList).reshape(noLabels,1) ,
                          np.array(rowList).reshape(noLabels,1)])
        newXrefTable = xref.create_dataset(runName, data.shape, data.dtype,
                                           maxshape=maxArrayShape, compression=hdfCompressionMethod)
        newXrefTable[...] = data[...]
                
        # add the coordinate data
        xref = hdfObj[self.GetPath([entityName, 'xrefs', 'coordinates'])]
        newCoordTable = xref.create_dataset(runName, coordArray.shape, coordArray.dtype,
                                            maxshape=maxArrayShape, compression=hdfCompressionMethod)
        newCoordTable[...] = coordArray[...]
        
        # add meta data
        newCoordTable.attrs['column_names'] = ['source label', 'internal label']
     
    
    def AddProjIPLabelXRef(self, nodeLabelList, elementLabelList, rowList, entityName, runName):
        """ add cross reference info for arrays which hold data which has been
            projected from element integration points to nodes
            
            the cross reference table has three columns: element label, node label
            and data row number
        """
        appendMode = self.GetValue('mode')=='append'
        
        # check if the entityName exists
        if not self.CheckEntityExists(entityName):
            errMsg = 'No entity with name < {0} > exists.'.format(entityName)
            self.ErrorHandling(errMsg)
            
        # check if the given runName already has an xref table
        hdfObj = self.GetHdfObj()
        xref = hdfObj[self.GetPath([entityName, 'xrefs', 'projected_labels'])]
        
        if runName in xref.keys():
            errMsg = 'Source/run with name < {0} > already has an xref table.'.format(runName)
            self.ErrorHandling(errMsg, warningOnly=appendMode)
            return
        
        # check the list lengths
        noLabels = len(nodeLabelList)
        if not (noLabels==len(elementLabelList) and noLabels==len(rowList)):
            errMsg = 'node, element and row lists need to be lists of equal length.'
            self.ErrorHandling(errMsg)
            
        # add the new xref table
        data = np.hstack([np.array(nodeLabelList).reshape(noLabels,1) ,
                          np.array(elementLabelList).reshape(noLabels,1),
                          np.array(rowList).reshape(noLabels,1)])
        newXrefTable = xref.create_dataset(runName, data.shape, data.dtype,
                                           maxshape=maxArrayShape, compression=hdfCompressionMethod)
        newXrefTable[...] = data[...]
        
        # add meta data
        newXrefTable.attrs['column_names'] = ['source node label', 'source element label', 'internal label']
        
        
    def AddColValueRef(self, columnValueList, runName, quantity='time [s]'):
        """
        """
        # check if the entityName exists
        entityName = self.GetValue('entityName')
        appendMode = self.GetValue('mode')=='append'
        
        if not self.CheckEntityExists(entityName):
            errMsg = 'No entity with name < {0} > exists.'.format(entityName)
            self.ErrorHandling(errMsg)
            
        # check if the given runName already has an xref table
        hdfObj = self.GetHdfObj()
        xref = hdfObj[self.GetPath([entityName, 'xrefs', 'column_values'])]
        
        if runName in xref.keys():
            errMsg = 'Source/run with name < {0} > already has an xref table.'.format(runName)
            self.ErrorHandling(errMsg, warningOnly=appendMode)
            return
        
        noVals = len(columnValueList)
        data = np.array(columnValueList).reshape(noVals,1)
        newXrefTable = xref.create_dataset(runName, data.shape, data.dtype,
                                           maxshape=maxArrayShape, compression=hdfCompressionMethod)
        newXrefTable[...] = data[...]
        
        # add meta data
        newXrefTable.attrs['quantity']=quantity
        
        
    def AddNodeSet(self, runName, nodeSetName, nodeLabels):
        """ "nodeLabels" are the set of node labels used by source (eg Abaqus)
        
            they need to be indexed at the time the set is used for something..

        """
        hdfObj = self.GetHdfObj()
        entityName = self.GetValue('entityName')
        appendMode = self.GetValue('mode')=='append'
        
        # add an entry for the current run if necessary
        allSets = hdfObj[self.GetPath([entityName, 'sets'])]
        if not runName in allSets.keys():
            allSets.create_group(runName)
        
        # check if a set with the given name exists
        sets = hdfObj[self.GetPath([entityName, 'sets', runName])]
        if nodeSetName in sets.keys():
            errMsg = 'node set with name < {0} > already exists.'.format(nodeSetName)
            self.ErrorHandling(errMsg, warningOnly=appendMode)
            return
            
        # add the set - will convert the nodeLabel list to a numpy array, unless
        # it is already a 2D array (ie. a 'special' case such as element edge node
        # label lists)
        if self.Is2DNumpyArray(nodeLabels):
            data = nodeLabels
            
        else:
            noNodes = len(nodeLabels)
            data = np.array(nodeLabels, dtype=np.int).reshape(noNodes,1)
            
        newSet = sets.create_dataset(nodeSetName, data.shape, data.dtype,
                                     maxshape=maxArrayShape, compression=hdfCompressionMethod)
        newSet[...] = data[...]
        
        # add metadata
        newSet.attrs['column_labels'] = ['source node label']
        
        
    def AddCalculatedValue(self, valueName, valueArray, attribDict=None):
        """ add the given data as an array under the 'calculated_values' group
            in the current hdf file
            
            valueArray should be a 2D numpy array, and valueName should not
            already exist under the given run name
        """
        hdfObj = self.GetHdfObj()
        (entityName, runName) = self.GetCurrentEntityAndRun()
        
        if not self.Is2DNumpyArray(valueArray):
            errMsg = 'valueArray should be a 2D numpy array'
            self.ErrorHandling(errMsg)
            
        # create a data group for the current entity and run, if one does not
        # exist
        if not entityName in hdfObj.keys():
            self.CreateEntity(entityName)
        
        if not 'calculated_values' in hdfObj[entityName].keys():
            hdfObj[entityName].create_group('calculated_values')
        
        calculatedValues = hdfObj[self.GetPath([entityName, 'calculated_values'])]
        if not valueName in calculatedValues.keys():
            calculatedValues.create_group(valueName)
        
        # check if data with the same name has been added previously
        calcValuesForRun = calculatedValues[valueName]    
        if runName in calcValuesForRun.keys():
            errMsg = 'data for < {0} > already exists for run < {1} >.'.format(valueName, runName)
            self.ErrorHandling(errMsg)
            
        # add the new data
        newValues = calcValuesForRun.create_dataset(runName, valueArray.shape,
                                     valueArray.dtype, maxshape=maxArrayShape,
                                     compression=hdfCompressionMethod)
        newValues[...] = valueArray[...]
        
        # add meta data if provided
        if type(attribDict)==dict:
            for key in attribDict.keys():
                newValues.attrs[key] = attribDict[key]
     
     
    def GetNodeLabelLookupDict(self, indexByNativeLabels=True):
        """ return a python dictionary whose keys are the known labels, and
            values are the corresponding unknown labels
            
            if indexByNativeLabels==True (default) then the keys will be the
            node labels in the source(native) system, and the items will be
            the internal node labels (ie row numbers in the field variable
            and calculated data arrays)
        """
        hdfObj = self.GetHdfObj()
        (entityName, runName) = self.GetCurrentEntityAndRun()
        nodeLabels = hdfObj[self.GetPath([entityName, 'xrefs', 'labels', runName])]
        
        if indexByNativeLabels:
            return dict(nodeLabels)
            
        else:
            return dict(nodeLabels[:,[1,0]])
        
    
    def GetCalculatedValue(self, valueName, template=None):
        """ return the data array for the named value if it exists
        
            if 'template' is given (as a python dict), a new (empty) data set
            with the specified shape and dtype characteristics is returned
        """
        # select the entity and run
        hdfObj = self.GetHdfObj()
        (entityName, runName) = self.GetCurrentEntityAndRun()
        isNew = False
        
        # if the template is given, check if its suitable
        if not template is None:
            if not type(template)==dict:
                errMsg = 'template, if given, must be a python dictionary'
                self.ErrorHandling(errMsg)
                
            if not 'shape' in template.keys() and not 'dtype' in template.kesy():
                errMsg = 'template must contain items shape and dtype.'
                self.ErrorHandling(errMsg)
        
        # check if the given value name is already available
        calculatedValues = hdfObj[self.GetPath([entityName, 'calculated_values'])]
        if not valueName in calculatedValues.keys():
        
            if not template is None:
                calculatedValues.create_group(valueName)
                
            else:
                errMsg = 'No value with name < {0} > exists in entity < {1} >.'.format(valueName, entityName)
                self.ErrorHandling(errMsg)
                
        
        # if the run name is not there, either create a data set for it or raise an exception
        if not runName in calculatedValues[valueName].keys():
            if template is None:
                errMsg = 'No run with name < {0} > for value < {1} >.'.format(runName, valueName)
                self.ErrorHandling(errMsg)
            
            else:
                calculatedValues[valueName].create_dataset(runName, template['shape'],
                                            template['dtype'], maxshape=maxArrayShape,
                                            compression=hdfCompressionMethod)
                isNew = True
        
        # return the data set (blank or otherwise)
        dataset = calculatedValues[self.GetPath([valueName, runName])]
        if template is None:          
            return dataset
            
        else:
            return (dataset, isNew)

             
    def GetCoordsForNodeList(self, nodeLabelList, timeFrameNumber=0):
        """ get a 2D array of coordinates (rows= node label, columns = (x,y,z))
            for just the initial condition (initialCoordsOnly=True)
            or return a 3D array of coordinates for nodes at all time frames
            (rows=node label, cols = xyz, depth=time frame number)
            
        """
        # select the entity and run
        hdfObj = self.GetHdfObj()
        (entityName, runName) = self.GetCurrentEntityAndRun()

        # figure out how big the deformed coordinates array need to be (should be like U1)
        U1 = self.GetFieldVarCompForNodeList(varCompName='U1')
        template = {'shape':U1.shape, 'dtype':U1.dtype}
        
        # get the deformed coordinates (or blank data sets, if no data available)
        (x, isNewX) = self.GetCalculatedValue(valueName='deformed_coordinates_x', template=template)
        (y, isNewY) = self.GetCalculatedValue(valueName='deformed_coordinates_y', template=template)
        (z, isNewZ) = self.GetCalculatedValue(valueName='deformed_coordinates_z', template=template)
        needToCalculate = isNewX or isNewY or isNewZ
        
        # get indices for given node label list
        lookupDict = self.GetNodeLabelLookupDict()
        rowNumbers = [lookupDict[nodeLabel] for nodeLabel in nodeLabelList]
        noRows = len(rowNumbers)
                          
        if needToCalculate:
            # get the intial undeformed coordinates
            initialCoords = np.array(hdfObj[self.GetPath([entityName, 'xrefs', 'coordinates', runName])])
            
            # get displacements for the desired time frame number (U1 was fetched above)         
            U2 = self.GetFieldVarCompForNodeList(varCompName='U2')
            
            # check if the required time frame number is available
            if timeFrameNumber > U1.shape[1] or timeFrameNumber < 0:
                errMsg = 'Cant get time frame number < {0} >, only have data for {1} frames'.format(timeFrameNumber, U1.shape[1])
                self.ErrorHandling(errMsg)
            
            # add displacements to initial coordinates to get deformed coordinates
            if isNewX: x[...] = (initialCoords[:,0,None] + U1[:,:])
            if isNewY: y[...] = (initialCoords[:,1,None] + U2[:,:])
            
            # check if 2 or 3D and add z displacements accordingly, save result
            if 'U3' in hdfObj[self.GetPath([entityName, 'field_variables'])].keys() and isNewZ:

                U3 = self.GetFieldVarCompForNodeList(varCompName='U3')
                z[...] = (initialCoords[:,2,None] + U3[:,:])
                                    
            elif isNewZ:
                z[...] = (initialCoords[:,2,None] + np.zeros(template['shape']))


        # reorder the deformed coordinates according to the node labels in the given list
        # and the chosen time frame number. Note the data set has to be converted to a
        # numpy array to allow non-contiguous node label lists to index
        return np.hstack((np.array(x)[rowNumbers,timeFrameNumber].reshape(noRows,1),
                          np.array(y)[rowNumbers,timeFrameNumber].reshape(noRows,1),
                          np.array(z)[rowNumbers,timeFrameNumber].reshape(noRows,1)))

      
    def GetFieldVarCompForNodeList(self, varCompName, nodeLabelList=None):
        """ return the named field variable component for each of the nodes in 
            the given list of labels
        """
        hdfObj = self.GetHdfObj()
        (entityName, runName) = self.GetCurrentEntityAndRun()
        
        varValues = hdfObj[self.GetPath([entityName, 'field_variables', varCompName, runName])]
        
        # if no node label list is given, return all of the values
        if nodeLabelList is None:
            return np.array(varValues)
        
        # otherwise get indices for given node label list, and reorder accordingly
        else:
            lookupDict = self.GetNodeLabelLookupDict()
            rowNumbers = [lookupDict[nodeLabel] for nodeLabel in nodeLabelList]

            return np.array(varValues)[rowNumbers,:]
        
        
        
    def GetNodeSetLabels(self, nodeSetName):
        """
        """
        hdfObj = self.GetHdfObj()
        (entityName, runName) = self.GetCurrentEntityAndRun()
        
        sets = hdfObj[self.GetPath([entityName, 'sets', runName])]
        
        if not nodeSetName in sets.keys():
            errMsg = 'node set with name < {0} > not found'.format(nodeSetName)
            self.ErrorHandling(errMsg)
            
        return sets[nodeSetName]
