""" class to extract data from the compressed archives found in the HMS
    locdir folder
"""

# Prevent the legacy class type being used
__metaclass__ = type

# import robusta modules
from robusta.GenericRobusta import *
from robusta.config import *

# import native Python modules
import os
import glob
import tarfile

# import third party modules
try:
    import numpy as np
except ImportError:
    print 'numpy needs to be installed and available.'
    raise


class HmsArchive(GenericRobusta):
    """ A class which will contain methods for accessing and processing the
        archived output data from a HMS simulation        
    """
    
    def __init__(self, name, simRootFolder=os.getcwd()):
        """ 
        """
        GenericRobusta.__init__(self, modelName=name, dbaseType='generic',
                                folder=simRootFolder)
                                
        # note the path to the simulation data
        self.SetValue('simRootFolder', simRootFolder)
        self.SetValue('locDirFolder', os.path.join(simRootFolder, defaultLocdirFolderName))
        
        # note the archive naming and type (usually defaults)
        self.SetValue('archiveExtension', defaultArchiveExtension)
        self.SetValue('archivePrefix', defaultArchivePrefix)
        self.SetValue('IPdata', {})
        self.SetValue('IPdataFilePrefix', hmsIPDataFilePrefix)
        
    
    def ScanLocDir(self):
        """ scan the locdir folder to see what data is available
        """
        locDirFolder = self.GetValue('locDirFolder')        
        
        # store the list of integration point data available
        IPdataFolderList = [os.path.join(locDirFolder, name) \
                            for name in os.listdir(os.path.abspath(locDirFolder))\
                            if os.path.isdir(os.path.join(locDirFolder, name))]
                            
        self.SetValue('IPdataFolderList', IPdataFolderList)
        
        # step into each folder and get the list of which increments there is
        # update  data available for
        for folder in IPdataFolderList:
            self.ScanIPFolder(IPfolderName=folder)


    def ScanIPFolder(self, IPfolderName=None):
        """ scan the named IP folder and get the  list of increments/ data file
            numbers available
        """
        if IPfolderName is None:
            IPfolderName = self.GetValue('IPdataFolderList')[0]
        
        elif not IPfolderName in self.GetValue('IPdataFolderList'):
            raise KeyError('{0} is not in the list of IP data folders'.format(IPfolderName))
        
        # get the integration point number from the folder name
        IPnumber = self.DecodeIPFolderName(name=IPfolderName)
        
        # get the details of the archive directories and file naming
        locDirFolder = self.GetValue('locDirFolder')
        IPFolder = os.path.join(locDirFolder, str(IPnumber))
        
        filePrefix = self.GetValue('archivePrefix')
        fileExtension = self.GetValue('archiveExtension')
        
        # get the archive file list
        searchFileLocation = os.path.join(IPfolderName, '*.{0}'.format(fileExtension))
        archiveList = [name for name in glob.glob(searchFileLocation)]
        
        # decode the names and add them to the list
        incrementList = {}
        fileNumberList = {}
        for name in archiveList:
            (incrementNumber, dataFileNumber) = self.DecodeArchiveName(name)
            incrementList.update({incrementNumber:dataFileNumber})
            fileNumberList.update({dataFileNumber:incrementNumber})
            
        IPdata = self.GetValue('IPdata')
        IPdata.update({IPnumber:{'incrementList':incrementList,
                      'fileNumberList':fileNumberList, 'fileNames':archiveList,
                      'strain':{}}})
        self.SetValue('IPdata', IPdata)
        
        
    def DecodeArchiveName(self, name):
        """ decode the given archive data file name into increment name and 
            file number
        """
        fileName = os.path.basename(name)
        
        fileExtension = self.GetValue('archiveExtension')
        filePrefix = self.GetValue('archivePrefix')
        
        namePartList = fileName.translate(None, fileExtension).translate(None, filePrefix).split('_')
        incrementNumber = namePartList[0]
        fileNumber = namePartList[1]
        
        return(incrementNumber, fileNumber)
        
        
        
    def DecodeIPFolderName(self, name):
        """ determine the IP number based on the given name
        """
        decodeID = os.path.basename(name).split('_')
        IPID = str((int(decodeID[-2]) * 136) + int(decodeID[-1]))
        
        if int(IPID)==float(IPID):
            return int(IPID)
        
        else:
            raise Exception('Non integer IPID found <{0}> based on name <{1}>'.format(IPID, name))
        
        
    def GetStrainIncrementsForIP(self, IPnumber, snapNumberList=None):
        """ get the plastic strain increment data for all of the texture updates
        
            snapNumberList is a list of update file numbers (starting at zero)
            if not specified, all available archive files will be used
        """
        
        # get the available integration point data
        IPdata = self.GetValue('IPdata')
        IPnumberList = IPdata.keys()
        
        if IPdata is None:
            raise Exception('Use ScanLocDir or ScanIPFolder first.')
            
        if not IPnumber in IPnumberList:
            if verbose:
                print 'Data is available for these IPs:\n'
                print (IPnumberList)
            raise KeyError('No IP number {0} found.'.format(IPnumber))
        
        # if no snap numbers are specified, get the list of all available           
        data = IPdata[IPnumber]
        if snapNumberList is None:
            snapNumberList = data['fileNumberList'].keys()
            
        # get the strains for the specified snap numbers
        strainDataForIP  = data['strain']
        archiveFileNames = data['fileNames']

            
        for fileName in archiveFileNames:
            
            # get the increment number
            incrementNumber = self.DecodeArchiveName(os.path.basename(fileName))[1]
            
            # open the tar archive
            archive = tarfile.open(fileName, 'r')
            fileList = archive.getnames()
            if not hmsStrainDataFileName in fileList:
                raise Exception('The strain data file <{0}> was not found in archive {1}.').format(
                                  hmsStrainDataFileName, fileName)
            
            # extract the strain increment data file and read the data
            tempFile = archive.extractfile(hmsStrainDataFileName)
            rawStrainData = tempFile.readlines()
            tempFile.close()            
            archive.close()
            
            # parse the raw data. Only interested in the 11,22,33,12,13 components
            strainMatrix = np.array([[float(i) for i in rawStrainData[j].strip().split(' ') \
                                    if not i==''] for j in range(1,4,1)])
            strainComponents = {'E11':strainMatrix[0,0], 'E22':strainMatrix[1,1],
                                'E33':strainMatrix[2,2], 'E12':strainMatrix[0,1],
                                'E13':strainMatrix[0,2], 'E23':strainMatrix[1,2]}
            strainDataForIP.update({incrementNumber:strainComponents})
            
            
        # update the object data
        data.update({'strain':strainDataForIP})
        IPdata.update({IPnumber:data})
        self.SetValue('IPdata', IPdata)


    def WriteStrainIncrementForIP(self, IPnumber, fileName='strain_incs.txt', folder=None,
                                  components=['E11','E22','E33','E12','E13','E23'],
                                  delimiter=','):
        """ export the strain data for the given integration point number
        """
        
        # open the output file
        if folder is None:
            folder = self.GetValue('simRootFolder')
            
        fileID = open(os.path.join(folder, fileName), 'w')
        
        # write the header
        fileID.write('Strain output for IP {0}.\n'.format(IPnumber))
        numColumns = len(components)               
        formatString = '{0}' + delimiter + delimiter.join(['{1['+str(i)+']}' \
                       for i in range(numColumns)]) + '{0}\n'.format(delimiter)
        fileID.write(formatString.format('incNum', components))
        
        # write the data and close the file
        data = self.GetValue('IPdata')[IPnumber]['strain']
        incrementNums = data.keys()
        
        for incNum in incrementNums:
            dataForInc = data[incNum]
            itemList = [dataForInc[compName] for compName in components]
            fileID.write(formatString.format(incNum, itemList))
        
        fileID.close()
