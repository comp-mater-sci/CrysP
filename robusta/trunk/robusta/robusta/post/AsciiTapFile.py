""" class to represent an ascii "tap file" - a text file of input/output variables
    dumped from VUMAT

"""
# import native modules
import os
import sys
import glob

# import robusta modules
from robusta.GenericRobusta import *
from robusta.config import *

class AsciiTapFile(GenericRobusta):

    def __init__(self, name='Tap'):
        """ Constructor
        """
        
        # call base class constructor
        GenericRobusta.__init__(self, modelName=name, dbaseType='generic')
        
        # set some defaults
        self.SetValue('delimiter', ' ')
        self.SetValue('knownVariables', ['NDT', 'NBLOCK', 'NDIR', 'NSHR', 'NSTATEV',
                      'NFIELDV', 'NPROPS', 'LANNEAL', 'STEPTIME', 'TOTALTIME', 'DT',
                      'COORD', 'CHARLENGTH', 'PROPS', 'DENSITY', 'STRAININC',
                      'RELSPININC', 'TEMPOLD', 'STRETCHOLD', 'DEFGRADOLD', 'FVOLD',
                      'TEMPNEW', 'STRETCHNEW', 'DEFGRADNEW', 'FVNEW', 'S', 'SDV',
                      'ENERINTERN', 'ENERINELAS'])

                  
    def ReadTapFile(self, fileName, folder=os.getcwd(), discardFirstLine=True):
        """ read data from the ascii tap file
            if discardFirstLine==True the first data line is discarded - this
            is usually the case as VUMAT is called before the first 'proper'
            calculation increment to initialise
        """
        # read the data
        fileObj = open(os.path.join(folder, fileName), 'r')
        header = fileObj.readline()
        if discardFirstLine:
            null = fileObj.readline()
        textData = fileObj.readlines()
        fileObj.close()
        
        # detect the variables stored
        delimiter = self.GetValue('delimiter')
        variables = [item.strip() for item in header.split(delimiter) if not item=='']
        self.SetValue('variableNames', variables)
        
       
        # convert the ascii data to a numpy array
        noRows = len(textData)
        noCols = len([item for item in textData[0].split(delimiter)\
                      if not item==''])
        numData = np.zeros((noRows, noCols))
        for row in range(noRows):
            numData[row, :] = [float(col) for col in textData[row].split(delimiter)\
                                 if not col=='']
                                 
        self.SetValue('data', numData)
        
    
    def GetVariable(self, varName, asMatrix=False):
        """ return data for the named variable as an array for all increments
            
            columns are ordered depending on the variable type:
            scalar          single col
            2D vector       x,y
            3D vector       x,y,z
            symm tensor     11,22,33,12,13,23
            full tensor     11,12,13,21,22,23,31,32,33
            
        """
        # get the loaded data and find the columns of interest
        varNameLen = len(varName)
        loadedVariables = self.GetValue('variableNames')
        components = [name for name in loadedVariables if varName in name]
        if components==[]:
            raise Exception('No variable <{0}> in data'.format(varName))
        
        compNameList = [name[varNameLen:] for name in components]
        colList = [loadedVariables.index(name) for name in components]

        noCols = len(colList)
        data = self.GetValue('data')
        noRows = data.shape[0]        

        # return the data in the expected format, depending on the variable type
        if noCols==1:
            #varType = 'scalar'
            return data[:, colList[0]]
        
        elif noCols==2:
            #varType = '2Dvector'
            xCol = colList[compNameList.index('1')]
            yCol = colList[compNameList.index('2')]
            
            return np.hstack((data[:, xCol].reshape(noRows,1),
                               data[:, yCol].reshape(noRows,1)))
            
        elif noCols==3:
            #varType = '3Dvector'
            xCol = colList[compNameList.index('1')]
            yCol = colList[compNameList.index('2')]
            zCol = colList[compNameList.index('3')]
            
            return np.hstack((data[:, xCol].reshape(noRows,1),
                               data[:, yCol].reshape(noRows,1),
                               data[:, zCol].reshape(noRows,1)))
            
        elif noCols==6:
            #varType = 'stensor'
            col11 = colList[compNameList.index('11')]
            col22 = colList[compNameList.index('22')]
            col33 = colList[compNameList.index('33')]
            if '12' in compNameList:
                col12 = colList[compNameList.index('12')]
            else:
                col12 = colList[compNameList.index('21')]
            if '13' in compNameList:
                col13 = colList[compNameList.index('13')]
            else:
                col13 = colList[compNameList.index('31')]
            if '23' in compNameList:
                col23 = colList[compNameList.index('23')]
            else:
                col23 = colList[compNameList.index('32')]

            if asMatrix:
                result =  np.hstack((data[:, col11].reshape(noRows,1),
                                 data[:, col12].reshape(noRows,1),
                                 data[:, col13].reshape(noRows,1),
                                 data[:, col12].reshape(noRows,1),
                                 data[:, col22].reshape(noRows,1),
                                 data[:, col23].reshape(noRows,1),
                                 data[:, col13].reshape(noRows,1),
                                 data[:, col23].reshape(noRows,1),
                                 data[:, col33].reshape(noRows,1)))
            
                return result.reshape(noRows,3,3)
            
            else:
                result = np.hstack((data[:, col11].reshape(noRows,1),
                                    data[:, col22].reshape(noRows,1),
                                    data[:, col33].reshape(noRows,1),
                                    data[:, col12].reshape(noRows,1),
                                    data[:, col13].reshape(noRows,1),
                                    data[:, col23].reshape(noRows,1)))
            
                return result

        
        elif noCols==9:
            #varType = 'tensor'
            col11 = colList[compNameList.index('11')]
            col12 = colList[compNameList.index('12')]
            col13 = colList[compNameList.index('13')]
            col21 = colList[compNameList.index('21')]
            col22 = colList[compNameList.index('22')]
            col23 = colList[compNameList.index('23')]
            col31 = colList[compNameList.index('31')]
            col32 = colList[compNameList.index('32')]
            col33 = colList[compNameList.index('33')]

            result =  np.hstack((data[:, col11].reshape(noRows,1),
                                 data[:, col12].reshape(noRows,1),
                                 data[:, col13].reshape(noRows,1),
                                 data[:, col21].reshape(noRows,1),
                                 data[:, col22].reshape(noRows,1),
                                 data[:, col23].reshape(noRows,1),
                                 data[:, col31].reshape(noRows,1),
                                 data[:, col32].reshape(noRows,1),
                                 data[:, col33].reshape(noRows,1)))
            
            if asMatrix:
                return result.reshape(noRows,3,3)
            
            else:
                return result
                
        else:
            #varType = 'unknown'
            raise Exception('Could not determine variable type')

    def MatrixCompPrint(self, matrix1, matrix2, increment):
        """
        """
        delimiter = self.GetValue('delimiter')
        outFormat = ('|{0: 6.3e}{1}{2: 6.3e}{1}{3: 6.3e}|\t|{4: 6.3e}{1}{5: 6.3e}{1}{6: 6.3e}|\tINC:{7}\n'+
                     '|{8: 6.3e}{1}{9: 6.3e}{1}{10: 6.3e}|\t|{11: 6.3e}{1}{12: 6.3e}{1}{13: 6.3e}|\n' +
                     '|{14: 6.3e}{1}{15: 6.3e}{1}{16: 6.3e}|\t|{17: 6.3e}{1}{18: 6.3e}{1}{19: 6.3e}|\n\n')
        
        print outFormat.format(matrix1[0,0], delimiter, matrix1[0,1], matrix1[0,2],
                                matrix2[0,0], matrix2[0,1], matrix2[0,2], increment,
                                matrix1[1,0], matrix1[1,1], matrix1[1,2],
                                matrix2[1,0], matrix2[1,1], matrix2[1,2],
                                matrix1[2,0], matrix1[2,1], matrix1[2,2],
                                matrix2[2,0], matrix2[2,1], matrix2[2,2])
