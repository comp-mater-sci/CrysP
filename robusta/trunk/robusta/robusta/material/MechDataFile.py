""" class representing mechanical test data files in various 'standard' formats
"""

# import third party modules
try:
    import numpy as np
    import matplotlib.pylab as plt
    from scipy.optimize import leastsq as LeastSquares
except ImportError:
    print 'One of the following is missing:'
    print 'NumPy'
    print 'Matplotlib'
    print 'SciPy'
    raise

# import native modules
import os

class MechDataFile():

    
    def __init__(self, fileName, folder=None, precision=np.float32):
        """
        """
        
        # defaults
        delimiter = ','
        self.thisObjectsData = {}
        self.testData = {}
               
        # check the file exists
        if folder is None: folder = os.getcwd()
        filePath = os.path.abspath(os.path.join(folder, fileName))
        fileExtension = os.path.splitext(fileName)[1].lower()
        if not os.path.isfile(filePath): raise IOError('could not open <{0}>'.format(filePath))
        
        # call the relevant method to read and parse the test data
        self.SetValue('sourceFilePath', filePath)
        self.SetValue('sourceFileExtension', fileExtension)
        self.SetValue('delimiter', delimiter)
        self.SetValue('precision', precision)

    
    def ReadTataRawTensile(self):
        """ raw data from tensile test for TATA steel
        """
        # read the file (delimiter is tab)
        self.SetValue('delimiter', '\t')
        data = self.ReadParseTextFile()
        
        # interpret the header info
        self.SetValue('testLabel', data[0][0].split(':')[1])
        (date, time) = data[1][0].split(' ')
        self.SetValue('date', date)
        self.SetValue('time', time)        
        
        # interpret the footer info
        footer = self.RemoveEmptyLines(data[-82:-1])
        self.SetValue('thickness', float(
                      self.FindInParsedBlock(block=footer, keyWord='dikte(mm)')))
        self.SetValue('width', float(
                      self.FindInParsedBlock(block=footer, keyWord='breedte(mm)')))
        self.SetValue('originalGaugeLength', float(
                      self.FindInParsedBlock(block=footer, keyWord='Lo(mm)')))
        self.SetValue('E', float(self.FindInParsedBlock(block=footer, keyWord='E(MPa)')))
        self.SetValue('tensileStrength', float(
                      self.FindInParsedBlock(block=footer, keyWord='Rm(MPa)')))
        self.SetValue('proofYieldStress', float(
                      self.FindInParsedBlock(block=footer, keyWord='Rp(MPa)')))
        
        # parse the data
        self.ConvertSaveDataText(data=data[5:-83])
        
        
    def ReadTataProcData(self):
        """ read processed tensile test data
        """
        # read the file
        self.SetValue('delimiter', ' ')
        data = self.ReadParseTextFile()
        
        # interpret the header nfo
        self.SetValue('testLabel', data[0][2])
        self.SetValue('date', data[1][8])
        self.SetValue('time', data[1][9])
        
        # parse the data
        self.ConvertSaveDataText(data=data[3:-2])
        
        
    def ConvertRawTensileToPlastic(self):
        """ convert raw tensile test data to plastic table data
        """
        strains = self.GetData(colName='Strain (%)')
        stresses = self.GetData(colName='Stress (MPa)')
        noStrainPoints = len(stresses)
        
        # determine the stress/strain values between the yield point and the
        # ultimate tensile strength
        yieldStress = self.GetValue('proofYieldStress')
        UTS = self.GetValue('tensileStrength')

        # find the yield point
        strainNotFound = True
        strainIndex = 0
        while(strainNotFound):
            
            if stresses[strainIndex] <= yieldStress and stresses[strainIndex+1] >= yieldStress:
                strainNotFound = False
                yieldStrainIndex = strainIndex
            
            strainIndex += 1
            if strainIndex +1 > noStrainPoints: raise Exception('Could not find yield point')
        
        # find the tensile strength point (continuing from the yield point)
        strainNotFound = True
        while(strainNotFound):
            
            if round(stresses[strainIndex]) <= UTS and round(stresses[strainIndex+1]) >= UTS:
                strainNotFound = False
                ultimateStrainIndex = strainIndex
            
            strainIndex += 1
            if strainIndex +1 > noStrainPoints: raise Exception('Could not find UTS point')
            
        # calculate true plastic stresses and strains from displacements and forces
        self.SetValue('yieldStrainIndex', yieldStrainIndex)
        self.SetValue('ultimateStrainIndex', ultimateStrainIndex)
        force = self.GetData(colName='Force (N)', startRow=yieldStrainIndex,
                                     endRow=ultimateStrainIndex)
        extension = self.GetData(colName='Extension (mm)', startRow=yieldStrainIndex,
                                     endRow=ultimateStrainIndex)
        
        engStrain = extension/self.GetValue('originalGaugeLength')
        engStress = force/(self.GetValue('width')*self.GetValue('thickness'))
        
        trueStress = engStress*(1 + engStrain)
        trueStrain = np.log(1 + engStrain)
        
        # shift the strain values so that they start at zero at yielding
        truePlasticStrain = trueStrain - trueStrain[0]
        
        # store the results
        self.testData.update({'plasticArray':{'stress':trueStress, 'strain':truePlasticStrain}})

    
    def FitVoce(self, sigma0Guess=None, AGuess=None, epsilon0Guess=None,
                scaleFactor=1.):
        """ finds the least squares fit for the Voce hardening law based on the
            data stored in 'plasticArray'
            
            scaleFactor is used to avoid numerical problems due to the strains
            typically being much smaller than the stresses
        """        
        
        if sigma0Guess is None or AGuess is None or epsilon0Guess is None:
            initialGuess = None
        else:
            initialGuess = [sigma0Guess, AGuess, epsilon0Guess * scaleFactor]
        
        # get the plastic table data
        data = self.testData.get('plasticArray')
        if data is None:
            raise Exception ('define the plasticArray first')
        else:
            stress = data['stress'].astype(np.float64)
            strain = data['strain'].astype(np.float64) * scaleFactor
        
        
        # use SciPy optimisation function "leastsq" to find the Voce parameters
        (voceParameters, null, diagnostics, message, errorLevel) = LeastSquares(
                         self.VoceResiduals, args=(strain, stress), x0=initialGuess,
                         full_output=True, Dfun=self.VoceJacobian, col_deriv=True)
        voceParameters[2] = voceParameters[2] / scaleFactor
        self.SetValue('voceParameters', voceParameters)
        self.SetValue('voceDiagnostic', diagnostics)
        
        # raise an exception if the fitting procedure didn't work
        if errorLevel in [1,2,3,4]:
            print message
            
        else:
            print ('Could not fit the voce parameters to the data')
            raise Exception(message)
    
    
    def Voce(self, parameters, epsilon, variant=False):
        """ the voce hardening law
            
            parameters = sigma0, K, n
            
            or for the variant:            
            parameters = sigma0, A, epsilon0 
        """
                
        if variant:
            sigma0, A, epsilon0 = parameters
            return sigma0 * (1 - np.exp((epsilon + epsilon0) * -A))
            
        else:
            sigma0, K, n = parameters
            return sigma0 + K*(1 - np.exp(-n*epsilon))
        
        
    def VoceResiduals(self, parameters, epsilon, sigma):
        """ calculates the residuals for the Voce function for the given parameters
        """
        return -(sigma - self.Voce(parameters, epsilon))
        
        
    def VoceJacobian(self, parameters, epsilon, sigma, variant=False):
        """ calculates the jacobian for the Voce function, in the form expected
            by scipy.optimize.leastsq. ie. this function should except the same
            inputs as the objective function (in this case "VoceResiduals") and
            should return the partial derivatives of the core function (in this
            case "Voce") with respect to each parameter
        
            parameters of Voce function = sigma0, A, epsilon0
            
            so this function returns [ d(Voce)/d(sigma0), d(Voce)/d(A),
                                       d(Voce)/d(epsilon0)]
                                       
            note that providing the return values in this way requires the
            flag col_deriv=True to be specified to leastsq (otherwise you
            would need to give the transpose of the result to avoid the error
            "there is a mismatch between the input and output shape of the 'Dfun'
            argument"
            
        """
        if variant:
            sigma0, A, epsilon0 = parameters
            epsSum = (epsilon + epsilon0)   
            exp = np.exp(epsSum * -A)
            
            return [(1 - exp), (sigma0 * epsSum * exp), (sigma0 * A * exp)]
            
        else:
            sigma0, K, n = parameters
            exp = np.exp(-n * epsilon)
                        
            return[np.ones(len(epsilon)), (1 - exp), (K * epsilon * exp)]
    
    
    def PlotData(self, xData, yData, xLabelName='', yLabelName='', origin=True):
        """ plot the given data
        """
        
        # plot the result
        plt.plot(xData, yData)
        plt.xlabel(xLabelName)
        plt.ylabel(yLabelName)
        
        if origin:
            plt.xlim([0, max(xData)])
            plt.ylim([0, max(yData)*1.25])
        plt.show()        
    
    
    def FindInParsedBlock(self, keyWord, block, returnSingleValue=True):
        """ return a single value by searching for the given key word in a parsed
            block of text (ie. a list of lists of strings)
            
            search is case insensitive
        """
        searchKey = keyWord.lower().strip()        
        values = [line for line in block if searchKey in line[0].lower().strip()]
        
        if values is None:
            return None
        elif returnSingleValue:
            return values[0][1].strip()
        else:
            return values[0].strip()


    def RemoveEmptyLines(self, block):
        """ remove lines where there is a keyword but no data, or where there is
            no data at all
        """
        return [line for line in block if len(line)>1]
        

    def ConvertSaveDataText(self, data):
        """ take a block of parsed text data, including column headers, convert
            it to floats and save it
        """
        # create a column header/ column number cross reference dictionary
        columnHeaders = data[0]
        crossRefDict = {}
        for headerIndex in range(len(columnHeaders)):
            crossRefDict.update({columnHeaders[headerIndex].strip():headerIndex})
        
        # save the data
        dataArray = np.array(data[1:-1], dtype=self.GetValue('precision'))
        self.testData.update({'dataArray':dataArray, 'crossRef':crossRefDict})
        
        
    def GetData(self, colName, startRow=0, endRow=None):
        """ return data for the column with the given header name
        """
        data = self.testData['dataArray']
        crossRef = self.testData['crossRef']
        if not colName in crossRef.keys():
            return None
        else:
            colNumber = crossRef[colName]
            if endRow is None: endRow = data.shape[0]
            
            return data[startRow:endRow, colNumber]

       
    def ReadParseTextFile(self):
        """ read and parse the data from a text file
        """
        # read the file
        filePath = self.GetValue('sourceFilePath')
        fileObj = open(filePath, 'r')
        data = fileObj.readlines()
        fileObj.close()
        print 'Read {0} lines from {1}'.format(len(data),
                                                os.path.basename(filePath))
        
        # parse the data
        delimiter = self.GetValue('delimiter')
        parsedData = []
        for line in data:
            parsedData.append(line.strip().split(delimiter))
        
        # if the delimiter is a space, there might be empty entries which need
        # to be removed
        if delimiter==' ':
            cleanData = []
            for line in data:
                cleanData.append([item.strip() for item in line if not item==''])
            
            return cleanData
        
        else:
            return parsedData

        
    def GetValue(self, parameterName, checkConfigFile=True, suppressError=False):
        """ returns the named parameter value 
        """        
        return self.thisObjectsData.get(parameterName)
        
        
    def SetValue(self, parameterName, parameterValue):
        """ sets the named parameter value
        """    
        self.thisObjectsData.update({parameterName:parameterValue})
