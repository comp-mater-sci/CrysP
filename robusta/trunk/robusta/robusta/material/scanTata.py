""" script to scan mechanical test data stored in the TATA formats for the 0deg
    tensile raw and processed data, and the shear test data
    
"""

# native modules
import os
import cPickle
import glob

# custom modules
from MechDataFile import *

# config
folder = os.getcwd()
saveFileName = os.path.basename(folder) + '.pik'

# check folder structure
tensileRawFolder = os.path.join(folder,'1-Uniaxial tensile test','Raw data files','0')
tensileProcFolder = os.path.join(folder,'1-Uniaxial tensile test',
                                 'Stress and strain data', '0')
shearTestFolder = os.path.join(folder, '4-Shear test', 'Stress and strain data',
                               '0 test for 45 principle stress')

if not(os.path.isdir(tensileRawFolder) and os.path.isdir(tensileProcFolder) and
        os.path.isdir(shearTestFolder)):
    raise IOError('The folder structure is not as expected.')
    
# load the raw tensile data
tensileRawFileList = glob.glob(os.path.join(tensileRawFolder,'*.MUS'))
tensileRaw = {}
voceGuess = [100.,1.,1.]
for fileName in tensileRawFileList:
    newTestObj = MechDataFile(fileName=fileName, folder=tensileRawFolder)
    newTestObj.ReadTataRawTensile()
    newTestObj.ConvertRawTensileToPlastic()
    newTestObj.FitVoce(voceGuess[0],voceGuess[1],voceGuess[2])
    tensileRaw.update({os.path.basename(fileName):newTestObj})

# save and quit
#allTests = {'tensileRaw':tensileRaw, 'tensileProc':tensileProc, 'shear':shear}
allTests = {'tensileRaw':tensileRaw}
cPickle.dump(obj=allTests, file=open(saveFileName, 'w'), protocol=-1)
