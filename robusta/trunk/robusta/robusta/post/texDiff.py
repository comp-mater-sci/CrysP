""" module which calls VERSCH from the MTM-FHM package
    to calculate the difference between two textures.
    
    The main purpose at was to use this a residual calculator in
    the minimisation of the difference between two textures by 'rotating' one
    of the textures.
"""

# native libraries
from subprocess import Popen, PIPE, STDOUT, check_call
import os
import sys
import shutil
import glob
import stat

# third part libraries
from scipy.optimize import minimize
import numpy as np
#import matplotlib.pylab as plt

global ODFEXE
global LIB
platform = 'lx64'

if 'ROBUSTAHOME' in os.environ.keys():
    ODFEXE = os.path.join(os.environ['ROBUSTAHOME'], 'bin', platform)
    LIB = os.path.join(os.environ['ROBUSTAHOME'], 'lib', platform)
    
else:
    ODFEXE = os.path.abspath('c:/odf/odfexe')
    LIB = os.path.abspath('c:/odf')


def GetTexIndex(refKeyword='hms', folder=os.getcwd()):
    """ go through a list of c files containing the given keyword and return
        their texture indices
        
        c files should have the format prefix_filenumber_keyword.c
        
        requires the printc binary to be in the given folder (this needs to be
        cleaned up)
    """
    printc = './printc_lx64.exe'
    
    fileList = glob.glob('*{0}*.c'.format(refKeyword))
    fileNumbers = [int(name.split('_')[1]) for name in fileList]
    
    fileSortIndices = np.argsort(fileNumbers)
    noFiles = len(fileList)
    
    results = np.zeros((noFiles, 1))
    
    currentDir = os.getcwd()
    os.chdir(folder)
    
    for index in range(noFiles):
    
        fileName = fileList[fileSortIndices[index]]
        shutil.copyfile(os.path.join(folder, fileName),
                        os.path.join(folder, 'input.c'))
                        
        check_call(printc)
        
        outputFile = open('printc.l01', 'r')
        results[index] = float(outputFile.readlines()[-1].strip().split(' ')[-1])
        outputFile.close()
        
    os.chdir(currentDir)
    return (np.array(fileList)[fileSortIndices], results)
    

def GetVerschValFromNpy(folder=os.getcwd()):
    """ read the value calculated by Versch and stored in npy files in the
        given folder (current folder by default). The values returned are the
        minimum in each file
        
        file names should follow the format number_label.npy
        
        returns the list of file names and the corresponding list of minimum
        Versch values
    """
    fileList = glob.glob('*.npy')
    fileNumbers = [int(name.split('_')[0]) for name in fileList]
    
    fileSortIndices = np.argsort(fileNumbers)
    noFiles = len(fileList)
    
    results = np.zeros((noFiles, 1))
    
    for index in range(noFiles):
        
        fileName = fileList[fileSortIndices[index]]
        results[index] = np.min(np.load(fileName)[:,3])
        
    return (np.array(fileList)[fileSortIndices], results)
        

def ComparePairs(keyFixed='std', keyMove='hms', folder=os.getcwd()):
    """ compare pairs of c files in the current directory by calling SearchSpace
        on each pair
        
        file names should be "something_incrementnumber_keyword.c"
        
    """ 
    # get the file lists
    fixedNameList = glob.glob('*{0}*'.format(keyFixed))
    moveNameList = glob.glob('*{0}*'.format(keyMove))
    
    # sort the lists so that corresponding files are in the same positions in
    # each of the lists
    fixedFileNums = [int(fileName.split('_')[1]) for fileName in fixedNameList]
    fixedFileOrder = np.argsort(fixedFileNums)
    
    moveNameNums = [int(fileName.split('_')[1]) for fileName in moveNameList]
    moveFileOrder = np.argsort(moveNameNums)
    
    # check if all files have a corresponding pair
    matches = np.array([num in fixedFileNums for num in moveNameNums])
    noFiles = len(fixedFileNums)
    if not (matches.all() and len(moveNameNums)==noFiles):
        missing = np.array(moveNameList)[np.logical_not(matches)]
        print missing, len(missing), noFiles, len(moveNameNums)
        raise Exception('The files listed above are missing a match')
        
    # start analysis
    minFile = open('minVals.txt', 'w')
    minFile.write('File1,File2,minRotation\n')
    for fileNum in range(noFiles):
    
        # files are copied to temp files in order stay within the 8 character
        # limit for ROTTEX
        constFileName = fixedNameList[fixedFileOrder[fileNum]]
        moveFileName = moveNameList[moveFileOrder[fileNum]]        
        print 'Comparing {0} and {1}:'.format(constFileName, moveFileName)
        
        shutil.copyfile(os.path.join(folder, constFileName),
                        os.path.join(folder, 'const.c'))
        shutil.copyfile(os.path.join(folder, moveFileName),
                        os.path.join(folder, 'move.c'))

        # write results to file
        minVal = SearchSpace(constFileName='const.c', moveFileName='move.c',
                             resultFileName='{0}_res.npy'.format(fixedFileNums[fixedFileOrder[fileNum]]))
        minFile.write('{0},{1},{2}\n'.format(constFileName, moveFileName,minVal))

    minFile.close()
    

def SearchSpace(constFileName, moveFileName, limPhi1=180, limPHI=1, limPhi2=1,
                step=1.0, folder=os.getcwd(), resultFileName='output'):
    """ search the entire rotation space to see how closely two textures can be
        made to coincide by rotating one of them
    """
    # get test values
    noPhi1Values = int(limPhi1/step)
    phi1Values = np.linspace(0, limPhi1, noPhi1Values)
    
    noPHIValues = int(limPHI/step)
    PHIValues = np.linspace(0, limPHI, noPHIValues)
    
    noPhi2Values = int(limPhi2/step)
    phi2Values = np.linspace(0, limPhi2, noPhi2Values)
    
    # calculate texture differences
    difference = np.zeros((noPhi1Values*noPHIValues*noPhi2Values,4))
    diffIndex = 0
    
    for phi1index in range(noPhi1Values):
        
        for PHIindex in range(noPHIValues):
            
            for phi2index in range(noPhi2Values):
                sys.stdout.write('.')
                sys.stdout.flush()
                parameters = (phi1Values[phi1index], PHIValues[PHIindex],
                              phi2Values[phi2index])                
                diffValue =  PackedTextureDiff(constFileName=constFileName,
                                                moveFileName=moveFileName,
                                               folder=folder, parameters=parameters)
                difference[diffIndex, :] = np.hstack((parameters, diffValue))
                diffIndex +=1
                
    # save the result and return the minimum for information
    print '\n'
    np.save(resultFileName, difference)       
    
    minDiff = np.argmin(difference[:,3])
    print 'min appears to be phi1 {0[0]:5.2f} PHI {0[1]:5.2f} phi2{0[2]:5.2f}'.format(difference[minDiff,0:3])
    return minDiff
    
    
def MinimiseDiff(constFileName, moveFileName, folder=os.getcwd(),
                 initialGuess=(0., 0., 0.)):
    """ minimise the difference between two texture files by rotating one
    """
    
    result = minimize(fun=PackedTextureDiff, x0=initialGuess, args=(constFileName,
             moveFileName, folder), method='Nelder-Mead')
             
    return result
    

def PackedTextureDiff(parameters, constFileName, moveFileName, folder):
    """ this function combines the TextureDiff and RotateTexture functions in a
        way that scipy minimise can use them
        
        triclinic symmetry is assumed in the rotation
        
    """
    # unpack arguments
    (phi1, PHI, phi2) = parameters
    #(constFileName, moveFileName, folder) = arguments
    
    # rotate one texture
    tempFileName = 'minrot.c'
    RotateTexture(texFileName=moveFileName, folder=folder, phi1=phi1, PHI=PHI,
                  phi2=phi2, resultFileName=tempFileName)
                  
    # return the difference between the rotated and the other non-rotated texture
    return TextureDiff(texFileName1=constFileName, texFileName2=tempFileName, folder=folder)
    
    
def TextureDiff(texFileName1, texFileName2, folder=os.getcwd(), verschHome=ODFEXE):
    """ return the texture difference between two textures expressed as c files.
    
        uses VERSCH from the MTM-FHM package
    """
    texFilePath1 = os.path.join(folder, texFileName1)
    texFilePath2 = os.path.join(folder, texFileName2)
    initialDir = os.getcwd()
    os.chdir(folder)
    
    # check the input is ok, and that we are running on windows 32bit (for now)
    if not (os.path.isfile(texFilePath1) and os.path.isfile(texFilePath2)):
        raise IOError('one or more files not found: {0}\t{1}'.format(texFilePath1, texFilePath2))

        
    # call Versch and feed the input stream (Versch is supposed to be interactive)
    if sys.platform=='win32':
        verschProcess = Popen([os.path.join(verschHome,'VERSCH.EXE')], stdout=PIPE, stdin=PIPE, stderr=STDOUT)
        verschResult = verschProcess.communicate(input='{0}\n1\n{1}\n'.format(texFileName1, texFileName2))[0]
        
    elif 'linux' in sys.platform:
        binPath =  os.path.join(folder, 'versch_lx64.exe')
        if not os.path.isfile(binPath):
            shutil.copyfile(os.path.join(ODFEXE, 'versch_lx64.exe'), binPath)
            permissions = os.stat(binPath)
            os.chmod(binPath, permissions.st_mode or stat.S_IEXEC)
        
        shutil.copyfile(os.path.join(folder, texFileName1), os.path.join(folder, 'first.c'))
        shutil.copyfile(os.path.join(folder, texFileName2), os.path.join(folder, 'second.c'))
        
        verschResult = check_call(os.path.join(folder, './versch_lx64.exe'))
    else:
        OSError('Dont know if Versch will run on platform {0}'.format(sys.platform))    

    
    # read the result from the output file (hard coded as 'versch.l01')
    resultFile = open('versch.l01', 'r')
    parsedDataLine2 =  [item.strip() for item in resultFile.readlines()[1].split(' ') \
                        if not item.strip()=='']
    resultFile.close()
    
    os.chdir(initialDir)
    return float(parsedDataLine2[0])
    
    
def RotateTexture(texFileName, folder=os.getcwd(), phi1=0., PHI=0., phi2=0.,
                  rottexHome=ODFEXE, symmetry='triclinic', resultFileName='result.c',
                  libPath=LIB):
    """ use ROTTEX to apply a rotation to a texture file (c coefficients file)
    """
    
    texFilePath = os.path.join(folder, texFileName)
    initialDir = os.getcwd()
    os.chdir(folder)
    
    # check the input is ok, and that we are running on windows 32bit (for now)
    if len(os.path.splitext(texFileName)[0]) > 8:
        raise OSError('texture file name must be 8 characters or less')
    
    if not (os.path.isfile(texFilePath)):
        raise IOError('file not found: {0}'.format(texFilePath))

                                              
    # determine symmetry flag values
    if symmetry=='triclinic':
        imagValue = 2
        idnValue = 1
    
    elif symmetry=='monoclinic':
        imagValue = 2
        idnValue = 2
        
    elif symmetry=='orthorhombic':
        imagValue = 1
        idnValue = 2
    
    else:
        raise KeyError('Unknown symmetry case <{0}>'.format(symmetry))
        
    # build the config file for rottex
    configFile = open(os.path.join(folder, 'ROTTEX.I01'), 'w')
    configFile.write('{0:10.3f} {1:9.3f} {2:9.3f}\n'.format(phi1, PHI, phi2))
    configFile.write('{0:5d} {1:4d}\n\n'.format(imagValue, idnValue))
    configFile.close()
        
    # call rottex
    
    if sys.platform=='win32':

        batch = open('rot.bat', 'w')
        batch.write('rottex ROTTEX.I01 rottex.l00 wagner.B04 {0} {1}\n\n'.format(texFileName, resultFileName))
        batch.close()
        
        rottexLocalPath = os.path.join(folder, 'rottex.exe')
        libLocalPath = os.path.join(folder, 'wagner.B04')
        
        if not os.path.isfile(rottexLocalPath):
            shutil.copyfile(os.path.join(ODFEXE, 'rottex.exe'), rottexLocalPath)
            shutil.copyfile(os.path.join(LIB, 'wagner.B04'), libLocalPath)
        
        rottexProcess = check_call('rot.bat', shell=True)
    
    elif 'linux' in sys.platform:
        
        rottexLocalPath = os.path.join(folder, 'rottex_lx64.exe')
        libLocalPath = os.path.join(folder, 'WAGNER.B04')
        
        if not os.path.isfile(rottexLocalPath):
            shutil.copyfile(os.path.join(ODFEXE, 'rottex_lx64.exe'), rottexLocalPath)
            shutil.copyfile(os.path.join(LIB, 'WAGNER.B04'), libLocalPath)
            permissions = os.stat(rottexLocalPath)
            os.chmod(rottexLocalPath, permissions.st_mode or stat.S_IEXEC)
        
        shutil.copyfile(os.path.join(folder, texFileName), os.path.join(folder, 'INPUT.C'))
        rottexProcess = check_call('./rottex_lx64.exe')
        os.rename('OUTPUT.C', resultFileName)
    
    # return to starting state
    #os.remove(rottexLocalPath)
    #os.remove(libLocalPath)
    os.chdir(initialDir)
