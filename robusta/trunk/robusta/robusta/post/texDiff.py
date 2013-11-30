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

# third part libraries
from scipy.optimize import minimize

global ODFEXE
global LIB

if 'ROBUSTAHOME' in os.environ.keys():
    ODFEXE = os.path.join(os.environ['ROBUSTAHOME'], 'bin','win32')
    LIB = os.path.join(os.environ['ROBUSTAHOME'], 'lib','win32')
    
else:
    ODFEXE = os.path.abspath('c:/odf/odfexe')
    LIB = os.path.abspath('c:/odf')


def MinimiseDiff(constFileName, moveFileName, folder=os.getcwd(),
                 initialGuess=(0., 0., 0.)):
    """ minimise the difference between two texture files by rotating one
    """
    
    result = minimize(fun=PackedTextureDiff, x0=initialGuess, args=(constFileName,
             moveFileName, folder), method='BFGS')
             
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
    
    
    if not (sys.platform=='win32' and os.path.isfile(os.path.join(verschHome,'versch.exe'))):
        raise OSError('this function requires VERSCH.exe to be in the given ' +
                       ' directory <{0}>. Versch.exe currently only runs in on a ' +
                       'windows 32bit OS'.format(verschHome))
        
    # call Versch and feed the input stream (Versch is supposed to be interactive)
    verschProcess = Popen([os.path.join(verschHome,'versch.exe')], stdout=PIPE, stdin=PIPE, stderr=STDOUT)
    verschResult = verschProcess.communicate(input='{0}\n1\n{1}\n'.format(texFileName1, texFileName2))[0]
    
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
    
    if not (sys.platform=='win32' and os.path.isfile(os.path.join(rottexHome,'rottex.exe'))):
        raise OSError('this function requires rottex.exe to be in the given ' +
                       ' directory <{0}>. rottex.exe currently only runs in on a ' +
                       'windows 32bit OS'.format(rottexHome))
                                              
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
    configFile = open(os.path.join(folder, 'rottex.i00'), 'w')
    configFile.write('{0:10.3f} {1:9.3f} {2:9.3f}\n'.format(phi1, PHI, phi2))
    configFile.write('{0:5d} {1:4d}\n\n'.format(imagValue, idnValue))
    configFile.close()
        
    # call rottex
    rottexLocalPath = os.path.join(folder, 'rottex.exe')
    libLocalPath = os.path.join(folder, 'wagner.B04')
    shutil.copyfile(os.path.join(ODFEXE, 'rottex.exe'), rottexLocalPath)
    shutil.copyfile(os.path.join(LIB, 'wagner.B04'), libLocalPath)
    
    rottexProcess = check_call('rottex rottex.i00 rottex.l00 wagner.B04 {0} {1}'.format(texFileName, resultFileName))
    
    # return to starting state
    #os.remove(rottexLocalPath)
    #os.remove(libLocalPath)
    os.chdir(initialDir)
