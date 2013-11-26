""" functions to convert between rotation matrices and Euler angles
"""

# Prevent the legacy class type being used
__metaclass__ = type

import warnings

# import robusta modules
from robusta.config import *

# import third party modules
try:
    import numpy as np
except ImportError:
    print 'This module requires the following: numpy'
    raise

def GetEulerAnglesForMatrix(cosineMatrix, units='degrees'):
    """ return the Bunge convention Euler angles phi1, PHI, phi2 
        for the given rotation matrix
    
        The formulae below are derived from the definition of the
        Euler angles as a series of rotations which give three
        rotation matrices R1, R2, R3. When combined into one
        matrix the relationship between the Euler angles and the
        33, 31 and 23 components of the matrix can be derived
        
        There is a special case when PHI = 0, as the values of
        phi1 and phi2 are then linked. Here phi2 is set to be
        zero in this case.
        
        The function 'CorrectedCosine' is used to prevent problems
        with cosines greater than 1 arising from round off error in Abaqus
    """
    R11 = CorrectedCosine(cosineMatrix[0,0])
    R33 = CorrectedCosine(cosineMatrix[2,2])
    PHI = np.arccos(R33)
    
    # note simpler relationships exist between the Euler angles, but
    # the more roundabout method here is needed because of round off error
    R23 = CorrectedCosine(cosineMatrix[1,2])
    R32 = CorrectedCosine(cosineMatrix[2,1])
    R31 = CorrectedCosine(cosineMatrix[2,0])
    R13 = CorrectedCosine(cosineMatrix[0,2])        
    
    sinPhiNomSquared = -(R23*R32 + R13*R31*R33)
    
    if PHI==0. or (np.sqrt(abs(sinPhiNomSquared)) < toleranceCosineZero and\
                    abs(R11) < toleranceCosineZero):
        # in this case can set phi2 to zero, and the rotation matrix is only
        # a function of phi1
        PHI = 0.
        phi2 = 0.        
        phi1 = np.arccos(R11)
        
    else:
        sinPHI = np.sqrt(sinPhiNomSquared/R11)
    
        phi1 = np.arccos((R23/sinPHI))
        phi2 = np.arcsin((R31/sinPHI))
    
    
    # convert the units if required
    if units=='degrees':
        return (np.degrees(phi1), np.degrees(PHI), np.degrees(phi2))
    
    elif units=='radians':    
        return (phi1, PHI, phi2)
    
    else:
        raise Exception('Unknown units {0}.'.format(units))
    

def CorrectedCosine(cosineValue):
    """ check if the given cosine value is greater than one or less than minus
        one, and return the 'corrected value' one or minus one accordingly
        
        also check if the value is very close to zero, (as defined by the
        value of toleranceCosineZero in the config file). If so, return zero
    """
    
    if cosineValue > 1.:
        return 1.
        
    elif cosineValue <-1.:
        return -1.
        
    elif abs(cosineValue) < toleranceCosineZero:
        return 0.
        
    else:
        return cosineValue
