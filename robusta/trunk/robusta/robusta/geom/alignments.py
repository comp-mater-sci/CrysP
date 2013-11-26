""" some miscelleaneous geometry related functions
"""

# Prevent the legacy class type being used
__metaclass__ = type

# import native Python modules
from warnings import warn

# import abaqus modules
try:
    from abaqusConstants import *
    from abaqus import *
except ImportError:
    print ('This module requires the abaqus modules to be available.')
    raise

# import third part modules
import numpy as np

# import robusta modules
from robusta.config import *


def DetermineClosestPrincipalPlane(point, limits, plane):
    """ determine if the given point is closer one of the principal planes
        
        "point" is a 2D or 3D point given as a tuple
        "plane" specifies which of the values of the tuple "point" to consider
        "limits" is a list of tuples which give min and max values for each
        of the axes of "point"
        
        returns 0 if the lower or 1 if the upper limit is closest to the point
    """
    
    position = point[plane]
    posDist = abs(position - limits[1])
    negDist = abs(position - limits[0])
    
    if posDist >= negDist:
        return 1
    else:
        return 0
    


def CategoriseVectorXYZ(vector):
    """ return an integer from (0,1,2) or (0,1) depending on how close the given
        vector (3D or 2D) is to the principal axes XYZ or XY. If it is equally close 
        to more than one axis it is assigned to the first such axis in (X,Y,Z) or
        (X,Y)
    """
    # convert the vector to a normalised column vector
    vector = np.abs(np.array(vector, dtype=defaultPrecision).reshape(len(vector),1))
    normVector = vector/np.linalg.norm(vector)
    
    # check dimensionality
    if len(vector)==2:
        # 2D case
        if np.dot([1,0], normVector) >= np.dot([0,1], normVector):
            return 0
        else:
            return 1
                
    elif len(vector)==3:
        # 3D case
        xDot = np.dot([1,0,0], normVector)
        yDot = np.dot([0,1,0], normVector)
        zDot = np.dot([0,0,1], normVector)
        maxDot = np.max((xDot, yDot, zDot))
        
        if xDot==maxDot:
            return 0
        elif yDot==maxDot:
            return 1
        else:
            return 2
        
    else:
        raise Exception('CategoriseVectorXYZ needs a 2D or 3D vector.')
    
    
