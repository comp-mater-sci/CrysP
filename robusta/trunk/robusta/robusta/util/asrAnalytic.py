""" functions realted to analytic formulae for rolling, in particular asymmetric
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

def GetKangStrain(initialSheet, finalSheet, largeRadius, smallRadius):
    """ calculate the shear tangent gamma for a given asymmetric rolling
        set up, based on the formula described by Kang et. al Metall. Mat.
        Trans. A. 36 (2005) p3141
    """
    
    D1 = initialSheet
    D2 = finalSheet
    Davg = np.mean((D1, D2))
    deltaD = D1 - D2
    R1 = largeRadius
    R2 = smallRadius
    
    return ((1/Davg)*((R1*np.arccos((R1-deltaD/2)/R1))-(R2*np.arccos((R2-deltaD/2)/R2))))
