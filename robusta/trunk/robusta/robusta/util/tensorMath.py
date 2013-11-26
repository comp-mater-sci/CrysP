""" functions for simple postprocessing tasks
"""
# import native modules

# import third part modules
try:
    import numpy as np
except ImportError:
    print 'Numpy needs to be installed and accessible.'
    raise
    
    
def VonMisesEquivStrain(data=None, le11=None, le22=None, le33=None, le12=0., le13=0., le23=0.):
    """ return the von Mises equivalent strain for the given, assuming the
        von Mises yield criterion defines the equivalent stress
    
        requires either:
            (i) 3 principal strain components (le11,le22,le33)
           (ii) all 6 components (le11, le22, le33, le12, le13, le23) 
          (iii) a 1x6 numpy array (keyword argument 'data') with the components
                in the above (Abaqus) order
    """
    
    # if a numpy array is given
    if not data is None:
        le11 = data[0]
        le22 = data[1]
        le33 = data[2]
        le12 = data[3]
        le13 = data[4]
        le23 = data[5]

    return (np.sqrt(2*( (le11-le22)**2 + (le22-le33)**2 + \
                   (le33-le11)**2 + 6*(le12**2+le23**2+le13**2) )))/3

        
