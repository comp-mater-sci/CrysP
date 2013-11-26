# Prevent the legacy class type being used
__metaclass__ = type

 
# import native libraries
import os

# import robusta classes
from robusta.GenericRobusta import *
from robusta.config import *

# import third party modules
try:
    import h5py
except ImportError:
    print 'This module requires the H5py and HDF5 libraries to be accessible.'
    raise


class GenericHDF5(GenericRobusta):

    def __init__(self, hdfFileName, folder=os.getcwd(), mode='read'):
        """ Constructor
        """
              
        # call the base class method
        GenericRobusta.__init__(self, modelName=hdfFileName, folder=folder, dbaseType='hdf')
        
        # create or open the file as required
        fullFileName = hdfFileName+hdfFileExtension
        fullPath = os.path.join(folder, fullFileName)
        if os.path.isfile(fullPath) and mode=='read':
            
            hdfFileObj = h5py.File(fullPath, 'r')
            self.SetValue('hdfFileObj', hdfFileObj)
            
        elif mode=='write':
            hdfFileObj = h5py.File(fullPath, 'w')
            self.SetValue('hdfFileObj', hdfFileObj)
            
            if verbose: print 'Created <{0}> in folder <{1}>.'.format(fullFileName, folder)
            
        elif mode=='append':

            hdfFileObj = h5py.File(fullPath, 'a')
            self.SetValue('hdfFileObj', hdfFileObj)
        
            if verbose and not os.path.isfile(fullPath):
                print 'Created <{0}> in folder <{1}>.'.format(fullFileName, folder)

        else:
        # else there must have been a mistake with the given file info
            errMsg = 'Problem opening <{0}> in folder <{1}> with mode <{2}>. Check filename, path and mode are correct.'.format(fullFileName, folder, mode)
            self.ErrorHandling(errMsg, errType='io')
            
            
    def GetHdfObj(self):
        """ return the h5py file object
        """
        
        return self.GetValue('hdfFileObj')
        
        
    def Close(self):
        """ close the hdf file
        """
        hdfFile = self.GetHdfObj()
        hdfFile.close()
        
        
    def Tree(self, hdfDict=None, indent=1, depthLimit=10):
        """ display a tree structure for the current hdf file
        """
        if hdfDict is None:
            hdfDict = self.GetHdfObj()['/']
            
        self.__PrettyPrint__(hdfDict, indent=indent, depthLimit=depthLimit, currentDepth=0)
        
        
    def __PrettyPrint__(self, hdfDict, indent, depthLimit, currentDepth):
        """
        """
        for key, value in hdfDict.iteritems():
            print '\t' * indent + str(key)
            
            if isinstance(value, h5py._hl.group.Group) and currentDepth < depthLimit:
                self.__PrettyPrint__(value, indent=indent+1, depthLimit=depthLimit,
                                     currentDepth=currentDepth+1)
            else:
                print '\t' * (indent+1) + str(value)
                
                
    def Is2DNumpyArray(self, array):
        """ check ifrunName = self.GetValue('runName') the given array is a 2D numpy array
        """
        if type(array)==np.ndarray:
            if array.ndim==2:
                if not maxArrayShape[0] is None and not maxArrayShape[0] is None:
                    return (array.shape[0] <= maxArrayShape[0] and \
                             array.shape[1] <= maxArrayShape[1])
                else:
                    return True
                    
            else:
                return False
        else:
            return False 
