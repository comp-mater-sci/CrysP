# Prevent the legacy class type being used
__metaclass__ = type

# import abaqus modules
try:
    from abaqusConstants import *
    from abaqus import *
    import numpy as np
except ImportError:
    print ('This module requires the abaqus modules to be available.')
    raise

# import native modules
import os
import cPickle

# import robusta modules
from robusta.GenericRobusta import *
from robusta.config import *


class PickleJar(GenericRobusta):
    """ class for querying collections of pickled robusta objects. cPickle is used
    
    """
    

    def __init__(self, name):
        """ Constructor
        """
        # call base class constructor
        GenericRobusta.__init__(self, modelName=name, dbaseType='generic')
    
    
    def LoadAllPickles(self):
        """ load all the pickled objects in the current directory
        """
        
        # pickled robusta objects are files ending with 'pickleFileExtension'
        fileList = [fileName for fileName in os.listdir(os.getcwd()) \
                    if fileName.endswith(pickleFileExtension) and \
                    os.path.isfile(fileName)]
                    
        # load the file list and store the object data
        self.SetValue('pickleList', fileList)
        for fileName in fileList:
        
            pickleFile = file(fileName, 'r')
            self.SetValue(fileName, cPickle.load(pickleFile))
            pickleFile.close()
            
            
    def PrintPropertyValue(self, propertyName):
        """ print the value of the named property for all loaded pickles
        """
        pickleList = self.GetValue('pickleList')
        
        for pickle in pickleList:
            
            currentPickle = self.GetValue(pickle)
            print 'pickle: {0}\t value: {1}'.format(pickle, currentPickle.GetValue(propertyName))
        
