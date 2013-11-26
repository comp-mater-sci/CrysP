# Prevent the legacy class type being used
__metaclass__ = type

# import robusta modules
from robusta.GenericRobusta import *
from robusta.post.hdfPlot import *
from robusta.config import *

# import third party modules
try:
    import numpy as np
    from numpy.core.umath_tests import matrix_multiply as matMultiply
except ImportError:
    print 'This module requires the following: H5py, HDF5, numpy, matplotlib.'
    raise
    
# import native Python modules
import os

class statistics(GenericRobusta):
    """ a class for analysing the results of FE simulations
    """
    
    def __init__(self, hdfFileName, entityName, mode='append', folder=os.getcwd()):
        
        GenericRobusta.__init__(self, modelName=hdfFileName, dbaseType='hdf', folder=folder)
        
        # open the hdf database
        hdfObj = FlatODB(hdfFileName=hdfFileName, folder=folder, mode=mode)
        self.SetValue('hdfFileName', hdfFileName)
        self.SetValue('hdfObj', hdfObj)
        
        # select the run of interest
        hdfObj.SetCurrentEntity(entityName)
        self.SetValue('entityName', entityName)
        
        
    def CalcElHamalawiFactor(self, runName, frameNumbers):
        """ 'A simple and effective element distortion factor' A. El-Hamalawi,
            Computers and Structures, 75(2000) 507-513
            
            the 'factor' as implemented here assesses the level of distortion in
            a given list of (quad) elements by considering the deviation of the
            internal angles from the ideal 90 degrees.
            
            Note that the factor is not appropriate when the internal angles
            approach or exceed 180 deg (ie. you might get an indication of 'high
            quality' for such highly distorted elements)
        """
        # select the entity in the hdf file
        hdfObj = self.GetValue('hdfObj')
        hdfObj.SetCurrentRun(runName)
        self.SetValue('runName', runName)
        
        # for each frame number
        for frameNumber in frameNumbers:
        
            # get the data
            elementAngles = hdfObj.GetCalculatedValue(valueName= \
                                   'elEdgeAngles_time#-{0}'.format(frameNumber))
            
            # calculate the factor
            pass
        
        
        
        
    def CalculateElementInternalAngles(self, runName, frameNumbers):
        """ calculate the angles made up by the sets of element edges which share
            a common node
        """
        # select the entity in the hdf file
        hdfObj = self.GetValue('hdfObj')
        hdfObj.SetCurrentRun(runName)
        self.SetValue('runName', runName)
        
        # get the element edges (ie. the edge node labels)
        elementEdgeNodeSet = hdfObj.GetNodeSetLabels('ElementEdgeNodeLabels')

        nodeLabels = elementEdgeNodeSet[:,1::]
        noElements = elementEdgeNodeSet.shape[0]
        noNodesLabelsPerRow = elementEdgeNodeSet.shape[1] - 1
        noEdgesPerElement = noNodesLabelsPerRow/2
        noEdges = noElements * noEdgesPerElement
        
        cycleRange = ([noEdgesPerElement-1] + [0]*(noEdgesPerElement-2) + \
                      [-(noEdgesPerElement-1)]) * noElements
        cycleIndex = np.array(cycleRange) + np.arange(noEdges)

        # define a n-dimensional vector dot product function for arrays of vectors
        dotProduct = lambda vecArray1, vecArray2: matMultiply(vecArray1[:,None,:],\
                                                   vecArray2[:,:,None]).squeeze()
                
        # for every frame number in the list:
        for frameNumber in frameNumbers:
            
            # get the coordinates for the nodes
            coordinates = hdfObj.GetCoordsForNodeList(nodeLabels.flatten(),
                                                      timeFrameNumber=frameNumber)
                                                      
            # vectors making up the edges of the elements are arrived at by subtracting
            # the coordinates of first node from the second for each edge.
            edgeVectors = coordinates[0::2,:] - coordinates[1::2,:]
                                                       
            # take the dot product between adjacent edge vectors, in the same element only.
            # this is acheived by taking the dot product of the array of vectors with
            # a copy of itself in which the rows have been 'cycled' one position
            dotProducts = dotProduct(edgeVectors[:,:], edgeVectors[cycleIndex,:])
            
            # divide the dotProducts by the norms, and return the inverse cosine                       
            norms = np.array(map(np.linalg.norm, dotProducts))
            angles = np.reshape(np.arccos(dotProducts/norms),
                               (noElements, noEdgesPerElement), order='C')
            
            hdfObj.AddCalculatedValue(valueName='elEdgeAngles_time#-{0}'.format(frameNumber),
                                      valueArray=angles)

        
        
        
        
        
            
