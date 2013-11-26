# Prevent the legacy class type being used
__metaclass__ = type

# import robusta modules
from robusta.GenericRobusta import *
from robusta.config import *
from robusta.util import units as units

# import abaqus modules (or robusta modules that themselves need abaqus)
try:
    from abaqusConstants import *
    from abaqus import *
    import visualization
    
    from robusta.views import common as common
except ImportError:
    print ('This module may require the abaqus modules to be available.')

# import third party modules
try:
    import numpy as np
except ImportError:
    print 'Plotting requires numpy to be installed and available.'
    raise
    
try:
    import h5py    
    import matplotlib.pyplot as plt
except ImportError:
    print 'Warning: Matplotlib and/or h5py appear not to be installed.'

# import native modules
import os


class GenericPlot(GenericRobusta):
    """ A class which will contain all plotting methods.        
    """
    
    def __init__(self, ODBname, folder, dbaseType):
        """ Constructor
        """
        
        # call base class constructor
               
        if dbaseType=='hdf':
            GenericRobusta.__init__(self, modelName=ODBname, dbaseType='hdf', folder=folder)
            self.SetValue('dbaseType', dbaseType)
            
        elif dbaseType=='native' or dbaseType=='abaqus' or dbaseType=='odb':
            GenericRobusta.__init__(self, modelName=ODBname, dbaseType='odb', folder=folder)
            self.SetValue('dbaseType', dbaseType)
            
            # set up the plotting environment
            common.SetBackground()
            
        else:
            errMsg = 'Dont know how to define a plot object based on a DB of type <{0}>.'.format(dbaseType)
            self.ErrorHandling(errMsg)

 
            
    def FocusOnThisODB(self):
        """ make the odb to which this object is attached visible in the current
            viewport. This is required for certain cae modules to work correctly.
        """
        odb = self.GetOdbHandle()
        viewportName = session.currentViewportName
        self.SetValue('viewportName', viewportName)
        session.viewports[viewportName].setValues(displayedObject=odb)
        
        
    def UpdateUnits(self, quantity, unitName, multiplier):
        """ add the given unit type to the units dictionary
        """
        units = self.GetValue('unitsDict')
        units.update({quantity:{'unitName':unitName, 'multiplier':multiplier}})
        self.SetValue('unitsDict', units)
        
            
    def ResetCurrentViewPortDims(self):
        """ set the viewport dimensions to a standard size (see config file)
        """
        viewport = session.viewports[self.GetValue('viewportName')]
        
        viewport.restore()
        viewport.setValues(width=viewPortWidth)
        viewport.setValues(height=viewPortHeight)
        
    
    def GetPSResolution(self, resolution):
        """ return the closest match to the desired DPI for postscript export
        """
        possibleResolutions = np.array([75,150,300,600,1200]) 
        nearestMatchIndex = (np.abs(possibleResolutions-resolution)).argmin()
        
        return [DPI_75, DPI_150, DPI_300, DPI_600, DPI_1200][nearestMatchIndex]
        
        
    def PrintA4PS(self, fileName, resolution=printResolutionDPI):
        """ print the current viewport using default settings for postscript
        """
        # the viewport dimensions affect the exported image dimensions
        viewportName = self.GetValue('viewportName')
        objectToPlot = self.GetValue('objToExport')
        session.viewports[viewportName].setValues(displayedObject=objectToPlot)
        self.ResetCurrentViewPortDims()
        
        # postscript options
        dpi = self.GetPSResolution(resolution)
        session.psOptions.setValues(paperSize=A4, orientation=LANDSCAPE, logo=OFF, 
                date=OFF, resolution=dpi, shadingQuality=EXTRA_FINE)
        session.printOptions.setValues(vpDecorations=OFF)
        
        # print to file
        viewport = session.viewports[self.GetValue('viewportName')]
        session.printToFile(fileName=fileName, format=PS, canvasObjects=(viewport, ))
        
        
    def CloseODB(self):
        """ close the ODB associated with this object
        """
        odb = self.GetOdbHandle()
        odb.close()
