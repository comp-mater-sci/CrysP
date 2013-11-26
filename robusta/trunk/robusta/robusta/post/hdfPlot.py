# Prevent the legacy class type being used
__metaclass__ = type

# import robusta modules
from robusta.post.GenericPlot import *
from robusta.inuit.FlatODB import *
from robusta.config import *

# import third party modules
try:
    import h5py
    import numpy as np
    import matplotlib.pyplot as plt
except ImportError:
    print 'This module requires the following: H5py, HDF5, numpy, matplotlib.'
    raise
    
# import native Python modules
import os
from itertools import cycle


class hdfPlot(GenericPlot):
    """ class to generate and track xy data and xy plots using an external hdf
        based data file
    """
    
    def __init__(self, hdfFileName, folder=os.getcwd(), mode='append'):
        """ constructor
        """
        # call the parent class constructor
        GenericPlot.__init__(self, ODBname=hdfFileName, folder=folder, dbaseType='hdf')
        
        # open the hdf file
        hdfObj = FlatODB(hdfFileName=hdfFileName, folder=folder, mode=mode)
        self.SetValue('hdfObj', hdfObj)
                
        # set defaults
        self.SetValue('figureList', [])
        self.SetValue('plotLinesList', [])
        
    
    def GetHdfObj(self):
    
        return self.GetValue('hdfObj')
    
        
    def GetFigure(self):
        """
        """
        
        # create a new figure and add it to the figure list
        figureList = self.GetValue('figureList')
        newFigure = plt.Figure(dpi=matplotDPI, frameon=True)
        figureList.append(newFigure)
        self.SetValue('figureList', figureList)
        
        # apply standard figure settings
        newFigure.hold(True)
        
        return newFigure
        
        
    def GetColourCycle(self):
        """
        """
        return cycle(matplotColourList)
        
        
    def GetMarkerCycle(self):
        """
        """
        return cycle(matplotMarkerList)
        
        
    def PlotXY(self, xData, yData, xLabel, yLabel, legendLabels, title=None, scatter=False):
        """ xData and yData are expected to be 2D numpy arrays: each column is
            taken to be a series (matplotlib default)
        """
              
        
        # add new plot to plot list
        fig = self.GetFigure()
        plotLinesList = self.GetValue('plotLinesList')
        newPlotLines = plt.plot(xData, yData)
        plotLinesList.append(newPlotLines)
        self.SetValue('plotLinesList', newPlotLines)
        
        # apply standard plot settings
        lineColorSet = self.GetColourCycle()
        markerSet = self.GetMarkerCycle()
        index = 0
        if scatter:
            linestyle = '.'
        else:
            linestyle = '-'
        
        for line in newPlotLines:
            plt.setp(line, linewidth=matplotLineWidth, color=lineColorSet.next(),
                    marker=markerSet.next(), markeredgecolor='k', markeredgewidth=0.5,
                    antialiased=matplotAntiAliasing, linestyle=linestyle,
                    markersize=matplotMarkerSize, label=legendLabels[index])
            index += 1
        
        # add text
        plt.xlabel(xLabel)
        plt.ylabel(yLabel)
        if not title is None:
            plt.title(title)
            
                    
        
