# Prevent the legacy class type being used
__metaclass__ = type

# import robusta modules
from robusta.post.GenericPlot import *
from robusta.config import *

# import abaqus modules
try:
    from abaqusConstants import *
    from abaqus import *
    import xyPlot
except ImportError:
    print ('This module requires the abaqus modules to be available.')
    raise
    
# import native Python modules
import os


class dataXY(GenericPlot):
    """ class to generate and track xy data and xy plots in the abaqus session
        environment
    """
    def __init__(self, ODBname, folder=os.getcwd()):
        """ constructor
        """
        GenericPlot.__init__(self, ODBname=ODBname, folder=folder, dbaseType='native')
      
    
    def MakeNodalXYPlot(self, plotName, nodeSetName, variableName, componentName, title=None, legendList=None):
        """ create XY data in the session XY repository (not in the ODB) and create
            a plot to go with it
        """
        # import robusta classes and modules
        from robusta.views import common as view
        from robusta.util import units

        # check if the given plot name can be used
        existingSessionPlots = session.xyPlots.keys()
        if plotName in existingSessionPlots:
            errMsg = 'A plot with name <{0}> already exists.'.format(plotName)
            self.ErrorHandling(errMsg)
        
        # create (extract) the required XY data for the plot
        newXYData = self.ExtractNodalXYData(variableName, componentName, nodeSetName)
        
        # create the plot object in the Abaqus session
        plotObj = session.XYPlot(plotName)
        chartName = plotObj.charts.keys()[0]
        chart = plotObj.charts[chartName]
        
        # store references to the new plot object
        self.SetValue('chartName',chartName)
        self.SetValue('xyDataObjName',newXYData[0].name)
        self.SetValue('objToExport', plotObj)
        self.SetValue('viewportName', session.currentViewportName)

        # create the plot
        (curveList, XAxisQuantity, YAxisQuantity) = self.SetPlotCurves(
                                                    xyData=newXYData, chart=chart)
        
        # get axis titles and scaling factor
        (xUnits, xUnitsMultiplier) = units.GetUnitName(XAxisQuantity.type, version='mf')
        (yUnits, yUnitsMultiplier) = units.GetUnitName(YAxisQuantity.type, version='mf')
        xTitle = self.MakeAxisTitle(XAxisQuantity.label, xUnits)
        yTitle = self.MakeAxisTitle(YAxisQuantity.label, yUnits)
        
        # scale and plot data. Note that the curve list is replaced with curves
        # for the scaled data
        allData = newXYData[0].data
        scaledData = self.ScaleXYData(xUnitsMultiplier, yUnitsMultiplier, allData)
        newXYData[0].setValues(data=scaledData)        
        (curveList, XAxisQuantity, YAxisQuantity) = self.SetPlotCurves(
                                                    xyData=newXYData, chart=chart)
        
        # format the plot
        view.SetXYPlotDefaults(chartName=chartName, legendList=legendList)
        view.SetLegendTitle(chartName, title)
        view.SetCurveThicknesses(curveList=curveList)
        self.ResetCurrentViewPortDims()
        
        chart.axes1[0].axisData.setValues(useSystemTitle=False, title=xTitle)
        chart.axes2[0].axisData.setValues(useSystemTitle=False, title=yTitle)


    def SetPlotCurves(self, xyData, chart):
        """
        """
        curveList = session.curveSet(xyData=xyData)
        chart.setValues(curvesToPlot=curveList)
        XAxisQuantity = chart.axes1[0].axisData.quantityType
        YAxisQuantity = chart.axes2[0].axisData.quantityType
        
        return (curveList, XAxisQuantity, YAxisQuantity)


    def ExtractNodalXYData(self, variableName, componentName, nodeSetName):
        """ get a list of data for the given node set and variable
        """
        # switch the current viewport to show the loaded odb. This is necessary
        # for the XY data extraction to work.
        self.FocusOnThisODB()
        
        odb = self.GetOdbHandle()
                
        return xyPlot.xyDataListFromField(odb=odb, outputPosition=NODAL,
                       variable=((variableName, NODAL, ((COMPONENT,
                       componentName), )), ), nodeSets=(nodeSetName, ))


    def MakeAxisTitle(self, quantityName, units):
        """ return a string to label an axis based on the quantity label and units string
        """
        return '{0} [{1}]'.format(quantityName, units)
        

    def ScaleXYData(self, xUnitsMultiplier, yUnitsMultiplier, data):
        """ scale the list of x,y data tuples by the corresponding multiplier
        """
        scaledData = []
        
        for dataPair in data:
            scaledData.append((dataPair[0]*xUnitsMultiplier, dataPair[1]*yUnitsMultiplier))
            
        return tuple(scaledData)

