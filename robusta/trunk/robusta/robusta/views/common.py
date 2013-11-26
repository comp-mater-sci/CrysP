""" common view settings for abaqus
"""
# import abaqus modules
try:
    from abaqus import *
    from abaqusConstants import *
    import xyPlot
except ImportError:
    print 'The abaqus modules need to be available for these scripts.'
    raise

# import native python moduls
from itertools import cycle

# get config info
from robusta.config import *


def SetBackground(viewportName=session.currentViewportName):
    """ set the background colour, font sizes, etc
    """
    fontString = GetFontString(fontSize=140)
    
    session.viewports[viewportName].partDisplay.geometryOptions.setValues(
                                     referenceRepresentation=ON)
    session.graphicsOptions.setValues(backgroundStyle=SOLID, 
                                     backgroundColor='#FFFFFF')
    session.viewports[viewportName].viewportAnnotationOptions.setValues(
                                     legendFont=fontString)
    session.viewports[viewportName].viewportAnnotationOptions.setValues(
                                     titleFont=fontString)
    session.viewports[viewportName].viewportAnnotationOptions.setValues(
                                     stateFont=fontString)
    session.viewports[viewportName].viewportAnnotationOptions.setValues(
                                     triadFont=fontString)
                                    
    session.viewports[viewportName].viewportAnnotationOptions.setValues(triad=ON, 
                                     compass=OFF)


def GetFontString(fontSize, fontName=globalFontName, fontWeight=globalFontWeight):
    """ return a string specifying the font details
    """
    
    return '-*-{0}-{1}-r-normal-*-*-{2}-*-*-p-*-*-*'.format(fontName, fontWeight, fontSize)
   
    
def SetLegendTitle(chartName, title):
    """
    """
    if not title is None:
        session.charts[chartName].legend.setValues(title=title)

                                     
def SetXYPlotDefaults(chartName, legendList=None):
    """ set the usual properties of the XY plot
    """
    chart = session.charts[chartName]
    fontString = GetFontString(fontSize=120)
    
    # set the x axis ticks and font details
    chart.axes1[0].labelStyle.setValues(font=fontString)
    chart.axes1[0].setValues(tickLength=3)
    chart.axes1[0].tickStyle.setValues(thickness=0.5)
    chart.axes1[0].axisData.setValues(minorTickCount=3)

    # set the y axis ticks and font details
    chart.axes2[0].labelStyle.setValues(font=fontString)
    chart.axes2[0].setValues(tickLength=3)
    chart.axes2[0].tickStyle.setValues(thickness=0.5)
    chart.axes2[0].axisData.setValues(minorTickCount=3)

    chart.gridArea.style.setValues(fill=False)

    # set the curve options
    curveNames = chart.curves.keys()
    if useXYPlotSymbols:
        symbolList = GetSymbolIterator()
        for curveName in curveNames:
            chart.curves[curveName].symbolStyle.setValues(show=True)
            chart.curves[curveName].symbolStyle.setValues(marker=symbolList.next())
            chart.curves[curveName].symbolStyle.setValues(size=2)
            
    # update the legend
    chart.legend.area.setValues(alignment=CENTER_RIGHT)
    fontString = GetFontString(fontSize=100)
    chart.legend.setValues(textStyle=xyPlot.TextStyle(font=fontString))
    
    noCurves = len(curveNames)
    if not legendList is None:
        if len(legendList)>=noCurves:
            for index in range(noCurves):
                curve = chart.curves[curveNames[index]]
                curve.setValues(useDefault=False, legendLabel=legendList[index])
        
    
def GetSymbolIterator():
    """ return an infinte iterator which gives an abaqus constant specifying a
        XY plot symbol
    """
    return cycle([FILLED_CIRCLE, FILLED_SQUARE, FILLED_DIAMOND, FILLED_TRI,
                  HOLLOW_CIRCLE, HOLLOW_SQUARE, HOLLOW_DIAMOND, HOLLOW_TRI,
                  CROSS, XMARKER, POINT])
    
    
def SetCurveThicknesses(curveList=session.curves, thickness=0.5):
    """ set all curves in the current session to have the given thickness
    """
    for curve in curveList:
        curve.lineStyle.setValues(thickness=thickness)
