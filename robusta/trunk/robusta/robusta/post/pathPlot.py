""" module which contains functions for producing path based plots
"""
# import abaqus modules
try:
    from abaqusConstants import *
    from abaqus import *
    import displayGroupOdbToolset as dgo
    import numpy as np
except ImportError:
    print ('This module requires the abaqus modules to be available.')
    raise

# import native modules
import os
import glob

# import robusta modules
from robusta.config import *


def CircularPathPlot(varList, radius, prefix, stepName, frameNo,
                      axisPoint1 = (0.,0.,0.), axisPoint2=(0.,0.,1.),
                      planePoint=(-1.,0.,0.), folder=os.getcwd(),
                      numPoints=50, imageSize=(1000,650),
                      viewPortName='Viewport: 1', odbView='Front',
                      toolInstanceNameList=[], includeIntersections=True,
                      shape=UNDEFORMED):
     """ produce plots (ps/png) for the given list of field variables,
         based on field values lying on a circular path, with given radius
         
         will process all odbs in the given/current folder
         
         eg  plt.CircularPathPlot(varList=['S','LE','CPRESS'], radius=25, prefix='R25', stepName='shear_step',frameNo=20, folder='/home/diarmuid/SYNC/IWT_sims/ShearSims/MTM_machine/3D/elSizeStudy_VM/')
     """

     # get the list of odbs
     path = os.path.join(folder, '*.odb')
     odbNameFileList = glob.glob(path)
     
     # shortcuts
     xyDataObj = session.xyDataObjects
     viewPort = session.viewports[viewPortName]
     
     print 'Found odbs: {0}'.format(odbNameFileList)

     # create the path
     pathName = 'circ_r={0}'.format(radius)
     session.Path(name=pathName, type=CIRCUMFERENTIAL,
                  expression=(axisPoint1, axisPoint2, planePoint),
                  circleDefinition=ORIGIN_AXIS, numSegments=numPoints, 
                  startAngle=0, endAngle=360, radius=radius)
     currentPath = session.paths[pathName]
                  
     # set general print options
     session.printOptions.setValues(vpDecorations=OFF, reduceColors=False)
     
         
     # for each odb
     for odbPath in odbNameFileList:
        
        # set shortcuts
        currentOdb = session.openOdb(odbPath)             
        steps = currentOdb.steps
        currentFileName = os.path.basename(odbPath)
        
        currentODBXYNames = []
        varCompList = []
        varDescriptionList = []
        
        # select the step and frame of interest (need to find the corresponding
        # step 'number'. Note that you need to subtract 1 from step.number for this)
        viewPort.setValues(displayedObject=currentOdb)
        stepNumberXref = {}
        for name in steps.keys():
            stepNumberXref.update({name:steps[name].number-1})

        viewPort.odbDisplay.setFrame(step=stepNumberXref[stepName], frame=frameNo)
        currentStep = steps[stepName]
        currentFrame = currentStep.frames[frameNo]
        
        # reset the view and focus on the part of interest in undeformed state
        viewPort.view.setValues(session.views[odbView])
        viewPort.odbDisplay.display.setValues(plotState=(UNDEFORMED, ))
        session.pngOptions.setValues(imageSize=imageSize)
        
        # hide the tools
        if not toolInstanceNameList==[]:
            leaf = dgo.LeafFromPartInstance(partInstanceName=tuple(toolInstanceNameList))
            viewPort.odbDisplay.displayGroup.remove(leaf=leaf)
        
        # make a plot (of the path:: not possible?) of undeformed body
        undefImageFileName = '{0}:{1}_undef'.format(prefix, currentFileName)
        session.printToFile(fileName=os.path.join(folder, undefImageFileName),
                            format=PNG, canvasObjects=(viewPort, ))
        
        # for each field variable, get the path XY data for each available component
        for varName in varList:
            
            # get the list of availabel components & determine type
            fieldObj = currentFrame.fieldOutputs[varName]
            availableVarComps = fieldObj.componentLabels
            dataPosition = fieldObj.values[0].position
            
            # for each component:
            for varComp in availableVarComps:
                
                if not varComp in varCompList:
                    varCompList.append(varComp)
                    varDescriptionList.append(fieldObj.description)
                
                # set the current variable            
                viewPort.odbDisplay.setPrimaryVariable(variableLabel=varName,
                                    outputPosition=dataPosition,
                                    refinement=(COMPONENT, varComp))
                
                # generate the XY data
                XYDataName = '{0}:{1}_{2}'.format(prefix, currentFileName, varComp)
                currentODBXYNames.append(XYDataName)
                session.XYDataFromPath(name=XYDataName, path=currentPath,
                                       includeIntersections=includeIntersections, 
                                       shape=shape, labelType=NORM_DISTANCE)
             
                                      
            # save the XY data for this odb
            session.xyReportOptions.setValues(interpolation=ON)
            xyDataObjList = [xyDataObj[name] for name in currentODBXYNames]
            
            session.writeXYReport(fileName='{0}:XYdat_{1}'.format(prefix,
                                  currentFileName), xyData=tuple(xyDataObjList))
         
        # close the odb
        currentOdb.close()
        
        # make a comparison plot for each of the variable componenets, for all odbs
        for varComp in varCompList:
            
            # determine the names of the relevant xyData objects
            xyDataObjNames = [name for name in xyDataObj.keys() \
                              if name.endswith(varComp)]
                       
            # delete existing curves and plots
            newCurves = []
            curves = session.curves
            for name in curves.keys():
                del curves[name]
            
            plots = session.xyPlots
            for name in plots.keys():
                del plots[name]
                
            
            # for each odb with xy data, create new curves
            for xyDataName in xyDataObjNames:
                newCurve = session.Curve(xyData=xyDataObj[xyDataName])
                newCurves.append(newCurve)
                odbName = xyDataName[len(prefix)+1::].replace('.odb_{0}'.format(varComp),'')
                newCurve.setValues(useDefault=False, legendLabel=odbName)
                
            # create a plot, and add the curves
            newPlotObj = session.XYPlot('newPlot')
            newChartName = newPlotObj.charts.keys()[0]
            newChartObj = newPlotObj.charts[newChartName]
            newPlotObj.title.setValues(text='{0}: component {1}'.format(
                                       varDescriptionList[varCompList.index(varComp)],
                                       varComp))
            
            newChartObj.setValues(curvesToPlot=tuple(newCurves))
            
            # print the chart
            viewPort.setValues(displayedObject=newPlotObj)
            printFileName='{0}:plt_{1}'.format(prefix, varComp)
            session.printToFile(fileName=os.path.join(folder, printFileName),
                                format=PNG, canvasObjects=(viewPort,))
            
