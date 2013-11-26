""" script to build the asymmetric rolling model
"""
import robusta.simbuild.asr as asr
from robusta.config import *
import sys
import os


def build(jobName, path, topRollRadius, botRollRadius, sheetThick,
          rollSpeed, rollSpeedRatio, topRollFriction, botRollFriction,
          sheetElementSize, sheetMoveTime, contactStepTime,
          rollingStepTime, reductionPercent, rollElementSize=defaultElementSize,
          sheetDepth=defaultPartSizeZ):
    
    # build the model
    asr.build(topRollRadius=topRollRadius, botRollRadius=botRollRadius,
              sheetThick=sheetThick, reductionPercent=reductionPercent,
              rollSpeed=rollSpeed, rollSpeedRatio=rollSpeedRatio,
              topRollFriction=topRollFriction, botRollFriction=botRollFriction,
              rollElementSize=rollElementSize, sheetElementSize=sheetElementSize,
              sheetMoveTime=sheetMoveTime, contactStepTime=contactStepTime,
              rollingStepTime=rollingStepTime, sheetDepth=sheetDepth)
    
    # write the input file
    filePath = os.path.join(path, '{0}.png'.format(jobName))    
    mdb.Job(name=jobName, model='rolling_sim', description='', type=ANALYSIS, 
        atTime=None, waitMinutes=0, waitHours=0, queue=None, 
        explicitPrecision=SINGLE, nodalOutputPrecision=SINGLE, echoPrint=OFF, 
        modelPrint=OFF, contactPrint=OFF, historyPrint=OFF, userSubroutine='', 
        scratch='', parallelizationMethodExplicit=DOMAIN, numDomains=1, 
        activateLoadBalancing=False, multiprocessingMode=DEFAULT, numCpus=1)
    mdb.jobs[jobName].writeInput(consistencyChecking=OFF)

    # print a screen shot of the mesh for checking
    session.viewports['Viewport: 1'].assemblyDisplay.setValues(mesh=ON, 
                                      optimizationTasks=OFF,
                                      geometricRestrictions=OFF,
                                      stopConditions=OFF)
    session.viewports['Viewport: 1'].assemblyDisplay.meshOptions.setValues(
                                      meshTechnique=ON)
    p1 = mdb.models['rolling_sim'].parts['sheet']
    session.viewports['Viewport: 1'].setValues(displayedObject=p1)
    session.viewports['Viewport: 1'].partDisplay.setValues(mesh=ON)
    session.viewports['Viewport: 1'].partDisplay.meshOptions.setValues(
                                      meshTechnique=ON)
    session.viewports['Viewport: 1'].partDisplay.geometryOptions.setValues(
                                      referenceRepresentation=OFF)
    session.viewports['Viewport: 1'].view.setValues(session.views['Front'])
    session.viewports['Viewport: 1'].view.zoom(zoomFactor=50, mode=ABSOLUTE)
    session.printOptions.setValues(vpDecorations=OFF, reduceColors=False, 
                                   compass=ON)
    session.printToFile(fileName=filePath, format=PNG,
                        canvasObjects=(session.viewports['Viewport: 1'], ))
