""" script to test the asymmetric rolling model
"""
import robusta.simbuild.asr as asr
from robusta.config import *
import sys
import os

# config
jobName = 'Job-1'
path = os.path.abspath('C:/Users/Administrator/Documents/all/TEMP/')
filePath = os.path.join(path, '{0}.png'.format(jobName))


asr.build(topRollRadius=203., botRollRadius=203., sheetThick=1.556,
          reductionPercent=50, rollSpeed=10.,
          rollSpeedRatio=1.0, topRollFriction=0.4, botRollFriction=0.4,
          rollElementSize=defaultElementSize, sheetElementSize=0.2,
          sheetMoveTime=0.001, contactStepTime=0.1, rollingStepTime=4)
          
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
    
sys.exit()