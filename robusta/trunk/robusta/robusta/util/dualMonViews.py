from abaqus import *
from abaqusConstants import *
session.Viewport(name='Viewport: 1', origin=(0.0, 0.0), width=185.037503123283, 
    height=165.322496086359)
session.viewports['Viewport: 1'].makeCurrent()
session.viewports['Viewport: 1'].maximize()
from caeModules import *
from driverUtils import executeOnCaeStartup
executeOnCaeStartup()
session.viewports['Viewport: 1'].partDisplay.geometryOptions.setValues(
    referenceRepresentation=ON)
session.Viewport(name='Viewport: 2', origin=(8.47500014305115, 
    -100.81499761343), width=448.46875756979, height=257.677493900061)
session.viewports['Viewport: 2'].makeCurrent()
session.viewports['Viewport: 2'].maximize()
session.viewports['Viewport: 1'].restore()
session.viewports['Viewport: 2'].restore()
session.viewports['Viewport: 1'].setValues(origin=(0.0, -100.814987182617), 
    width=465.771881103516, height=266.137481689453)
#* RangeError: width must be a Float in the range: 30 <= width <= 452
session.viewports['Viewport: 2'].setValues(origin=(465.771881103516, 
    -100.814987182617), width=465.771881103516, height=266.137481689453)
#* RangeError: width must be a Float in the range: 30 <= width <= 452
session.viewports['Viewport: 2'].setValues(origin=(234.828125, 
    -102.224990844727))
session.viewports['Viewport: 2'].setValues(origin=(403.268768310547, 
    -101.872482299805))
session.viewports['Viewport: 2'].setValues(width=469.65625)
#* RangeError: width must be a Float in the range: 30 <= width <= 452
session.viewports['Viewport: 2'].setValues(origin=(337.234375, 
    -99.7574920654297))
session.viewports['Viewport: 2'].setValues(height=223.485000610352)
session.viewports['Viewport: 2'].setValues(origin=(405.740631103516, 
    -99.0525054931641))
session.viewports['Viewport: 2'].setValues(origin=(427.634368896484, 
    -99.7575073242188))
session.viewports['Viewport: 2'].setValues(origin=(488.018768310547, 
    -99.7575073242188), width=387.378143310547)
session.viewports['Viewport: 2'].setValues(origin=(562.528137207031, 
    -106.102500915527))
session.viewports['Viewport: 2'].setValues(origin=(678.0, -106.102493286133), 
    width=271.90625, height=259.087493896484)
session.viewports['Viewport: 2'].setValues(origin=(509.559387207031, -105.75), 
    width=439.640625, height=258.029998779297)
session.viewports['Viewport: 1'].setValues(origin=(0.0, -106.102478027344), 
    width=500.378143310547, height=271.072479248047)
#* RangeError: width must be a Float in the range: 30 <= width <= 452
session.viewports['Viewport: 1'].setValues(origin=(0.0, -58.5149993896484), 
    width=358.421875, height=223.485000610352)
session.viewports['Viewport: 1'].setValues(origin=(0.0, -79.6649932861328), 
    width=400.090637207031, height=244.634994506836)
session.viewports['Viewport: 1'].setValues(origin=(0.0, -91.2975006103516), 
    width=445.996887207031, height=256.619995117188)
session.viewports['Viewport: 1'].setValues(origin=(0.0, -103.634994506836), 
    width=443.524993896484, height=268.957489013672)
session.viewports['Viewport: 1'].setValues(width=254.956253051758)
session.viewports['Viewport: 1'].setValues(width=306.865631103516)
session.viewports['Viewport: 2'].setValues(origin=(574.887512207031, -105.75))
session.viewports['Viewport: 1'].setValues(width=263.078125)
session.Viewport(name='Viewport: 3', origin=(16.9500002861023, 
    -107.864997446537), width=448.46875756979, height=256.267493933439)
session.viewports['Viewport: 3'].makeCurrent()
session.viewports['Viewport: 3'].setValues(origin=(275.790618896484, 
    -107.864990234375), width=189.275009155273)
session.viewports['Viewport: 3'].setValues(width=288.149993896484)
session.viewports['Viewport: 3'].setValues(origin=(267.668762207031, 
    -92.7075042724609))
session.viewports['Viewport: 3'].setValues(origin=(267.668762207031, 
    -106.102508544922), width=296.978118896484, height=269.309997558594)
session.viewports['Viewport: 2'].setValues(height=238.994995117188)
session.graphicsOptions.setValues(backgroundStyle=SOLID, 
    backgroundColor='#FFFFFF', translucencyMode=2)