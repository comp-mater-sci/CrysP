from abaqus import *
from abaqusConstants import *
from robusta.post.combineXY import *

folder = '/home/diarmuid/SYNC/IWT_sims/PSC/2D/latin_hypercube/fric_swiftn_dieLen'
nsetName = 'REFERENCE_POINT_HAMMER        1'
exclude = ['_2_','_10_','_9_','_11_','_6_','_8_','_4_','_14_','_16_','_0_','_12_']

a = combineXY('hammer_force')
a.MakeNewChartAndPlot('hammer_force')
a.GetODBListFromFolder(folder=folder, excludePatternList=exclude)
a.MakeXYDataForAllOdbs(variableName='RF', componentName='RF2', nodeSetName=nsetName, closeAfter=False)
a.PlotCombinedXYData()

b = combineXY('hammer_disp')
b.MakeNewChartAndPlot('hammer_disp')
b.GetODBListFromFolder(folder=folder, excludePatternList=exclude)
b.MakeXYDataForAllOdbs(variableName='U', componentName='U2', nodeSetName=nsetName, closeAfter=False)
b.PlotCombinedXYData()


