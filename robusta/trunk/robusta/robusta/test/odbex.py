#def testExport():
from robusta.inuit.odb2Simple import *
sampleName = 'SHEET'
a = odb2Simple('Job-1')
a.ExportNodeSets()
a.ExportNodeCoords(sampleName)
a.ExportFieldVarsForInstance(instanceName=sampleName, ignoreVars=['PE','A','AR','V', 'VR'])
a.Close()

#import cProfile
#cProfile.run('testExport()', 'prof_results.txt')
