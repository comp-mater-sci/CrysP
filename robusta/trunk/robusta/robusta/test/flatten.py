from robusta.inuit.export2Flat import *

odbName = 'Job_PSC_2D_0_run0'
caeFileName = 'PSC_2D_0'
modelName = caeFileName
partName = 'sample'
folder = '/home/diarmuid/SYNC/IWT_sims/PSC/2D/latin_hypercube/fric_swiftn_dieLen'
runName = 'test_run'
entityName = 'test_entity'
hdfFileName = 'test_hdf'

instanceName = 'SHEET'

a = export2Flat(odbName=odbName, runName=runName, entityName=entityName, hdfFileName=hdfFileName, folder=folder, mode='append')


a.SetStepsToExtract()
a.EntityFromInstance(instanceName)
a.DefineXrefs()
a.ExportNodalVarList(varList=['U'], sourceType='nodal')
a.ExportNodalVarList(varList=['S','LE'], sourceType='IP')

a.ExportRootAssemblyNodeSets()
a.ExportAllNodeSetsForInstance(excludeSetNameList=[''], instanceName=instanceName)
a.ExportElementConnectivtyAsNodeSet(mdbName=caeFileName, modelName=modelName, partName=partName, folder=folder)
a.Close()
