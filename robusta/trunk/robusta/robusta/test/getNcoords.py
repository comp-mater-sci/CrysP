from robusta.inuit.FlatODB import *

odbName = 'Job_PSC_2D_0_run0'
caeFileName = 'PSC_2D_0'
modelName = caeFileName
partName = 'sample'
folder = '/home/diarmuid/SYNC/IWT_sims/PSC/2D/latin_hypercube/fric_swiftn_dieLen'
runName = 'test_run'
entityName = 'test_entity'
hdfFileName = 'test_hdf'

instanceName = 'SHEET'

a = FlatODB(hdfFileName=hdfFileName, folder=folder, mode='append')
a.SetCurrentEntity(entityName)
a.SetCurrentRun(runName)

coordLabels = [1,2,3,50,60,70,500,5000,50000]

coordArray = a.GetCoordForNodeList(coordLabels)

print coordLabels
print coordArray
