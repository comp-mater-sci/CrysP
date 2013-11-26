import odbAccess as odb
import numpy as np

a = odb.openOdb(path='workingCopies/abaqus/Job-1.odb')
b = a.rootAssembly.instances['SHEET'].nodes
x = [node.coordinates[0] for node in b]
y

d = np.array(x)
d.reshape((len(c),1))

from robusta.inuit.FlatODB import *
f = FlatODB('test')
f.BulkStoreFieldVariable(varName='x_coord',time=0,jobID='test_1',nativeIDList=range(1,len(d)+1),sourceType='node',dataColumn=d)

f.Close()
