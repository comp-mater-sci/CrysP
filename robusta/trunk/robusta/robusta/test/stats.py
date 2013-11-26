from robusta.config import *
from robusta.post.statistics import *

a = statistics(hdfFileName='test', entityName='test_entity', mode='append', folder=os.getcwd())
b = a.GetValue('hdfObj')
b = a.CalculateElementInternalAngles(runName='test_run', frameNumbers=range(37))
print b.shape
