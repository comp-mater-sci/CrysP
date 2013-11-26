import cPickle, glob
import matplotlib.pylab as plt

execfile('scanTata.py')

name = glob.glob('*.pik')[0]
a = cPickle.load(open(name))
b = a['tensileRaw']['20010161-L-3_77-146_97772_2606.MUS']
c = a['tensileProc']['20010161-L-3_77-146_97772_2606.str']

d = b.testData['plasticArray']
strStress = c.GetData(colName='sig(MPa)')
strStrain = c.GetData(colName='eps-long')
plStress = d['stress']
plStrain = d['strain']

plt.plot(plStrain, plStress, label='mine')
plt.plot(strStrain, strStress, label='str')
plt.xlim([0, max(strStrain.max(), plStrain.max())])
plt.ylim([0, max(strStress.max(), plStress.max())*1.25])
plt.legend()
plt.show()
