from robusta.para.Sampling import *

testCase = Sampling('test')

# define some parameters
testCase.AddParameter(parameterName='friction', distribParameters={
                      'distName':'uniform', 'minVal':0., 'maxVal':1.})
                      
testCase.AddParameter(parameterName='dieWidth', distribParameters={
                      'distName':'uniform', 'minVal':8., 'maxVal':50.}) 
                      
testCase.AddParameter(parameterName='dieRadius', distribParameters={
                      'distName':'uniform', 'minVal':0.1, 'maxVal':10.})
                      
                      
# get the parameter values
testCase.BuildHyperCubeParmList(noOfValues=10)

a = testCase.GetNextTestValues()
print a.get('dieRadius')

a = testCase.GetNextTestValues()
print a.get('dieRadius')

a = testCase.GetNextTestValues()
print a.get('dieRadius')

