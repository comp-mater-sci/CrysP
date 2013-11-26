from robusta.config import *
from robusta.post.AsciiTapFile import *
import numpy as np
import scipy.linalg as linAlg
matrixLog = linAlg.logm
matrixSqrt = linAlg.sqrtm

# config
tapFileName = 'tapOut.txt'
outputFileName = 'strainInc.txt'
delimiter = ','

def SquareRoot(matrix):
    """ return square root of positive definite nxn matrix
    """
    
    

# read data
shearsim = AsciiTapFile()
shearsim.ReadTapFile(tapFileName)

# get known variables
FStart = shearsim.GetVariable('DEFGRADOLD', asMatrix=True)
FEnd = shearsim.GetVariable('DEFGRADNEW', asMatrix=True)
dT = shearsim.GetVariable('DT')
DeltaE = shearsim.GetVariable('STRAININC', asMatrix=True)

# calculate increment by increment derived values
arrayDims = FStart.shape
noIncrements = arrayDims[0]

FDot = np.zeros(arrayDims)
FStartInv = np.zeros(arrayDims)
DeltaF = np.zeros(arrayDims)
LStart = np.zeros(arrayDims)
LEnd = np.zeros(arrayDims)
DeltaV = np.zeros(arrayDims)
LogDeltaV = np.zeros(arrayDims)
FStartInv = np.zeros(arrayDims)
FEndInv = np.zeros(arrayDims)
L = np.zeros(arrayDims)
EigVal_LogDeltaV = np.zeros(arrayDims)
EigVec_LogDeltaV = np.zeros(arrayDims)
EigVal_DeltaE = np.zeros(arrayDims)
EigVec_DeltaE = np.zeros(arrayDims)

for incNumber in range(noIncrements):
    # deformation gradients
    FStartInv[incNumber, :] = np.linalg.inv(FStart[incNumber, :])
    FEndInv[incNumber, :] = np.linalg.inv(FEnd[incNumber, :])
    DeltaF[incNumber, :] = np.dot(FEnd[incNumber, :], FStartInv[incNumber, :])    
    FDot[incNumber, :] = DeltaF[incNumber, :] / float(dT[incNumber])
    
    # velocity gradient
    #LStart[incNumber, :] = np.dot(FDot[incNumber, :], FStartInv[incNumber, :])
    #LEnd[incNumber, :] = np.dot(FDot[incNumber, :], FStartInv[incNumber, :])
    L[incNumber, :] = matrixLog(DeltaF[incNumber, :])/ float(dT[incNumber]) 
    
    # delta V
    DeltaV[incNumber, :] = matrixSqrt(np.dot(DeltaF[incNumber, :], DeltaF[incNumber, :].transpose()))
    LogDeltaV[incNumber, :] = matrixLog(DeltaV[incNumber, :])
    
    # eigen values and vectors
    (eigVals, EigVec_LogDeltaV[incNumber, :]) = np.linalg.eig(LogDeltaV[incNumber, :])
    EigVal_LogDeltaV[incNumber, :] = eigVals.reshape((3,1))*np.eye(3)
    
    (eigVals, EigVec_DeltaE[incNumber, :]) = np.linalg.eig(DeltaE[incNumber, :])
    EigVal_DeltaE[incNumber, :] = eigVals.reshape((3,1))*np.eye(3)

symL = 0.5*(L + L.transpose((0,2,1)))
symLInc = np.zeros(arrayDims)
for incNumber in range(noIncrements):
    symLInc[incNumber, :] = symL[incNumber, :] * float(dT[incNumber])

# print intermediate results to file
labels = ['DeltaF','FDot','L','DeltaV','LogDeltaV', 'straininc','symLInc','eigValLogDelV','eigVecLogDelV','eigValDelE','eigVecDelE']
components = ['11','12','13','21','22','23','31','32','33']

headerString = ''
formatString = ''

for label in labels:
    headerString = headerString + delimiter + delimiter.join([label+component for component in components])
        
headerString = headerString[1::] + '\n'

for VarNumber in range(len(labels)):
    formatString = formatString + delimiter.join(['{' +str(VarNumber) + '['+str(index)+']}' for index in range(9)]) + ','

formatString = formatString + '\n'
fileObj = open(outputFileName, 'w')
fileObj.write(headerString)
for lineNo in range(noIncrements):
    
    fileObj.write(formatString.format(DeltaF[lineNo,:].reshape(9),
                  FDot[lineNo,:].reshape(9), L[lineNo,:].reshape(9),
                  DeltaV[lineNo,:].reshape(9),LogDeltaV[lineNo,:].reshape(9),
                  DeltaE[lineNo,:].reshape(9), symLInc[lineNo,:].reshape(9),
                  EigVal_LogDeltaV[lineNo, :].reshape(9), EigVec_LogDeltaV[lineNo, :].reshape(9),
                  EigVal_DeltaE[lineNo, :].reshape(9), EigVec_DeltaE[lineNo, :].reshape(9)))

fileObj.close()


#noIncrements = Delta_F.shape[0]
#F_DOT = np.zeros(Delta_F.shape)
#F_INV = np.zeros(Delta_F.shape)
#L = np.zeros(Delta_F.shape)
#V = np.zeros(Delta_F.shape)
#Log_V = np.zeros(Delta_F.shape)
#for increment in range(noIncrements):
#    F_DOT[increment, :] = Delta_F[increment, :] / float(DT[increment])
#    try:
#        F_INV[increment, :] = np.linalg.inv(Delta_F[increment, :])
#    except np.linalg.linalg.LinAlgError:
#        print 'Problem inverting increment {0}'.format(increment)
#        F_INV[increment, :] = np.zeros((3,3))

#    L[increment, :] = np.dot(F_DOT[increment, :], F_INV[increment, :])
#    V[increment, :] = matrixSqrt(np.dot(Delta_F[increment, :], Delta_F[increment, :].transpose()))

#    

#symL = 0.5*(L + L.transpose((0,2,1)))
#asymL = 0.5*(L - L.transpose((0,2,1)))


#shearsim.MatrixCompPrint(matrixLog(V[100,:]), E_INC[100,:], 100)

