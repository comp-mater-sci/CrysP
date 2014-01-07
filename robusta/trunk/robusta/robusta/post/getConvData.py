""" script to extract all the data from a convolution run (convolu.exe from
    the MTM-FHM package) and format it as a csv file for importing to Excel, etc.
""" 

# import native modules
import os, sys, glob

# import third part modules
import numpy as np


# config
delimiter = ','
outFileName = 'convolu.csv'
precision = np.float32
excludeList= []
suffix = 'L01'

# get file list
unsortedFileList = [name for name in glob.glob('*.{0}'.format(suffix)) if not name in excludeList]
prefixes = np.array([os.path.splitext(name)[0] for name in unsortedFileList], dtype=np.int16)
prefixes.sort()
fileList = ['{0}.{1}'.format(prefix, suffix) for prefix in prefixes]
numFiles = len(fileList)

# scan one input file to see how many components are being considered
currentFile = open(fileList[0], 'r')
rawData = currentFile.readline()
currentFile.close()

noOfTextureComponents = int(float(rawData.split(':')[1].strip()))
outputData = np.zeros((numFiles, noOfTextureComponents), dtype=precision)

# process files 
for index in range(numFiles):
    
    # read next file
    currentFile = open(fileList[index], 'r')
    rawData = currentFile.readlines()
    currentFile.close()

    # get data
    startLine = (noOfTextureComponents * 7) + 12
    outputData[index, :] = np.array([line.split(' ')[-1].strip() \
                                     for line in rawData[startLine:startLine+noOfTextureComponents]],
                                    dtype=precision)                                     

# save result
np.savetxt(fname=outFileName, X=outputData, delimiter=delimiter)
