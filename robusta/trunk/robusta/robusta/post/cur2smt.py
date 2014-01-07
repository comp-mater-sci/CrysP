#!/usr/bin/env python
""" script to extract all textures from a cur file and convert to smt files
"""

# config
headerText = 'CRYSTAL WEIGHT      phi1      PHI       phi2        GAMMA'
smtFlavour = 'type 2'
excludeList = ['']
outputFilePrefix = ''

# import native modules
import os
import sys
import glob

# import third party modules
import numpy as np

# get file list
curFileList = [name for name in glob.glob('*.CUR') if not name in excludeList]

for curFileName in curFileList:

    # open file
    curFile = open(curFileName, 'r')
    rawData = curFile.readlines()
    curFile.close()
        
    # find the header lines
    noDataLines = len(rawData)
    headerLines = [index for index in range(noDataLines) \
                   if headerText in rawData[index]]

    # remove first header, as it refers to the original texture
    headerLines = headerLines[1::]
    numTextureFiles = len(headerLines)
    label = rawData[0].strip()
    if numTextureFiles==0:
        print 'There seems to only be the original texture in {0}'.format(curFileName)

    else:
        print '{0} textures found in {1}'.format(numTextureFiles, curFileName)

        # for each header line export the data as an smt file
        for index in range(numTextureFiles):
            
            sys.stdout.write('.')
            sys.stdout.flush()
            
            # get the header info for the current texture
            headerLine = headerLines[index]
            infoLine = [item.strip() for item in rawData[headerLine-1].split(' ') \
                        if not item=='']
            noCrystals = int(float(infoLine[1]))
            defStepNumber = int(float(infoLine[0]))
            
            # open the output file and add the header
            outFileName = '{0}{1}.smt'.format(outputFilePrefix, defStepNumber)        
            outFile = open(outFileName, 'w')                           
            if smtFlavour=='type 1':
                outFile.write(' {0} {1}\n'.format(noCrystals, label))
                
            elif smtFlavour=='type 2':
                outFile.write(' 1\n {0}\n {1}\n'.format(label, noCrystals))
            
            # convert the data to an array and reformat/write it to the output file
            data = np.genfromtxt(rawData[headerLine+1:headerLine+noCrystals+1], delimiter=(6,10,12,10,10,12))
            np.savetxt(fname=outFile, X=data[:,[4,3,2,1]], fmt=' %9.3f %9.3f %9.3f              1 %14.5f')
                        
            outFile.close()
        print '\ndone\n'
