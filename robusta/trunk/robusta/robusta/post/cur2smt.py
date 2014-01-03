#!/usr/bin/env python
""" script to extract all textures from a cur file and convert to smt files
"""

# config
headerText = 'CRYSTAL WEIGHT      phi1      PHI       phi2        GAMMA'
smtFlavour = 'type 2'
excludeList = ['']

# import native modules
import os
import sys
import glob

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
            
            # get the header info for the current texture
            headerLine = headerLines[index]
            infoLine = [item.strip() for item in rawData[headerLine-1].split(' ') \
                        if not item=='']
            noCrystals = int(float(infoLine[1]))
            defStepNumber = int(float(infoLine[0]))
            
            # open the output file and add the header
            outFile = open('{0}-{1}.smt'.format(os.path.splitext(curFileName)[0],
                           defStepNumber), 'w')
                           
            if smtFlavour=='type 1':
                outFile.write(' {0} {1}\n'.format(noCrystals, label))
                
            elif smtFlavour=='type 2':
                outFile.write(' 1\n {0}\n {1}\n'.format(label, noCrystals))
                
            # write the data
            for line in rawData[headerLine+1:headerLine+noCrystals+1]:
                parsedLine = [float(item.strip()) for item in line.split(' ') \
                              if not item=='']
                outFile.write(' {0:9.3f} {1:9.3f} {2:9.3f}              1 {3:16.3f}\n'.format(
                              parsedLine[4], parsedLine[3], parsedLine[2], parsedLine[1]))
                              
            outFile.close()
