import os
import sys

# config
delimiter = ' '
keyword = 'STRAININC'
scale = 1000.
#components = ['11','12','13','21','22','23','31','32','33']
components = ['11','12','31','22','23','33']

# tap file ascii name given as argument
tapFileName = sys.argv[1]

# read the file
tapFile = open(tapFileName, 'r')
data = tapFile.readlines()
noLines = len(data)
tapFile.close()

# parse
parsedData = []
for lineNo in range(noLines):
    parsedData.append([item for item in data[lineNo].strip().split(delimiter)\
                         if not item==''])
    
header = parsedData[0]

# format the matrix and print to file
outFileName = 'form_{0}'.format(tapFileName)
outFile = open(outFileName, 'w')

noComponents = len(components)
if noComponents==9:
    
    col00 = header.index(keyword+components[0])
    col01 = header.index(keyword+components[1])
    col02 = header.index(keyword+components[2])
    col10 = header.index(keyword+components[3])
    col11 = header.index(keyword+components[4])
    col12 = header.index(keyword+components[5])
    col20 = header.index(keyword+components[6])
    col21 = header.index(keyword+components[7])
    col22 = header.index(keyword+components[8])

elif noComponents==6:
    
    col00 = header.index(keyword+components[0])
    col01 = header.index(keyword+components[1])
    col02 = header.index(keyword+components[2])
    col10 = col01
    col11 = header.index(keyword+components[3])
    col12 = header.index(keyword+components[4])
    col20 = col02
    col21 = col12
    col22 = header.index(keyword+components[5])             
        
else:
    raise Exception('Not implemented yet for matrix with {0} components'.format(noComponents))


outFormat = ('|{0: 6.3f}{1}{2: 6.3f}{1}{3: 6.3f}| INC:{4} scale:{5}\n' +
            '|{6: 6.3f}{1}{7: 6.3f}{1}{8: 6.3f}|\n' +
            '|{9: 6.3f}{1}{10: 6.3f}{1}{11: 6.3f}|\n\n')

for lineNo in range(1,noLines):
    dat = [float(strVal)*scale for strVal in parsedData[lineNo]]
    outFile.write(outFormat.format(dat[col00], delimiter,
                  dat[col01], dat[col02], lineNo, scale,
                  dat[col10], dat[col11], dat[col12],
                  dat[col20], dat[col21], dat[col22]))

outFile.close()
