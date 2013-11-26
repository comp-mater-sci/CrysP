""" script to calculate a plastic table for a keyword file based
    on parameters for the Voce law
    
    The result is saved as a text file which can be pasted into a
    keyword file
    
    inputs:
    sigma_0
    K
    n
        
    max_epsilon
    no_of_points
    
    Voce law:
    sigma = sigma_0 + K(1 - exp(-n epsilon) )
"""
import sys
import numpy as np

# config
outputFileName = 'Voce.txt'

# check the input
noArgs = len(sys.argv)

if not noArgs==6:
    print '\nUsage:\n'
    print 'vocePlasTable sigma_0 K n max_epsilon no_of_points\n'
    print sys.argv
    sys.exit()

# get the parameters    
sigma_0 = float(sys.argv[1])
K = float(sys.argv[2])
n = float(sys.argv[3])
maxEpsilon = float(sys.argv[4])
noPoints = int(sys.argv[5])

# calculate the sample points, with unique values when rounded to
# two decimal places (this is necessary for the keyword file)
epsilon = np.array(sorted([float(val) for val in \
                            set([round(point,2) for point in\
                            np.linspace(0, maxEpsilon, noPoints)])]))
sigma = sigma_0 + K*(1 - np.exp(-n*epsilon))
noNonDuplicatePoints = len(epsilon)

# write the output
formatString = '{0:12.5e},{1:5.2f}\n'
fileID = open(outputFileName, 'w')
fileID.write('*Plastic\n')

for line in range(noNonDuplicatePoints):
    fileID.write(formatString.format(sigma[line], epsilon[line]))
    
fileID.close()
