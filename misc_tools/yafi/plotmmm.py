""" script to convert a facet .grd file to a .vtu file which can be
    viewed in mayavi2
    
    conversion is specified in the appendix 1 of:
    Van Houtte, P., Yerra, S. K., & Van Bael, A. (2009). "The Facet method:
    A hierarchical multilevel modelling scheme for anisotropic convex
    plastic potentials." International Journal of Plasticity, 25(2),
    332-360. http://doi.org/10.1016/j.ijplas.2008.02.001
    
"""
import os, sys
from tvtk.api import tvtk, write_data
import numpy as np


def main(gridFilePath, resultFolder, convert=True):

    # define paths
    assert os.path.isdir(resultFolder), 'no directory {0}'.format(resultFolder)
    assert os.path.isfile(gridFilePath), 'no input file {0}'.format(gridFilePath)
    
    # read file and convert to vtu
    facetGrid = np.genfromtxt(gridFilePath, delimiter=21, skip_header=1)    
    print '{0} points in grid'.format(facetGrid.shape[0])
    
    convertedValues = convert_to_matrix(facetGrid, convert)
    outputGrid = np.hstack((convertedValues['11'][:,None], 
                            convertedValues['22'][:,None],
                            convertedValues['12'][:,None]))
    
    # create vtu file
    gridFolder, gridName, gridStem = split_path(gridFilePath)
    vtuPath = os.path.join(resultFolder, '{0}_mmm2vtu.vtu'.format(gridStem))
    write_unstructured_vtk(pointArray=outputGrid, filePath=vtuPath)
    
    # write the output to a log file for reference
    logData = np.hstack((convertedValues['11'][:,None], 
                         convertedValues['22'][:,None],
                         convertedValues['33'][:,None],
                         convertedValues['12'][:,None],
                         convertedValues['13'][:,None],
                         convertedValues['23'][:,None]))
    np.savetxt(os.path.join(gridFolder, '{0}.log'.format(gridStem)), logData)
    


def convert_to_matrix(facetGrid, convert=True, offset=5):
    A1 = facetGrid[:,offset]
    A2 = facetGrid[:,offset+1]
    A3 = facetGrid[:,offset+2]
    A4 = facetGrid[:,offset+3]
    A5 = facetGrid[:,offset+4]

    # convert to vectors in 6D
    root6over6 = 1./np.sqrt(6)
    root2over2 = 1./np.sqrt(2)
    root2thirds = np.sqrt(2./3.)
    oneOverRoot2 = 1.
    
    if convert:
        A11 = root6over6*A2 + root2over2*A1
        A22 = root6over6*A2 - root2over2*A1
        A33 = -root2thirds*A2
        A12 = root2over2*A5
        A13 = root2over2*A4
        A23 = root2over2*A3

    else:
        A11 = A1
        A22 = A2
        A33 = -(A11 + A22)
        A12 = A5
        A13 = A4
        A23 = A3
    
    return {'11':A11, '22':A22, '33':A33, '12':A12, '13':A13, '23':A23}
    

def split_path(path):
    name, folder = os.path.basename(path), os.path.dirname(path)
    return folder, name, os.path.splitext(name)[0]


def write_unstructured_vtk(pointArray, filePath, addVectors=True,
                           addCoulomb=True):
    """
    """
    dimensions = (len(np.unique(pointArray[:,0])), len(np.unique(pointArray[:,1])),
                  len(np.unique(pointArray[:,2])))
    grid = tvtk.UnstructuredGrid(points=pointArray)

    if addVectors:
        grid.point_data.vectors = pointArray
        grid.point_data.vectors.name = 'mode vectors'
    
    if addCoulomb:
        forces = get_coulomb_force_for_all(pointArray)
        distInStdDevs = (forces - np.average(forces))/np.average(forces)

        grid.point_data.scalars = distInStdDevs
        grid.point_data.scalars.name = '# avg normed centred Coulomb'
        np.save(os.path.join(os.path.dirname(filePath),
                '{0}_forces.npy'.format(os.path.splitext(os.path.basename(filePath))[0])),
                forces)

    write_data(grid, filePath)
    


def get_stack(flatArray):
    return np.hstack((flatArray[:,None], flatArray[:,None], flatArray[:,None]))
    

def get_coulomb_force(pointCloud, particlePosition):
    # f = K(q1.q2/r^2), but set K = q1 = q2 = 1
    joiningVectors = pointCloud-particlePosition
    joiningDistances = np.linalg.norm(joiningVectors, axis=1)
    nonCoincident = joiningDistances != 0.
    
    forceMagnitudes = joiningDistances**-2.
    unitDirectionVectors = joiningVectors / get_stack(joiningDistances)
    forceVectors = unitDirectionVectors * get_stack(forceMagnitudes)
    resultant = np.sum(forceVectors[nonCoincident,:], axis=0)
    return np.linalg.norm(resultant)
    
    
def get_coulomb_force_for_all(pointCloud):
    forces = np.zeros((pointCloud.shape[0],1))
    for index in range(pointCloud.shape[0]):
        forces[index,0] = get_coulomb_force(pointCloud=pointCloud,
                                            particlePosition=pointCloud[index,:])
    return forces


if __name__=='__main__':
    usage = '\n\nUsage:\npython viewGrid.py <grid file path> [convert=F]'
    assert len(sys.argv) >= 2, usage

    # check input file
    gridFilePath = os.path.realpath(sys.argv[1])
    assert os.path.isfile(gridFilePath), 'cant find file {0}'.format(inputPath)

    convert = True
    if len(sys.argv) == 3:
        convert = not(str(sys.argv[-1])[0].lower() == 'f')
        
    if not convert:
        print 'not converting'
        
    main(gridFilePath=gridFilePath, resultFolder=os.getcwd(), convert=convert)
