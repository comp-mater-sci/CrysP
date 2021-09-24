""" read an mmm grid file and assess the spread of the vector
    representations of the input (x) and output(y) grids
"""
import os, sys
from tvtk.api import tvtk, write_data
import numpy as np
import matplotlib.pyplot as plt
from scipy.stats import gaussian_kde

def convert_3Dgrid(gridFilePath, whichGrid='input',convert=False,
                   xComponent='11', yComponent='22', zComponent='12',
                   energyType='s1'):

    # determine whether to look at 
    gridColumnStart = {'input':0,'output':5}
    assert whichGrid in gridColumnStart.keys(), 'whichGrid must be one of {0}'.format(gridColumnStart.keys())

    # read file
    assert os.path.isfile(gridFilePath), 'no input file {0}'.format(gridFilePath)
    mmmGrid = np.genfromtxt(gridFilePath, delimiter=21, skip_header=1)    
    print '{0} points in grid'.format(mmmGrid.shape[0])
    
    convertedValues = convert_to_matrix(mmmGrid, convert,
                                        offset=gridColumnStart[whichGrid])
    outputGrid = np.hstack((convertedValues[xComponent][:,None], 
                            convertedValues[yComponent][:,None],
                            convertedValues[zComponent][:,None]))
    
    # create energy file for grid
    gridFolder, gridName, gridStem = split_path(gridFilePath)
    energies = get_energy_for_all(outputGrid, energyType=energyType)
    np.save(os.path.join(gridFolder,'{0}_{1}_{2}.npy'.format(gridStem,
            whichGrid, energyType)), energies)
    
    # create vtu file
    vtuPath = os.path.join(gridFolder, '{0}_{1}_{2}.vtu'.format(gridStem,
                           whichGrid, energyType))
    write_unstructured_vtk(pointArray=outputGrid, filePath=vtuPath,
                           energies=energies)
    
    return outputGrid, energies
    
    

def convert_to_matrix(grid, convert=False, offset=5):
    A1 = grid[:,offset]
    A2 = grid[:,offset+1]
    A3 = grid[:,offset+2]
    A4 = grid[:,offset+3]
    A5 = grid[:,offset+4]

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
                           energies=None):
    """
    """
    dimensions = (len(np.unique(pointArray[:,0])), len(np.unique(pointArray[:,1])),
                  len(np.unique(pointArray[:,2])))
    grid = tvtk.UnstructuredGrid(points=pointArray)

    if addVectors:
        grid.point_data.vectors = pointArray
        grid.point_data.vectors.name = 'mode vectors'
    
    if not energies is None:
        grid.point_data.scalars = energies
        grid.point_data.scalars.name = '# point energy'

    write_data(grid, filePath)
    


def get_stack(flatArray):
    return np.hstack((flatArray[:,None], flatArray[:,None], flatArray[:,None]))
    

def _get_energy(pointCloud, particlePosition, energyType='s1'):
    """ calculates the "s energy" or "log energy"
        see Saff, E., and Kuijlaars, A. Distributing many points on a
        sphere. The Mathematical Intelligencer 19, 1 (1997), 5-11.
    """
    # check the specified energy type
    try:
        assert energyType[0] in ['s','l'], 'unknown energy type'
    except Exception:
        raise ValueError('energyType must be "log" or "sn" where n is a float or int')
    
    # calculate euclidean distances to all points
    joiningVectors = pointCloud - particlePosition
    joiningDistances = np.linalg.norm(joiningVectors, axis=1)
    nonCoincident = joiningDistances != 0.
    
    # calculate energy
    if energyType == 'log':
        # log energy is the sum of the logs of the inverse of the
        # euclidean distance - it is related to the product of the
        # distances.
        energies = np.log(joiningDistances[nonCoincident]**-1)
        
    else:
        # s-energy is the inverse of the distance raised to the power n
        # for n=1 it is proportional to the Coloumb energy
        energies = joiningDistances[nonCoincident]**-float(energyType[1::])

    return np.sum(energies)
    
    
def get_energy_for_all(pointCloud, energyType='s1', normalise=True,
                       projectPointsOnUnitSphere=True):
    """ calculate the s energy or the log energy for all points
    
        see function _get_energy for details
    """
    # project the points onto the unit sphere if required
    if projectPointsOnUnitSphere:
        pointCloud = pointCloud/np.linalg.norm(pointCloud, axis=1)[:,None]

    # calculate energy for each point
    energies = np.zeros((pointCloud.shape[0],1))
    for index in range(pointCloud.shape[0]):
        energies[index,0] = _get_energy(pointCloud=pointCloud,energyType=energyType,
                                        particlePosition=pointCloud[index,:])
    
    # rounding to prevent funny round off related plotting errors
    energies = np.round(energies,5)
    if normalise:
        energyMin, energyMax = energies.min(), energies.max()
        if energyMin==energyMax: # this happens if the grid is almost perfect
            return np.zeros(energies.shape)
        else:
            return (energies - energyMin)/np.max((energies - energyMin))
    else:
        return energies


def plot_hist(values, filePath, numBins=50, normed=True, dpi=500):

    plt.hist(values, numBins, normed=normed, range=(0,1))
    #ax = plt.gca()
    #ax.set_aspect(1)
    #plt.axes([0.,1.,0., ax.get_ylim()[1]])
    plt.savefig(filePath, dpi=dpi)
    
    
def plot_density(values, bandWidth=None, calcPoints=200):
    
    density = gaussian_kde(values)
    xs = np.linspace(0,max(values), calcPoints)
    
    if not bandWidth is None:
        density.covariance_factor = lambda : bandWidth
        density._compute_covariance()

    plt.plot(xs, density(xs))
    plt.gca().set_aspect(1)
    plt.show()
    
    

if __name__=='__main__':
    usage = '\n\nUsage:\npython assess_mmm.py <mmm file path> ["input" or "output"] [energy type "s1", "s2", .. "sn" or "log"]'
    assert len(sys.argv) == 4, usage

    # check input file
    gridFilePath = os.path.realpath(sys.argv[1])
    assert os.path.isfile(gridFilePath), 'cant find file {0}'.format(inputPath)

    gridData, energyData = convert_3Dgrid(gridFilePath, whichGrid=str(sys.argv[2]),convert=False,
                                         xComponent='11', yComponent='22', zComponent='12',
                                         energyType=str(sys.argv[3]))
    gridFolder, gridName, gridStem = split_path(gridFilePath)
    plotPath = os.path.join(gridFolder, '{0}_{1}_{2}.png'.format(gridStem,
                            sys.argv[2], sys.argv[3]))
    plot_hist(energyData, normed=False, filePath=plotPath)
    
    
    
