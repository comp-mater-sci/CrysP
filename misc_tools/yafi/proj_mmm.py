""" script to plot input and output grids as pole figures (equi-angular
    stereographic projection)
"""
""" read an mmm grid file and assess the spread of the vector
    representations of the input (x) and output(y) grids
"""
import os, sys, tempfile, shutil, subprocess
import numpy as np
import matplotlib
import matplotlib.pyplot as plt
import matplotlib.cm as cm
import matplotlib.colors as clr
from scipy.stats import gaussian_kde

global DPI
global WORK_DIR
global NORMALISE_ENERGY
global POLE_BGCOLOUR

NORMALISE_ENERGY = True
DPI = 100
WORK_DIR = os.getcwd()
POLE_BGCOLOUR = [1.,1.,1.]#[252./255.,248./255.,201/255.]

# global matplotlib settings
xkcdStyle = False
if xkcdStyle:
    plt.xkcd()
matplotlib.rcParams.update({'path.sketch':(0.3,500.,20.0),
                       'font.size':24})



def convert_grid(gridFilePath, gridType, whichGrid='input', energyType='s1'):

    # determine whether to look at 
    gridColumnStart = {'full':{'input':0,'output':5},
                       'reduced':{'input':3,'output':0}}
    assert gridType in gridColumnStart.keys(), 'gridType must be one of {0}'.format(gridColumnStart.keys())
    assert whichGrid in gridColumnStart['full'].keys(), 'whichGrid must be one of {0}'.format(gridColumnStart['full'].keys())

    # read file
    assert os.path.isfile(gridFilePath), 'no input file {0}'.format(gridFilePath)
    if gridType=='full':
        gridData = np.genfromtxt(gridFilePath, delimiter=21, skip_header=1)
    else:
        gridData = np.genfromtxt(gridFilePath)
    print '{0} points in grid'.format(gridData.shape[0])
    
    convertedValues = convert_to_matrix(gridData, gridType,
                                        offset=gridColumnStart[gridType][whichGrid])
    outputGrid = np.hstack((convertedValues['11'][:,None], 
                            convertedValues['22'][:,None],
                            convertedValues['12'][:,None]))
    
    # calculate energies
    energies = get_energy_for_all(outputGrid, energyType=energyType, normalise=False)
    
    return outputGrid, energies
   

def convert_to_matrix(grid, gridType, offset=5):
    A1 = grid[:,offset]
    A2 = grid[:,offset+1]
    
    if gridType=='full':
        A3 = grid[:,offset+2]
        A4 = grid[:,offset+3]
        A5 = grid[:,offset+4]
    else:
        A3 = None
        A4 = None
        A5 = grid[:,offset+2]

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
    
    
def get_energy_for_all(pointCloud, energyType='s1', normalise=False,
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
        if np.isclose(energyMin, energyMax): # this happens if the grid is almost perfect
            return np.zeros(energies.shape)
        else:
            return (energies - energyMin)/np.max((energies - energyMin))
    else:
        return energies


def plot_hist(axObj, values, rangeMin=0, rangeMax=1, numBins=50, normed=True, cmap=None):
    if cmap is None:
        cmap = cm.coolwarm

    # add histogram
    n, bins, patches = axObj.hist(values, numBins, normed=normed, range=(rangeMin, rangeMax))
    bin_centers = 0.5 * (bins[:-1] + bins[1:])
    bar_colours = (bin_centers - min(bin_centers))/max(bin_centers - min(bin_centers))
    
    # colour the bars by value
    for c, p in zip(bar_colours, patches):
        plt.setp(p, 'facecolor', cmap(c))
    
    # other formatting
    if normed:
        axObj.set_xlabel('normalised energy (clustering)')
    else:
        axObj.set_xlabel('energy (clustering)')
    axObj.set_ylabel('number of points')
    
    return n, bins, patches

    


def stereographic_project_XY(xyzData):
    """ stereographic projection
    """
    # project xyz onto unit sphere
    unitXYZ = xyzData /np.linalg.norm(xyzData, axis=1)[:,None]
    x, y, z = unitXYZ[:,0], unitXYZ[:,1], unitXYZ[:,2]
    
    # reflect bottom half of sphere through origin
    negZ = z < 0.
    x[negZ] = -x[negZ]
    y[negZ] = -y[negZ]
    z[negZ] = -z[negZ]
    
    # sterographic projection onto plane
    upper = np.logical_not(negZ)
    lower = negZ
    return x/(1+z), y/(1+z), upper, lower

    

def plot_stereographic(axObj, xyzData, intensity, cmap=None, limits=None,
                       addOnly=False, normMin=None, normMax=None):
    # set the colour scale
    if cmap is None:
        cmap = cm.coolwarm
    
    # convert xyz to r alpha
    X, Y, upper, lower = stereographic_project_XY(xyzData)
    rho = np.linalg.norm(np.hstack((X[:,None], Y[:,None])), axis=1)
    alpha = np.arctan2(X, Y)

    # if adding to existing plot
    if addOnly:
        numAddPoints = alpha.shape[0]
        markerRgb = np.tile(np.array([0,153./255.,51./255.]), numAddPoints).reshape((numAddPoints, 3))
        axObj.scatter(alpha, rho, marker='*',edgecolors='w', facecolors='None',
                      s=500., lw=3, zorder=4, alpha=.75)
        axObj.scatter(alpha, rho, marker='*',edgecolors='k', facecolors='None',
                      s=300., lw=1.5, zorder=5)

    # if constructing the plot for the first time
    else:
        # set colour map normalisation
        if normMin is None:
            normFunction = clr.Normalize()
        else:
            normFunction = clr.Normalize(vmin=normMin, vmax=normMax)

        # lower hemisphere plotted with squares
        axObj.scatter(alpha[lower], rho[lower], c=intensity[lower], cmap=cmap,s=70., lw=0.5,
                      zorder=1, marker='s', norm=normFunction)
        
        # upper hemisphere plotted with circles
        axObj.scatter(alpha[upper], rho[upper], edgecolors='None', facecolors='w',
                      zorder=2, marker='o', s=120, alpha=.75)
        axObj.scatter(alpha[upper], rho[upper], c=intensity[upper], cmap=cmap,s=70., lw=0.5,
                      zorder=3, marker='o', norm=normFunction)
        
        # format plot
        if limits is None:
            axLim = 1
            limits = [-axLim, axLim, 0, axLim]
        axObj.set_axis_bgcolor(POLE_BGCOLOUR)
        axObj.annotate(r'${\bf \ \hat{e}_{1}}$', xy=(0, 1))
        axObj.annotate(r'${\bf \hat{e}_{2}}$', xy=(np.radians(90), 1))
    
    
        # remove grid
        axObj.grid(False)
        axObj.get_xaxis().set_ticks([])
        axObj.get_yaxis().set_visible(False)

        axObj.set_xlim([limits[0], limits[1]])
        axObj.set_ylim([limits[2], limits[3]])
        axObj.set_aspect(1)


def get_all_energy_grid_data(mmmPath, yafFilePath, energyType):
    # get mmm grid data
    mmmGridIn, mmmEnergyIn = convert_grid(mmmPath, gridType='full',
                                          whichGrid='input', energyType=energyType)
    mmmGridOut, mmmEnergyOut = convert_grid(mmmPath, gridType='full',
                                            whichGrid='output', energyType=energyType)
    yafUsedIn, yafEnergyIn = convert_grid(yafFilePath, gridType='reduced',
                                          whichGrid='input', energyType=energyType)
    yafUsedOut, yafEnergyOut = convert_grid(yafFilePath, gridType='reduced',
                                            whichGrid='output', energyType=energyType)

    return {'mmm':{'gridin':mmmGridIn, 'energyin':mmmEnergyIn,
                   'gridout':mmmGridOut, 'energyout':mmmEnergyOut},
            'yaf':{'gridin':yafUsedIn, 'energyin':yafEnergyIn,
                    'gridout':yafUsedOut, 'energyout':yafEnergyOut}}


def dot_product_of_grid_array(gridx, gridy):
    numRows, numCols = gridx.shape
    assert gridx.shape == gridy.shape, 'grids must have same shape'
    
    result = np.zeros((numRows, 1))
    for rowNum in range(numRows):
        result[rowNum] = np.dot(gridx[rowNum,:], gridy[rowNum,:])
    
    return result


def plot_direction_similarity_projections(mmmPath, yafFilePath, energyType):
    # get the grid data and associated energies
    enerGrids = get_all_energy_grid_data(mmmPath, yafFilePath, energyType)
    mmmStem = split_path(mmmPath)[-1]
    yafStem = split_path(yafFilePath)[-1]
    
    # calculate cosines between stress and associated strain rates
    norm_array = lambda x: x / np.tile(np.linalg.norm(x, axis=1), 3).reshape(x.shape)
    strainRateDirs = norm_array(enerGrids['mmm']['gridin'])
    stressDirs = norm_array(enerGrids['mmm']['gridout'])
    cosines = dot_product_of_grid_array(strainRateDirs, stressDirs)
    cosines[cosines < 0.995] = np.nan
    
    # set up figure
    fig = plt.figure()
    fig.set_figheight(15)
    fig.set_figwidth(15)
    axTopLeft = plt.subplot(2,1,1, projection='polar')
    axTopRight = plt.subplot(2,1,2)

    plot_stereographic(axTopLeft, enerGrids['mmm']['gridin'], cosines,
                       cmap=cm.coolwarm, normMin=0.98, normMax=1)
    plot_hist(axTopRight, cosines, numBins=30, normed=False, rangeMin=0.98, rangeMax=1)
    
    plt.show()
    

def plot_directions_and_hists(mmmPath, yafFilePath, energyType):
    """
    """
    # get the grid data and associated energies
    enerGrids = get_all_energy_grid_data(mmmPath, yafFilePath, energyType)
    mmmStem = split_path(mmmPath)[-1]
    yafStem = split_path(yafFilePath)[-1]

    # normalise energy values in common
    enerMin = min(enerGrids['mmm']['energyin'].min(), enerGrids['mmm']['energyout'].min())
    enerMax = max(enerGrids['mmm']['energyin'].max(), enerGrids['mmm']['energyout'].max())
    if NORMALISE_ENERGY:
        rangeMin = 0
        rangeMax = 1
        
        mmmEnergyInNorm = (enerGrids['mmm']['energyin'] - enerMin) / (enerMax - enerMin)
        mmmEnergyOutNorm = (enerGrids['mmm']['energyout'] - enerMin) / (enerMax - enerMin)
        
    else:
        rangeMin = histMin
        rangeMax = histMax
        
        mmmEnergyInNorm = enerGrids['mmm']['energyin']
        mmmEnergyOutNorm = enerGrids['mmm']['energyout']
    

    # set up subplots
    fig = plt.figure()
    fig.set_figheight(15)
    fig.set_figwidth(20)
    axTopLeft = plt.subplot(2,2,1, projection='polar')
    axTopRight = plt.subplot(2,2,2)
    axBotLeft = plt.subplot(2,2,3, projection='polar')
    axBotRight = plt.subplot(2,2,4)
    
    # add stereographic plots of directions
    plot_stereographic(axTopLeft, enerGrids['mmm']['gridin'], mmmEnergyInNorm,
                       cmap=cm.coolwarm, normMin=rangeMin, normMax=rangeMax)
    plot_stereographic(axBotLeft, enerGrids['mmm']['gridout'], mmmEnergyOutNorm,
                       cmap=cm.coolwarm, normMin=rangeMin, normMax=rangeMax)
    plot_stereographic(axTopLeft, enerGrids['yaf']['gridin'], enerGrids['yaf']['energyin'],
                       addOnly=True, normMin=rangeMin, normMax=rangeMax)
    plot_stereographic(axBotLeft, enerGrids['yaf']['gridout'], enerGrids['yaf']['energyout'],
                       addOnly=True, normMin=rangeMin, normMax=rangeMax)
    
    
    # add histograms
    plot_hist(axTopRight, mmmEnergyInNorm, numBins=30, normed=False, rangeMin=rangeMin, rangeMax=rangeMax)
    plot_hist(axBotRight, mmmEnergyOutNorm, numBins=30, normed=False, rangeMin=rangeMin, rangeMax=rangeMax)
    
    # add titles
    axTopLeft.set_xlabel('Strain rate directions')
    axTopRight.set_xlabel('Normalised {0}-energy: strain rate directions'.format(energyType.upper()))
    axBotLeft.set_xlabel('Stress directions')
    axBotRight.set_xlabel('Normalised {0}-energy: stress directions'.format(energyType.upper()))
    
    # save output to file
    plotName = '{0}_{1}_{2}_enpole.tiff'.format(mmmStem, yafStem, sys.argv[2])
    tempDir = tempfile.mkdtemp()
    try:
        tempPath = os.path.join(tempDir, 'temp.tiff')
        plt.savefig(tempPath, DPI=DPI)
        print subprocess.check_output('convert temp.tiff -compress lzw comp.tiff',
                                      cwd=tempDir, shell=True)
        shutil.copyfile(os.path.join(tempDir, 'temp.tiff'),
                        os.path.join(WORK_DIR, plotName))

    finally:
        plt.close(fig)
        shutil.rmtree(tempDir)
        



if __name__=='__main__':
    usage = '\n\nUsage:\npython proj_mmm.py <mmm file path> <yaf path> [energy type "s1", "s2", .. "sn" or "log"]'
    assert len(sys.argv) == 4, usage

    # check input file
    mmmPath = os.path.realpath(sys.argv[1])
    assert os.path.isfile(mmmPath), 'cant find file {0}'.format(mmmPath)
    yafFilePath = os.path.realpath(sys.argv[2])
    assert os.path.isfile(yafFilePath), 'cant find file {0}'.format(yafFilePath)
    energyType = str(sys.argv[3])

    # plot the direction projections and energy histograms
    plot_directions_and_hists(mmmPath, yafFilePath, energyType)
    
    # plot stress strain rate parallelness pole figures
    plot_direction_similarity_projections(mmmPath, yafFilePath, energyType)

