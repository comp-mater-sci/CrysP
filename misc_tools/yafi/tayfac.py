#!/usr/bin/env python
""" script to calculate taylor factors for an smt file
"""
import os
import pyaltay.getTayFac
import numpy as np
import click
from pyhull import halfspace as qhalf
from pyhull import simplex as simplex

# --- command prompt interface -----------------------------------------
@click.group()
def main():
    pass

@main.command(name='all')
@click.argument('smtpath', type=click.Path(exists=True))
@click.argument('velgrad', nargs=9)
@click.option('--lattice', default='fcc', help='crystal lattice type')
@click.option('--nonorm', is_flag=True, help='disable normalisation')
def _SA_get_all_tayfacs(**kwargs):
    np.savetxt(new_path_for_extension(originalPath=kwargs['smtpath'],
               extension='tay'), taylor_factors(**kwargs))
    
@main.command(name='avg')
@click.argument('smtpath', type=click.Path(exists=True))
@click.argument('velgrad', nargs=9)
@click.option('--lattice', default='fcc', help='crystal lattice type')
@click.option('--nonorm', is_flag=True, help='disable normalisation')
def _SA_get_avg_tayfac(**kwargs):
    print np.average(taylor_factors(**kwargs))
    

@main.command(name='simpgrid')
@click.argument('smtpath', type=click.Path(exists=True))
@click.argument('gridpath', type=click.Path(exists=True))
@click.option('--lattice', default='fcc', help='crystal lattice type')
@click.option('--nonorm', is_flag=True, help='disable normalisation')
def _SA_get_simplex(**kwargs):
    # calculate average taylor factors for grid
    convertedGrid, avgTaylorFactors = avg_taylor_factors_for_grid(**kwargs)
    saveGridPath = '{0}_{1}'.format()
    np.savetxt(new_path_for_extension(originalPath=kwargs['smtpath'],
               extension='tay'), np.hstack((convertedGrid, avgTaylorFactors)))

    # calculate the vertices on the yield simplex
    yield_surf_simplex_vertices(velgrads, taylorfactors)
    np.savetxt(new_path_for_extension(originalPath=kwargs['smtpath'],
               extension='smp', addendum=split_path(kwargs['gridpath'])['stub']),
               np.hstack((convertedGrid, avgTaylorFactors)))
    

# --- ^^^ end command prompt interface ^^^ -----------------------------

def new_path_for_extension(originalPath, extension, addendum=None):
    splitPath = split_path(originalPath)
    if addendum is none:
        name = '{0}.{1}'.format(splitPath['stub'], extension)
    else:
        '{0}_{1}.{2}'.format(splitPath['stub'], addendum, extension)
    
    return os.path.join(splitPath['dir'], name)


def split_path(path):
    return {'dir':os.path.dirname(path),
            'base':os.path.basename(path),
            'stub':os.path.splitext(os.path.basename(path))[0]}


def taylor_factors(smtpath, velgrad, lattice='fcc', nonorm=False, roundPlaces=5):
    """ calculate taylor factors for an smt file
    """
    # convert velgrad to numpy array
    assert len(velgrad)==9, 'velGradMode must have 9 components'
    velGradMode = np.array(velgrad, dtype=np.float).reshape((3,3))

    # normalise
    if not nonorm:
        velGradMode = velGradMode/np.linalg.norm(velGradMode)

    # check volume is constant, if not round it down
    velGradMode = np.round(velGradMode, roundPlaces)
    while not np.trace(velGradMode)==0.:
        roundPlaces = roundPlaces - 1
        assert roundPlaces > 2, 'could not make velgrad volume constant'        
        print 'velocity gradient does not conserve volume, rounding to {0} places'.format(roundPlaces)
        velGradMode = np.round(velGradMode, roundPlaces)        


    # read the smt file
    rawSmtData = np.genfromtxt(smtpath, skip_header=1)
    orientations = rawSmtData[:,[2,1,0]]
    
    # calculate taylor factors - note the FC taylor model has to be used
    simulation = pyaltay.getTayFac.getTayFac()
    try:
        return simulation.taylor_factor_for_orientation_array(\
                                  orientations, velGradMode, lattice)
    except:
        print 'taylor_factors failed with velgrad:'
        print velGradMode
        print 'with trace'
        print np.trace(velGradMode)
        raise


def avg_taylor_factors_for_grid(smtpath, gridpath, lattice='fcc', nonorm=False):
    """ calculate taylor factors for each strain rate mode in a grid file
        assuming the usual grd format
    """
    # read grid
    gridData = np.genfromtxt(gridpath, skip_header=1, delimiter=21)
    convertedGrid = convert_to_matrix(gridData)
    numGridPoints = convertedGrid.shape[0]

    # get average taylor factors
    avgTaylorFactors = np.zeros((numGridPoints,))
    for gridNum in range(numGridPoints):
        velG = convertedGrid[gridNum,:]
        velgradRearranged = np.hstack((velG[0], velG[3], velG[4],
                                       velG[3], velG[1], velG[5], 
                                       velG[4], velG[5], velG[2]))
        avgTaylorFactors[gridNum] = np.average(taylor_factors(smtpath,
                                                   velgradRearranged, lattice,
                                                   nonorm))
    return convertedGrid, avgTaylorFactors
    
    
def convert_to_matrix(devGrid):
    A1 = devGrid[:,0]
    A2 = devGrid[:,1]
    A3 = devGrid[:,2]
    A4 = devGrid[:,3]
    A5 = devGrid[:,4]

    # convert to vectors in 6D
    root6over6 = 1./np.sqrt(6)
    root2over2 = 1./np.sqrt(2)
    root2thirds = np.sqrt(2./3.)
    
    A = np.zeros((devGrid.shape[0],6))
    
    A11 = root6over6*A2 + root2over2*A1
    A22 = root6over6*A2 - root2over2*A1
    A33 = -root2thirds*A2
    A12 = root2over2*A5
    A13 = root2over2*A4
    A23 = root2over2*A3
    
    return np.hstack((A11[:,None], A22[:,None], A33[:,None],
                      A12[:,None], A13[:,None], A23[:,None],))


def yield_surf_simplex_vertices(velgrads, taylorfactors):
    """ find the vertices of the yield surface as known in terms of the
        hyperplanes defined by taylor factors
    
        i.e. calculate the vertex representation of the intersection of 
        a set of halfspaces using the qhull package
    """
    # create halfspace objects for the pyhull wrapper
    numVelGrads = velgrads.shape[0]
    halfPlanes = []
    for normalIndex in range(numVelGrads):
        halfPlanes.append(qhalf.Halfspace(velgrads[normalIndex,:], taylorfactors[normalIndex]))

    # find the intersections of all the planes
    allIntersects = qhalf.HalfspaceIntersection(halfPlanes,
                                                interior_point=np.zeros(numVelGrads,))

    # find the vertices of the simplex (innermost polytope)
    yieldSurfSimplex = simplex(allIntersects.vertices())
    return yieldSurfSimplex.coords()


if __name__=='__main__':
    main()

