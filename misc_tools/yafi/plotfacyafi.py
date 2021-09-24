""" plot a facet expression
"""
import os, sys
import numpy as np
from mayavi.tools import pipeline
from tvtk.api import tvtk, write_data
import yafi
        

def fac_to_vts(path, yafPath, numPointsPerAx=10, axlims=[-3,3], truncate=0,
               overriden=None):
    """ convert a fac file to a scalar field in mayavi
    """
    # check input
    fullPath = os.path.realpath(path)
    yafFullPath = os.path.realpath(yafPath)
    assert os.path.isfile(fullPath), '{0} no accessible'.format(fullPath)
    assert os.path.isfile(yafFullPath), '{0} no accessible'.format(yafFullPath)

    # read the facet file
    facetTerms = np.genfromtxt(fullPath, skip_header=6)
    if overriden is None:
        with open(fullPath, 'r') as facFile:
            for i in range(4):
                null = facFile.readline()
            order = int(facFile.readline().strip().split(' ')[0])
    else:
        order = overriden

    # only using the terms relevant to the 'ordinary' deviatoric stress
    # space (i.e. A1, A2, A5 = columns 1,2,5]
    numPoints = facetTerms.shape[0]
    selected = np.ones((numPoints,)).astype(np.bool)
    removeIndices = np.random.choice(range(numPoints),truncate)
    selected[removeIndices] = False
    
    normals = facetTerms[:,[1,2,5]][selected,:]
    lambdas = facetTerms[selected,0][:,None]
    
    
    # evaluate the facet expression on a 3D grid (of course this doesn't
    # happen in real usage)
    x, y, z = np.mgrid[axlims[0]:axlims[1]:eval('{}j'.format(numPointsPerAx)),
                       axlims[0]:axlims[1]:eval('{}j'.format(numPointsPerAx)),
                       axlims[0]:axlims[1]:eval('{}j'.format(numPointsPerAx))]
    facetValues = facet_eval(lambdas, normals, order, x, y, z)                
    
    # ploot it by creating a scalar field and adding a surface
    scalarField = pipeline.scalar_field(x, y, z, facetValues)
    folder = os.path.dirname(fullPath)
    vtsName = '{0}_fac_sfield.vts'.format(os.path.splitext(os.path.basename(fullPath))[0])
    scalarField.save_output(os.path.join(folder, vtsName))

    # also export a vtu file with vectors for the strain rate directions
    # and scalars for the associated lambda values
    yafData = np.genfromtxt(yafFullPath)
    stressPoints = yafData[:,[0,1,2]]
    normals = yafData[:,[3,4,5]]

    if not len(lambdas) == len(normals):
        lambdas=None
    vtuName = '{0}_fac_vfield.vtu'.format(os.path.splitext(os.path.basename(yafFullPath))[0])
    write_facet_vtu(os.path.join(folder, vtuName), stressPoints,
                    normals, lambdas)
    
    
def write_facet_vtu(filePath, stressPoints, strainRateVectors, lambdas):
    """
    """
    grid = tvtk.UnstructuredGrid(points=stressPoints)

    grid.point_data.vectors = strainRateVectors
    grid.point_data.vectors.name = 'strain rate direction vectors'
    
    if not lambdas is None:
        grid.point_data.scalars = lambdas
        grid.point_data.scalars.name = 'facet lambda values'

    write_data(grid, filePath)
    
     
    
def facet_eval(lambdas, normals, order, x, y, z):
    """ evaluate a facet expression in 3D
    """
    # construct yafi object
    yafiObj = yafi.yafiStressExpression(s=None, ds=normals, n=order, sStarHat=None)
    yafiObj.set_all_lambdas(lambdas)
    yafiObj.normalise()

    scalars = np.zeros(x.shape)
    for i in range(x.shape[0]):
        for j in range(x.shape[1]):
            for k in range(x.shape[2]):
                # need to convert a point in ordinary deviatoric space to vector space
                svector = np.hstack((x[i,j,k], y[i,j,k], z[i,j,k]))
                scalars[i,j,k] = yafiObj.evaluate_for_stress(s=svector)
    return scalars
    
    
if __name__=='__main__':
    assert len(sys.argv)==4, 'usage: python plotfacyafi <fac file> <yaf file> numPointsPerAx'
    
    facPath = os.path.realpath(sys.argv[1])
    yafPath = os.path.realpath(sys.argv[2])
    numPoints = int(sys.argv[3])
    fac_to_vts(facPath, yafPath, numPointsPerAx=numPoints)
    
    
