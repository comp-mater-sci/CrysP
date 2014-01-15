""" functions to calculate deformation gradients and strains based on point clouds
"""

# import native modules


# import third party modules
import numpy as np
import matplotlib.pylab as plt

def Calc2DDefGrad(x, y, u, v):
    """ return a deformation gradient based on least squares fitting of a plane
        through the provided set of x,y points and their associated u and v values
        
        x,y,u,v should be 1D numerical lists or arrays of the same length
    """
    # build a 'coefficient matrix' (see numpy documentation)
    numTerms = len(x)
    coeffMatrix = np.hstack((x.reshape(numTerms,1), y.reshape(numTerms,1),
                            np.ones((numTerms,1))))

    # get plane fit equation for du/dx and du/dy
    (dudx, dudy, c) = np.linalg.lstsq(coeffMatrix, u.reshape(numTerms,1))[0]
    
    # likewise for dv/dx and dv/dy
    (dvdx, dvdy, c) = np.linalg.lstsq(coeffMatrix, v.reshape(numTerms,1))[0]
    
    return np.array([[dudx[0]+1, dudy[0]], [dvdx[0], dvdy[0]+1]])
    
    
def Select2DInCircle(centre, radius, x, y):
    """ return a logical index array for a subset of points based on a centre
        point and a radius
    
        circle equation (x-a)^2 + (y-b)^2 = r^2
    """
    a = centre[0]
    b = centre[1]
    rSquared = (x - a)*(x - a) + (y - b)*(y - b)
    
    return rSquared < radius**2
    
    
def CalcGreenLagrange(defGrad):
    """ return the green lagrange strain for a 2D or 3D deformation gradient matrix
    
        E = 0.5*(F.Ft - I)
    """
    dim = len(defGrad)
    
    return 0.5*(np.dot(defGrad,defGrad.transpose()) - np.eye(dim))
    
    
def CalcStrainFor2DField(x,y,u,v, radius, centre=[0,0], returnDefGrad=False):
    """ return an array representing the green lagrange
        strain based on plane fitting to a displacement field
    """
    
    # calculate strain for every point
    numPoints = len(x)
    E = np.zeros((numPoints, 4))
    F = np.zeros((numPoints, 4))
    
    for index in range(numPoints):
        currentX = x[index]
        currentY = y[index]
        
        # get the set of points in a circle around the current point
        subSetIndex = Select2DInCircle(centre=[currentX, currentY], radius=radius, x=x, y=y)
        
        if np.any(subSetIndex):
        
            # calculate deformation gradient and strain
            F_temp = Calc2DDefGrad(x=x[subSetIndex], y=y[subSetIndex],
                                   u=u[subSetIndex], v=v[subSetIndex])
            E_temp = CalcGreenLagrange(F_temp)
            
            # flatten matrices
            F[index,:] = F_temp.reshape((1,4))            
            E[index,:] = E_temp.reshape((1,4))
            
        else:
            print 'point ({0}, {1}) excluded'.format(currentx, currentY)
    
    if returnDefGrad:
        return F
    else:                                  
        return E
        
        
def PlotHist(matrix, bins=50, alpha=0.75, normed=False, fill=False, limits=None):
    """ plot a histogram of the given array of strains or defgrads
    """
    numValues, numComponents = matrix.shape
    fig = plt.figure()
    
    if numComponents==4:
        n, bins, patches = plt.hist(matrix, bins=bins, normed=normed, histtype='step',
                                color=['g','r','b','m'], label=['11', '12', '21', '22'],
                                fill=fill, alpha=alpha)
                                
    elif numComponents==6:
        raise Exception ('not done yet :)')

    plt.legend()                               
    if not limits is None:
        plt.axis(limits)
    plt.show()
    plt.close(fig)
    
