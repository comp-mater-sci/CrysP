""" functions to calculate deformation gradients and strains based on point clouds
"""

# import native modules
import os
import glob

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
    
        E = 0.5*(Ft.F - I)
    """
    dim = len(defGrad)
    
    return 0.5*(np.dot(defGrad.transpose(), defGrad) - np.eye(dim))
    
    
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
            print 'point ({0}, {1}) excluded'.format(currentX, currentY)
    
    if returnDefGrad:
        return F
    else:                                  
        return E
        
        
def PlotHist(matrix, bins=50, alpha=0.75, normed=False, fill=True, limits=None,
             plotName='plot.png', dpi=120, papertype='a4'):
    """ plot a histogram of the given array of strains or defgrads
    """
    numValues, numComponents = matrix.shape
    fig = plt.figure()
    
    if numComponents==4:
        n, bins, patches = plt.hist(matrix, bins=bins, normed=normed, histtype='step',
                                color=['g','r','b','m'], label=['11', '12', '21', '22'],
                                fill=fill, alpha=alpha)
                                
        plt.setp(patches[0], edgecolor='k', facecolor='g')
        plt.setp(patches[1], edgecolor='k', facecolor='r')
        plt.setp(patches[2], edgecolor='k', facecolor='b')
        plt.setp(patches[3], edgecolor='k', facecolor='m')
                                
    elif numComponents==6:
        raise Exception ('not done yet :)')

    plt.legend()                               
    if not limits is None:
        plt.axis(limits)
    plt.savefig(plotName, bbox_inches='tight', facecolor='w', dpi=dpi, papertype=papertype)
    plt.close(fig)
    
   
def ProcessVIC2DFile(name, radius=50, folder=os.getcwd(), delimiter=',', bins=200,
                     window=None):
    """ extract displacements from one output file from VIC 2D
    
        assumes x,y,u,v are the first four columns in the data
    """
    # check file exists
    filePath = os.path.join(folder, name)    
    if not os.path.isfile(name):
        raise IOError('cant find {0} in {1}'.format(name, folder))
    
    # get displacements and x y pixel coordinates within the given 'window'
    data = np.genfromtxt(filePath, delimiter=delimiter)[1::]
    extents = [np.min(data[:,0]), np.max(data[:,0]), np.min(data[:,1]), np.max(data[:,1])]
    if window is None:
        window = extents
    
    xMask = np.multiply((data[:,0]>window[0]),(data[:,0]<window[1]))
    yMask = np.multiply((data[:,1]>window[2]),(data[:,1]<window[3]))
    allMask = np.multiply(xMask, yMask)
    
    # calculate the strain and the deformation gradient
    E = CalcStrainFor2DField(x=data[allMask,0], y=data[allMask,1], u=data[allMask,2], v=data[allMask,3], radius=radius)
    F = CalcStrainFor2DField(x=data[allMask,0], y=data[allMask,1], u=data[allMask,2], v=data[allMask,3], radius=radius, returnDefGrad=True)
    
    # save the result
    print 'saving in folder {0}'.format(folder)
    np.save('E_{0}.npy'.format(name), E)
    np.save('F_{0}.npy'.format(name), F)
    
    # save plots
    PlotHist(E, plotName='E_{0}.png'.format(name), bins=bins)
    PlotHist(F, plotName='F_{0}.png'.format(name), bins=bins)
    
    # create summary file
    statsFile = open('stats_{0}.txt'.format(name), 'w')
    statsFile.write('number of points = {0}\nradius{1}\ncomponent averages:\n'.format(F.shape[0], radius))
    statsFile.write('extents (xmin, xmax, ymin, ymax) : [{0[0]}, {0[1]}, {0[2]}, {0[3]}]\n'.format(extents))
    statsFile.write('windows size(xmin, xmax, ymin, ymax) : [{0[0]}, {0[1]}, {0[2]}, {0[3]}]\n'.format(window))
    statsFile.write('F11 = {0}\nF12 = {1}\nF21 = {2}\nF22 = {3}\n'.format(np.average(F[:,0]),
                    np.average(F[:,1]), np.average(F[:,2]), np.average(F[:,3])))
    statsFile.write('E11 = {0}\nE12 = {1}\nE21 = {2}\nE22 = {3}\n\n'.format(np.average(E[:,0]),
                    np.average(E[:,1]), np.average(E[:,2]), np.average(E[:,3]))   )                 
    statsFile.write('standard deviations per component\n')
    statsFile.write('F11 = {0}\nF12 = {1}\nF21 = {2}\nF22 = {3}\n'.format(np.std(F[:,0]),
                    np.std(F[:,1]), np.std(F[:,2]), np.std(F[:,3])))         
    statsFile.write('E11 = {0}\nE12 = {1}\nE21 = {2}\nE22 = {3}\n\n'.format(np.std(E[:,0]),
                    np.std(E[:,1]), np.std(E[:,2]), np.std(E[:,3])))                 
    
    statsFile.close()
    

def ProcessAllVicFiles(radius=50, folder=os.getcwd(), delimiter=',', bins=200, window=None):
    """ process all csv files exported from VIC 2D found in the current folder
    """
    
    fileList = glob.glob('*.csv')
    
    for fileName in fileList:
        ProcessVIC2DFile(name=fileName, radius=radius, folder=folder,
                         delimiter=delimiter, bins=bins, window=window)
                         
def GetAvgE():
    """ get the average E values from all txt files in the current directory
    
        for sorting purposes the filenames should follow the convention of
        stats_xxxxx_0_commonLabel.txt 
        
        where xxxxx is the sequence number of the origial snapshot
    """
    fileList = glob.glob('*.txt')
    
    numFiles = len(fileList)
    data = np.zeros((numFiles,4))
    
    for index in range(numFiles):
    
        # read each stats file
        currentFile = open(fileList[index],'r')
        textData = currentFile.readlines()
        currentFile.close()
        
        # get the strain data and convert it to float
        data[index,:] = [float(item.strip().split(' ')[-1]) for item in textData[9:13]]
        
    # sort and save the array
    fileSequenceNums = [int(name.split('_')[1]) for name in fileList]
    np.save('stats_summary.txt', data[np.argsort(fileSequenceNums), :])


def PlotAvgE(name):
    """ plot the strain trends in the named file
    """
    
    
