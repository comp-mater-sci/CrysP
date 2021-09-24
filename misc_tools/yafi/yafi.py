#!/usr/bin/env python
""" Yet Another Facet Implementation
"""
import os, sys
import numpy as np
from scipy.optimize import nnls as scipy_nnls

class yafiStressTerm(object):
    """ a term in a yafi expression
    
        it has the form lambda_k (s . d_k)^n
        
        the variable names correspond directly with the thesis text:
        lamb = lambda_k
        s = independent stress
        d = dependent unit strain rate mode
    """
    
    def __init__(self, n, d):
        self.dimensionality = len(d)
        self.n = n
        self.lamb = 1.0
        self.d = d.ravel()
        
    def _s_dot_d_tothe_n(self, s, n=None):
        """ returns (s.d)^n
        """
        if n is None:
            n = self.n
        return np.dot(s.ravel(), self.d) ** n
        
                
    def _lambda_s_dot_d_tothe_n(self, s, n=None):
        """ returns lambda * (s.d_i)^n
        """
        return self.lamb * self._s_dot_d_tothe_n(s, n)
        
        
    def get_derivative_numerator_value(self, s, strainRateCompIndex):
        assert strainRateCompIndex < self.dimensionality, 'index {0} exceeds dimensionality'.format(strainRateCompIndex)
        return self.d[strainRateCompIndex] * self._lambda_s_dot_d_tothe_n(s, n=(self.n-1))
        
        
        
class yafiStressExpression(object):
    """ a yafi expression and its first derivatives
    
        the variable names correspond directly with the thesis text:
        lamb = lambda_k
        s = independent stress
        d = dependent unit strain rate mode
    """
    
    def __init__(self, s, ds, n, sStarHat=None):
        """ ds = array of strain rate modes
            s = array of stresses associated with ds
            n = order of expression
            sStarHat = reference stress mode for which the expression
                       shoule evaluate to one
        """
        self._initialise_or_reset(s, ds, n, sStarHat=None)
        
        
    def _initialise_or_reset(self, s, ds, n, sStarHat=None):
        
        # store common properties
        self.dimensionality = ds.shape[1]
        self.numTerms = ds.shape[0]
        self.n = n
        self.sStar = 1.0
        self.ds = ds
        self.s = s
        self.numZeroTerms = None
        
        # define reference stress mode vector if unspecified
        if sStarHat is None:
            self.sStarHat = self._uniax_tensile_stress_in_1_dir()
        else:
            self.sStarHat = sStarHat
        
        # create a list of yafi terms, and normalise per the mode sStarHat
        self.termList = [yafiStressTerm(n, ds[i,:]) for i in range(self.numTerms)]
        self.normalise()
        
        
    def _uniax_tensile_stress_in_1_dir(self):
        """ return a unit uniaxial tensile stress (not deviatoric)
            expressed in the 3D vector basis of the yafi model
        """
        uniaxStress = np.zeros((1,self.dimensionality))
        uniaxStress[0,0] = 1./np.sqrt(2.)
        uniaxStress[0,1] = 1./np.sqrt(6.)
        
        return uniaxStress
        
        
    def _s_dot_d_tothen_for_term(self, s, termIndex):
        n = self.n
        assert termIndex <= self.numTerms, '{0} exceeds num of available terms'.format(termIndex)
        return self.termList[termIndex]._s_dot_d_tothe_n(s=s, n=n)
    

    def set_all_lambdas(self, lambdaValues):
        """ set all the lambda coefficient values from a list
        """
        numLambdas = len(lambdaValues)
        assert numLambdas == self.numTerms, 'incorrect number of lambda values'
        for i in range(len(lambdaValues)):
            self._set_lambda(termNum=i, lamb=lambdaValues[i])
        self.numZeroTerms = np.sum(np.isclose(lambdaValues,0.))
        

    def _get_all_lambdas(self):
        return np.array([self.termList[i].lamb for i in range(self.numTerms)])
        
    
    def _set_lambda(self, termNum, lamb):
        """ change a lambda value
        """
        self.termList[termNum].lamb = lamb
    
    
    def normalise(self):
        """ calculate sStar so that the expression evaluates to one
            for the reference stress mode sStarHat
        """
        self._calc_and_set_sStar()
            

    def _calc_and_set_sStar(self, n=None):
        """ calculate the value for sStar, the reference (scalar) stress
        """
        self.sStar = 1./self._sum_lam_s_dot_d_tothe_n(s=self.sStarHat, n=n)


    def _sum_lam_s_dot_d_tothe_n(self, s, n=None):
        """ calculate sum of (lambda_k * (s dot d_k)^n)
        """
        return np.sum([self.termList[i]._lambda_s_dot_d_tothe_n(s=s, n=n) \
                       for i in range(self.numTerms)])
                       
                       
    def _sStar_sum_lam_s_dot_d_tothe_n(self, s, n=None):
        """ calculate sStar * (sum of (lambda_k * (s dot d_k)^n))
        """
        return self._sum_lam_s_dot_d_tothe_n(s=s,n=n) * self.sStar
                       

    def _nthroot_sStar_sum_lam_s_dot_d_tothe_n(self, s, n=None):
        """ return (sStar * (sum of (lambda_k * (s dot d_k)^n))) ^ 1/n
        """
        if n is None:
            n = self.n
        else:
            assert int(n)==n, 'n must be an integer'
        return self._sStar_sum_lam_s_dot_d_tothe_n(s=s,n=n) ** (1./n)
        
        
    def evaluate_for_stress(self, s):
        """ this is a wrapper for _nthroot_sStar_sum_lam_s_dot_d_tothe_n
            with the assumption that the degree n is as specified during
            object initialisation
        """
        return self._nthroot_sStar_sum_lam_s_dot_d_tothe_n(s=s, n=self.n)
        

    def _sum_lam_dl_s_dot_d(self, s, strainRateCompIndex):
        """ the sum of lam_i * dl * s dot d is part of the numerator of
            the derivative of the facet expression
        """
        return np.sum([self.termList[i].get_derivative_numerator_value(s=s,
                       strainRateCompIndex=strainRateCompIndex) \
                       for i in range(self.numTerms)])
                       
                       
    def _first_derv_numerator(self, s, strainRateCompIndex):
        """ numerator of the first derivative is sStar times _sum_lam_dl_s_dot_d()
        """
        return self.sStar * self._sum_lam_dl_s_dot_d(s=s,
                                 strainRateCompIndex=strainRateCompIndex)
        
    
    def _first_derv_denomenator(self, s, n=None):
        """ the denominator of the first derivative is the value of the
            facet expression raised to the power (n-1)/n rather than the
            power (1/n)
        """
        return self._sStar_sum_lam_s_dot_d_tothe_n(s=s, n=n)**((n-1.)/n)
        


    def eval_first_derv(self, s, strainRateCompIndex):
        """ the first partial derivative with respect to the strain rate
            component with index "strainRateCompIndex"
        """
        n = self.n
        return self._first_derv_numerator(s=s, strainRateCompIndex=strainRateCompIndex) /\
               self._first_derv_denomenator(s=s, n=n)
               
               
    def eval_first_derv_at_surface(self, smode, strainRateCompIndex):
        """ the first partial derivative with respect to the strain rate
            component with index "strainRateCompIndex", for a stress on
            the yield surface corresponding to the stress mode smode
        """
        smode = smode/np.linalg.norm(smode) # garauntees its a stress mode
        s = self.dist_origin_to_surf(smode) * smode
        return self.eval_first_derv(s, strainRateCompIndex)
               
    
    def save_rvalues_to_file(self, path, minAlpha=0., maxAlpha=np.pi, numPoints=200.):
        """ calculate r values and save to file
        """
        # calculate the r values
        angles = np.linspace(minAlpha, maxAlpha, numPoints)
        rValues = np.zeros(angles.shape[0])
        for i in range(angles.shape[0]):
            rValues[i] = self.get_rvalue(angles[i])
            
        # write to file
        np.save(path, np.hstack((np.degrees(angles)[:,None], rValues[:,None])))
                
        
               
    def get_rvalue(self, beta):
        """ return an r value corresponding to a tensile test at an
            angle beta to the rolling (1) direction. The meaning of the
            r value here is is not ambigious because there is only one
            reference frame (RD = 1, TD = 2, ND = 3) for which it is
            possible to calculate r values
            
            NOTE: assumes 3D representation
        """
        # stress mode tensor in the full space, in the global frame
        cosBeta, sinBeta = np.cos(beta), np.sin(beta)
        cosSqr, sinSqr = cosBeta**2, sinBeta**2
        sig11 = cosSqr
        sig12 = -cosBeta*sinBeta
        sig22 = sinSqr
        sig33 = 0.
        
        # convert this stress mode tensor to the reduced space
        root2, root6 = np.sqrt(2.), np.sqrt(6.)
        oneOverRoot2, oneOverRoot6 = 1./root2, 1./root6
        s = np.zeros((1, self.dimensionality))
        s[0,0] = oneOverRoot2*(sig11 - sig22)
        s[0,1] = oneOverRoot6*(sig11 + sig22 - 2*sig33) # constant
        s[0,2] = root2 * sig12
        
        D11unscaled, D22unscaled, D12unscaled = get_full_space_strainrate_modes(s)
        
        # strain rate components in frame of the tensile test
        D11unscaledTest =-2*sinBeta*cosBeta*D12unscaled + cosSqr*D11unscaled + sinSqr*D22unscaled
        D22unscaledTest = 2*sinBeta*cosBeta*D12unscaled + cosSqr*D22unscaled + sinSqr*D11unscaled
        D33unscaledTest = -(D11unscaled + D22unscaled)
        
        return D22unscaledTest/D33unscaledTest
        
        
    def get_full_space_strainrate_modes(self, stressMode):
        """ return the strain rate mode in full strain rate space
            for a given stress mode
            
            NOTE: assumes 3D representation
        """
        dfds1 = self.eval_first_derv_at_surface(stressMode, strainRateCompIndex=0)
        dfds2 = self.eval_first_derv_at_surface(stressMode, strainRateCompIndex=1)
        dfds3 = self.eval_first_derv_at_surface(stressMode, strainRateCompIndex=2)
        
        return self.reduced_to_full_space(dfds1, dfds2, dfds3)

        
    def reduced_to_full_space(self, A1, A2, A5):
        """ converts reduced space tensor (vector rep) to full space
            tensor (vector rep)
        
           NOTE: assumes 3D representation
        """
        root2, root6 = np.sqrt(2.), np.sqrt(6.)
        oneOverRoot2, oneOverRoot6 = 1./root2, 1./root6

        return (oneOverRoot2*A1 + oneOverRoot6*A2,
               -oneOverRoot2*A1 + oneOverRoot6*A2,
                oneOverRoot2*A5)
               
               
    def full_to_reduced_space(self, A11, A22, A12):
        """ converts full space tensor (vector rep) to reduced space
            tensor (vector rep)
        
           NOTE: assumes 3D representation
        """
        root2, root6 = np.sqrt(2.), np.sqrt(6.)
        oneOverRoot2, oneOverRoot6 = 1./root2, 1./root6

        return (oneOverRoot2*(A11 - A22),
                oneOverRoot6*3.*(A11 + A22),
                root2 * A12)
        


    def dist_origin_to_surf(self, s, normalise=True):
        """ returns the distance from the origin to the surface in stress
            space in the direction of s
        """
        if normalise:
            sMode = s / np.linalg.norm(s)
        else:
            sMode = s
        return 1./self.evaluate_for_stress(s=sMode) 

        
    
    def _get_nonzero_lambdas_and_count(self):
        """ return indexing array for non zero lambdas and a count
        """
        lambdas = self._get_all_lambdas()
        nonZeroTerms = np.logical_not(np.isclose(lambdas, 0.))
        numNonZeroTerms = np.sum(nonZeroTerms)
        return lambdas[nonZeroTerms], nonZeroTerms, np.sum(nonZeroTerms)
               
        
        
    def write_3dfac_file(self, path):
        """ write the current yafi expression to a .fac format file
        """
        assert self.dimensionality == 3, 'this routine assumes a 3d facet'
        
        lambdas, nonZeroTerms, numNonZeroTerms = self._get_nonzero_lambdas_and_count()
        data = np.zeros((numNonZeroTerms, 6))

        data[:,0] = lambdas
        data[:,1] = self.ds[nonZeroTerms,:][:,0]
        data[:,2] = self.ds[nonZeroTerms,:][:,1]
        data[:,5] = self.ds[nonZeroTerms,:][:,2]
        
        headerText = 'generated by yafi\nno title\n  1     \n  F    \n  {0:<3d}  \n  {1:<3d}  '.format(self.n, numNonZeroTerms)
        np.savetxt(path, data, header=headerText, comments='', fmt='% 15.10f')
        
        
    def load_from_3dfac_file(self, path):
        """ read a standard fac file 
        """
        assert self.dimensionality == 3, 'this routine assumes a 3d facet'
        assert os.path.isfile(path), '{0} not found'.format(path)
        
        # load the fac file
        with open(path, 'r') as facFile:
            null = [facFile.readline() for i in range(4)]
            order = int(facFile.readline()[0:8].replace(' ',''))

        facData = np.genfromtxt(path, skip_header=6)
        numTerms = facData.shape[0]
        
        # select the relevant data
        lambdas = facData[:,0]
        ds = facData[:,[1,2,5]]
        
        # reinitialise and store this data
        self._initialise_or_reset(s=np.zeros((numTerms, self.dimensionality)),
                                  ds=ds, n=order, sStarHat=None)
        self.set_all_lambdas(lambdas)
        
        
    
    def write_yaf_file(self, path, nonzero=True):
        """ write a file which contains only the stress/ strain rate
            direction pairs used in the calibrated facet expression
        """
        assert self.s is not None, 'yafi object not calibrated?'
        if nonzero:
            lambdas, nonZeroTerms, numNonZeroTerms = self._get_nonzero_lambdas_and_count()
            
            stressPoints = self.s[nonZeroTerms,:]
            strainRateDirs = self.ds[nonZeroTerms,:]
            
        else:
            lambdas = self._get_all_lambdas()
        
            stressPoints = self.s
            strainRateDirs = self.ds
        
        np.savetxt(path, np.hstack((stressPoints, strainRateDirs)), fmt='% 15.10f')
        
        
        
class yafiCalibrator(object):
    """ a class for calibrating yafi expressions
    """
    
    def calibrate_nnls(self, sdPairs, n, xtraS=None, refStressMode=None):
        """ use the Lawson & Hanson nnls algorithm to find lambda values
            for a yafi expression. returns a yafi object
            
            sdPairs is an (a X 2b) array of vectors representing stress
            strain-rate direction pairs derived from a CP model or
            similar. (b is the dimensionality of the stress/strain rate
            space, a is the number of pairs)
            
            xtraS is a (c x b) array of points in stress space which are
            considered in the fitting, but do not have associated
            strain rate directions
            
            the calibration is done by solving the linear system of
            equations Ax = B, with:
                A is an (a x b) matrix of (s:d)^n values
                B is an (a x 1) column vector of ones
                x is an (a x 1) column vector of the desired lambda values
        """
        # check input
        assert sdPairs.shape[1] % 2 == 0, 'must be same num components in stress and strain rates'
        assert n % 2 == 0, 'n must be even'
        assert n > 1, 'n must be positive'
        assert n==int(n), 'n must be integer'
        numPairs = sdPairs.shape[0]
        dimensionality = sdPairs.shape[1] / 2
        
        # set up yafi object
        ds = sdPairs[:,0:dimensionality]
        s = sdPairs[:,dimensionality::]
        yafiObj = yafiStressExpression(s=s, ds=ds, n=int(n), sStarHat=refStressMode)
        
        # handle the extra stress points
        if xtraS is None:
            numUnpaired = 0
            allSpoints = s
        else:
            assert xtraS.shape[1] == dimensionality, 'xtra stress points have incorrect dimensionality'
            numUnpaired = xtraS.shape[0]
            allSpoints = np.vstack((s, xtraS))
        totalNumStressPoints = numPairs + numUnpaired
        
        
        # construct matrices A, x, and B
        print '{0} paired stresses, {1} xtra (unpaired) stresses'.format(numPairs, numUnpaired)
        A = np.zeros((totalNumStressPoints, numPairs))
        B = np.ones((totalNumStressPoints,))
        for i in range(totalNumStressPoints):
            for j in range(numPairs):
                A[i,j] = yafiObj._s_dot_d_tothen_for_term(s=allSpoints[i,:],termIndex=j)
        
        
        # find the lambda values
        lambdas, residuals = scipy_nnls(A, B)
        yafiObj.set_all_lambdas(lambdas)
        
        return yafiObj
        
    
    def partition_indices_by_energy(self, directions, method='quota',
                                    threshold=None, numSelectors=None):
        """ break an (a x b) matrix into two matrices (c x b) and (a-c x b)
            where the first matrix is the set of directions
            (in b dimensional space, projected on the b-sphere) with
            lowest energy, and the second matrix is the remainder
            
            the method by which the directions are seperated into "low"
            or "high" is specified by the method keyword, which may be
            "quota" or "biasedselectors"
        """
        # check input
        numDirections = directions.shape[0]
        assert numDirections > 3, 'need more than 2 directions'
        assert not (threshold is None), 'must specify threshold'
        assert threshold > 0. and threshold <= 1., 'must specify 0 < threhold <= 1'
        
        # calculate energies 
        energies = self._get_energy_for_all(directions, energyType='log')
        
        if method == 'quota':
            """ the number of directions selected for the "low energy" set
                is determined by the value of threshold (between 0 and 1).
                The values are sorted by energy the specified fraction
                with lowest energy are returned
            """
        
            if threshold==1.:# corresponds to selecting all (redundant case)
                selectedIndices = np.arange(numDirections)
                remainderIndices = None
            
            else:
                # sort  energies lowest to highest
                partionPosition = int(numDirections*threshold)
                sortIndices = np.argsort(energies.ravel())
                """
                # some debug code
                np.save('debug_energies.npy', energies)
                np.save('debug_sort.npy', sortIndices)
                e = energies[sortIndices]
                np.save('debug_norm.npy', (e - e.min())/(e - e.min()).max())
                """
                selectedIndices = sortIndices[0:partionPosition]
                remainderIndices = sortIndices[partionPosition::]
                
        elif method=='biasedselectors':
            """ this is a sort of binning selection where a fixed number of
                directions is selected, but selections are made across
                the full energy spectrum, with a bias which is a function
                of the energy. Thus the form of the energy distribution
                is important, e.g. if a large number of points have the
                same energy, many of them will be chosen.
                
                two parameters are required:
                1) numSelectors - the number of directions to choose
                2) threshold    - a number between 0-1 which specifies
                                  an upper limit of the energies for the
                                  directions which may be choosen from
            """
            # check input
            assert not(numSelectors is None), 'must specify numSelectors'
            numSelectors = int(abs(numSelectors))
            assert numSelectors > 1, 'must specify numSelectors > 1'
            assert numSelectors < numDirections, 'must specify numSelectors < numDirections'
            
            # get the log energies and sort lowest to highest
            energies = self._get_energy_for_all(directions, energyType='log')
            
            # define selector energy values
            maxSelectableEnergy = energies.max() * threshold
            selectorValues = np.linspace(energies.min(), maxSelectableEnergy, numSelectors)
            
            # make selection binning
            selectable = np.ones((numDirections,), dtype=np.bool)
            selectedIndices = np.zeros((numSelectors,), dtype=np.int)
            for i in range(numSelectors):
                nearestIndex = self._index_of_nearest_val_in_array(array=energies[selectable],
                                                                   value=selectorValues[i])
                selectedIndices[i] = nearestIndex
                selectable[nearestIndex] = False # prevents same point being chosen twice

            remainderIndices = np.array([i for i in range(numDirections) if not i in selectedIndices])
        
        else:
            raise ValueError('unknown sampling method {0}'.format(method))

        # result
        return selectedIndices, remainderIndices, energies
        
    def _index_of_nearest_val_in_array(self, array, value):
        return (np.abs(array-value)).argmin()


    def _get_energy(self, pointCloud, refPosition, energyType='s1'):
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
        joiningVectors = pointCloud - refPosition
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

    
    def _get_energy_for_all(self, pointCloud, roundOff=5, energyType='s1'):
        """ calculate the log energy for all points
        """
        # project the points onto the unit sphere if required (related to Taylor Factor???)
        pointCloud = pointCloud/np.linalg.norm(pointCloud, axis=1)[:,None]

        # calculate energy for each point
        numPoints = pointCloud.shape[0]
        energies = np.zeros((numPoints, ))
        for index in range(numPoints):
            energies[index] = self._get_energy(pointCloud=pointCloud,
                                                 refPosition=pointCloud[index,:],
                                                 energyType=energyType)
        
        return energies


if __name__=='__main__':
    """ if yafi is called as a script, load a specified mmm file and
        calibrate a yafi object
    """
    debug = True
    
    # check input arguments
    assert len(sys.argv)==6, 'usage: python yafi.py <mmm file> <partition ratio> <order> <use excluded stresses t/f> <num selectors>'
    mmmPath = os.path.abspath(sys.argv[1])
    assert os.path.isfile(mmmPath), 'cant find {0}'.format(mmmPath)
    partitionRatio = float(sys.argv[2])
    assert partitionRatio > 0. and partitionRatio<=1., 'must have 0 > partition ratio <= 1'
    order = int(sys.argv[3])
    excludeExtraStresses = not(str(sys.argv[4]).lower()[0] == 't')
    
    # load stress/strain rate pairs from mmm file, partition data
    mmmData = np.genfromtxt(mmmPath, skip_header=2)
    
    # here I assume a plane strain facet formulation (3 dimensions, A1, A2, A5)
    mmmReducedIndices = [0,1,4,5,6,9]
    mmmStrainRateIndices = [0,1,4]
    mmmStressIndices = [5,6,9]
    numDirections = mmmData.shape[0]
    
    # calibrate the yafi object
    calib = yafiCalibrator()
    lowIndices, highIndices, energies = calib.partition_indices_by_energy(\
                                        directions=mmmData[:,mmmStressIndices],
                                        threshold=partitionRatio,
                                        method='quota',
                                        numSelectors=int(sys.argv[5]))
    

    if highIndices is None or excludeExtraStresses:
        sdPairs = mmmData[:,mmmReducedIndices][lowIndices,:]
        xtraS = None
    else:
        sdPairs = mmmData[:, mmmReducedIndices][lowIndices,:]
        xtraS = mmmData[:, mmmStressIndices][highIndices, :]
    
    yafiObj = calib.calibrate_nnls(sdPairs, order, xtraS)
    
    stub = os.path.splitext(os.path.basename(mmmPath))[0]
    print '{0} of {1} terms ({2}%) are zero ({3} non zero terms)'.format(yafiObj.numZeroTerms,
           yafiObj.numTerms, (yafiObj.numZeroTerms*100)/yafiObj.numTerms,
           yafiObj.numTerms-yafiObj.numZeroTerms)

    yafiObj.write_3dfac_file(path=os.path.join(os.getcwd(),'{0}_o{1}.fac'.format(stub, order)))
    yafiObj.write_yaf_file(path=os.path.join(os.getcwd(),'{0}_o{1}_nonz.yaf'.format(stub, order)), nonzero=True)
    yafiObj.write_yaf_file(path=os.path.join(os.getcwd(),'{0}_o{1}_all.yaf'.format(stub, order)), nonzero=False)
    
    if debug:
        np.save('{0}_energies.npy'.format(stub), energies)
        np.save('{0}_selected_energies.npy'.format(stub), energies[lowIndices])
