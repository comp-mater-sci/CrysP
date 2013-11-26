""" convenience math functions not already found in numpy, 'pure python'
    versions of algorithms already available in other languages, or interfaces
    for functions available in FORTRAN or C libraries

    references are included for each function
"""
# import native modules
import math

# import third part modules
try:
    import numpy as np
except ImportError:
    print 'Numpy needs to be installed and accessible.'
    raise


def dec2baseN(num, base, numerals="0123456789abcdefghijklmnopqrstuvwxyz"):
    """ return an integer 'num' expressed in base 'b' using the symbols given
        in 'numerals'
        
        see http://code.activestate.com/recipes/65212/
    """
    noNumerals = len(numerals)
    
    if base in [0,1]:
        raise ValueError('Base {0} doesnt make sense.'.format(base))
    elif noNumerals < base:
        raise ValueError('Need ({0}) numerals, got ({1})'.format(base, noNumerals))
    elif num == 0:
        return numerals[0]
    else:
        return dec2baseN(num // base, base, numerals).lstrip(numerals[0]) + \
               numerals[num % base]
                       
                       
def baseN2dec(numString, base, numerals="0123456789abcdefghijklmnopqrstuvwxyz"):
    """ return numString as a base 10 int, based on the symbols given by'numerals'
        and the given base
    """
    if base in [0,1]:
        raise ValueError('Base {0} doesnt make sense.'.format(base))
        
    elif numString=='0':
        return 0
    else:
        # split into integer and fractional part
        if '.' in numString:
            fractional = True
            [intString, fracString] = numString.split('.')
            
            # calc value of fractional part
            noDigits = len(fracString)
            positionVals = [base**-(position+1) for position in range(noDigits)]
            digitVals = [numerals.index(digit) for digit in fracString]
            
            fracPart = sum([digitVals[index]*positionVals[index] for index in range(noDigits)])
            
        else:
            intString = numString
            fractional = False
        
        # calc value of integer part
        noDigits = len(intString)
        positionVals = [base**position for position in range(noDigits-1,-1,-1)]
        digitVals = [numerals.index(digit) for digit in intString]
        
        intPart = sum([digitVals[index]*positionVals[index] for index in range(noDigits)])
        
        if fractional:
            return intPart + fracPart
        else:
            return intPart
        
    
def halton1D(jth, k, decimal=False):
    """ return the value of the jth term of the 1D Halton sequence, which uses a
        base b, where b is the kth prime number 
        
        see Numerical Recipies in Fortran 77, p305-, ISBN 0 521 43064 X
    """
    # get j written in base b, where b is the kth prime number
    base = getNthPrime(k)
    jInBaseB = dec2baseN(jth, base)

    # reverse the digits and insert a radix point at the start
    reverse = jInBaseB[::-1]
    answer = '0.{0}'.format(reverse)
    
    if decimal:
        return baseN2dec(answer, base)
    else:
        return answer
    
    
def haltonND(noTerms, n):
    """ return a list of n-tuples (the list has "noTerms" items)
    
        the first n primes are used for the bases in the Halton sequences
    """
    returnList = []
    
    for term in range(noTerms):
        newItem = []
        
        for tupleComponent in range(n):
            newItem.append(halton1D(j=term, k=tupleComponent))
            
        returnList.append(tuple(newItem))
        
    return returnList
    
    
def isPrime(num):
    """ return True if num is prime, by checking integers below the square
        root of num
    """
    import math

    for j in range(2,int(math.sqrt(num)+1)):
        if (num % j) == 0: 
            return False
    return True
    

def getNthPrime(n):
    """ return the nth prime number, by testing every number using isPrime(number)
    """
    ith = 0
    num = 1
        
    while ith < n:
        num += 1 
        if isPrime(num): 
            ith += 1
    
    return num
    
    
def LatinHypercubeIndices(noParameters, noLevels):
    """ latin hypercube sub-random sampling. In the following explanation
        'noParameters'=N and noLevels=M.
        
        returns an array whose columns correspond to the parameters and rows
        to the chosen values of those parameters
    
        From "Numerical Recipies in Fortran 77", p305-, ISBN 0 521 43064 X:
        
        "useful when you must sample an N-dimensional space exceedingly sparsely,
         at M points... The idea is to partition each design parameter (dimension)
         into M segments, so that the whole space is partitioned into M^N cells.
         
      ...choose M cells to contain the sample points by the following algorithm:
         Randomly choose one of the M**N cells for the first point. Now eliminate
         all cells that agree with this point on any of its parameters (that is,
         cross out all cells in the same row, column, etc.), leaving (M - 1)**N
         candidates. Randomly choose one of these, eliminate new rows and columns,
         and continue the process until there is only one cell left, which then
         contains the final sample point.
         
         The result of this construction is that each design parameter will have
         been tested in every one of its subranges. If the response of the system
         under test is dominated by ONE of the design parameters, that parameter
         will be found with this sampling technique. On the other hand, if there
         is an important interaction among different design parameters, then the
         Latin hypercube gives no particular advantage. Use with care." 
    """
        
    if noParameters < 2:
        raise ValueError('noParamters should be 2 or greater')
    
    # build a boolean array representing all possible parameter combinations
    cellsLeft = np.ones([noLevels]*noParameters, dtype=np.bool)
    
    # first randomly select a starting cell
    firstCellIndex = GetRandomIndices(noAxes=noParameters, maxValue=noLevels)
    selectedCellList = [firstCellIndex]
    cellsLeftToChooseFrom = True
    indices = range(noParameters)
    
    while(cellsLeftToChooseFrom):
        # eliminate rows and columns beloning to the selected cell
        # (note there should be a more elegant way to do this than exec and strings,
        # perhaps using numpy iterators?)
        for index in indices:
            # build a string to index the array holding which cells remain
            indexString = list(':' + ',:'*(noParameters-1))
            if index==0:
                indexString[0] = str(selectedCellList[-1][0])
            else:
                indexString[(index*2)] = str(selectedCellList[-1][index])
            
            # set the row/column corresponding to 'index' to False
            exec('cellsLeft[{0}] = False'.format(''.join(indexString)))
            
        # randomly choose a cell from the remaining cells
        possibleCells = np.array(np.where(cellsLeft==True))
        noCellsLeft = len(possibleCells[0])
        cellsLeftToChooseFrom = not noCellsLeft==0
        
        if cellsLeftToChooseFrom:
            randIndex = int(round((np.random.rand()*noCellsLeft)))-1
            selectedCellList.append(possibleCells[:,randIndex])
            
    return np.array(selectedCellList)
    
    
def GetRandomIndices(noAxes, maxValue):
    """ return an array of random integers within the given maximum, of
        length 'noAxes'
        
        Note that values are to be used as array indices, so the values
        will be in the range 0 to (maxValue-1)
    """
    
    randArray = np.random.random((noAxes))*(maxValue-1)
    return np.array(np.round(randArray), dtype=np.int)
   
    
def Erf(x):
    """ implementation of the error function from:
    
        http://www.johndcook.com/blog/2009/01/19/stand-alone-error-function-erf/,
        which in turn is based on the textbook "Handbook of Mathematical Functions:
        with Formulas, Graphs, and Mathematical Tables", M. Abramowitz &
        I.A. Stegun.
        
        note erf is available in SciPy, but it is better not to rely on SciPy
        in the abaqus environment (it has to be built to work with abaqus's
        numpy version 1.4 for one thing)
    """
    # constants
    a1 =  0.254829592
    a2 = -0.284496736
    a3 =  1.421413741
    a4 = -1.453152027
    a5 =  1.061405429
    p  =  0.3275911

    # Save the sign of x
    sign = 1
    if x < 0:
        sign = -1
    x = abs(x)

    # from textbook (see doc string above)
    t = 1.0/(1.0 + p*x)
    y = 1.0 - (((((a5*t + a4)*t) + a3)*t + a2)*t + a1)*t*math.exp(-x*x)

    return sign*y


def Gauss(x, mu=0., sigma=1.):
    """ return the value of the Gauss function. if no values are specified for
        mu and sigma, the value of the standard function is returned.
    
        https://en.wikipedia.org/wiki/Normal_distribution
    """
    
    coeff = 1/(sigma*math.sqrt(2*math.pi))
    power = -((x - mu)**2)/(2*(sigma**2))
    
    return coeff*math.exp(power)
    
    
def CumulativeGauss(mu, sigma, x):
    """ return the value of the Gauss cumulative distribution function
    
        https://en.wikipedia.org/wiki/Normal_distribution
    """
    
    return 0.5*(1 + Erf((x-mu)/(math.sqrt(2*(sigma**2)))))


def InvGauss(mu, scale, x):
    """ return the value of the inverser Gauss distribution function at x
    
        http://en.wikipedia.org/wiki/Inverse_Gaussian_distribution
    """
    if x<0.:
        raise ValueError('x must be greater or equal to zero.')

    term1 = (scale/(2*math.pi*(x**3)))**0.5
    term2 = -(scale*((x - mu)**2))/(2*(mu**2)*x)
    
    return term1 * math.exp(term2)


def CumInvGauss(mu, scale, x):
    """ return the value of the cumulative inverse gauss function for
        mean mu and scaling parameter 'scale'. x must be greater or equal to 0.
        
        http://en.wikipedia.org/wiki/Inverse_Gaussian_distribution
    """
    if x<0.:
        raise ValueError('x must be greater or equal to zero.')
    
    term1 = math.sqrt(scale/x) * ((x/mu) - 1)
    term2 = math.exp((2*scale)/mu)
    term3 = -math.sqrt(scale/x)*((x/mu) + 1)
    
    return Gauss(term1) + term2*Gauss(term3)
    
    
    
def InvStandardCumGauss(p):
    """ return the inverse cumulative Gauss function
    
        Modified from the author's original perl code (original comments follow
        below) by dfield@yahoo-inc.com.  May 3, 2004.

        Lower tail quantile for standard normal distribution function.

        This function returns an approximation of the inverse cumulative
        standard normal distribution function.  I.e., given P, it returns
        an approximation to the X satisfying P = Pr{Z <= X} where Z is a
        random variable from the standard normal distribution.

        The algorithm uses a minimax approximation by rational functions
        and the result has a relative error whose absolute value is less
        than 1.15e-9.

        Author:      Peter John Acklam
        Time-stamp:  2000-07-19 18:26:14
        E-mail:      pjacklam@online.no
        WWW URL:     http://home.online.no/~pjacklam
    """

    if p <= 0 or p >= 1:
        # The original perl code exits here, we'll throw an exception instead
        raise ValueError( "Argument to InvCumulativeGauss %f must be in open interval (0,1)" % p )

    # Coefficients in rational approximations.
    a = (-3.969683028665376e+01,  2.209460984245205e+02, \
         -2.759285104469687e+02,  1.383577518672690e+02, \
         -3.066479806614716e+01,  2.506628277459239e+00)
    b = (-5.447609879822406e+01,  1.615858368580409e+02, \
         -1.556989798598866e+02,  6.680131188771972e+01, \
         -1.328068155288572e+01 )
    c = (-7.784894002430293e-03, -3.223964580411365e-01, \
         -2.400758277161838e+00, -2.549732539343734e+00, \
          4.374664141464968e+00,  2.938163982698783e+00)
    d = ( 7.784695709041462e-03,  3.224671290700398e-01, \
          2.445134137142996e+00,  3.754408661907416e+00)

    # Define break-points.
    plow  = 0.02425
    phigh = 1 - plow

    # Rational approximation for lower region:
    if p < plow:
       q  = math.sqrt(-2*math.log(p))
       return (((((c[0]*q+c[1])*q+c[2])*q+c[3])*q+c[4])*q+c[5]) / \
               ((((d[0]*q+d[1])*q+d[2])*q+d[3])*q+1)

    # Rational approximation for upper region:
    if phigh < p:
       q  = math.sqrt(-2*math.log(1-p))
       return -(((((c[0]*q+c[1])*q+c[2])*q+c[3])*q+c[4])*q+c[5]) / \
                ((((d[0]*q+d[1])*q+d[2])*q+d[3])*q+1)

    # Rational approximation for central region:
    q = p - 0.5
    r = q*q
    return (((((a[0]*r+a[1])*r+a[2])*r+a[3])*r+a[4])*r+a[5])*q / \
           (((((b[0]*r+b[1])*r+b[2])*r+b[3])*r+b[4])*r+1)
