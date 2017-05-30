====
fopt
====

:Author: Jerzy Gawad <jerzy.gawad@cs.kuleuven.be>
:Date: $Date$
:Revision: $Revision$
:Copyright: KU Leuven

Overview
========


Documentation
=============

The code is documented using Doxygen. Developer manual can be made by usual means.

Third-party code
----------------

The library includes a implementation of Non-Negative Least Squares algorithm. 
The implementation is based on `public-domain NNLS Fortran 90`_ code.
The original version of this code was developed by Charles L. Lawson and 
Richard J. Hanson at Jet Propulsion Laboratory, 1973 JUN 15, and published in 
[LawsonHanson1995]_.  The `translation into Fortran 90`_ was done by Alan Miller, 
February 1997. J. Gawad has done further refactoring by embedding the procedures in a
module, which as a result provides the necessary explicit interface to the subroutine 
``nnls``.


.. [LawsonHanson1995] Charles L. Lawson and Richard J. Hanson, 
   "SOLVING LEAST SQUARES PROBLEMS", Prentice-HalL, 1974., 1995 SIAM.

.. _translation into Fortran 90: http://jblevins.org/mirror/amiller/nnls.f90
.. _public-domain NNLS Fortran 90: https://hesperia.gsfc.nasa.gov/~schmahl/nnls/nnls.f90

