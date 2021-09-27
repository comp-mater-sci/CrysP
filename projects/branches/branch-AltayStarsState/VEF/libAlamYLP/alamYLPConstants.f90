!
! $Id: alamYLPConstants.f90 2394 2015-11-20 17:10:57Z jgawad $
!
!>    \author Jerzy Gawad                                                
!>    Email:  Jerzy.Gawad@cs.kuleuven.be
!>
!>    Organization: Katholieke Universiteit Leuven (KU Leuven)
!>    Organization unit: Dept.Comp.Sci., TWR Group
!>                                                             
!>    \date Date of the first release: 2010-11-03 (in alamYLP.f90)
!>    $Revision: 2394 $
!>    $Date: 2015-11-20 18:10:57 +0100 (Fri, 20 Nov 2015) $
!>
!>    History of modifications: (see svn log)
!
!> Constants commonly used in libAlamYLP and in projects that depend on it.
module alamYLPConstants
implicit none

    !> Dimensionality of th search space for vector representation (Stress/Strain rate)
    integer,parameter :: alamEval_vSD_dim = 5
    !> Dimensions of second-rank tensor representation of Stress and Strain rate
    integer,parameter :: alamEval_tSD_dim = 3 
    
end module
