!
! $Id$
!
!>    \author Jerzy Gawad
!>    Email:  Jerzy.Gawad@cs.kuleuven.be
!>
!>    Organization: Katholieke Universiteit Leuven (KU Leuven)
!>    Organization unit: Dept.Comp.Sci., TWR Group
!>    \copyright KU Leuven
!>
!>    \date Date of first release: 2012-11-09
!>    $Revision$
!>    $Date$
!>
!>    History of modifications: (see svn log)
!>
!>    \file criNamedRange.f90 
!
#include "criStdDefs.fpp"
!
!
#ifdef FORT_IMPLEMENTS_OOP
!
!
!> Provides object factory for range types from the criRange module.
!>
!> The module also defines named constants for the known types of ranges.
!> \remark This module is not available in the builds with old compilers (such as Intel Fortran 11.1)
module criNamedRange
use criLinearMap
#ifdef IFORT_OOP_FLAW_FIXED   
use criRange
#endif
implicit none

      integer,parameter :: range_ntypes = 4
      
      integer,parameter :: range_uniform_id = 1, range_biased_id = 2, range_doublebiased_id = 3, range_multibiased_id = 4
      
      type(MapItem),dimension(range_ntypes) :: range_name_map = [ &
                              MapItem('uniform',range_uniform_id), &
                              MapItem('biased', range_biased_id), & 
                              MapItem('doublebiased',range_doublebiased_id), &
                              MapItem('multibiased',range_multibiased_id) ] 

contains
      
#ifdef IFORT_OOP_FLAW_FIXED      
      !> Creates an instance of relevant 
      function rangeFactory(name) result(instance)
      implicit none
      class(range_type),pointer      :: instance
      character(len=*),intent(in)   :: name
      !
      integer :: id
      !
            nullify(instance)
            if (resolveName(range_name_map,name,id)) then
                  select case(id)
                  case(range_uniform_id)
                        allocate(uniformRange :: instance)
                  case(range_biased_id)
                        allocate(biasedRange :: instance)
                  case(range_doublebiased_id, range_multibiased_id)
                        allocate(multibiasedRange :: instance)
                  end select
            endif
      !
      end function
#else
#warning('Module criNamedRange provides limited features because of compiler bug.') 
#endif

end module

! End of: FORT_IMPLEMENTS_OOP
#endif


