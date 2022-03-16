!
! $Id: criNamedRange.f90 2607 2016-06-02 08:33:05Z jgawad $
!
!>    \author Jerzy Gawad
!>    Email:  Jerzy.Gawad@cs.kuleuven.be
!>
!>    Organization: Katholieke Universiteit Leuven (KU Leuven)
!>    Organization unit: Dept.Comp.Sci., TWR Group
!>    \copyright KU Leuven
!>
!>    \date Date of first release: 2012-11-09
!>    $Revision: 2607 $
!>    $Date: 2016-06-02 10:33:05 +0200 (Thu, 02 Jun 2016) $
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

      integer,parameter,private :: range_ntypes = 5
      
      integer,parameter ::  range_uniform_id = 1, &
                            range_biased_id = 2, &
                            range_doublebiased_id = 3, &
                            range_multibiased_id = 4, &
                            range_discrete_id = 5
      
      
      type(MapItem),dimension(range_ntypes),parameter :: range_name_map = [ &
                              MapItem('uniform',range_uniform_id), &
                              MapItem('biased', range_biased_id), & 
                              MapItem('doublebiased',range_doublebiased_id), &
                              MapItem('multibiased',range_multibiased_id), &
                              MapItem('discrete', range_discrete_id)] 

      ! Extended set of range names or aliases. These are implemented by the existing
      ! range types.
      
      integer,parameter,private :: range_nextensions = 2
   
      !>@{ \name Identifiers of extended range names.
      !> These identifiers must not overlap with 
      !> the identifiers for named range types.

      integer,parameter :: range_zero_id = 100, &
                           range_one_id = 101
      !>@}
      
      !> Map of extended range names
      type(MapItem),dimension(range_nextensions),parameter :: range_name_extensions_map = [ &
                              MapItem('zero',range_zero_id), &
                              MapItem('one', range_one_id)]

      
contains
      
#ifdef IFORT_OOP_FLAW_FIXED      
      !> Creates an instance of relevant range type
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
                  case(range_discrete_id)
                        allocate(discreteRange :: instance)
                  end select
            endif
      !
      end function
      
      
      
      !> Creates an instance of the relevant range type given its name 
      !> OR makes an appropriate instance for a name in extended set of range
      !> names.
      !>
      !> Currently supported extended set of range names includes:
      !> - 'zero' : one-elemental range, the element has value 0.0
      !> - 'one'  : one-elemental range, the element has value 1.0
      function rangeFactory_extended(name) result(instance)
      implicit none
      class(range_type),pointer      :: instance
      character(len=*),intent(in)   :: name
      !
      integer :: id
      !
            nullify(instance)
            if (resolveName(range_name_extensions_map, name, id)) then
                  ! The currently avialable extended names can be implemented
                  ! by using the discrete range.
                  allocate(discreteRange :: instance)
                  ! explicit casting to discreteRange
                  select type(instance)
                  type is (discreteRange)
                        select case(id)
                        case(range_zero_id)
                              instance = discreteRange([0.D0])
                        case(range_one_id)
                              instance = discreteRange([1.D0])
                        end select   
                  end select
            else
                  instance => rangeFactory(name)
            endif
      !
      end function
      
#else
#warning('Module criNamedRange provides limited features because of compiler bug.') 
#endif

end module

! End of: FORT_IMPLEMENTS_OOP
#endif


