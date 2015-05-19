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
!>    \date Date of first release: 2011-12-04
!>    $Revision$
!>    $Date$
!>
!>    History of modifications: (see svn log)
!>
!>    \file criMkTemp.f90 
!
#include "criStdDefs.fpp"
!
!> Provides pseudo-random strings that can be used as names of temporary files.
module criMkTemp

      !> Set of characters that the strings will be composed of.
      character(len=*),parameter,private :: tmpbase = 'abcdefghijklmnopqrstuvwxyz1234567890'
      !> Length of the base string
      integer,parameter,private :: tmpbase_len = len(tmpbase)
      
contains
      
      !> Initialize the module. Should be called prior to any call to 
      !> other functions/subroutines that are defined in this module.
      subroutine mkTemp_init(seed_value)
#ifdef _WIN32
      use DFLIB
#else
      use IFPORT
#endif
      implicit none
      integer,intent(in),optional   :: seed_value !< Seed value. If not provided, time-based seed will be used.
      integer :: sd
      ! Initialization of seed to the default value
#ifdef _WIN32
            sd = (RND$TIMESEED)
#else
            sd = -1 
#endif
            if (present(seed_value)) sd = seed_value
            call SEED(sd)
      end subroutine

      !> Returns a string of length strlen containing a pseudo-random sequence of characters
      function tmpname(strlen)
      use IFPORT
      implicit none
      integer,intent(in)      :: strlen   !< Requested length of the string
      character(len=strlen)   :: tmpname  
      !
      integer :: i,pos
      tmpname = ''
      do i=1,strlen
            pos = modulo(irand(), tmpbase_len-1) + 1
            tmpname = trim(tmpname) // tmpbase(pos:pos)
      enddo
      end function
      
end module
