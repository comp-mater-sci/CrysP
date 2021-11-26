! $Id$
!
!>    \author Jerzy Gawad                                                
!>    Email:  Jerzy.Gawad@cs.kuleuven.be
!>
!>    Organization: Katholieke Universiteit Leuven
!>    Organization unit: Dept.Comp.Sci., TWR Group
!>                                                             
!>    \date Date of the initial release: 2011-09-17 (under the name dmcUtils)
!>    $Revision$
!>    $Date$
!>
!>    History of modifications: (see svn log)
!
!> Various utility subroutines and functions
module dmcUtils
use criMathUtils
use criRuntime
      
      integer,parameter       :: display_unit = 6

      character,parameter     :: default_comment_sign = '#'
     
      ! Meta-data
      
      integer,parameter       :: nSymTensComps = 6
      integer,parameter       :: nDevTensComps = 5
      
contains
      !
      ! Functions ported from FNG
      ! -->>

      double precision pure function average(a)
      double precision,dimension(:),intent(in) :: a
      integer :: n
      !
            n = size(a)
            if (n >= 1) average = sum(a) / dble(n)                 
            ! Undefined for empty array
      end function

      subroutine printIdentResults(outunit,vS,vA,vSonA,vSonAn,R,info)
      implicit none
      integer,intent(in)                        :: outunit
      double precision,dimension(5),intent(in)  :: vS, vA, vSonA,vSonAn
      double precision,intent(in)               :: R
      integer,intent(out)                       :: info

            write(outunit,('(/)'))
            write(outunit,200) 'Requested stress:', vS
            write(outunit,200) 'Identified scaled stress:',vSonAn
            write(outunit,201) 'Norm of stress residual:', R      
            write(outunit,('(/)'))
            write(outunit,200) 'Stress on vA:',vSonA
            write(outunit,201) 'Norm of stress on vA:', vec_norm2(vSonA) 
            write(outunit,('(/)'))
            !
            200 format(A,T40,5(E12.5,1X))
            201 format(A,T40,E12.5)
            info = 0
      end subroutine


      subroutine printIdentResultsT(outunit,S,SIdent,SonA,D,info)
      implicit none
      integer,intent(in)                              :: outunit
      double precision,dimension(3,3),intent(in)      :: S, SIdent,SonA, D
      integer,intent(out)                             :: info
      !
      integer :: j
           !
            write(outunit,*) 'Stress:'
            write(outunit,410)
            do j=1,3
                  write(outunit,411) S(j,:), SIdent(j,:), SonA(j,:)
            enddo
            write(outunit,*)
            write(outunit,*) 'Resulting strain rate:'
            write(outunit,420)
            write(outunit,421) D
            info = 0
            
            410 format('| Sm',T45,'| SmIdent*||Sm||',T90,'|SonA')
            411 format(3(E12.5,1X),T45,'|',3(E12.5,1X),'|',T90,3(E12.5,1X))

            420 format('| D')
            421 format(3(E12.5,1X))
      end subroutine

end module
