! $Id$
!
!>    \author Jerzy Gawad                                                
!>    Email:  Jerzy.Gawad@cs.kuleuven.be
!>
!>    Organization: Katholieke Universiteit Leuven
!>    Organization unit: Dept.Comp.Sci., TWR Group
!>                                                             
!>    \date Date of first release: 2011-09-17
!>    $Revision$
!>    $Date$
!>
!>    History of modifications: (see svn log)
!
!> Various utility subroutines and functions
module alamUtils

      double precision,parameter ::  rad2deg = (180.D0 / acos(-1.D0)), deg2rad = (acos(-1.D0) / 180.D0)

      double precision,parameter ::  root23 = sqrt(2.D0/3.D0)

      integer,parameter       :: display_unit = 6

      character,parameter     :: default_comment_sign = '#'
      
      !> Error message to be written by the subroutine finalize 
      character(len=128),save :: errmsg

contains
      !
      ! Functions ported from FNG
      ! -->>
      
      ! From fngMathUtils
      pure function vec_norm2(v)
      implicit none
      double precision :: vec_norm2
      double precision,dimension(:),intent(in) :: v
      vec_norm2 = sqrt(dot_product(v,v))
      end function

      double precision pure function average(a)
      double precision,dimension(:),intent(in) :: a
      integer :: n
      !
            n = size(a)
            if (n >= 1) average = sum(a) / dble(n)                 
            ! Undefined for empty array
      end function

      ! From fngMMMCore
      !> Finalization code
      subroutine finalize(errcode)
      implicit none
      integer,intent(in)      :: errcode
            !
            if (errcode /= 0) write(*,'(A)') trim(errmsg)
            !
            call exit(errcode)
      end subroutine

      ! <<-- Ported form FNG
      !

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
            200 format(A,T40,5F10.6)
            201 format(A,T40,F10.6)
            info = 0
      end subroutine

      subroutine stripComment(line,comment_mark)
      implicit none
      character(len=*),intent(inout)      :: line
      character,intent(in),optional       :: comment_mark
      !
      integer     :: idx
      character   :: comment_sign
      !
            if (len(line) == 0) return  ! nothing to do
            ! Set default comment sign, override if comment_mark is provided by user
            comment_sign = default_comment_sign
            if (present(comment_mark)) comment_sign = comment_mark
            !
            idx = index(line,comment_sign)
            if (idx /= 0) line(idx:) = ' '
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
            
            410 format('| Sm',T40,'| SmIdent*||Sm||',T80,'|SonA')
            411 format(3(F10.6,1X),T40,'|',3(F10.6,1X),'|',T80,3(F10.6,1X))

            420 format('| D')
            421 format(3(F10.6,1X))
      end subroutine

      
      
      !> The function converts Voigh-style vector into symmetrical rank-two tensors.
      !> Ordering of the terms in the vector: 11, 22, 33, 12, 23, 13
      pure function Vec6ToMat33(vec) result(mat)
      implicit none
      double precision,dimension(6),intent(in)  :: vec
      double precision,dimension(3,3)           :: mat

      !
            mat(1,1) = vec(1)
            mat(2,2) = vec(2)
            mat(3,3) = vec(3)
            mat(1,2) = vec(4)
            mat(2,3) = vec(5)
            mat(1,3) = vec(6)
            mat(2,1) = mat(1,2)
            mat(3,1) = mat(1,3)
            mat(3,2) = mat(2,3)
      !
      end function

      pure function Mat33ToVec6(mat) result(vec)
      implicit none
      double precision,dimension(3,3),intent(in)      :: mat
      double precision,dimension(6)                   :: vec
      !
            vec(1) = mat(1,1)
            vec(2) = mat(2,2)
            vec(3) = mat(3,3)
            vec(4) = mat(1,2)
            vec(5) = mat(2,3)
            vec(6) = mat(1,3)
      !
      end function

      
      
end module
