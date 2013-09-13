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
!>    \date Date of the initial release: 2013-07-07
!>    $Revision$
!>    $Date$
!>
!>    History of modifications: (see svn log)
!>
!
!> Parsing of HMS defdata communication files (Skyfall format).
module updateData

      type :: defData
            integer :: step                           !< Time increment number
            integer :: seq                            !< Sequence number
            double precision,dimension(3,3) :: tDEps  !< Plastic strain increment
            integer :: req_texu                       !< Request for texture update
            integer :: req_aniso                      !< Request for anisotropy update
            integer :: req_hard                       !< Request for hardening update
            double precision :: eps_0                 !< For the hardening update: lower strain limit
            double precision :: eps_1                 !< For the hardening update: upper strain limit
      end type

contains

      subroutine readUpdateData(inunit,dta,info)      
      implicit none
      integer,intent(in)            :: inunit
      type(defData),intent(out)     :: dta
      integer,intent(out)           :: info
      !
      integer :: i
      double precision :: tr
      !
            info = 0
            tr = 0.D0
            read(inunit,*,iostat=info,err=900) dta%step, dta%seq
            do i=1,3
                  read(inunit,*,iostat=info,err=900) dta%tDeps(:,i)
                  tr = tr + dta%tDeps(i,i)
            enddo
            ! Fix tDeps: make it traceless
            if (tr > epsilon(0.D0)) then
                  tr = tr / 3.0
                  do i=1,3
                         dta%tDeps(i,i) = dta%tDeps(i,i) - tr
                  enddo
            endif
            ! Request: tex
            read(inunit,*,iostat=info,err=900) dta%req_texu
            read(inunit,*,iostat=info,err=900) dta%req_aniso
            read(inunit,*,iostat=info,err=900) dta%req_hard
            read(inunit,*,iostat=info,err=900) dta%eps_0
            read(inunit,*,iostat=info,err=900) dta%eps_1
            info = 0
            return
      900   info = -1
      !
      end subroutine
      
end module