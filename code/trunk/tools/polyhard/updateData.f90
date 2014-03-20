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
use fngErrcodes
use fngConstants, only: fng_dsv_dim
implicit none
      type :: defData
            integer :: step = 0                             !< Time increment number
            integer :: seq  = 0                             !< Sequence number
            double precision,dimension(3,3) :: tDEps = 0.D0 !< Plastic strain increment
            integer :: req_texu = 0                         !< Request for texture update
            integer :: req_aniso = 0                        !< Request for anisotropy update
            integer :: req_hard  = 0                        !< Request for hardening update
            double precision :: eps_0 = 0.D0                !< For the hardening update: lower strain limit
            double precision :: eps_1 = 0.D0                !< For the hardening update: upper strain limit
            double precision,dimension(fng_dsv_dim) :: vStrainMode = 0.D0   !< Direction of the plastic strain (mode)
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
            info = fngSuccess
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
            read(inunit,*,iostat=info,err=900) dta%vStrainMode
            info = fngSuccess
            return
      900   info = fngErr_IORead
            return
      
      !
      end subroutine
      
end module