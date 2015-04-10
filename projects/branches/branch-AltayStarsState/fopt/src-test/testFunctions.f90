!
! $Id$
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!>    \author     Jerzy Gawad 
!>    Email:      Jerzy.Gawad@cs.kuleuven.be
!>
!>    Organization: Katholieke Universiteit Leuven
!>    Organization unit: Dept.Comp.Sci., TWR Group
!>
!>    \revision    
!>    Date of initial release: 2012-04-02
!>    History of modifications: (see SVN log).
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! 
! 
      
!> Test functions for the nllsTR module
module testFunctions
use nllsTR
use objectiveFx

      integer,parameter :: testFunctions_n = 3, testFunctions_m = 4

      type,extends(MKLFDJacobiObjFunction) :: quadraticFX
            double precision,dimension(3) :: vParams = (/ 1.D0, 1.D0, 1.D0 /)
            procedure(JacobiAnalytical),pointer,pass(this) :: jacobiAnalytical 
            
      contains
            procedure,pass(this) :: quadra
            
            procedure :: objectiveEval => quadraEval
            
      end type

      abstract interface 
            subroutine JacobiAnalytical(this,vX,mJ,info)
                  import :: quadraticFX
                  class(quadraticFX)                              :: this
                  double precision,dimension(:),intent(in)        :: vX       !< Dimension must be: [n_X_dim]
                  double precision,dimension(:,:),intent(out)     :: mJ       !< Dimension must be: [mF_dim x n_X_dim]
                  integer,intent(out)                             :: info      
            end subroutine
      end interface
      
      
      type,extends(quadraticFX) :: quadraticAnalyticFX
           
      contains
            procedure :: jacobiMatrixEval => quadraJacobiAnalyticEval
            
      end type
      
contains

      subroutine quadraEval(this,vX,info)
      implicit none
      class(quadraticFX),intent(inout)      :: this
      double precision,dimension(:),intent(in)    :: vX     !< Dimension must be: [n_X_dim]
      integer,intent(out)                         :: info   !< Set to 0 on success
      !
            ! Just call the computational routine
            call this%quadra(vX,this%state%vF,info)
      !
      end subroutine

      !  m = n + 1
      subroutine quadra(this,vX,vFval,info)
      implicit none
            class(quadraticFX)                          :: this
            double precision,dimension(:),intent(in)    :: vX       !< Dimension must be: [n_X_dim]
            double precision,dimension(:),intent(inout) :: vFval    !< Dimension must be: [m_F_dim]
            integer,intent(out)                         :: info
      
      vFval = this%vParams(1) * vX * vX  ! assing x^2
      vFval(1) = vFval(1) + this%vParams(2) * vX(2)**2  ! add x_2^2
      vFval(3) = this%vParams(3) * (vFval(3) + vFval(1))  ! add x_2^2 and x_1^2
      vFval(4) = this%vParams(3) * ( vX(3)**2 + vX(1)**2)  
      info = 0
      end subroutine

      ! Suitable for djacobi
      !  m = n + 1
      subroutine djquadra(m,n,vX,vFval)
      implicit none
            integer     :: m, n
            double precision,dimension(n),intent(in)        :: vX       !< Dimension must be: [n_X_dim]
            double precision,dimension(m),intent(inout)     :: vFval    !< Dimension must be: [m_F_dim]
      !
      vFval = vX * vX  ! assing x^2
      vFval(1) = vFval(1) + vX(2)**2  ! add x_2^2
      vFval(3) = vFval(3) + vFval(1)  ! add x_2^2 and x_1^2
      vFval(4) = vX(3)**2 + vX(1)**2
      end subroutine

      !> Analytical calculations
      !  m = n + 1
      subroutine quadraJacobiAnalytic(this,vX,mJ,info)
      implicit none
      class(quadraticFX)                              :: this
      double precision,dimension(:),intent(in)        :: vX       !< Dimension must be: [n_X_dim]
      double precision,dimension(:,:),intent(inout)   :: mJ       !< Dimension must be: [mF_dim x n_X_dim]
      integer,intent(out)                             :: info
      !
            mJ(1,1) = 2.D0 * this%vParams(1) * vX(1)
            mJ(2,1) = 0.D0
            mJ(3,1) = 2.D0 * this%vParams(1) * this%vParams(3) * vX(1)
            mJ(4,1) = 2.D0 * this%vParams(3) * vX(1)
            !
            mJ(1,2) = 2.D0 * this%vParams(2) * vX(2)
            mJ(2,2) = 2.D0 * this%vParams(1) * vX(2)
            mJ(3,2) = 2.D0 * this%vParams(2) * this%vParams(3) *vX(2)
            mJ(4,2) = 0.D0
            !
            mJ(1,3) = 0.D0
            mj(2,3) = 0.D0
            mJ(3,3) = 2.D0 * (this%vParams(3)**2) *vX(3)
            mJ(4,3) = 2.D0 * this%vParams(3) *vX(3)
            !
            info = 0
      end subroutine
      
  
           
      subroutine quadraJacobiAnalyticEval(this,vX,info)
      implicit none
      class(quadraticAnalyticFX),intent(inout)        :: this
      double precision,dimension(:),intent(in)        :: vX       !< Dimension must be: [n_X_dim]
      integer,intent(out)                             :: info
      !
            call quadraJacobiAnalytic(this,vX,this%state%mJ,info)
      end subroutine
      
end module