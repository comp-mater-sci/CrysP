!
! $Id$
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!!    Author:     Jerzy Gawad 
!!    Email:      Jerzy.Gawad@cs.kuleuven.be
!!
!!    Organization: Katholieke Universiteit Leuven
!!    Organization unit: Dept.Comp.Sci., TWR Group
!!    
!!    Date of initial release: 2010-06-25
!!    History of modifications: (see SVN log).
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! 
! 

!! TODO: support for analytical calculations of Jacobi matrix.

!> nllsTR  -- wrapper module for MKL Non-Linear Least Squares Trust Region algorithm
!> \author  Jerzy Gawad 
!> \remark The module can be compiled with: ifort >= 11.1, gfortran >= 4.5. Earlier versions
!>         are unable to handle OO technique in proper way or simply fail at compilation time. 
module nllsTR



      !> Control over Trust Region algorithm
      type nllsTRConf
            !> Array of parameters controling stop criteria
            !>
            !> Various convergence criteria are evaluated, see MKL documentation for details
            double precision,dimension(6)                :: eps = 1.e-10
            integer                                      :: iter1 = 300 !< Maximum number of iterations
            integer                                      :: iter2 = 50  !< Maximum number of trial steps
            double precision                             :: init_step = 100.0  !< Initial step bound factor
            !> Lower constraints for design vector
            double precision                             :: lo_limit = 0.D0
            !> Upper constraints for design vector
            double precision                             :: up_limit = 1.D2

            !> Helper for problems with invariant Jacobi matrix
            !>
            !> If this variable is set to .true., the library will assume that Jacobi
            !> matrix is constant over subsequent steps. As effect, the matrix will be
            !> evaluated only once, before the first step of optimization procedure.
            !> This parameter is useful if the optimization problem is linear or can be
            !> linearized.
            logical                                      :: constJacobi = .false.

      end type       
      

      !> Default step for finite difference evaluation of Jacobi matrix
      double precision,parameter                        :: nllsTR_jacobi_eps = 1.D-7


      !>@{ \name Other parameters 
      !>  These parameters are not directly accessible. Use \ref nlls_TR_init to control them. \sa nlls_TR_init

      !> Output unit
      integer,private                            :: nllsTR_ounit = 6

      !> Verbosity level. 
      !>
      !> The following values of verbosity are allowed:
      !>   -  0 - only error messages, 
      !>   -  1 - some diagnostic informations, 
      !>   -  2 and higher - detailed informations (huge amount of output is expected!)
      integer,private                            :: nllsTR_iw = 0

     !>@} 


      type,abstract :: objectiveFunction
            integer     :: n_X_dim        !< Dimensionality of vector X (argument)
            integer     :: m_F_dim        !< Dimensionality of objective function
      contains

            procedure(IF_objectiveFx),deferred,pass(this)     :: objectiveFx

            procedure(IF_JacobiObjFx),deferred,pass(this)     :: jacobiMatrixFx

      end type


      abstract interface 
            !> Abstract interface for objective function calculations. 
            !> \note It is user's responsibility to provide a function that complies with this interface.
            subroutine IF_objectiveFx(this,vX,vFval,info)
                  import  ::  objectiveFunction
                  class(objectiveFunction)                    :: this
                  double precision,dimension(:),intent(inout) :: vX       !< Dimension must be: [n_X_dim]
                  double precision,dimension(:),intent(inout) :: vFval    !< Dimension must be: [m_F_dim]
                  integer,intent(out)                         :: info
            end subroutine

            !> Abstract interface for a function that calculates Jacobi matrix.
            !> \note It is user's responsibility to provide a function that complies with this interface.
            subroutine IF_JacobiObjFx(this,vX, mJacobi,info)
                  import :: objectiveFunction
                  class(objectiveFunction)       :: this
                  double precision,dimension(:),intent(inout)     :: vX       !< Dimension must be: [n_X_dim]
                  double precision,dimension(:,:),intent(inout)   :: mJacobi  !< Dimension must be: [m_F_dim x n_X_dim]
                  integer,intent(out)                             :: info
            end subroutine
      end interface
      

      type,abstract,extends(objectiveFunction) :: FDJacobiObjFunction
            double precision                          ::  jacobi_eps = nllsTR_jacobi_eps 
      contains
            procedure :: jacobiMatrixFx => JacobiObjFx_djacobi
      end type

      ! Internal components of the module.
      private writeMatrix
      private checkMKLRescode 
contains 
      
      !> Initialization of nlls_TR module. 
      subroutine nlls_TR_init(ounit,verbose)
      implicit none
      integer,optional,intent(in)         ::  ounit  !< IO unit number for outputs
      integer,optional,intent(in)         ::  verbose !< Verbosity level, \sa nllsTR_iw
      !!
      if (present(ounit)) nllsTR_ounit = ounit
      if (present(verbose)) nllsTR_iw = verbose
      end subroutine
      

      !> This subroutine solves the mnimization problem. Trust region algorithm from MKL library is used.
      !> \param objFx Objective function to minimize
      !> \param jacobiFx Function that calculates Jacobi matrix of objective function
      subroutine nlls_TR_solve(objFx,vX,config,r1,r2,info)
      implicit none
      include  "mkl_rci.fi"
      ! Formal parameters
      class(objectiveFunction),intent(inout)          :: objFx    !< objective function
      double precision,dimension(:),intent(inout)     :: vX       !< Design vector, dimension of vX must correspond to those in objFX
      type(nllsTRConf),intent(in)                     :: config   !< Configuration of nllsTR 
      double precision,intent(out)                    :: r1       !< Initial residual of the solution
      double precision,intent(out)                    :: r2       !< Final residual of the solution
      !> Return code, 0 if successful, < 0 if error, > 0 if failure/warning    
      integer,intent(out)                             :: info   
      !!!! Local variables
      integer                              :: n        !< Dimension of design vector
      integer                              :: m        !< Dimension of objective function vector
      integer(kind=8)   :: handle = 0
      integer           :: res 
      ! 
      double precision,allocatable,dimension(:)    :: vFval    ! would be of size m   
      double precision,allocatable,dimension(:,:)  :: mJacobi  ! m * n array
      double precision,allocatable,dimension(:)    :: vLW, vUP   ! would be of size    
      ! Variables for TR query
      integer                        :: iter
      integer                        :: stop_crit
      ! RCI loop control
      logical                        :: next_solve = .true.
      integer                        :: RCI_Req = 0
      ! Other variables
      integer                        :: ierr = 0, i = 0
      !---------------------------------------------------
            info = -1
            !! Check preconditions
            ! TODO 
            n = objFx%n_X_dim        ! Dimensionality of vector X (argument)
            m = objFx%m_F_dim        ! Dimensionality of objective function
            !
            !! Allocate memory
            allocate(vLW(n),vUP(n),vFval(m),mJacobi(m,n),stat=ierr)
            if (ierr /= 0) return
            ! Set square box constraints
            vLW =  config%lo_limit
            vUP =  config%up_limit
            ! Make sure that the initial guess is inside the constraints
            where (vX < vLW) vX = vLW
            where (vX > vUP) vX = vUP
            ! Initialize MKL solver
            handle = 0
            res = dtrnlspbc_init(handle, n, m, vX, vLW, vUP, config%eps,  config%iter1,  config%iter2,  config%init_step)
            ! Check result      
            if (checkMKLRescode(res,'initialization of TR nlls solver', nllsTR_ounit) /= 0) return
            !!! TESTING -->>
            write( nllsTR_ounit,*) 'TR initialized, handle: ', handle
            !!! TESTING <<--
            !! Evaluate function
            call objFx%objectiveFx(vX,vFval,info)      
            !! Calculate Jacobi matrix
            call objFx%jacobiMatrixFx(vX, mJacobi,info)
            if (info /= 0) then
                   write(nllsTR_ounit,*) 'Cannot calculate initial Jacobi matrix'
                  return
            endif
            !
            !!! TESTING -->>
            if (nllsTR_iw > 2) then
                  write(nllsTR_ounit,'(A)') 'Initial guess X -->' 
                  write(nllsTR_ounit,'(F12.8,1X)') (vX(i), i=1,n) 
                  write(nllsTR_ounit,'(A)') 'Initial guess X <--' 
                  write(nllsTR_ounit,'(A)') 'Objective function vF at initial guess -->' 
                  write(nllsTR_ounit,'(F12.8,1X)') (vFval(i), i=1,m)
                  write(nllsTR_ounit,'(A)') 'Objective function vF at initial guess <--' 
                  if (nllsTR_iw > 3) then
                        write( nllsTR_ounit,*)  'Jacobi matrix at initial guess -->'
                        call writeMatrix(mJacobi,  nllsTR_ounit)
                        write( nllsTR_ounit,*)  'Jacobi matrix at initial guess <--'
                  endif
            endif
            !!! TESTING <<--
            !
            !! RCI loop for 'solve'
            next_solve = .true.
            RCI_Req = 0
            do while (next_solve)
                  res = dtrnlspbc_solve(handle, vFval, mJacobi, RCI_Req)      
                  !!! TESTING -->>
                  ! write(*,*) 'res=',res
                  !!! TESTING <<--
                  if (res /= TR_SUCCESS) exit
                  ! RCI status
                  select case (RCI_Req)
                        !!-----------------------------------------------------------------------
                        case(-1)          ! Iteration count has been exceeded
                              next_solve = .false.      
                        !!-----------------------------------------------------------------------
                        case(-6:-2)       ! Epsilon has been reached
                              next_solve = .false.      
                        
                        !!-----------------------------------------------------------------------
                        case(0)           ! Successful
                              next_solve = .true.
                        !!-----------------------------------------------------------------------
                        case(1)           ! Recalculate function at vector x
                              if (nllsTR_iw > 2) write( nllsTR_ounit,*) 'Recalculation of vF'
                              call objFx%objectiveFx(vX,vFval,info)      
                              if (nllsTR_iw >= 1) write( nllsTR_ounit,'(A,F15.10,1X,A,F15.10)')   &
                                                 '||X|| = ',sqrt(dot_product(vX,vX)),           &
                                                 '||vF|| = ',sqrt(dot_product(vFval,vFval))
                               if (nllsTR_iw > 2) then
                                    write(nllsTR_ounit,'(A)') 'X' 
                                    write(nllsTR_ounit,'(F12.8,1X)') (vX(i), i=1,n) 
                                    write(nllsTR_ounit,'(A)') 'vF' 
                                    write(nllsTR_ounit,'(F12.8,1X)') (vFval(i), i=1,m)
                                    if (nllsTR_iw > 3) then
                                         write( nllsTR_ounit,*)  'Jacobi matrix  -->'
                                          call writeMatrix(mJacobi,  nllsTR_ounit)
                                          write( nllsTR_ounit,*)  'Jacobi matrix  <--'
                                    endif
                              endif
                             !! TESTING -->>
                              !write(*,*) 'X: ', vX  
                              !write(*,*) 'F(X): ', vFval
                              !! TESTING <<--
                        !!-----------------------------------------------------------------------
                        case(2)           ! Recalculate Jacobian 
                              if (.not.(config%constJacobi)) then 
                                    if (nllsTR_iw > 2) write( nllsTR_ounit,*) 'Recalculation of Jacobi matrix'
                                    call objFx%jacobiMatrixFx(vX, mJacobi,info)
                                    if (info /= 0) write(nllsTR_ounit,*) 'Cannot recalculate Jacobi matrix'
                                    !
                                    !write(*,*) 'Jacobi matrix: ', mJacobi 
                                    !write( nllsTR_ounit,*)  'Jacobi matrix -->'
                                    !call writeMatrix(mJacobi,  nllsTR_ounit)
                                    !write( nllsTR_ounit,*)  'Jacobi matrix <--'
                                    !call flush( nllsTR_ounit)
                              endif
                        !!-----------------------------------------------------------------------
                        case default      ! Unknown RCI, it should never happen!! 
                              write( nllsTR_ounit,*) 'Error: unknown RCI control code!!!'
                              exit
                  end select

            end do
            if (RCI_Req == -1) then
                   ! RCI loop finished due to iteration count, issue a warning.
                   info = 1
            else
                  info = 0
            endif
            ! Query solution info
            res = dtrnlspbc_get(handle, iter, stop_crit, r1, r2)
            if (nllsTR_iw > 0) then
                  write( nllsTR_ounit,'(A,1X,I2)') 'Stop criterion: ', stop_crit
                  write( nllsTR_ounit,'(A,1X,I5,2(1X,A,1X,F12.8))') 'step ',iter, 'R0=', r1, 'R1=',r2 
                  write( nllsTR_ounit,'(A)') 'X = '
                  write( nllsTR_ounit,'(F12.6,1X)') (vX(i), i=1,n)
            endif
            ! Release MKL resources
            res = dtrnlspbc_delete(handle)
            if (res /= TR_SUCCESS) then
                  write( nllsTR_ounit,*) 'dtrnlspbc_delete failed, res=',res 
            endif
            call mkl_free_buffers()
            ! Deallocate temporary arrays
            deallocate(vLW,vUP,vFval,mJacobi,stat=ierr)
            !
      end subroutine            



      !> Calculation of Jacobi matrix by means of central difference method.
      !>
      !> This subroutine uses djacobi_solve RCI subroutine from MKL.
      subroutine JacobiObjFx_djacobi(this,vX, mJacobi,info)
      implicit none
      include  "mkl_rci.fi"
      class(FDJacobiObjFunction)                      :: this
      double precision,dimension(:),intent(inout)     :: vX       !< Dimension must be: [n_X_dim]
      double precision,dimension(:,:),intent(inout)   :: mJacobi  !< Dimension must be: [m_F_dim x n_X_dim]
      integer,intent(out)                             :: info

      integer     :: res, memstat
      integer(kind=8)   :: handle = 0
      integer     :: RCI_Req
      logical     :: next_solve
      ! Temporary arrays f1 & f2 which contains f1 = f(x+eps) | f2 = f(x-eps)
      double precision,allocatable,dimension(:)       :: f1, f2
      !
      info = 1
      ! Check dimensions, return error if mismatch is detected.
      if (  (size(mJacobi,dim=1) /= this%m_F_dim) .or.      &
            (size(mJacobi,dim=2) /= this%n_X_dim) .or.      &
            (size(vX) /= this%n_X_dim)                      &
         ) return
      ! Allocate temporary arrays
      allocate(f1(this%m_F_dim), f2(this%m_F_dim), stat=memstat)            
      if (memstat /= 0) return
      !
      handle = 0
      !! TESTING -->>
      write(*,*) 'jacobi_eps=', this%jacobi_eps
      !! TESTING <<--
      res =  djacobi_init(handle, this%n_X_dim, this%m_F_dim, vX, mJacobi, this%jacobi_eps)
      ! detect error conditions
      if (checkMKLRescode(res,'recalculation of Jacobi matrix', nllsTR_ounit) /= 0) then
            info = 1
            return
      endif
      !
      ! Enter RCI loop
      info = 2
      next_solve = .true.
      RCI_Req = 0
      do while (next_solve)
            res = djacobi_solve(handle, f1, f2, RCI_Req)      
            !! TESTING -->>
            ! write(*,*) 'RCI_Req = ',RCI_Req, ' res = ', res, ' success = ', res == TR_SUCCESS
            !! TESTING <<--
            if (res /= TR_SUCCESS) exit
            ! RCI status
            select case (RCI_Req)
                  !!-----------------------------------------------------------------------
                  case(1)       
                        call this%objectiveFx(vX,f1,info)      
                  !!-----------------------------------------------------------------------
                  case(2)       
                        call this%objectiveFx(vX,f2,info)      
                  !!-----------------------------------------------------------------------
                  case(0)           ! Successful
                        info = 0
                        next_solve = .false.
                  case default           ! Unknown error conditionn
                        info = 3
                        exit
            end select
      enddo
      ! Free temporary arrays
      deallocate(f1,f2)
      ! Finalize Jacobi solver, release resources
      res = djacobi_delete(handle)
      if (checkMKLRescode(res,'recalculation of Jacobi matrix', nllsTR_ounit) /= 0) then
            info = 1
      else
            info = 0
      endif
      end subroutine


      !> The subroutine will calculate Jacobi matrix by means of finite
      !> difference method. The module parameter nllsTR_jacobi_eps is used.
!      subroutine jacobiObjFxFD(objFx, n, m, vX, mJacobi,info)
!      implicit none
!      include  "mkl_rci.fi"
!      ! Formal paramete nllsTR_init_step
!      procedure(objectiveFx)                          :: objFx
!      integer,intent(in)                              :: m, n
!      double precision,dimension(n),intent(in)        :: vX
!      double precision,dimension(m,n),intent(out)     :: mJacobi
!      integer,intent(out)                             :: info
!      ! Local variables
!      integer     :: res
!      !!
!      info = 0 
!      res = djacobi(objfx, n, m, mJacobi, vX, nllsTR_jacobi_eps) 
!      if (checkMKLRescode(res,'recalculation of Jacobi matrix', nllsTR_ounit) /= 0) info = 1
!      end subroutine


!----------------------------------------------------------------------------------------------------------------------------------
! Private components
!----------------------------------------------------------------------------------------------------------------------------------

      !> \internal
      !> Performs a check of TR MKL specific return values. If the value
      !> indicates an error conditions, the function will write an error message
      !> containing 'decrypted' description of error.
      integer function checkMKLRescode(res, leadmsg,  ounit)
      implicit none
      include  "mkl_rci.fi"
      integer,intent(in)            :: res
      character(len=*),intent(in)   :: leadmsg
      integer,intent(in)            :: ounit
      !!
      character(len=20)             :: errname
      !!      
            checkMKLRescode = 0
            if (res /= TR_SUCCESS) then
                  checkMKLRescode = -1
                  select case (res)
                        case(TR_INVALID_OPTION)  
                                    errname = 'TR_INVALID_OPTION'                  
                        case(TR_OUT_OF_MEMORY)
                                    errname = 'TR_OUT_OF_MEMORY'
                        case default
                                    errname = 'Unknown'
                  end select
                  write( ounit,9900) leadmsg, errname 
            endif
      9900 format(A,1X,'failed, reason:',1X,A)
      end function

      subroutine writeMatrix(A, ounit)
      implicit none
      double precision,dimension(:,:),intent(in) :: A
      integer,intent(in)                         ::  ounit
      !
      integer :: m,i,j
      m = size(A,dim=2)
      do i=1,size(A,dim=1)
            !write( ounit,'(F10.6,1X)') (A(i,j), j=1,m)
            write( ounit,'(<m>(F10.6,1X))') A(i,:)
      end do
      end subroutine


end module

