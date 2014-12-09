!
! $Id$
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!>    \author     Jerzy Gawad 
!>    Email:      Jerzy.Gawad@cs.kuleuven.be
!>
!>    Organization: Katholieke Universiteit Leuven
!>    Organization unit: Dept.Comp.Sci., TWR Group
!>    
!>    \date Date of initial release: 2012-04-02
!>    $Revision$
!>    $Date$
!>    History of modifications: (see SVN log).
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! 
! 
      
!> Data type for objective functions.
!>
module objectiveFx
implicit none

      !> Solution at given point. It consists of: 1) the point, 2) function value, and 3) Jacobi matrix.
      type :: SolutionPoint
            !> Dimensionality of vector X (argument)
            integer                                         :: n_X_dim = 0
            
            !> Dimensionality of objective function
            integer                                         :: m_F_dim = 0
            
            double precision,dimension(:),allocatable       :: vX       !< The point
            double precision,dimension(:),allocatable       :: vF       !< Function at vX
            double precision,dimension(:,:),allocatable     :: mJ       !< Jacobi matrix at vX, [m_F_dim x n_X_dim]
            
      contains
            !> Constructor: initialization guided by the dimensions n_X_dim and m_F_dim 
            procedure,pass(this)       :: init => SolutionPoint_Init
            
            !> Constructor: copy from other SolutionPoint object
            procedure,pass(this)       :: copy => SolutionPoint_Copy
            
            !> Destructor: state of the object is changed to uninitialized.
            procedure,pass(this)       :: finalize => SolutionPoint_Finalize
            
            !> Generic name for constructors
            generic,public :: construct => init, copy

      end type
 
      
      !> Abstract data type for objective functions. 
      !>
      !> It is assumed that every objective function contains a state variable that
      !> represents the value of the function and its Jacobian.
      type,abstract :: objectiveFunction
            
            !> State variable
            type(SolutionPoint)                               :: state

            
      contains
            !> Initialization function
            procedure,pass(this)                 :: initFx => objectiveFunction_initFx
            
            !>@{ \name Stateful interface
            
            !> Evaluation of objective function vector.
            !>
            !> See remarks in IF_objectiveFx_stateful for a guidance how to implement it.
            procedure(IF_objectiveFx_stateful),deferred,pass(this)     :: objectiveEval

            !> Evaluation of Jacobi matrix for the objective function.
            !>
            !> See remarks in IF_JacobiObjFx_stateful for a guidance how to implement it.
            procedure(IF_JacobiObjFx_stateful),deferred,pass(this)     :: jacobiMatrixEval
            
            !>@}
            
            procedure,pass(this)                            :: getProblemSize
            procedure,pass(this)                            :: getXSize
            procedure,pass(this)                            :: getFSize

            
      end type

      !> Named constants for trackableObjFunc 
      integer,parameter       :: TOF_None = 0, TOF_Function = 1, TOF_Jacobian = 2, TOF_All = 4
      
      !> Objective function with tacking of the last evaluations.
      !>
      !> vX corresponding to the last evaluation of the function is stored in state%vX 
      !> (member of the parent class)
      !> vX that corresponds to the last evaluation of the Jacobian is stored in vXofJacobi
      type,abstract,extends(objectiveFunction) :: trackableObjFunc
            
            integer                                         :: tracking_level = TOF_None
            
            double precision,dimension(:),allocatable       :: vXofJacobi
            
      contains

            procedure :: objectiveEval => trackableObjFunc_objectiveEval
            procedure :: jacobiMatrixEval => trackableObjFunc_jacobiMatrixEval

            procedure,pass(this) :: track => trackableObjFunc_track
            
      end type
      

      abstract interface 
            !> Abstract interface for initalization of an instance of objectiveFunction object. 
            subroutine IF_initFx(this,n_X_dim,m_F_dim,info)
                  import  ::  objectiveFunction
                  class(objectiveFunction),intent(inout)      :: this         !< Instance of the object.
                  integer,intent(in)                          :: n_X_dim      !< Requested dimensionality of the function argument.
                  integer,intent(in)                          :: m_F_dim      !< Requested dimensionality of the function value.
                  integer,intent(out)                         :: info   !< Set to 0 on success
            end subroutine

            !> Abstract interface for stateful-style objective function calculations.
            !>
            !> The function should update the vF component of state member (\sa SolutionPoint)
            !> \note It is user's responsibility to provide a function that complies with this interface.
            !> \note This function must not modify any component of SolutionPoint except for vX and vF.
            subroutine IF_objectiveFx_stateful(this, vX, info)
                  import  ::  objectiveFunction
                  class(objectiveFunction),intent(inout)      :: this   !< Instance of the object.
                  double precision,dimension(:),intent(in)    :: vX     !< Dimension must be: [n_X_dim]
                  integer,intent(out)                         :: info   !< Set to 0 on success
            end subroutine

            !> Abstract interface for a function that calculates Jacobi matrix in a stateful-style.
            !>
            !> The function should update the mF component of state member (\sa SolutionPoint).
            !> \note It is user's responsibility to provide a function that complies with this interface.
            !> \note This function must not modify any component of SolutionPoint except for vX and mJ.
            subroutine IF_JacobiObjFx_stateful(this, vX, info)
                  import :: objectiveFunction
                  class(objectiveFunction),intent(inout)          :: this     !< Instance of the object.
                  double precision,dimension(:),intent(in)        :: vX       !< Dimension must be: [n_X_dim]
                  integer,intent(out)                             :: info     !< Set to 0 on success
            end subroutine
            
      end interface
      
      

contains
      
            !
            ! Methods of SolutionPoint
            !

            integer function SolutionPoint_Init(this, n_X_dim, m_F_dim) result(info)
            implicit none
            class(SolutionPoint),intent(out)    :: this
            integer,intent(in)                  :: n_X_dim
            integer,intent(in)                  :: m_F_dim
            integer :: memstat
            !
                  info = -1
                  if ((n_X_dim <= 0) .or. (m_F_dim <= 0)) return
                  this%n_X_dim = n_X_dim
                  this%m_F_dim = m_F_dim
                  allocate(this%vX(n_X_dim),this%vF(m_F_dim),this%mJ(m_F_dim,n_X_dim),stat=memstat)
                  if (memstat == 0) info = 0
            end function
            

            integer function SolutionPoint_Copy(this,other) result(info)
            class(SolutionPoint),intent(out)    :: this
            class(SolutionPoint),intent(in)     :: other
            !      
                  info = SolutionPoint_Init(this,other%n_X_dim, other%m_F_dim)
                  if (info == 0) then
                        this%vX = other%vX
                        this%vF = other%vF
                        this%mJ = other%mJ
                  endif
            end function
            
            subroutine SolutionPoint_Finalize(this)
            implicit none
            class(SolutionPoint),intent(inout)    :: this
            !      
                  if (allocated(this%vX)) deallocate(this%vX)
                  if (allocated(this%vF)) deallocate(this%vF)
                  if (allocated(this%mJ)) deallocate(this%mJ)
                  this%n_X_dim = 0
                  this%m_F_dim = 0
            end subroutine

            
            !
            ! Methods of objectiveFunction
            !

            !> Initialization. It must be called by all derived classes.
            subroutine objectiveFunction_initFx(this,n_X_dim,m_F_dim,info)
            implicit none
            class(objectiveFunction),intent(inout)      :: this
            integer,intent(in)                          :: n_X_dim
            integer,intent(in)                          :: m_F_dim
            integer,intent(out)                         :: info
            !
                  info = -1
                  if (this%state%init(n_X_dim,m_F_dim) == 0) info = 0
                  this%state%vX = 0.D0
                  this%state%vF = 0.D0
                  this%state%mJ = 0.D0
            !
            end subroutine
            
            !> Returns rank-one two-elemental array containing:
            !> (1) dimension of variables X and (2) number of components in the function F.
            pure function getProblemSize(this) result(outval)
            implicit none
            class(objectiveFunction),intent(in)       :: this
            integer,dimension(2)                      :: outval
            !
                  outval = [ this%state%n_X_dim, this%state%m_F_dim ]
            !
            end function
            
            !> Returns dimension of variables X.
            pure function getXSize(this)
            implicit none
            class(objectiveFunction),intent(in)         :: this
            integer                                   :: getXSize
            !
                  getXSize = this%state%n_X_dim
            !
            end function
            
            !> Returns number of components in the objective function F.
            pure function getFSize(this)
            implicit none
            class(objectiveFunction),intent(in)         :: this
            integer                                   :: getFSize
            !
                  getFSize = this%state%m_F_dim
            !
            end function

            
            
            
            !
            ! Methods of trackableObjFunc
            !
            
            
            !> Basic implementation of trackableObjInterface. The method defers actual 
            !> calculations of the Jacobi matrix to the derived class. The implementation 
            !> of objectiveEval should call this method at the end of its execution.
            subroutine trackableObjFunc_objectiveEval(this, vX, info)
            implicit none
            class(trackableObjFunc)                    :: this
            double precision,dimension(:),intent(in)  :: vX       !< Dimension must be: [n_X_dim]
            integer,intent(out)                       :: info
            !
                  ! call this%objectiveFx(vX, info)
                  if (info == 0) call this%track(vX, TOF_Function, info)
            !
            end subroutine

            !> Basic implementation of trackableObjInterface. The method defers actual 
            !> calculations of the Jacobi matrix to the derived class. The implementation 
            !> of jacobiMatrixEval should call this method at the end of its execution.
            subroutine trackableObjFunc_jacobiMatrixEval(this, vX, info)
            implicit none
            class(trackableObjFunc)       :: this
            double precision,dimension(:),intent(in)  :: vX       !< Dimension must be: [n_X_dim]
            integer,intent(out)                       :: info
            !                  
                  ! call this%jacobiMatrixFx(vX, this%state%mJ, info)
                  if (info == 0) call this%track(vX, TOF_Jacobian, info)
            !
            end subroutine

            !> Tracking of the evaluations
            subroutine trackableObjFunc_track(this, vX, request, info)
            implicit none
            class(trackableObjFunc)       :: this
            double precision,dimension(:),intent(in)  :: vX       !< Dimension must be: [n_X_dim]
            integer,intent(in)                        :: request
            integer,intent(out)                       :: info
            !
                  info = 0
                  select case(this%tracking_level)
                  !
                  case(TOF_None) 
                        continue
                  !
                  case(TOF_Function) 
                        if (request == TOF_Function) this%state%vX = vX
                  !
                  case(TOF_Jacobian)
                        ! lhs-reallocation if needed
                        if (request == TOF_Jacobian) this%vXofJacobi = vX
                  !
                  case(TOF_All)
                        ! Handle action for two requests:
                        if (request == TOF_Function) this%state%vX = vX
                        ! lhs-reallocation if needed
                        if (request == TOF_Jacobian) this%vXofJacobi = vX
                  case default
                        info = -1
                  end select
            !
            end subroutine

            
end module