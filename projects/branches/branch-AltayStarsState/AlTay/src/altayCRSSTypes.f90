!
! $Id$
!
!> Data structure for Critical Resolved Shear Stress (CRSS) and associated 
!> basic operations.
!>
!> Constants defined in this module are prefixed with `CRSS_`.
module altayCRSSTypes
use criErrcodes
implicit none


    !> Number of slip directions per slip system. It reflect that there are
    !> "positive" and "negative" slips on a given slip system
    integer,parameter   :: CRSS_n_slip_dirs = 2
    
    !> Index of the "positive" slip in the first dimension of CRSSData
    integer,parameter   :: CRSS_pos_dir_idx = 1

    !> Index of the "negative" slip in the first dimension of CRSSData
    integer,parameter   :: CRSS_neg_dir_idx = 2

    !> Representation of Critical Resolved Shear Stresses
    type :: CRSSData
        
        !> Array that contains the CRSS of a crystal. It is defined
        !> by CRSS_n_slip_dirs (number of slip directions) and the
        !> number of deformation systems  (either slips or twinnings).
        !>
        !> The shape of the array is [n_slip_dirs, n_slip_systems]
        !> The first dimension is for direction of slip system: positive and 
        !> negative (in that order).
        !> The second dimension is sequence number of pre-defined deformation
        !> systems.
        double precision,dimension(:,:),allocatable     :: crss
        
    end type
    
    !> Initialization procedure of a CRSSData object
    interface CRSSData
        module procedure CRSSData_init
    end interface
    
contains


    !> Initializes the CRSSData object
    elemental subroutine CRSSData_init(this, nsystems, info)
    implicit none
    type(CRSSData),intent(out)  :: this
    integer,intent(in)          :: nsystems !< Number of deformation systems
    integer,intent(out)         :: info !< Exit code
    !
    integer :: memerr
    !
        info = criErr_BadArgs
        if (nsystems > 0) then
            info  = criErr_MemAlloc
            allocate(this%crss(CRSS_n_slip_dirs , nsystems), &
                     stat=memerr, source=0.D0)
            if (memerr == 0) info  = criSuccess
        endif
    !   
    end subroutine


    !> Provides the number of deformation systems included in
    !> the CRSSData object.
    elemental integer function CRSSData_size(this) result(n)
    implicit none
    type(CRSSData),intent(in)  :: this
    !
        n = 0
        if (allocated(this%crss)) n = size(this%crss)
    !
    end function
    
    
end module
