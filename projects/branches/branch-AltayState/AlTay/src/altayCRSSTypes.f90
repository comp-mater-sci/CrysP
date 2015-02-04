module altayCRSSTypes
use criErrcodes
implicit none

    ! TODO: these constants may deserve a better place to live.
    integer,parameter :: altayMaterial_fcc12 = 1, &
                         altayMaterial_bcc24 = 2, &
                         altayMaterial_bcc48 = 3, &
                         altayMaterial_user = 99

    !> Number of slip directions per slip system. It reflect that there are
    !> "positive" and "negative" slips on a given slip system
    integer,parameter   :: altayCRSSTypes_n_slip_dirs = 2

    !> Representation of Critical Resolved Shear Stresses
    type :: CRSSData
        
        !> Array that contains the CRSS of a crystal. It is defined
        !> by n_slip_dirs (number of slip directions) and number of slip systems.
        !>
        !> The shape of the array is [n_slip_dirs, n_slip_systems]
        !> The first index is for direction of slip system: positive and negative 
        !> (in that order).
        !> The second index is sequence number of pre-defined deformation systems
        !> (either slips or twinnings).
        double precision,dimension(:,:),allocatable     :: crss
        
    end type
    
    !> Initialization procedure of a CRSSData object
    interface CRSSData
        module procedure CRSSData_init
    end interface
    
contains
    
    elemental subroutine CRSSData_init(this, nsystems, info)
    implicit none
    type(CRSSData),intent(out)  :: this
    integer,intent(in)          :: nsystems
    integer,intent(out)         :: info
    !
    integer :: memerr
    !
        info = criErr_BadArgs
        if (nsystems > 0) then
            info  = criErr_MemAlloc
            allocate(this%crss(altayCRSSTypes_n_slip_dirs , nsystems), &
                     stat=memerr, source=0.D0)
            if (memerr == 0) info  = criSuccess
        endif
    !   
    end subroutine
    
    elemental integer function CRSSData_size(this) result(n)
    implicit none
    type(CRSSData),intent(in)  :: this
    !
        n = 0
        if (allocated(this%crss)) n = size(this%crss)
    !
    end function
    
    
end module
