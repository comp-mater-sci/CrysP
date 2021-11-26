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

    !> The dimension in CRSSData%crss for direction of slip systems, i.e. positive 
    !> and negative (in the order defined by CRSS_pos_dir_idx and CRSS_neg_dir_idx).
    integer,parameter   :: CRSS_dim_dir_slip = 1
    
    !> The dimension in CRSSData%crss for sequence number of slip systems
    integer,parameter   :: CRSS_dim_seq_slip = 2

    !> Number of slip directions per slip system. It reflect that there are
    !> "positive" and "negative" slips on a given slip system
    integer,parameter   :: CRSS_n_slip_dirs = 2

    !> Index of the "positive" slip in the CRSS_dim_dir dimension of CRSSData%crss
    integer,parameter   :: CRSS_pos_dir_idx = 1

    !> Index of the "negative" slip in the CRSS_dim_dir dimension of CRSSData%crss
    integer,parameter   :: CRSS_neg_dir_idx = 2

    !> Representation of Critical Resolved Shear Stresses
    type :: CRSSData
        
        !> Array that contains the CRSS of a crystal.
        !> The shape of the array is given by CRSS_n_slip_dirs (number of slip 
        !> directions) and the number of deformation systems  (either slips or 
        !> twinnings).
        !> Note: in current implementation, twinning systems are formally treated
        !> as slip systems. Deformation along the anti-twinning direction is prevented 
        !> through the statement marked "notePE20150428" in Pancake module.
        double precision,dimension(:,:),allocatable     :: crss
        
    end type

    
    !> Representation of shear rates of deformation (slip and twinning) systems
    type :: ShearRateData

        !> array containing the shear rates.
        !> The shape of the array is given by the number of
        !> deformation systems  (either slips or twinnings).
        double precision,dimension(:),allocatable     :: shearrate
        
    end type
    
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


    !> Initializes the ShearRateData object
    elemental subroutine ShearRateData_init(this, nsystems, info)
    implicit none
    type(ShearRateData),intent(out)  :: this
    integer,intent(in)               :: nsystems !< Number of deformation systems
    integer,intent(out)              :: info !< Exit code
    !
    integer :: memerr
    !
        info = criErr_BadArgs
        if (nsystems > 0) then
            info  = criErr_MemAlloc
            allocate(this%shearrate(nsystems), &
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
        if (allocated(this%crss)) n = size(this%crss,dim=CRSS_dim_seq_slip)
    !
    end function
    
    
    !> Provides the number of deformation systems included in
    !> the ShearRateData object.
    elemental integer function ShearRateData_size(this) result(n)
    implicit none
    type(ShearRateData),intent(in)  :: this
    !
        n = 0
        if (allocated(this%shearrate)) n = size(this%shearrate)
    !
    end function
    

    !> Calculates (plastic) work rate from  CRSSdata object and vector of deformation rates
    pure subroutine altayCRSSTypes_CalcWorkRate(workrate, crss, shearrate, info)
    implicit none
    double precision, intent(out)            :: workrate
    type(CRSSData),intent(in)                :: crss
    type(ShearRateData),intent(in)           :: shearrate
    integer,intent(out)                      :: info !< Exit code
    !
    integer :: n, i
    !
        workrate = 0.0D0
        info = criErr_BadDims
        n = CRSSData_size(crss)
        if (ShearRateData_size(shearrate) == n) then
            info  = criErr_NumNaN
            !
            associate (rate => shearrate%shearrate)
                do i=1,n 
                    if (rate(i) > 0.0D0) then !positive deformation rate
                        workrate = workrate + rate(i) * crss%crss(CRSS_pos_dir_idx,i) 
                    else                                 !negative or 0 deformation rate
                        workrate = workrate - rate(i) * crss%crss(CRSS_neg_dir_idx,i) 
                    endif
                end do
            end associate            
            !
            if (.not.isNaN(workrate)) info  = criSuccess
        endif
    !   
    end subroutine
    
end module
