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
        !> as slip systems with a 'very large' crss in the negative direction.
        double precision,dimension(:,:),allocatable     :: crss
        
    end type
    
    !> \fixme: the "_" is added to the generic name as a work-around for this built error:
    !>     "error #8515: If generic name is the same as derived type name all
    !>      of the procedures in the interface block must be functions.   [CRSSDATA_INIT]"
    !> Initialization procedure of a CRSSData object
    interface CRSSData_
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
        if (allocated(this%crss)) n = size(this%crss,dim=CRSS_dim_seq_slip)
    !
    end function
    

    !> Calculates (plastic) work rate from  CRSSdata object and vector of deformation rates
    pure subroutine CRSSData_CalcWorkRate(this, crss, deformationrates, info)
    implicit none
    double precision, intent(out)            :: this
    type(CRSSData),intent(in)                :: crss
    double precision,intent(in),dimension(:) :: deformationrates
    integer,intent(out)                      :: info !< Exit code
    !
    integer :: n, i
    !
        this = 0.0D0
        info = criErr_BadDims
        n = CRSSData_size(crss)
        if (size(deformationrates) >= n) then
            info  = criErr_NumNaN
            !
            do i=1,n 
                if (deformationrates(i).GT.0.0) then !positive deformation rate
                    this = this + deformationrates(i) * crss%crss(CRSS_pos_dir_idx,i) 
                else                                 !negative or 0 deformation rate
                    this = this - deformationrates(i) * crss%crss(CRSS_neg_dir_idx,i) 
                endif
            end do
            !
            if (.not.isNaN(this)) info  = criSuccess
        endif
    !   
    end subroutine
    
end module
