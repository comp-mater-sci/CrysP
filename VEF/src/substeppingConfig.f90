!> Configuration of substepping schemes
module dmcSubsteppingConfig
use criRange
use altay_definitions
use commonConfig
implicit none

    !> Base class for configuration related to substepping
    type :: SubsteppingConfig
    contains
        procedure,pass(this) :: readConfig => SubsteppingConfig_readConfig
    end type


    !> Configuration of substepping schemes that impose the size of the step
    type,extends(SubsteppingConfig) :: FixedSubsteppingConfig
        !> Range of scalars that define substepping.
        !>
        !> The range must run from 0.0 till 1.0. So, It must include at least
        !> two values: [0., 1.]
        class(range_type),pointer           :: ptr_range => null()
    contains
        procedure,pass(this) :: readConfig => FixedSubsteppingConfig_readConfig
        procedure,pass(this) :: getNumberOfIncrements => FixedSubsteppingConfig_getNumberOfIncrements
    end type


    !> Constructors of FixedSubsteppingConfig
    interface FixedSubsteppingConfig
        module procedure FixedSubsteppingConfig_init_single, &
                         FixedSubsteppingConfig_init_nintervals, &
                         FixedSubsteppingConfig_init_intervals, &
                         FixedSubsteppingConfig_init_range
    end interface

contains

    !> Dummy for reading substepping config
    integer function SubsteppingConfig_readConfig(this, cnfunit) result(info)
    class(SubsteppingConfig),intent(inout)      :: this
    integer,intent(in)                          :: cnfunit
    !
        info = VEF_OK
    !
    end function


    !> Read fixed substepping configuration from file and construct a proper
    !> range ptr_range from user's input
    !>
    !> The file input follows convention: only the boundaries of the increments
    !> are given. Example: [0.1, 0.5, 0.9]. On the other hand, ptr_range
    !> must contain also 0.0 at the beginning and 1.0 at the end. So, the range
    !> from in the earlier example would be: [0.0, 0.1, 0.5, 0.9, 1.0]. This
    !> function ensures that a proper range is constructed from user's input.
    integer function  FixedSubsteppingConfig_readConfig(this, cnfunit) result(info)
    class(FixedSubsteppingConfig),intent(inout) :: this
    integer,intent(in)                          :: cnfunit
    !
    class(range_type),pointer :: tmp_range
    !
        tmp_range => rangeFromConfig(cnfunit, info)
        if (info /= VEF_OK) return
        !
        ! Check user input, make sure a proper range is constructed
        this%ptr_range => properSubsteppingRange(tmp_range, info)
        if (.not. associated(this%ptr_range)) info = VEF_BADVAL
        deallocate(tmp_range)
    !
    end function


    !> Create a FixedSubsteppingConfig that defines substepping with n_increments
    pure function FixedSubsteppingConfig_init_nintervals(n_increments) result(this)
    type(FixedSubsteppingConfig) :: this
    integer,intent(in)  :: n_increments !< Number of increments
    !
        allocate(this%ptr_range, &
                 source=uniformRange(0.D0, 1.D0, npoints=n_increments, endpoint=.true.))
    !
    end function


    !> Create a FixedSubsteppingConfig from an array of specified increments
    pure function FixedSubsteppingConfig_init_intervals(increments) result(this)
    type(FixedSubsteppingConfig) :: this
    double precision,dimension(:),intent(in)  :: increments
    !
        allocate(this%ptr_range, source=discreteRange(increments))
    !
    end function


    !> Create a FixedSubsteppingConfig from a range
    function FixedSubsteppingConfig_init_range(range) result(this)
    type(FixedSubsteppingConfig) :: this
    class(range_type),intent(in) :: range
    !
        allocate(this%ptr_range, source=range)
    !
    end function


    !> Create a FixedSubsteppingConfig that defines one increment.
    function FixedSubsteppingConfig_init_single() result(this)
    type(FixedSubsteppingConfig) :: this
    !
        allocate(this%ptr_range, source=DiscreteRange([0.D0, 1.D0]))
    !
    end function


    !>
    integer function FixedSubsteppingConfig_getNumberOfIncrements(this) result(n)
    class(FixedSubsteppingConfig),intent(in) :: this
    !
        n = 0
        if (associated(this%ptr_range)) n = this%ptr_range%size() - 1
        if (n < 0) n = 0
    !
    end function


    function properSubsteppingRange(range, info) result(inst)
    class(range_type),pointer       :: inst
    class(range_type),intent(inout) :: range
    integer,intent(out)             :: info
    !
    double precision, dimension(:),allocatable :: tmp_points
    integer :: i, n_points, ierr
    double precision :: value
    !
        info = VEF_BADVAL
        n_points = range%size()
        if (n_points < 1) return
        allocate(tmp_points(n_points + 2)) ! + 2 is to make space for leading 0. and trailing 1.
        tmp_points(1) = 0.D0
        i = 2 ! Start filling from 2nd element
        do while (range%next(value))
            if (value < 0.D0 .or. value > 1.D0) return
            if (value == 0.D0 .or. value == 1.D0) cycle
            tmp_points(i) = value
            i = i + 1
        enddo
        tmp_points(i) = 1.D0 ! Fill the last element with 1.
        !
        ! construct new range that conforms with the internal representation
        allocate(inst, source=DiscreteRange(tmp_points(1:i)),stat=ierr)
        if (ierr == 0) info = VEF_OK
    !
    end function

end module
