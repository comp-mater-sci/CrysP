#include "criMacros.fpp"

!> In-memory cache/table of the recent results from the multi-level model.
module dmcResultTable
    use utils
    use alamYLPConstants

    implicit none
    private

    type:: ResultTableRecord
        real(DP), dimension(alamEval_vSD_dim):: vA = 0.D0
        real(DP), dimension(alamEval_vSD_dim):: vSonA = 0.D0
    end type

    type:: ResultTable
        private
        type(ResultTableRecord), dimension(:), allocatable:: table
        integer:: n_records = 0
    contains
        procedure, pass(this)    :: put
        procedure, pass(this)    :: get
    end type

    public:: ResultTable

contains

    !> Add result to the database
    subroutine put(this, A, SonA)
        class(ResultTable), intent(inout)   :: this
        real(DP), dimension(alamEval_vSD_dim), intent(in):: A
        real(DP), dimension(alamEval_vSD_dim), intent(in):: SonA
        type(ResultTableRecord), dimension(:), allocatable:: buffer    

        if (allocated(this%table)) then
            if (this%n_records == size(this%table)) then
                allocate(buffer(this%n_records*2))
                buffer(1:this%n_records) = this%table
                call move_alloc(buffer, this%table)
            end if
        else
            allocate(this%table(8))
        end if

        this%n_records = this%n_records+1
        this%table(this%n_records) = ResultTableRecord(A, sonA)
    end subroutine

    !> Find item in the database that has the smallest angle between
    !> S and item%vSonA, optionally restricting the choice to acceptable angles smaller than max_angle.
    !> Unless VEF_OK is returned, the argument A is undefined.
    !> \return VEF_OK on success, VEF_FAIL if no item satisfies the requirement
    integer function get(this, S, A, max_angle) result(info)
        class(ResultTable), intent(inout)                 :: this
        real(DP), dimension(alamEval_vSD_dim), intent(in)  :: S
        real(DP), dimension(alamEval_vSD_dim), intent(out):: A
        real(DP), intent(in), optional                     :: max_angle !< Threshold angle (in radians)
        integer:: i, min_idx_a(1), min_idx
        equivalence(min_idx_a(1), min_idx)
        real(DP), dimension(:), allocatable:: angles
    
        info = VEF_FAIL
        if (this%n_records > 0) then
            allocate(angles(this%n_records))
            forall (i = 1:this%n_records) angles(i) = vec_angle(S, this%table(i)%vSonA)
            min_idx_a = minloc(angles)
            if (present(max_angle)) then
                CHOOSE(info, angles(min_idx) > max_angle, VEF_FAIL, VEF_OK)
            else
                info = VEF_OK
            endif
            if (info == VEF_OK) A = this%table(min_idx)%vA
        endif
    end function
end module
