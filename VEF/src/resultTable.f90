#include "criMacros.fpp"

!> In-memory cache/table of the recent results from the multi-level model.
module dmcResultTable
use definitions
use criMathUtils
use alamYLPConstants
use dmcResultTableRecord
implicit none

    public :: ResultTable
    private

    type :: ResultTable

        integer,private                         :: saved_session_idx = 0

        type(xVector_ResultTableRecord),private :: table

    contains
        procedure,pass(this)    :: reserve

        procedure,pass(this)    :: put

        procedure,pass(this)    :: get
    end type


contains

    !> Reserve capacity slots in the table
    integer function reserve(this, capacity) result(info)
    class(ResultTable),intent(inout)    :: this
    integer,intent(in)                  :: capacity
    !
        call xVector_expand(this%table, capacity, info)
    !
    end function


    !> Add result to the database
    integer function put(this, A, SonA) result(info)
    class(ResultTable),intent(inout)   :: this
    double precision,dimension(alamEval_vSD_dim),intent(in) :: A
    double precision,dimension(alamEval_vSD_dim),intent(in) :: SonA
    !
        info = xVector_push(this%table, ResultTableRecord(A, SonA))
    !
    end function


    !> Find item in the database that has the smallest angle between
    !> S and item%vSonA, optionally restricting the choice to acceptable angles
    !> smaller than max_angle.
    !> Unless VEF_OK is returned, the argument A is undefined.
    !>
    !> \return VEF_OK on success, VEF_ERROR if no item satisfies the
    !> requirement
    integer function get(this, S, A, max_angle) result(info)
    class(ResultTable),intent(inout)   :: this
    double precision,dimension(alamEval_vSD_dim),intent(in) :: S
    double precision,dimension(alamEval_vSD_dim),intent(out):: A
    double precision,intent(in),optional                    :: max_angle !< Threshold angle (in radians)
    !
    integer :: npoints, i, min_idx_a(1), min_idx
    equivalence(min_idx_a(1), min_idx)
    double precision,dimension(:),allocatable :: angles
    !
        info = VEF_FAIL
        npoints = size(this%table)
        if (npoints > 0) then
            allocate(angles(npoints))
            do concurrent (i = 1:npoints)
                angles(i) = vec_angle(S, this%table%values(i)%vSonA)
            enddo
            min_idx_a = minloc(angles)
            if (present(max_angle)) then
                CHOOSE(info, angles(min_idx) > max_angle, VEF_FAIL, VEF_OK)
            else
                info = VEF_OK
            endif
            if (info == VEF_OK) A = this%table%values(min_idx)%vA
        endif
    !
    end function

end module
