#include "criMacros.fpp"

!> In-memory cache/table of the recent results from the multi-level model.
module dmcResultTable
use utils
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

        procedure,pass(this)    :: store

        procedure,pass(this)    :: load

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
    real(DP),dimension(alamEval_vSD_dim),intent(in) :: A
    real(DP),dimension(alamEval_vSD_dim),intent(in) :: SonA
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
    real(DP),dimension(alamEval_vSD_dim),intent(in) :: S
    real(DP),dimension(alamEval_vSD_dim),intent(out):: A
    real(DP),intent(in),optional                    :: max_angle !< Threshold angle (in radians)
    !
    integer :: npoints, i, min_idx_a(1), min_idx
    equivalence(min_idx_a(1), min_idx)
    real(DP),dimension(:),allocatable :: angles
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


    !> Store the values in file fpath
    integer function store(this, fpath) result(info)
    class(ResultTable),intent(inout)    :: this
    character(len=*),intent(in)         :: fpath
    !
    integer :: iounit, ierr, i

        if (size(this%table) > this%saved_session_idx) then
            open(newunit=iounit, file=fpath, position='APPEND', action='WRITE',&
                 status='UNKNOWN', form='UNFORMATTED', iostat=ierr)
            RETURN_IF_WITH(ierr /= 0, info=VEF_ERROR)
            do i = this%saved_session_idx + 1, size(this%table)
                write(iounit, iostat=ierr) this%table%values(i)
                if (ierr /= 0) exit
            enddo
            if (ierr /= 0) then
                ! clear the cache
                close(iounit, status='DELETE', iostat=ierr)
                this%saved_session_idx = 0
            else
                this%saved_session_idx = this%saved_session_idx + i - 1
                close(iounit, iostat=ierr)
            endif
        endif
        info = VEF_OK
    !
    end function


    !> Load the values from file fpath. The file may or may not exist.
    integer function load(this, fpath) result(info)
    class(ResultTable),intent(inout)    :: this
    character(len=*),intent(in)         :: fpath
    !
    integer :: iounit, ierr
    type(ResultTableRecord) :: tmp
    !
        info = VEF_OK
        open(newunit=iounit, file=fpath, status='OLD', &
             form='UNFORMATTED', iostat=ierr)
        if (ierr == 0) then
            ! the file exists, load data from it
            do while ((ierr == 0) .or. (info /= VEF_OK))
                read(iounit, iostat=ierr) tmp
                if (ierr == 0) then
                    info = xVector_push(this%table, tmp)
                else
                    exit
                endif
            enddo
            ! negative ierr on end-of-file or end-of-record; positive on error
            if (ierr > 0 .or. info /= VEF_OK) then
                info = VEF_ERROR
            else
                this%saved_session_idx = size(this%table)
                info = VEF_OK
            endif
            close(iounit)
        endif
    end function


end module
