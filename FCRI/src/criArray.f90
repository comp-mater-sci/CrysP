#include "criStdDefs.fpp"

!> Various high-level operations on arrays
module criArray
use criErrcodes
implicit none

contains

    !> Symmetry folding of arrays
    !>
    !> The algorithm implements folding of arrays. It imposes n-fold mirror symmetry.
    !> Folding of ragged arrays is not allowed, thus the size of the array
    !> must satisfy that N % (2**nfolds) == 0, where N is the size of the array
    !> and `%` denotes modulo operator.
    !>
    !> Example:
    !> The array X = [A B] to be folded once. The result is
    !> Y = [(A+) + (B-)] / 2, where the suffix `+` denotes the order
    !> of elements as in the original array, and `-` denotes the reverse order.
    !> The array X [A B C D] to be folded twice. The result is
    !> Y = [(A+) + (B-) + (C+) + (D-)] / 4
    subroutine fold_array(array, nfolds, outarray, info)
    double precision, dimension(:),intent(in)   :: array
    integer,intent(in)                          :: nfolds
    double precision, dimension(:),intent(out)  :: outarray
    integer,intent(out) :: info
    !
    integer :: i, n, nout, nf, istart, iend, istep, blockstart
    !
        ! check pre-requisites
        n = size(array)
        nf = 2**nfolds
        nout = n / nf
        info = criErr_BadArgs
        if ((n < 2) .or. (size(outarray) /= nout) .or. (modulo(n, nf) /= 0)) return
        !
        ! Initialize the output array
        outarray = 0.D0
        !
        ! Let's distribute the input matrix over nf blocks of size nout
        ! The folding takes the element order in the odd blocks
        ! [block_begin:block_end:1]
        ! and in the even blocks: [block_end:block_begin:-1]
        blockstart = 1
        do i = 1, nf
            if (modulo(i,2) == 1) then
                istart = blockstart
                iend = blockstart + nout - 1
                istep = 1
            else
                istart = blockstart + nout - 1
                iend = blockstart
                istep = -1
            endif
            !_ASSERT(abs(istart - iend) == nout - 1)
            !_ASSERT(iend <= n .and. istart <= n)
            outarray = outarray + array(istart:iend:istep)
            ! move to another block
            blockstart = blockstart + nout
        enddo
        ! Average over the folded blocks
        outarray = outarray / dble(nf)
        info = criSuccess
    !
    end subroutine

end module
