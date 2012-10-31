module range
private

      type :: rangeDouble
      private
            double precision  :: rbegin = 0.D0
            double precision  :: rend = 0.D0
            double precision  :: rstep = 0.D0
            logical           :: rlast = .true.
      contains
            procedure,pass(r) :: next => next_rangeDouble
      end type

      interface rangeDouble
            module procedure rangeDouble_init
      end interface

      interface next
            module procedure next_rangeDouble
      end interface
      
public :: rangeDouble, next
      
contains


      elemental function rangeDouble_init(rbegin,rend,rstep,intervals,last) result(res)
      implicit none
      type(rangeDouble)                         :: res
      double precision,intent(in)               :: rbegin
      double precision,intent(in)               :: rend
      double precision,intent(in),optional      :: rstep
      integer,intent(in),optional               :: intervals
      logical,intent(in),optional               :: last
      !
            res%rbegin = rbegin
            res%rend = rend
            if (present(rstep)) then
                  res%rstep = rstep
            else
                  if (present(intervals)) then
                        res%rstep = merge((rend-rbegin)/dble(intervals),rend+rend,(intervals > 0))
                  else
                        res%rstep = rend
                  endif
            endif
            if (present(last)) res%rlast = last
      !
      end function

      
      logical function next_rangeDouble(r,value) result(next)
      implicit none
      class(rangeDouble),intent(inout)    :: r
      double precision,intent(inout)      :: value
      !
      double precision :: tmp
      !
            next = .false.
            ! The step cannot be non-advancing
            if (abs(r%rstep) < epsilon(0.D0)) return
            !
            if (r%rstep > 0.D0) then
                  next = merge((r%rbegin <= r%rend),(r%rbegin < r%rend),r%rlast)
            else
                  next = merge((r%rbegin >= r%rend),(r%rbegin > r%rend),r%rlast)
            endif
            if (next) then
                  ! Calculate the associated value
                  value = r%rbegin
                  r%rbegin = r%rbegin + r%rstep
                  if (r%rstep > 0.D0) then
                        if ((r%rbegin > r%rend) .and. (r%rlast)) r%rbegin = r%rend
                  else
                        if ((r%rbegin < r%rend) .and. (r%rlast)) r%rbegin = r%rend
                  endif
            endif
      !
      end function
      
      
end module