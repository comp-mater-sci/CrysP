module fngRange
implicit none
private

      type :: rangeDouble
      private
            double precision  :: value = 0.D0
            
            double precision  :: endpoint = 0.D0
            
            !> Number of points to be processed.
            !>
            !> Possible values:
            !>     * npoints is zero: If with_endpoint is true, the value rend is returned. Otherwise, no point is returned
            !>     * npoints < 0: no point is returned.
            integer           :: npoints = 0
            
            double precision  :: step = 0.D0
            
            logical           :: with_endpoint = .true. 
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

      !> Constructor of rangeDouble object.
      elemental function rangeDouble_init(rbegin,rend,rstep,npoints,endpoint) result(res)
      implicit none
      type(rangeDouble)                         :: res
      double precision,intent(in)               :: rbegin
      double precision,intent(in)               :: rend
      double precision,intent(in),optional      :: rstep
      !> Number of points inside the range.
      !>
      !> This parameter is ignored if rstep is also provided.
      integer,intent(in),optional               :: npoints
      logical,intent(in),optional               :: endpoint
      !
            res%value = rbegin
            res%endpoint = rend
            if (present(rstep)) then
                  res%step = rstep
                  res%npoints = (merge(ceiling((rend-rbegin)/rstep),0,(abs(rstep) > epsilon(0.D0))))
            else
                  res%step = rend
                  if (present(npoints)) then
                        if (npoints > 0) then
                              res%npoints = npoints
                              res%step = (rend-rbegin)/dble(npoints)
                        endif
                  endif
            endif
            if (present(endpoint)) res%with_endpoint = endpoint
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
            ! Terminate if either no points are left.
            if  (r%npoints < 0) return
            ! There can be some points to be processed:
            if (r%npoints > 0) then
                  ! The regular points
                  value = r%value
                  r%value = r%value + r%step
                  next = .true.
            else
                  ! Consider the endpoint:
                  next = r%with_endpoint
                  if (next) value = r%endpoint
            endif
            r%npoints = r%npoints - 1
      !
      end function
      
      
end module