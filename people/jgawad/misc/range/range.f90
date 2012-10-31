!> Range of floating point numbers.
!>
!> The module implements the datatype rangeDouble that represents an evenly stepped range (or an interval) of real numbers.
module fngRange
implicit none
private

      !> The type represents range (or interval) of [a,b] or [a,b), where a and b are double precision real numbers.
      !>
      !> The type provides method "next" that can be used for obtaining subsequent points belonging to
      !> the range.
      type :: rangeDouble
      private
            !> Current value. Left endpoint on the beginning.
            double precision  :: value = 0.D0
            
            !> The endpoint (right)
            double precision  :: endpoint = 0.D0
            
            !> Number of points to be processed.
            !>
            !> Possible values:
            !>     * npoints is zero: If with_endpoint is true, the value rend is returned. Otherwise, no point is returned
            !>     * npoints < 0: no point is returned.
            integer           :: npoints = 0
            
            double precision  :: step = 0.D0
            
            !> Flag: if .true. the right endpoint will be considered as included in the range.
            logical           :: with_endpoint = .true. 
            
      contains
      
            !> Function that provides next point belonging to the range. \sa next_rangeDouble
            !> Subsequent calls to this function give a sequence of points that are within the
            !> range.
            procedure,pass(r) :: next => next_rangeDouble
            
            !> The function provides number of points that remain within the range. \sa size_rangeDouble
            !>
            !> Note that the result of the function is not a constant. It actually provides 
            !  the number of calls to next function that shall end with .true..
            procedure,pass(r) :: size => size_rangeDouble
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
      !>
      !> Examples:  (notation for ranges: [begin:end:step]
      !>    * rangeDouble(2.D0, 5.D0,-1.D0)  is [2.:5.:-1], which is an empty range
      !>    * rangeDouble(0.D0, 0.D0, 1.D0 endpoint=.false.) is an empty range [0:0:1)
      !>    * rangeDouble(0.D0, 0.D0) is a single element range: {0.0}
      !>    * rangeDouble(2.D0, 5.D0, 1.D0) is a range consisting of {2.D0, 3.D0, 4.D0, 5.D0}
      !>    * rangeDouble(2.D0, 5.D0, 1.D0, endpoint=.false.) is a range comprising {2.D0, 3.D0, 4.D0}
      !>    * rangeDouble(1.D0, 3.D0, npoints=4, endpoint=.true.) consists of {1.D0,1.5D0,2.D0,2.5D0,3.D0}
      elemental function rangeDouble_init(rbegin,rend,rstep,npoints,endpoint) result(res)
      implicit none
      type(rangeDouble)                         :: res
      double precision,intent(in)               :: rbegin   !< Begin of the range
      double precision,intent(in)               :: rend     !< End of the range.
      double precision,intent(in),optional      :: rstep    !< Size of steps over the range. Default is 0.D0
      !> Number of points inside the range. Default is 0
      !>
      !> This parameter is ignored if rstep is also provided.
      integer,intent(in),optional               :: npoints
      !> Flag: if set .true. then the range will include the right endpoint.
      !> Default is .true.
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
                              res%step = (rend-rbegin)/dble(npoints)
                              res%npoints = merge(npoints,0,abs(res%step) > epsilon(0.D0))
                        endif
                  endif
            endif
            if (present(endpoint)) res%with_endpoint = endpoint
      !
      end function

      !> Provides a sequence of points belonging to the range.
      !>
      !> \returns .true. if a point belongs to the range. The corresponding value is returned
      !> in value.
      logical function next_rangeDouble(r,value) result(next)
      implicit none
      class(rangeDouble),intent(inout)    :: r
      !> The value at the point belonging to the interval. This is meaningful if and only if
      !> the function returnes .true.
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
      
      elemental integer function size_rangeDouble(r)    result(n)
      implicit none
      class(rangeDouble),intent(in)    :: r
      !
            n = 0
            if (r%npoints >= 0) n = r%npoints + merge(1,0,r%with_endpoint)
      !
      end function
      
end module