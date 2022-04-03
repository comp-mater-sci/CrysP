!
! $Id$
!
!>    \author Jerzy Gawad
!>    Email:  Jerzy.Gawad@cs.kuleuven.be
!>
!>    Organization: Katholieke Universiteit Leuven (KU Leuven)
!>    Organization unit: Dept.Comp.Sci., TWR Group
!>    \copyright KU Leuven
!>
!>    \date Date of first release: 2012-10-31
!>    $Revision$
!>    $Date$
!>
!>    History of modifications: (see svn log)
!>
!>    \file criRange.f90
!
#include "criStdDefs.fpp"
!
!
!
!> Range of floating point numbers.
!>
!> The module implements the datatype uniformRange that represents an evenly stepped range (or an interval) of real numbers.
!> \remark This module is not available in the builds with old compilers (such as Intel Fortran 11.1)
module criRange
implicit none
private

      type,abstract :: range_type
      contains
            !> Provides next value belonging to the range (purely virtual function)
            procedure(range_type_next),pass(r),deferred    :: next

            !> Provides number of points left within the range (purely virtual function)
            procedure(range_type_size),pass(r),deferred    :: size
      end type

      abstract interface
            logical function range_type_next(r,value)
                  import :: range_type
                  implicit none
                  class(range_type),intent(inout)    :: r
                  double precision,intent(inout)      :: value
            end function

            elemental integer function range_type_size(r)
                  import :: range_type
                  implicit none
                  class(range_type),intent(in)    :: r
            end function
      end interface

      !> The type represents range (or interval) of [a,b] or [a,b),
      !> where a and b are double precision real numbers.
      !>
      !> The type provides method "next" that can be used for obtaining subsequent
      ! evenly distributed points belonging to the range.
      type,extends(range_type) :: uniformRange
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

            !> Function that provides next point belonging to the range. \sa next_uniformRange
            !> Subsequent calls to this function give a sequence of points that are within the
            !> range.
            procedure,pass(r) :: next => uniformRange_next

            !> The function provides number of points that remain within the range. \sa size_uniformRange
            !>
            !> Note that the result of the function is not a constant. It actually provides
            !  the number of calls to next function that shall end with .true..
            procedure,pass(r) :: size => uniformRange_size
      end type

      !> Constructors of the uniformRange
      interface uniformRange
            module procedure uniformRange_init
      end interface



      !> The type biasedRange represents range (or interval) of [a,b] or [a,b) with points inside the range
      !> distributed according to geometrical progression.
      !>
      !> Note that there are some settings that simply degenerate the biasedRange to the same
      !> behavior as the uniformRange type has.
      type,extends(uniformRange) :: biasedRange
      private
            !> Ratio in the geometrical progression: a_{k} = q*a_{k-1}
            double precision  :: q = 1.D0
      contains
            procedure,pass(r) :: next => biasedRange_next
      end type

      !> Constructors of the biasedRange
      interface biasedRange
            module procedure biasedRange_init
      end interface




      !> The type multiBiasedRange provides easy way for stitching many biasedRange-s.
      type,extends(range_type) :: multiBiasedRange
      private
            type(biasedRange),dimension(:),allocatable ::   ranges
            integer     :: active
      contains
            procedure,pass(r) :: next => multiBiasedRange_next
            procedure,pass(r) :: size => multiBiasedRange_size
      end type

      !> Helper type for constructing multiBiasedRange objects.
      !>
      !> The type bias_t facilitates representing subsequent biased ranges that ends at some right endpoint.
      !> Because the beginning of the range is not provided in this type, it is assumed
      !> that the next range starts at the end of the previous one.
      type :: bias_t
            double precision  :: endpoint = 0.D0      !< Right endpoint of the range

            !> Ratio between the largest and the smallest step over the range.
            double precision  :: ratio = 1.D0

            integer           :: npoints = 1          !< Number of points inside the range
      end type

      !> Constructor for multiBiasedRange
      interface multiBiasedRange
            module procedure multiBiasedRange_init, doubleBiasedRange_init
      end interface

      !> Specialized constructor multiBiasedRange: it creates a double-biased range.
      !>
      !> The double-biased range is just a special case of multiBiased objects:
      !> half of the the double-biased range uses ratio "r", while the other half
      !> uses the reciprocal of "r". Hovever, it is possible to specify arbitrary
      !> ratio for the second part of the range.
      interface doubleBiasedRange
            module procedure doubleBiasedRange_init
      end interface

      !> discreteRange represents an arbitrary sequence of data points, accesible via the range_type
      !> interface, i.e. `next` and `size` methods.
      type,extends(range_type) :: discreteRange
      private
            !> Sequence of points
            double precision,dimension(:),allocatable :: sequence
            !> Index of the previously used point inside the sequence. Zero denotes "no point has been used".
            integer           :: lastpoint = 0
      contains
            procedure,pass(r) :: next => discreteRange_next
            procedure,pass(r) :: size => discreteRange_size
      end type

      interface discreteRange
            module procedure discreteRange_init
      end interface

!>@{ \name Public datatypes
public :: range_type, uniformRange, biasedRange, doubleBiasedRange, multiBiasedRange, discreteRange
public :: bias_t
!>@}

contains

      !> Constructor of uniformRange object.
      !>
      !> Examples:  (notation for ranges: [begin:end:step]
      !>    * uniformRange(2.D0, 5.D0,-1.D0)  is [2.:5.:-1], which is an empty range
      !>    * uniformRange(0.D0, 0.D0, 1.D0 endpoint=.false.) is an empty range [0:0:1)
      !>    * uniformRange(0.D0, 0.D0) is a single element range: {0.0}
      !>    * uniformRange(2.D0, 5.D0, 1.D0) is a range consisting of {2.D0, 3.D0, 4.D0, 5.D0}
      !>    * uniformRange(2.D0, 5.D0, 1.D0, endpoint=.false.) is a range comprising {2.D0, 3.D0, 4.D0}
      !>    * uniformRange(1.D0, 3.D0, npoints=4, endpoint=.true.) consists of {1.D0,1.5D0,2.D0,2.5D0,3.D0}
      elemental function uniformRange_init(rbegin,rend,rstep,npoints,endpoint) result(res)
      implicit none
      type(uniformRange)                         :: res
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
                  if (abs(rstep) > epsilon(0.D0)) then
                        res%npoints = ceiling((rend-rbegin)/rstep)
                  else
                        res%npoints = 0
                  endif
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
      logical function uniformRange_next(r,value) result(next)
      implicit none
      class(uniformRange),intent(inout)    :: r
      !> The value at the point belonging to the interval. This is meaningful if and only if
      !> the function returns .true.
      double precision,intent(inout)      :: value
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

      elemental integer function uniformRange_size(r)    result(n)
      implicit none
      class(uniformRange),intent(in)    :: r
      !
            n = 0
            if (r%npoints >= 0) n = r%npoints + merge(1,0,r%with_endpoint)
      !
      end function



      !
      !  Members of biasedRange
      !

      elemental function biasedRange_init(rbegin,rend,ratio,npoints,endpoint) result(res)
      implicit none
      type(biasedRange)             :: res
      double precision,intent(in)   :: rbegin !< Left endpoint of the range
      double precision,intent(in)   :: rend   !< Right endpoint of the range
      !> Ratio between the largest and the smallest step over the range.
      !>
      !> It must be:  either  (0.0 < ratio < 1.0)  or (ratio > 1.0). Choosing other value
      !> makes the range use evenly distributed steps.
      !> If (0.0 < ratio < 1.0) then the smallest steps are located at the begining of
      !> the range; otherwise (ratio > 1.0) the refined steps appear an the end of the range.
      double precision,intent(in)   :: ratio
      integer,intent(in)            :: npoints !< Number of points inside the range
      !> Flag: if set .true. then the range will include the right endpoint.
      !> Default is .true.
      logical,intent(in),optional   :: endpoint
      !
      double precision :: length
      !
            res%value = rbegin
            res%endpoint = rend
            length = rend - rbegin
            ! Calculate effective number of points
            res%npoints = merge(npoints, 0, (abs(length) > epsilon(0.D0)) .and. (npoints > 0) )
            ! Calculate parameters of the geometrical progress
            ! a_k = a_1 * q^{k-1} => (a_1 / a_k) = q^{1-k} <-- ratio
            ! q = (a_1 / a_k)^{1/(1-k)}
            ! Check if the parameters describe valid geometrical progression,
            ! otherwise fall back to the uniform steps
            if ( (res%npoints > 1) .and.  &
                 ( (ratio > 0.D0)  .and.  ( (ratio > 1.D0) .or.  (ratio < 1.D0) ) ) ) then
                  res%q = ratio**(1.D0/(1.D0 - dble(res%npoints)))
                  ! q /= 1, it is OK to calculate a_1:
                  res%step = length * (1.D0 - res%q) / (1.D0 - res%q**res%npoints)
            else
                  if (res%npoints /= 0) then
                        res%step = length / dble(res%npoints)
                  else
                        res%step = length
                  endif
            endif
            if (present(endpoint)) res%with_endpoint = endpoint
      !
      end function


      logical function  biasedRange_next(r,value) result(next)
      implicit none
      class(biasedRange),intent(inout)    :: r
      !> The value at the point belonging to the interval. This is meaningful if and only if
      !> the function returns .true.
      double precision,intent(inout)      :: value
      !
            next = uniformRange_next(r,value)
            r%step = r%step * r%q
      !
      end function


      !
      !  Members of multiBiasedRange
      !

      pure function multiBiasedRange_init(rbegin,biases,endpoint) result(res)
      implicit none
      type(multiBiasedRange)                    :: res
      double precision,intent(in)               :: rbegin   !< Leftmost endpoint of the range.
      type(bias_t),dimension(1:),intent(in)     :: biases   !< Array of biases.
      !> Flag: if set .true. then the range will include the right endpoint.
      !> Default is .true.
      logical,intent(in),optional               :: endpoint
      !
      double precision :: start
      integer :: i,nranges,ierr
      logical :: with_endpoint
      !
            nranges = size(biases)
            if (present(endpoint)) then
                  with_endpoint = endpoint
            else
                  with_endpoint = .true.
            endif
            if (nranges > 0) then
                  allocate(res%ranges(nranges),stat=ierr)
                  start = rbegin
                  do i = 1,nranges
                        res%ranges(i) = biasedRange_init(start,biases(i)%endpoint, &
                                                         biases(i)%ratio, &
                                                         biases(i)%npoints,&
                                                         endpoint=merge(with_endpoint,.false.,i == nranges))
                        start = biases(i)%endpoint
                  enddo
                  res%active = 1
            endif
      !
      end function

      !> Initialization function for double-biased range. The result of the function
      !> is actually an instance of multiBiasedRange with two biased ranges. By default,
      !> the first range has progression ratio "ratio", while the second has "1.0/ratio".
      elemental function doubleBiasedRange_init(rbegin,rend,ratio,npoints,endpoint,ratio2) result(res)
      implicit none
      type(multiBiasedRange)        :: res
      double precision,intent(in)   :: rbegin   !< Left endpoint of the range
      double precision,intent(in)   :: rend     !< Right endpoint of the range
      !> Ratio between the largest and the smallest interval in the first half of the range.
      !>
      !> It must be:  either  0 < ratio < 1.0  or ratio > 1.0
      double precision,intent(in)   :: ratio
      integer,intent(in)            :: npoints  !< Number of points in the range
      !> Flag: if set .true., the last call to "next" will return the right
      !> endpoint of the range.
      logical,intent(in),optional   :: endpoint
      !> Ratio between the largest and the smallest interval in the second half of the range.
      !>
      !> The value of ratio2 is subjected to the same limitations as "ratio".
      !> Default value: 1.D0 / ratio
      double precision,intent(in),optional   :: ratio2
      !
      double precision :: midpoint, r1,r2
      type(bias_t),dimension(2)  :: biases
      !
            r1 = merge(ratio,1.D0,ratio > 0.D0)
            if (present(ratio2)) then
                  r2 = merge(ratio2,1.D0,ratio2 > 0.D0)
            else
                  r2 = (1.D0 / r1)
            endif
            !
            midpoint = 0.5D0 * (rend - rbegin) + rbegin
            biases = [ bias_t(midpoint,r1,npoints / 2), bias_t(rend,r2, npoints / 2) ]
            !
            res = multiBiasedRange_init(rbegin,biases,endpoint)
      !
      end function


      logical function multiBiasedRange_next(r,value) result(next)
      implicit none
      class(multiBiasedRange),intent(inout)    :: r
      !> The value at the point belonging to the interval. This is meaningful if and only if
      !> the function returns .true.
      double precision,intent(inout)      :: value
      !
      integer :: nranges
      !
            next = .false.
            if (allocated(r%ranges)) then
                  nranges = size(r%ranges)
                  ! Check whether the "active" range has any point left,
                  ! otherwise activate the subsequent one that has some points left.
                  do while ( (r%active <= nranges) .and. (.not. next) )
                        next = r%ranges(r%active)%next(value)
                        if (.not.next) r%active = r%active + 1
                  enddo
            endif
      !
      end function


      elemental integer function multiBiasedRange_size(r)    result(n)
      implicit none
      class(multiBiasedRange),intent(in)    :: r
      !
            n = sum(r%ranges(:)%size())
      !
      end function


      !
      !  Members of discreteRange
      !

      pure function discreteRange_init(values) result(res)
      implicit none
      type(discreteRange)                       :: res
      double precision,dimension(:),intent(in)  :: values   !< Sequence of points
      !
      integer :: ierr
      !
            allocate(res%sequence(size(values)),stat=ierr)
            res%sequence = values
      !
      end function

      !> Provide the next value belonging to the range.
      !> It returns .false. if the end of the range is reached.
      logical function discreteRange_next(r,value) result(next)
      implicit none
      class(discreteRange),intent(inout)    :: r
      !> The value at the point belonging to the range. This is meaningful if and only if
      !> the function returns .true.
      double precision,intent(inout)      :: value
      !
            if (discreteRange_size(r) > 0) then
                  r%lastpoint = r%lastpoint + 1
                  value = r%sequence(r%lastpoint)
                  next = .true.
            else
                  next = .false.
            endif
      !
      end function

      !> Return the number of points that remain in the sequence.
      elemental integer function discreteRange_size(r)    result(n)
      implicit none
      class(discreteRange),intent(in)    :: r
      !
            if (allocated(r%sequence)) then
                  n = size(r%sequence) - r%lastpoint
            else
                  n = 0
            endif
      !
      end function

end module
