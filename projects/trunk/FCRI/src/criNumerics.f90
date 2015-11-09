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
!>    \date Date of first release: 2015-11-09

!>    $Revision$
!>    $Date$
!>
!>    History of modifications: (see svn log)

#include "criStdDefs.fpp"
#include "criMacros.fpp"

!> Various numerical algorithms
module criNumerics
use criErrcodes
implicit none

    !> Calculate evenly spaced numbers over a specified interval
    interface linspace
        module procedure linspace_arr, linspace_dynarr
    end interface

    integer,parameter,private :: nbounds = 2, left_bound = 1, right_bound = 2

    !> Data needed by barycentric interpolation
    type :: BarycentricInterpolator
        
        integer :: order = 0 !< Order of interpolation polynomial
        
        integer :: npoints !< number of points in xi and yi
        
        !> x coordinates of the known points. 
        !>
        !> The interpolation nodes and the barycentric weighting factors 
        !> will be constructed from xi.
        double precision,dimension(:),allocatable   :: xi
        
        !> y coordinates of the known points.
        double precision,dimension(:),allocatable   :: yi
        
        !> Barycentric weighting factors of the interpolation polynomials
        !> It is useful to pre-compute the weights for every 
        !> polynomial that that is on the nodes from i to i+order+1. 
        !> This way the latst order+1 nodes 
        !> Shape: [1:order+1, 1:size(xi)-order]
        double precision,dimension(:,:),allocatable   :: wi
        
        !> The flag defines how the points outside the range 
        !> will be treated. If .true., linear interpolation from the
        !> two border points will be constructed and used for extrapolation. 
        !> If .false., the border point will be returned.
        logical :: extrapolate = .true.
        
    end type

contains
    
    !> Calculate evenly spaced numbers over a specified interval
    !> and place them in dynamically allocated array.
    pure subroutine linspace_dynarr(xstart, xend, n, array ,endpoint)
    use criAlgorithm, only: optionalDefault
    implicit none
    double precision,intent(in)             :: xstart !< Begin of the interval
    double precision,intent(in)             :: xend !< End of the interval
    integer,intent(in)                      :: n !< Number of values to be
    !> Array to be filled in. The allocated size of the array will be either
    !>  0 if n<1 or n otherwise. 
    double precision,dimension(:),allocatable,intent(out) :: array
    !> Flag: set the endpoint to xend (default: .true.)
    logical,intent(in),optional :: endpoint
    !
        allocate(array(max(0,n)))
        call linspace_arr(xstart, xend, array ,endpoint)
    !
    end subroutine


    !> Calculate evenly spaced numbers over a specified interval
    pure subroutine linspace_arr(xstart, xend, array ,endpoint)
    use criAlgorithm, only: optionalDefault
    implicit none
    double precision,intent(in)             :: xstart !< Begin of the interval
    double precision,intent(in)             :: xend !< End of the interval
    !< Array to be filled in. Size of the array determines 
    !> the number of numbers to be generated.
    double precision,dimension(:),intent(out):: array
    !> Flag: set the endpoint to xend (default: .true.)
    logical,intent(in),optional             :: endpoint
    !
    double precision :: xstep
    integer :: npoints, nitervals, i
    logical :: use_endpoint
    !
        npoints = size(array)
        if (npoints == 0) return ! Nothing to do
        !
        nitervals = npoints
        use_endpoint = optionalDefault(endpoint, .true.)
        if (use_endpoint .and. (npoints > 1)) nitervals = npoints - 1
        xstep = (xend - xstart) / dble(nitervals)
        do i = 1, nitervals
            array(i) = xstart + dble(i-1)*xstep
        enddo
        ! Make sure the endpoint is exactly the xend
        if (use_endpoint .and. (npoints > 1)) array(npoints) = xend
    !
    end subroutine

    
    !> Initialize and set BarycentricInterpolator object
    subroutine BarycentricInterpolator_init(this, order, xi, yi, info)
    implicit none
    type(BarycentricInterpolator),intent(out)   :: this
    integer,intent(in)                          :: order
    double precision,dimension(:),intent(in)    :: xi
    double precision,dimension(:),intent(in)    :: yi
    integer,intent(out)                         :: info
    !
    integer :: i
        info = criErr_BadArgs
        if (size(xi) /= size(yi)) return
        ! TODO: check if the nodes are in strictly ascending order
        
        call BarycentricInterpolator_init_allocate(this, order, size(xi), info)
        if (info /= criSuccess) return
        
        this%xi = xi
        this%yi = yi
        
        ! compute weighting factors starting from all points.
        ! Every set of the factors takes the block from i-th to i+order+1 
        ! points as the interpolation nodes.
        
        do i = 1, size(this%xi) - order
            call barycentric_weights(this%xi(i:i+order), this%wi(:,i), info)
        enddo
        
    !
    end subroutine
    
    
    !> Allocate internal structures of BarycentricInterpolator object
    subroutine BarycentricInterpolator_init_allocate(this, order, npoints, info)
    implicit none
    type(BarycentricInterpolator),intent(out)   :: this
    integer,intent(in)  :: order
    integer,intent(in)  :: npoints
    integer,intent(out) :: info
    !
        info = criErr_BadArgs
        if ((order < 1) .or. (npoints <= order)) return
        !
        this%npoints = npoints
        this%order = order
        allocate(this%xi(npoints), this%yi(npoints))
        allocate(this%wi(order+1, npoints - order))
        info = criSuccess
    !
    end subroutine
    
    
    !> Calculate interpolation by means of barycentric form of interpolation
    !> polynomial in Lagrange form.
    double precision pure function BarycentricInterpolator_interpolate(this, x) result(res)
    use criAlgorithm
    implicit none
    type(BarycentricInterpolator),intent(in)    :: this
    double precision,intent(in)                 :: x
    !
    integer :: i, j, last
    logical :: out_bounds(nbounds)
    !
        last = size(this%xi)
        out_bounds = [x < this%xi(1), x > this%xi(last)]
        ! Check if we fall inside the range
        if (out_bounds(left_bound) .or. out_bounds(right_bound)) then
            ! TODO:
            ! deal with extrapolation
            CHOOSE(res, out_bounds(left_bound), this%yi(1), this%yi(last))
            return
        else
            ! Pick the right chunk:
            i = min(last - this%order, lower_bound(this%xi, x))
            j = i + this%order
            res = barycentric_interpolation(x, this%xi(i:j), this%yi(i:j), this%wi(:,i))
        endif
    !
    end function
    
    !> Compute interpolation by polynomial of degree n using barycentric formula.
    !>
    !> The degree of the polynomial n is deduced from the size of xi. All xi, yi
    !> and wi must have identical shape.
    !>
    !> [1] J-P Berrut and L.N. Trefethen, Barycentric Lagrange Interpolation, SIAM Rev.
    !>     46(3), 501–517. DOI:10.1137/S0036144502417715
    double precision pure function barycentric_interpolation(x, xi, yi, wi) result(p)
    implicit none
    double precision,intent(in)                 :: x  !< interpolation point
    double precision,dimension(:),intent(in)    :: xi !< interpolation nodes. Shape is [1:n+1]
    double precision,dimension(:),intent(in)    :: yi !< function values at the interpolation nodes. Shape is [1:n+1]
    double precision,dimension(:),intent(in)    :: wi !< barycentric weights of the nodes. Shape is [1:n+1]
    !
    double precision,dimension(size(xi)) :: xterms
    integer :: i
    !
        xterms = x - xi
        ! Detect if we interpolate exactly on one of the nodes
        do i = 1, size(xterms)
            if (abs(xterms(i)) < epsilon(0.D0)) then
                p = yi(i)
                return
            endif
        enddo
        ! It's interpolation between the nodes
        xterms = wi / xterms
        p = dot_product(xterms, yi) / sum(xterms)
    !
    end function
    
    
    !> Compute barycentric weights of interpolation points
    !>
    !> [1] J-P Berrut and L.N. Trefethen, Barycentric Lagrange Interpolation, SIAM Rev.
    !>     46(3), 501–517. DOI:10.1137/S0036144502417715
    pure subroutine barycentric_weights(xi, wi, info)
    implicit none
    double precision,dimension(0:),intent(in)   :: xi
    double precision,dimension(0:),intent(out)  :: wi
    integer,intent(out)                         :: info
    !
    integer :: j,k, n
    ! double precision,dimension(0:ubound(xi,dim=1)) :: xdiff
    !
        info = criErr_BadDims
        n = ubound(xi,dim=1)
        if (n /= ubound(wi,dim=1)) return
        !
        ! Follow (3.2) in [1]
        do j = 0, n
            wi(j) = 1.0
            ! \prod_{k \ne j} (x_j - x_k)
            do k = 0, n
                if (j /= k) wi(j) = wi(j) * (xi(j) - xi(k))
            enddo
        enddo
        wi = 1.D0 / wi
        info = criSuccess
        
        ! A better alternative: follow the algorithm given in [1]
        ! Deplorably, the code below is buggy...
        !wi(0) = 1.D0
        !xdiff = 0.D0
        !do j = 1, n
        !    xdiff(:j) = xi(j)-xi(:j)
        !    wi(:j-1) = wi(:j-1) * xdiff(:j-1)
        !    wi(j) = product(-xdiff(:j))
        !enddo
        ! wi = 1.D0 / wi
        info = criSuccess
    !
    end subroutine
    
    
    
end module
