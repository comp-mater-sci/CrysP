!> Shared subroutines that offer (safer) access to the results of the multilevel
!> model.
!>
!> This design emphasizes separation of the algorithms in the driver modules
!> from the actual implementation of the underlying multilevel model.
module commonUtils
    use base_defs
    use altay
    use logging

    implicit none

    character(*), parameter, private:: MOD_NAME = 'commonutils'

    !> Data needed by barycentric interpolation
    type :: BarycentricInterpolator

        integer :: order = 0 !< Order of interpolation polynomial

        integer :: npoints !< number of points in xi and yi

        !> x coordinates of the known points.
        !>
        !> The interpolation nodes and the barycentric weighting factors
        !> will be constructed from xi.
        !> Size of xi must be at least [order + 1]
        real(DP),dimension(:),allocatable   :: xi

        !> y coordinates of the known points.
        !>
        !> Size of yi must be identical as the size of xi, so the same
        !> restrictions apply.
        real(DP),dimension(:),allocatable   :: yi

        !> Barycentric weighting factors of the interpolation polynomials
        !> It is useful to pre-compute the weights for every
        !> polynomial that that is on the nodes from i to i+order+1.
        !> This way the latst order+1 nodes
        !> Shape: [1:order+1, 1:size(xi)-order]
        real(DP),dimension(:,:),allocatable   :: wi

        !> The flag defines how the points outside the range
        !> will be treated. If .true., linear interpolation from the
        !> two border points will be constructed and used for extrapolation.
        !> If .false., the border point will be returned.
        logical :: extrapolate = .true.

    end type

    !> Interpolation function
    !> \param[in] this The interpolator (the type implicates the interpolation method used)
    !> \param[in] x The value at which the function shall be evaluated
    !> \result The interpolated value y(x)
    interface interpolate
        module procedure BarycentricInterpolator_interpolate
    end interface

    !> Test the presence of optional value, and return a default if the optional
    !> is not present.
    !>
    !> The function provides a simplified access pattern to optional parameters.
    !> The first formal argument is declared as optional parameter, butit must always
    !> appear as the actual parameter in a context where the actual parameter is
    !> declared itself as "optional".
    interface optionalDefault
        module procedure optionalDefault_logical, optionalDefault_integer
    end interface

contains

    !> Test the presence of optional logical value, and return a default if the optional
    !> is not present.
    pure logical function optionalDefault_logical(value, default) result(res)
        logical, intent(in), optional   :: value !< The parameter to be tested for presence. The actual parameter MUST have optional attribute.
        logical, intent(in)            :: default  !< Default value

        if(present(value))then; res = value; else; res = default; endif
    end function

    !> Test the presence of optional integer value, and return a default if the optional
    !> is not present.
    pure integer function optionalDefault_integer(value, default) result(res)
          integer, intent(in), optional   :: value !< The parameter to be tested for presence. The actual parameter MUST have optional attribute.
          integer, intent(in)            :: default  !< Default value
          if(present(value))then; res = value; else; res = default; endif

    end function

    !> Initialize and set BarycentricInterpolator object
    subroutine BarycentricInterpolator_init(this, order, xi, yi)
        type(BarycentricInterpolator),intent(out)   :: this
        integer,intent(in)                          :: order
        real(DP),dimension(:),intent(in)    :: xi
        real(DP),dimension(:),intent(in)    :: yi

        integer :: i

        if (size(xi) /= size(yi)) call log_error(MOD_NAME, 'BarycentricInterpolator_init', ERR_VAL, 'Sizes of inputs do not match')

        ! TODO: check if the nodes are in strictly ascending order

        call BarycentricInterpolator_init_allocate(this, order, size(xi))

        this%xi = xi
        this%yi = yi

        ! compute weighting factors starting from all points.
        ! Every set of the factors takes the block from i-th to i+order+1
        ! points as the interpolation nodes.

        do i = 1, size(this%xi) - order
            call barycentric_weights(this%xi(i:i+order), this%wi(:,i))
        enddo

    contains

        !> Allocate internal structures of BarycentricInterpolator object
        subroutine BarycentricInterpolator_init_allocate(this, order, npoints)
            type(BarycentricInterpolator),intent(out)   :: this
            integer,intent(in)  :: order
            integer,intent(in)  :: npoints

            character(*), parameter:: PROC_NAME = 'BarycentricInterpolator_init_allocate'

            if (order < 1) call log_error(MOD_NAME, PROC_NAME, ERR_VAL, 'Order must not be negative')
            if (npoints <= order) call log_error(MOD_NAME, PROC_NAME, ERR_VAL, 'Too few points.')

            this%npoints = npoints
            this%order = order
            allocate(this%xi(npoints), this%yi(npoints))
            allocate(this%wi(order+1, npoints - order))
        end subroutine

        !> Compute barycentric weights of interpolation points
        !>
        !> [1] J-P Berrut and L.N. Trefethen, Barycentric Lagrange Interpolation, SIAM Rev.
        !>     46(3), 501\96517. DOI:10.1137/S0036144502417715
        subroutine barycentric_weights(xi, wi)
            real(DP),dimension(0:),intent(in)   :: xi
            real(DP),dimension(0:),intent(out)  :: wi

            integer :: j,k, n

            n = ubound(xi,dim=1)
            if (n /= ubound(wi,dim=1)) call log_error(MOD_NAME, 'barycentric_weights', ERR_VAL, 'Too few values')
            !
            ! Follow (3.2) in [1]
            do j = 0, n
                wi(j) = 1.0_DP
                do k = 0, n
                    if (j /= k) wi(j) = wi(j) * (xi(j) - xi(k))
                enddo
            enddo
            wi = 1.D0 / wi
        end subroutine
    end subroutine

    !> Calculate interpolation by means of barycentric form of interpolation
    !> polynomial in Lagrange form.
    !>
    !> The function requires a properly initialized BarycentricInterpolator object.
    !> Otherwise the result of the function is undefined.
    real(DP) pure function BarycentricInterpolator_interpolate(this, x) result(res)
        !> Properly initialized object of type BarycentricInterpolator
        type(BarycentricInterpolator),intent(in)    :: this
        !> The point at which the interpolated function is evaluated
        real(DP),intent(in)                 :: x

        integer :: i, j

        ! TODO: deal with extrapolation
        if (.not. allocated(this%xi)) then
            res = 0.D0 ! Note MD: error stop would be more reasonable
        elseif (x < this%xi(1)) then
            res = this%yi(1)
        elseif (x > this%xi(size(this%xi))) then
            res = this%yi(size(this%xi))
        else
            ! Pick the right chunk. lower_bound will provide the position of the first element
            ! in that has a value greater than or equivalent to x
            i = min(size(this%xi) - this%order, max(1,lower_bound(this%xi, x)-1))
            j = i + this%order
            res = barycentric_interpolation(x, this%xi(i:j), this%yi(i:j), this%wi(:,i))
        endif

    contains
        !> Compute interpolation by polynomial of degree n using barycentric formula.
        !>
        !> The degree of the polynomial n is deduced from the size of xi. All xi, yi
        !> and wi must have identical shape.
        !>
        !> [1] J-P Berrut and L.N. Trefethen, Barycentric Lagrange Interpolation, SIAM Rev.
        !>     46(3), 501\96517. DOI:10.1137/S0036144502417715
        real(DP) pure function barycentric_interpolation(x, xi, yi, wi) result(p)
            real(DP),intent(in)                 :: x  !< interpolation point
            real(DP),dimension(:),intent(in)    :: xi !< interpolation nodes. Shape is [1:n+1]
            real(DP),dimension(:),intent(in)    :: yi !< function values at the interpolation nodes. Shape is [1:n+1]
            real(DP),dimension(:),intent(in)    :: wi !< barycentric weights of the nodes. Shape is [1:n+1]
            !
            real(DP),dimension(size(xi)) :: xterms
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
    end function

    integer pure function lower_bound(array,val) result(res)
        real(DP),dimension(:),intent(in)     :: array
        real(DP),intent(in)                  :: val

        integer :: first,dist,cnt,mid

        first = lbound(array,dim=1)
        dist = ubound(array,dim=1)     ! full range: [first:first+dist-1]

        do while (dist > 0)
            cnt = dist / 2
            mid = first + cnt
            if (array(mid) < val) then
                first = mid + 1
                dist = dist - (cnt + 1)
            else
                dist = cnt
            endif
        enddo
        res = first
    end function
end module
