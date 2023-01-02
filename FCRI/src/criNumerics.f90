!> Various numerical algorithms
module criNumerics
    use criErrcodes
    implicit none

    !> Data needed by barycentric interpolation
    type :: BarycentricInterpolator

        integer :: order = 0 !< Order of interpolation polynomial

        integer :: npoints !< number of points in xi and yi

        !> x coordinates of the known points.
        !>
        !> The interpolation nodes and the barycentric weighting factors
        !> will be constructed from xi.
        !> Size of xi must be at least [order + 1]
        double precision,dimension(:),allocatable   :: xi

        !> y coordinates of the known points.
        !>
        !> Size of yi must be identical as the size of xi, so the same
        !> restrictions apply.
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

    !> Interpolation function
    !> \param[in] this The interpolator (the type implicates the interpolation method used)
    !> \param[in] x The value at which the function shall be evaluated
    !> \result The interpolated value y(x)
    interface interpolate
        module procedure BarycentricInterpolator_interpolate
    end interface

contains

    !> Initialize and set BarycentricInterpolator object
    subroutine BarycentricInterpolator_init(this, order, xi, yi, info)
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

    contains

        !> Allocate internal structures of BarycentricInterpolator object
        subroutine BarycentricInterpolator_init_allocate(this, order, npoints, info)
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

        !> Compute barycentric weights of interpolation points
        !>
        !> [1] J-P Berrut and L.N. Trefethen, Barycentric Lagrange Interpolation, SIAM Rev.
        !>     46(3), 501\96517. DOI:10.1137/S0036144502417715
        pure subroutine barycentric_weights(xi, wi, info)
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

    end subroutine


    !> Calculate interpolation by means of barycentric form of interpolation
    !> polynomial in Lagrange form.
    !>
    !> The function requires a properly initialized BarycentricInterpolator object.
    !> Otherwise the result of the function is undefined.
    double precision pure function BarycentricInterpolator_interpolate(this, x) result(res)
    use criAlgorithm
    !> Properly initialized object of type BarycentricInterpolator
    type(BarycentricInterpolator),intent(in)    :: this
    !> The point at which the interpolated function is evaluated
    double precision,intent(in)                 :: x

    integer :: i, j, last

        ! Check the size of this%xi. We are going to speculate on the size of
        ! this%yi later on, but the 'out-of-bounds' check must be done in a safe
        ! way, i.e. this%xi must be allocated and this%xi(1) must be a valid
        ! element in the array.
        if (.not. allocated(this%xi)) then
            res = 0.D0 ! Note MD: error stop would be more reasonable
            return
        else
            last = size(this%xi)
        endif

        ! TODO: deal with extrapolation
        if (x < this%xi(1)) then
            res = this%yi(1)
        elseif (x > this%xi(last)) then
            res = this%yi(last)
        else
            ! Pick the right chunk. lower_bound will provide the position of the first element
            ! in that has a value greater than or equivalent to x
            i = min(last - this%order, max(1,lower_bound(this%xi, x)-1))
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
    !>     46(3), 501\96517. DOI:10.1137/S0036144502417715
    double precision pure function barycentric_interpolation(x, xi, yi, wi) result(p)
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

end module
