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
!>

#include "criStdDefs.fpp"
#include "criTest.fpp"
#include "criMacros.fpp"


module criTestNumerics
use criNumerics
implicit none

public criTestNumerics_main

    interface
        pure double precision function if_fx_dd(x)
        double precision,intent(in) :: x
        end function
    end interface

contains
      
    logical function criTestNumerics_main() result(stat)
    implicit none
        
        stat = .true. 
        
        stat = stat .and. test_linspace()
        
        stat = stat .and. test_barycentric_weights()
        
        stat = stat .and. test_barycentric_interpolate()
        
        stat = stat .and. test_barycentricInterpolatorKnownData()
        
        stat = stat .and. test_barycentricInterpolator()
        
    end function

    !
    ! Testing of linspace
    !
    
    logical function test_linspace() result(res)
    implicit none
    double precision,dimension(:),allocatable :: x
    double precision :: xmin, xmax
    integer :: npoints
    
    double precision,dimension(5) :: x5
    double precision,dimension(5),parameter :: ref5_noendpoint = [ 1.D0 ,  1.2D0,  1.4D0,  1.6D0,  1.8D0]
    double precision,dimension(5),parameter :: ref5_endpoint =   [ 1.D0 ,  1.25D0, 1.5D0 , 1.75D0, 2.D0 ]
    double precision,dimension(5),parameter :: ref5_reverse_noendpoint = [ 2.D0 ,  1.8D0,  1.6D0,  1.4D0,  1.2D0]
     
        res = .false.
        
        
        xmin = 1.D0
        xmax = 2.D0
        npoints = -5
        
        call linspace(xmin, xmax, npoints, x)
        _TEST('linspace, negative #points, default endpoint, array is allocated', allocated(x)) 
        _TEST('linspace,  negative #points, default endpoint, size is 0', size(x) == 0) 

        
        xmin = 1.D0
        xmax = 2.D0
        npoints = 0
        
        call linspace(xmin, xmax, npoints, x)
        _TEST('linspace, zero points, default endpoint, array is allocated', allocated(x)) 
        _TEST('linspace, zero points, default endpoint, size is 0', size(x) == npoints) 
         
        call linspace(xmin, xmax, npoints, x,  endpoint=.true.)
        _TEST('linspace, zero points, endpoint, array is allocated', allocated(x)) 
        _TEST('linspace, zero points, endpoint, size is 0', size(x) == npoints) 

        call linspace(xmin, xmax, npoints, x,  endpoint=.false.)
        _TEST('linspace, zero points, no endpoint, array is allocated', allocated(x)) 
        _TEST('linspace, zero points, no endpoint, size is 0', size(x) == npoints) 

        xmin = 1.D0
        xmax = 2.D0
        npoints = 1
        
        call linspace(xmin, xmax, npoints, x)
        _TEST('linspace, one point, default endpoint, array is allocated', allocated(x)) 
        _TEST('linspace, one point, default endpoint, size is 1', size(x) == npoints) 
        _TEST('linspace, one point, default endpoint, endpoint=xmin', x(size(x)) == xmin) 
         
        call linspace(xmin, xmax, npoints, x, endpoint=.true.)
        _TEST('linspace, one point, endpoint, array is allocated', allocated(x)) 
        _TEST('linspace, one point, endpoint, size is 1', size(x) == npoints) 
        _TEST('linspace, one point, endpoint, endpoint=xmin', x(size(x)) == xmin) 

        call linspace(xmin, xmax, npoints, x,  endpoint=.false.)
        _TEST('linspace, one point, no endpoint, array is allocated', allocated(x)) 
        _TEST('linspace, one point, no endpoint, size is 1', size(x) == npoints) 
        _TEST('linspace, one point, no endpoint, endpoint=xmin', x(size(x)) == xmin) 

        xmin = 1.D0
        xmax = 2.D0
        npoints = 2
        
        call linspace(xmin, xmax, npoints, x)
        _TEST('linspace, two points, default endpoint, size is 2', size(x) == npoints) 
        _TEST('linspace, two points, default endpoint, first=xmin', x(1) == xmin)
        _TEST('linspace, two points, default endpoint, last=xmax', x(size(x)) == xmax) 
         
        call linspace(xmin, xmax, npoints, x, endpoint=.true.)
        _TEST('linspace, two points, endpoint, size is 2', size(x) == npoints) 
        _TEST('linspace, two points, endpoint, first=xmin', x(1) == xmin)
        _TEST('linspace, two points, endpoint, last=xmax', x(size(x)) == xmax) 

        call linspace(xmin, xmax, npoints, x,  endpoint=.false.)
        _TEST('linspace, two points, no endpoint, size is 2', size(x) == npoints) 
        _TEST('linspace, two points, no endpoint, first=xmin', x(1) == xmin)
        _TEST('linspace, two points, no endpoint, last', ((x(size(x)) - 1.5D0 < epsilon(0.D0))))

        xmin = 1.D0
        xmax = 2.D0
        npoints = 5
        
        call linspace(xmin, xmax, x5,  endpoint=.false.)
        _TEST('linspace fixed array, no endpoint, data OK', all(x5 - ref5_noendpoint < epsilon(0.D0)))
        
        call linspace(xmin, xmax, npoints, x,  endpoint=.false.)
        _TEST('linspace allocatable, no endpoint, size OK', size(x) == npoints)
        _TEST('linspace allocatable, no endpoint, data OK', all(x - ref5_noendpoint < epsilon(0.D0)))
        
        call linspace(xmin, xmax, x5,  endpoint=.true.)
        _TEST('linspace fixed array, endpoint, data OK', all(x5 - ref5_endpoint < epsilon(0.D0)))
        
        call linspace(xmin, xmax, npoints, x,  endpoint=.true.)
        _TEST('linspace allocatable, no endpoint, size OK', size(x) == npoints)
        _TEST('linspace allocatable, no endpoint, data OK', all(x - ref5_endpoint < epsilon(0.D0)))

        !
        ! Reverse order
        !
        xmin = 2.D0
        xmax = 1.D0
        npoints = 5
        
        call linspace(xmin, xmax, x5,  endpoint=.false.)
        _TEST('linspace fixed array, no endpoint, data OK', all(x5 - ref5_reverse_noendpoint < epsilon(0.D0)))
        
        call linspace(xmin, xmax, npoints, x,  endpoint=.false.)
        _TEST('linspace allocatable, no endpoint, size OK', size(x) == npoints)
        _TEST('linspace allocatable, no endpoint, data OK', all(x - ref5_reverse_noendpoint < epsilon(0.D0)))
        
        ! If endpoint is requested, the ref5_endpoint in reverse order should be produced
        call linspace(xmin, xmax, x5,  endpoint=.true.)
        _TEST('linspace fixed array, endpoint, data OK', all(x5 - ref5_endpoint(5:1:-1) < epsilon(0.D0)))
        
        ! If endpoint is requested, the ref5_endpoint in reverse order should be produced
        call linspace(xmin, xmax, npoints, x,  endpoint=.true.)
        _TEST('linspace allocatable, no endpoint, size OK', size(x) == npoints)
        _TEST('linspace allocatable, no endpoint, data OK', all(x - ref5_endpoint(5:1:-1) < epsilon(0.D0)))

        
        res = .true.
    end function
    !
    ! Testing of interpolation
    !
    
    pure double precision function fx_identity(x) result(y)
    implicit none
    double precision,intent(in) :: x
        y = x
    end function

    pure double precision function fx_linear(x) result(y)
    implicit none
    double precision,intent(in) :: x
        y = 0.5D0 * x + 1.D0
    end function
    
    pure double precision function fx_quadratic(x) result(y)
    implicit none
    double precision,intent(in) :: x
        y = x**2
    end function

    pure double precision function fx_sin(x) result(y)
    implicit none
    double precision,intent(in) :: x
        y = sin(x)
    end function

    
    subroutine setUp_barycentric_linear(xi, yi, wi, wi_r)
    implicit none
    double precision,dimension(:),allocatable,intent(out) :: xi, yi, wi, wi_r
    !
    ! wi_ref is calculated by SciPy:
    ! xi = np.asarray([0., 1.0])
    ! scipy.interpolate.BarycentricInterpolator(xi).wi
    double precision,dimension(2),parameter :: wi_ref = [1.D0, -1.D0], &
                                               xi_ref = [0.D0, 1.D0]
    !
        allocate(xi(size(wi_ref)), yi(size(wi_ref)), wi(size(wi_ref)), wi_r(size(wi_ref)))
        xi = xi_ref
        ! Note: SciPy BarycentricInterpolator provides wi = [1., -1.]
        ! while it should be [-1., 1.]. In this case it does not matter 
        ! anyway.
        wi_r = - wi_ref
    !
    end subroutine

    
    subroutine setUp_barycentric_quadratic(xi, yi, wi, wi_r)
    implicit none
    double precision,dimension(:),allocatable,intent(out) :: xi, yi, wi, wi_r
    !
    ! wi_ref is calculated by SciPy:
    ! xi == np.asarray([0., 0.5, 1.0])
    ! scipy.interpolate.BarycentricInterpolator(xi).wi
    double precision,dimension(3),parameter :: wi_ref = [2.D0, -4.D0,  2.D0], &
                                               xi_ref = [0.D0, 0.5D0, 1.D0]
    !
        allocate(xi(size(wi_ref)), yi(size(wi_ref)), wi(size(wi_ref)), wi_r(size(wi_ref)))
        xi = xi_ref
        wi_r = wi_ref
    !
    end subroutine

      
    subroutine setUp_barycentric_big(xi, yi, wi, wi_r)
    implicit none
    double precision,dimension(:),allocatable,intent(out) :: xi, yi, wi, wi_r
    !
    integer :: i
    ! wi_ref is calculated by SciPy:
    ! xi = np.linspace(0, 10, 21)
    ! scipy.interpolate.BarycentricInterpolator(xi).wi
    double precision,dimension(21),parameter :: &
        wi_ref = [   4.30998041d-13,  -8.61996082d-12,   8.18896278d-11, &
                    -4.91337767d-10,   2.08818551d-09,  -6.68219363d-09, &
                     1.67054841d-08,  -3.34109682d-08,   5.42928233d-08, &
                    -7.23904310d-08,   7.96294741d-08,  -7.23904310d-08, &
                     5.42928233d-08,  -3.34109682d-08,   1.67054841d-08, &
                    -6.68219363d-09,   2.08818551d-09,  -4.91337767d-10, &
                     8.18896278d-11,  -8.61996082d-12,   4.30998041d-13 ]
    !
        allocate(xi(size(wi_ref)), yi(size(wi_ref)), wi(size(wi_ref)), wi_r(size(wi_ref)))
        do i=1, size(xi)
            xi(i) = 0.5 * (i-1)
        enddo
        wi_r = wi_ref
    !
    end subroutine
      
      
    logical function test_barycentric_weights() result(res)
    implicit none
    double precision,dimension(:),allocatable :: xi, yi, wi, wi_ref
    integer :: info
    !
        res = .false.
        !
        call setUp_barycentric_linear(xi, yi, wi, wi_ref)
        call barycentric_weights(xi, wi, info)
        _TEST('linear problem, wi ~= wi_ref',all(abs(wi-wi_ref)< epsilon(0.D0)))
        !
        call setUp_barycentric_quadratic(xi, yi, wi, wi_ref)
        call barycentric_weights(xi, wi, info)
        _TEST('quadratic problem, wi ~= wi_ref',all(abs(wi-wi_ref)< epsilon(0.D0)))
        !
        call setUp_barycentric_big(xi, yi, wi, wi_ref)
        call barycentric_weights(xi, wi, info)
        _TEST('big problem, wi ~= wi_ref',all(abs(wi-wi_ref)< 10.D0 * epsilon(0.D0)))
        
        res = .true.
    end function
    
    subroutine setUp_barycentric_interpolate(n, xi, yi, yi_res)
    implicit none
    integer,intent(in) :: n !< size multiplier
    double precision,dimension(:),intent(in) :: xi
    double precision,dimension(:),allocatable,intent(out) :: yi, yi_res
    !
        allocate(yi(n*size(xi)), yi_res(n*size(xi)))
    !
    end subroutine

    
    
    subroutine setUp_testarrays(n, x, y, y_res)
    implicit none
    integer,intent(in) :: n !< size
    double precision,dimension(:),allocatable,intent(out) :: x, y, y_res
    !
        allocate(x(n), y(n), y_res(n))
    !
    end subroutine

    
    
    logical function test_barycentric_interpolate() result(res)
    implicit none
    double precision,dimension(:),allocatable :: xi, yi, yi_res, wi, wi_ref, x_test, y_test, y_ref
    integer ::  info
    !
        res = .false.
        !
        call setUp_barycentric_linear(xi, yi, wi, wi_ref)
        call barycentric_weights(xi, wi, info)
        ! We should get exact result on the nodes
        call setUp_testOnNodes()
        _TEST('linear problem, yi = xi, on nodes',all(abs(yi - yi_res) == 0.D0))
        ! Assuming linear function, we should get the exact results at the mid-points , too.
        call setUp_testMidpoints(1, 0.5D0, x_test, y_test, y_ref)
        _TEST('linear problem, yi = xi, on mid-points', all(abs(y_test - y_ref) < epsilon(0.D0)))
        
        !
        call setUp_barycentric_quadratic(xi, yi, wi, wi_ref)
        call barycentric_weights(xi, wi, info)
        call setUp_testOnNodes()
        _TEST('quadratic problem, yi = xi, on nodes',all(abs(yi - yi_res) < epsilon(0.D0)))
        call setUp_testMidpoints(2, 0.5D0, x_test, y_test, y_ref)
        _TEST('quadratic problem, yi = xi**2, on mid-points', all(abs(y_test - y_ref) < epsilon(0.D0)))
        call setUp_testMidpoints(2, 0.33D0, x_test, y_test, y_ref)
        _TEST('quadratic problem, yi = xi**2, on 0.33', all(abs(y_test - y_ref) < epsilon(0.D0)))
        !
        call setUp_barycentric_big(xi, yi, wi, wi_ref)
        call barycentric_weights(xi, wi, info)
        call setUp_testOnNodes()
        _TEST('big problem (n=20), yi = xi, on nodes',all(abs(yi - yi_res)< epsilon(0.D0)))
        
        res = .true.
        
    contains
        subroutine setUp_testOnNodes()
        implicit none
        integer :: i
            call setUp_barycentric_interpolate(1, xi, yi, yi_res)
            yi = xi
            do i = 1, size(yi_res) ! dubious indexing...
                yi_res(i) =  barycentric_interpolation(xi(i), xi, yi, wi)
            enddo
        end subroutine
        
        subroutine setUp_testMidpoints(p, frac, x_test, y_test, y_ref)
        implicit none
        integer,intent(in) :: p
        double precision,intent(in) :: frac
        double precision,dimension(:),allocatable,intent(out) :: x_test, y_test, y_ref
        integer :: i, n
            call setUp_barycentric_interpolate(1, xi, yi, yi_res)
            n = size(xi) - 1
            allocate(x_test(n), y_test(n), y_ref(n))
            yi = xi**p
            x_test = xi(:n) + frac*(xi(2) - xi(1))
            y_ref = x_test**p
            
            do i = 1, size(x_test)
                y_test(i) =  barycentric_interpolation(x_test(i), xi, yi, wi)
            enddo
        end subroutine
        
    end function
    
    
    
    subroutine setUp_barycentricInterpolator(n, order, xstart, xend, bi, fx)
    implicit none
    integer,intent(in) :: n, order ! number of points, order
    type(BarycentricInterpolator),intent(out) :: bi
    procedure(if_fx_dd) :: fx
    double precision,intent(in) :: xstart, xend
    !
    double precision,dimension(:),allocatable :: xi, yi
    integer :: i, info
    
        call linspace(xstart, xend, n, xi)
        allocate(yi(size(xi)))
        ! Sadly, fx cannot be elemental...
        do i = 1, size(xi)
            yi(i) = fx(xi(i))
        enddo
        
        call BarycentricInterpolator_init(bi, order, xi, yi, info)
        
    end subroutine
    
    
    logical function test_barycentricInterpolatorKnownData() result(res)
    use criMathUtils, only: pi
    implicit none
    !    
    ! Known nodal data for linear interpolation:
    integer,parameter :: nnodes = 5, nresults = 5
    double precision,dimension(nnodes),parameter ::  &
        xi = [0.D0, 1.D0, 3.D0, 4.D0, 6.D0], &
        yi = [1.D0, 5.D0, 7.D0, 7.D0, 3.D0]
    double precision,dimension(nnodes) :: yi_res
    ! Known interpolated values at points between the nodes
    double precision,dimension(nresults),parameter :: &
        x_test = [0.5D0, 2.D0, 3.5D0, 4.5D0, 5.D0], &
        y_test = [3.D0,  6.D0, 7.D0,  6.D0,  5.D0]
    double precision,dimension(nresults) :: y_res
    
    !
    integer :: i, info
    type(BarycentricInterpolator) :: bi
    !
    
        res = .false.
        ! Set-up
        call BarycentricInterpolator_init(bi, 1, xi, yi, info)
        !
        do i = 1, size(x_test)
            y_res(i) =  BarycentricInterpolator_interpolate(bi, x_test(i))
        enddo
        _TEST('BarycentricInterpolator, linear on linear function', all(abs(y_test - y_res) < epsilon(0.D0)))
        !
        do i = 1, size(xi)
            yi_res(i) =  BarycentricInterpolator_interpolate(bi, xi(i))
        enddo
         _TEST('BarycentricInterpolator, linear on linear function at nodes', all(abs(yi - yi_res) < epsilon(0.D0)))
        res = .true.
    
    end function
    
    logical function test_barycentricInterpolator() result(res)
    use criMathUtils, only: pi
    implicit none
    integer :: npoints, ntest_points
    double precision,dimension(:),allocatable :: y_test, y_res
    double precision :: xstart, xend
    !
    
        res = .false.
        

        !
        ! Interpolate quadratic function
        !
        xstart = 0.D0
        xend = 10.D0
        npoints = 21

        call setUp_barycentricInterpolation(1, xstart, xend, 0.D0, npoints, npoints, fx_quadratic, y_test, y_res)
        _TEST("BarycentricInterpolator, linear, on nodes, crude", all(abs(y_test - y_res) < epsilon(0.D0)))

        call setUp_barycentricInterpolation(1, xstart, xend, 0.5D0, npoints, npoints, fx_quadratic, y_test, y_res)
        call printTestData()
        _TEST("BarycentricInterpolator, linear, between nodes, crude", all(abs(y_test - y_res) <= 6.25e-2))
        
        call setUp_barycentricInterpolation(2, xstart, xend, 0.D0, npoints, npoints, fx_quadratic, y_test, y_res)
        _TEST("BarycentricInterpolator, quadratic, on nodes, crude", all(abs(y_test - y_res) < epsilon(0.D0)))
        
        call setUp_barycentricInterpolation(2, xstart, xend, 0.5D0, npoints, npoints, fx_quadratic, y_test, y_res)
        call printTestData()
        _TEST("BarycentricInterpolator, quadratic, between nodes, crude", all(abs(y_test - y_res) < 65.D0*epsilon(0.D0)))

        
        xstart = 0.D0
        xend = 10.D0
        npoints = 100
        ntest_points = 200

        call setUp_barycentricInterpolation(1, xstart, xend, 0.D0, npoints, npoints, fx_quadratic, y_test, y_res)
        _TEST("BarycentricInterpolator, linear, on nodes, fine", all(abs(y_test - y_res) < epsilon(0.D0)))

        call setUp_barycentricInterpolation(1, xstart, xend, 0.5D0, npoints, ntest_points, fx_quadratic, y_test, y_res)
        call printTestData()
        _TEST("BarycentricInterpolator, linear, between nodes, fine", all(abs(y_test - y_res) < 3.D-3))
        
        
        call setUp_barycentricInterpolation(2, xstart, xend, 0.D0, npoints, npoints, fx_quadratic, y_test, y_res)
        _TEST("BarycentricInterpolator, quadratic, on nodes, fine", all(abs(y_test - y_res) < epsilon(0.D0)))
        
        call setUp_barycentricInterpolation(2, xstart, xend, 0.5D0, npoints, ntest_points, fx_quadratic, y_test, y_res)
        call printTestData()
        _TEST("BarycentricInterpolator, quadratic, between nodes, fine", all(abs(y_test - y_res) < 65.D0*epsilon(0.D0)))

        
        !
        ! Interpolate sin(x) on [-pi,pi] with linear polynomial
        !
        xstart = -pi
        xend = pi
        npoints = 100
        ntest_points = 200
        
        call setUp_barycentricInterpolation(1, xstart, xend, 0.5D0, npoints, ntest_points, fx_sin, y_test, y_res)
        call printTestData()
        _TEST("BarycentricInterpolator of sin(x), linear", all(abs(y_test - y_res) < 5.D-4))

        !
        ! Interpolate sin(x) on [-pi,pi] with quadratic polynomial
        !
        call setUp_barycentricInterpolation(2, xstart, xend, 0.5D0, npoints, ntest_points, fx_sin, y_test, y_res)
        call printTestData()
        _TEST("BarycentricInterpolator of sin(x), quadratic", all(abs(y_test - y_res) < 2.D-5))

        res = .true.
        
    contains
        subroutine printTestData()
        implicit none
#ifdef ENABLE_DIAGNOSTIC_OUTPUT
        integer :: i
            write(*,*) 'max absolute error:', maxval(abs(y_test - y_res))
            !
            write(*,*) '---'
            do i = 1, min(size(y_test),size(y_res))
                write(*,'(3(E15.7,1X))') y_test(i), y_res(i), y_test(i) - y_res(i)
            enddo
            write(*,*) '---'
#endif
        end subroutine
        
    end function
    
    
    subroutine setUp_barycentricInterpolation(order, xstart, xend, midratio, ni_points, ntest_points, fx, y_test, y_res)
    implicit none
    integer,intent(in) :: order
    double precision,intent(in) :: xstart, xend, midratio
    integer,intent(in) :: ni_points, ntest_points
    procedure(if_fx_dd) :: fx
    double precision,dimension(:),allocatable,intent(out)   :: y_test, y_res
    !
    type(BarycentricInterpolator) :: bi
    double precision,dimension(:),allocatable :: x_test
    double precision :: xmid
    integer :: i
    !
        ! Set the interpolator
        call setUp_barycentricInterpolator(ni_points, order, xstart, xend, bi, fx)
        !
        ! Set the test data
        call setUp_testarrays(ntest_points, x_test, y_test, y_res)
        if ((ni_points == ntest_points) .and. (midratio == 0.D0)) then
            ! If midratio is strictly 0.0, we want the array of interpolation nodes.
            x_test = bi%xi
        else
            xmid = bi%xi(1) + midratio * (bi%xi(2) - bi%xi(1))
            call linspace(xmid, xend, x_test, endpoint=.false.)
        endif
        ! Evaluate: y_test and y_res
        do i = 1, size(x_test)
            y_test(i) = fx(x_test(i))
            y_res(i) =  BarycentricInterpolator_interpolate(bi, x_test(i))
        enddo
    !
    end subroutine
    
    
end module
    