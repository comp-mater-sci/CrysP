!>Global definitions used in multiple modules within AlTay
module utils
    use,intrinsic :: iso_fortran_env, only: output_unit

    implicit none
    public

    integer, parameter :: DP = selected_real_kind(15,307)
    REAL(DP), parameter :: TOLERANCE = 1.E-9_DP, &
                           SQR0P5 = sqrt(0.5_DP), &
                           SQR0P67 = sqrt(2._DP/3._DP), &
                           SQR1P5 = sqrt(1.5_DP), &
                           SQR2 = sqrt(2._DP)
 

    !Status codes
    enum, bind(C)
        enumerator :: VEF_OK, &
                      VEF_FAIL, &
                      VEF_ERROR
    end enum
    integer,parameter       :: display_unit = output_unit

    integer :: LEC = 4   !< data set with slip systems
    integer :: IMP1 = 7  !< output-file with successive "current situations"
    integer :: IMP5 = 111     !< output of stress-strain or slip-stress

    !> Maximal length of path acceptable by the filesystem
    integer,parameter       :: MAX_PATHLEN = 2048

    interface convert_stress_strain_space
        module procedure convert_stress_strain_mat_vec, &
                         convert_stress_strain_vec_mat
    end interface


contains

    !> Convert second-rank tensor t into 5D vector following Van Houtte et al., 1992.
    !> Hydrostatic component is subtracted and tensor is symmetrized
    pure function convert_stress_strain_mat_vec(t) result(v)
        real(DP),dimension(3,3),intent(in)   :: t
        real(DP),dimension(5)                :: v
    
        v(1) =  SQR0P5*(t(1,1) - t(2,2))
        v(2) = -SQR1P5*(t(3,3) - (t(1,1) + t(2,2) + t(3,3)) / 3._DP)
        v(3) =  SQR0P5*(t(2,3) + t(3,2))
        v(4) =  SQR0P5*(t(3,1) + t(1,3))
        v(5) =  SQR0P5*(t(1,2) + t(2,1))
    end function
    
    !> Convert 5D vector v into second-rank tensor
    pure function convert_stress_strain_vec_mat(v) result(t)
        real(DP),dimension(5),intent(in)    :: v
        real(DP),dimension(3,3)             :: t
        real(DP),parameter ::  root6i = 1.D0/sqrt(6.D0)
      
        t(1,1) =  SQR0P5*v(1) + root6i*v(2)
        t(2,2) = -SQR0P5*v(1) + root6i*v(2)
        t(3,3) = -SQR0P67*v(2)
        t(2,3) =  SQR0P5*v(3)
        t(3,1) =  SQR0P5*v(4)
        t(1,2) =  SQR0P5*v(5)
        t(3,2) = t(2,3)
        t(1,3) = t(3,1)
        t(2,1) = t(1,2)
    end function

    pure function normalize(arr) result(normalized)
        integer, dimension(:,:), intent(in) :: arr
        real(DP), dimension(3,size(arr,2)) :: normalized
        integer :: i

        do i=1, size(arr,2)
            normalized(:,i) = real(arr(:,i),DP) / norm2(real(arr(:,i),DP))
        end do
    end function

    pure function outer_product(v1, v2) result(prod)
        real(DP), dimension(:), intent(in)      :: v1, v2
        real(DP), dimension(size(v1), size(v2)) :: prod
        integer                                 :: i, j

        forall(i=1:size(v1), j=1:size(v2)) prod(i,j) = v1(i) * v2(j)  
    end function

    pure function antisymmetric_part(mat) result(antisym)
        real(DP), dimension(:,:), intent(in) :: mat
        real(DP), dimension(size(mat,1),size(mat,2)) :: antisym

        antisym = (mat - transpose(mat)) / 2._DP
    end function

    pure function get_rotation(t) result(rot)
        real(DP), dimension(3,3), intent(in) :: t
        real(DP), dimension(3)               :: rot
        real(DP), dimension(3,3)             :: antisym
                
        antisym = antisymmetric_part(t)
        rot = [-antisym(3,2), -antisym(1,3), -antisym(2,1)] !This conversion can likely be replaced by a more intuitive one
    end function

    ! Returns the inverse of a matrix calculated by finding the LU
    ! decomposition.  Depends on LAPACK.
    function invert(A) result(Ainv)
      real(dp), dimension(:,:), intent(in) :: A
      real(dp), dimension(size(A,1),size(A,2)) :: Ainv
                                                                          
      real(dp), dimension(size(A,1)) :: work  ! work array for LAPACK
      integer, dimension(size(A,1)) :: ipiv   ! pivot indices
      integer :: n, info
    
      ! External procedures defined in LAPACK
      external DGETRF
      external DGETRI
                                                                         
      ! Store A in Ainv to prevent it from being overwritten by LAPACK
      Ainv = A 
      n = size(A,1)
                                                                         
      ! DGETRF computes an LU factorization of a general M-by-N matrix A
      ! using partial pivoting with row interchanges.
      call DGETRF(n, n, Ainv, n, ipiv, info)
                                                                         
      if (info /= 0) then
         stop 'Matrix is numerically singular!'
      end if
                                                                         
      ! DGETRI computes the inverse of a matrix using the LU factorization
      ! computed by DGETRF.
      call DGETRI(n, Ainv, n, ipiv, work, n, info)
                                                                         
      if (info /= 0) then
         stop 'Matrix inversion failed!'
      end if
    end function
end module 
