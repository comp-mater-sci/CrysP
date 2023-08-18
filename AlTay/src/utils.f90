!>Global definitions used in multiple modules within AlTay
module utils
    use,intrinsic :: iso_fortran_env, only: output_unit

    implicit none
    public

    integer, parameter :: DP = selected_real_kind(15,307)
    REAL(DP), parameter :: TOLERANCE = 1.E-9_DP, &
                           SQR2 = sqrt(2._DP), &
                           SQR0P5 = sqrt(0.5_DP), &
                           SQR0P67 = sqrt(2._DP/3._DP), &
                           SQR1P5 = sqrt(1.5_DP)
 

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



    end module 
