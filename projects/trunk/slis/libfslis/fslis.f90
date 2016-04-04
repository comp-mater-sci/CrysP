
!> Fortran binding for SLIS API
module fslis
use,intrinsic :: iso_c_binding
implicit none

      interface
            
            function initSlis(envvar) bind(C, name='initSlis')
            use,intrinsic :: iso_c_binding
            implicit none
            integer(kind=c_int)  :: initSlis
            !> Environment variable name that contains directory path
            !> where the license file should be searched for.
            !> This must be a null-terminated string, example: 'PRODUCT_ROOT'//C_NULL_CHAR
            character(kind=c_char),dimension(*),intent(in) :: envvar
            end function

            
            function isLicenseValid(uuid, print_output) bind(C, name='isLicenseValid')
            use,intrinsic :: iso_c_binding
            implicit none
            logical(kind=c_bool) isLicenseValid
            !> uuid
            !> This must be a null-terminated string, example:
            !> '6702b45e-f9dc-11e5-a8c3-ecf4bb152acb'//C_NULL_CHAR 
            character(kind=c_char),dimension(*),intent(in) :: uuid
            logical(kind=c_bool),intent(in),value :: print_output
            end function
      
      end interface

end module