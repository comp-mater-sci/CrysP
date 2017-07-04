
!> Fortran binding for SLIS APIs
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
            logical(kind=c_bool) :: isLicenseValid
            !> uuid
            !> This must be a null-terminated string, example:
            !> '6702b45e-f9dc-11e5-a8c3-ecf4bb152acb'//C_NULL_CHAR 
            character(kind=c_char),dimension(*),intent(in) :: uuid
            logical(kind=c_bool),intent(in),value :: print_output
            end function
      
      end interface

    !>@{ Interface to Token API
    interface

        function signFile(file_path, token_path, signature_path) bind(C, name='signFile')
        use,intrinsic :: iso_c_binding
        implicit none
        !
        integer(kind=c_int)  :: signFile
        !
        !> path to the file (null-terminated C string)
        character(kind=c_char),dimension(*),intent(in) :: file_path
        !> path to the token file (null-terminated C string)
        character(kind=c_char),dimension(*),intent(in) :: token_path
        !> path to the signature file (null-terminated C string)
        !>
        !> If left empty, it will be deduced from file_path
        character(kind=c_char),dimension(*),intent(in) :: signature_path
        end function


        !> Verify if the file is signed with the token.
        function isSignatureValid(file_path, token_path, signature_path) bind(C, name='isSignatureValid')
        use,intrinsic :: iso_c_binding
        implicit none
        !
        logical(kind=c_bool) :: isSignatureValid
        !
        !> path to the file (null-terminated C string)
        character(kind=c_char),dimension(*),intent(in) :: file_path
        !> path to the token file (null-terminated C string)
        character(kind=c_char),dimension(*),intent(in) :: token_path
        !> path to the signature file (null-terminated C string)
        !>
        !> If left empty, it will be deduced from file_path
        character(kind=c_char),dimension(*),intent(in) :: signature_path
        end function


        !> Verify authenticity of the token in token_path
        function isTokenValid(token_path) bind(C, name='isTokenValid')
        use,intrinsic :: iso_c_binding
        implicit none
        !
        logical(kind=c_bool) :: isTokenValid
        !
        !> path to the token file (null-terminated C string)
        character(kind=c_char),dimension(*),intent(in) :: token_path
        end function
      
    end interface


    !> Exit codes from Token API
    enum,bind(C)
        enumerator :: ok = 0
        enumerator :: invalid_token = -1
        enumerator :: invalid_signature = -2
        enumerator :: io_error = -3
        enumerator :: error = -10
    end enum

    !>@}
    
end module