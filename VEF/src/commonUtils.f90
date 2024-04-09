!> Shared subroutines that offer (safer) access to the results of the multilevel
!> model.
!>
!> This design emphasizes separation of the algorithms in the driver modules
!> from the actual implementation of the underlying multilevel model.
module commonUtils
    use utils

    implicit none

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

    interface convert_voigt
        module procedure convert_voigt_mat_vec, convert_voigt_vec_mat
    end interface

contains

      subroutine  getTaylorFactor(stepid, M, info)
      use altayConfig
      integer, intent(in)            :: stepid
      real(DP), intent(out)  :: M
      integer, intent(out)           :: info
      !
            info = -1
            M = 0.D0
            if (isStateOK(stepid)) then
                  M = astate%simulCalls(stepid)%output%taylor_factor
                  info = 0
            endif
      !
      contains
      !> Perform basic checks if the state variables in altayConfig are consistent.
      logical function isStateOK(stepid)
      use altayConfig
      integer, intent(in)      :: stepid
      !
            isStateOK = .false.
            if (allocated(astate%simulCalls)) then
                  isStateOK = (size(astate%simulCalls) <= stepid) .and. (astate%this >= stepid)
            endif
      !
      end function

      end subroutine

      subroutine makeTextureUpdateStep(D, S, M, output_flag, info)
      use altay
      use altayConfig
      real(DP), dimension(3, 3), intent(in)      :: D
      real(DP), dimension(3, 3), intent(out)     :: S
      real(DP), intent(out)                    :: M
      logical, intent(in)                              :: output_flag
      integer, intent(out)                             :: info
      !
      integer, parameter:: istp = 1
            info = -1
            
            !
            call initStepData(istp, astate, info)
            if (info /= 0) return
            ! Set input data for AlTay
            associate (input => astate%simulCalls(istp)%input)
                  input%dgf = D
                  input%keep_texture = .false.
                  input%keep_state = .false.
                  input%full_model = .true.
                  input%do_output_init = .false.
                  input%do_output_final = output_flag
            end associate
            call runSteps(astate, info)
            if (info /= 0) return
            !
            ! Get the result
            S = astate%simulCalls(istp)%output%stress_tensor(:,:)
            M = astate%simulCalls(istp)%output%taylor_factor

#define MSG_GROUP_ERRORS
#include "msgFormats.inc"
#undef MSG_GROUP_ERRORS
      end subroutine

      subroutine outputTexture(info)
      use altay
      integer, intent(out)     :: info
      !
            call outputCurrentState(info)
      !
      end subroutine

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

    pure function toString(num) result(str)
        class(*), intent(in)    :: num
        character(12):: str

        select type(num)
            type is (integer)
                write (str, '(I0)') num
            type is (real(DP))
                write (str, '(G10.4)') num
        end select

        str = trim(str)
    end function

    !> @brief Convert a voigt vector to tensor representation.
    !> @return Real 3x3 matrix containing the tensor representation of the voigt vector.
    pure function convert_voigt_vec_mat(vec) result(mat)
        real(DP), dimension(:), intent(in):: vec !< Voigt vector. Must be of size 6 or 9. If size is 6, it is assumed to represent a
                                                 !! stress or strain and the resulting tensor will be symmetrical. If size is 9, it
                                                 !! is assumed to be a velocity or deformation radient and the result matrix contains all
                                                 !! elements of the vector.
        real(DP), dimension(3, 3):: mat

        mat(1, 1) = vec(1)
        mat(2, 2) = vec(2)
        mat(3, 3) = vec(3)
        mat(1, 2) = vec(4)
        mat(2, 3) = vec(5)
        mat(3, 1) = vec(6)
        if (size(vec) == 6) then
            mat(1, 3) = mat(3, 1)
            mat(2, 1) = mat(1, 2)
            mat(3, 2) = mat(2, 3)
        else
            mat(1, 3) = vec(7)
            mat(2, 1) = vec(8)
            mat(3, 2) = vec(9)
        end if 
    end function

    !> @brief Convert a matrix to voigt notation.
    !> @return Real vector with the voigt representation of the matrix. Its size equals the input argument [length]. If length is 6, 
    !! the vector represents a stress or a strain. If length is 9, the vector represents a deformation or velocity gradient.
    pure function convert_voigt_mat_vec(mat, length) result(vec)
        real(DP), dimension(3, 3), intent(in):: mat !< The input matrix. If length is 6, it is assumed to be symmetrical and its
                                                    !! elements below the diagonal are ignored.
        integer, intent(in):: length                !< Length of the resulting vector. Must be 6 or 9.
        real(DP), dimension(length):: vec

        vec(1) = mat(1, 1)
        vec(2) = mat(2, 2)
        vec(3) = mat(3, 3)
        vec(4) = mat(1, 2)
        vec(5) = mat(2, 3)
        vec(6) = mat(1, 3)
        if (length == 9) then
            vec(7) = mat(2, 1)
            vec(8) = mat(3, 2)
            vec(9) = mat(1, 3)
        end if
    end function
end module
