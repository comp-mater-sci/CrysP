!> Elementary math constants and functions
!>
!> The module provides a set of typical mathematical constants
!> that are frequently used in various subroutunes in the library.
!> It also provides some simple functions, e.g. conversions.
module criMathUtils
    use utils
    implicit none


    real(DP), parameter         :: pi  = acos(-1.D0) !< Pi \f$ \pi \f$
    real(DP), parameter         :: pi_deg = pi/180.D0
    real(DP), parameter         :: deg_pi = 180.D0/pi

    real(DP), parameter         :: root2 = sqrt(2.D0) !< Square root of 2 \f$ \sqrt{2} \f$
    real(DP), parameter         :: root2i = 0.5D0*sqrt(2.D0) !< Inverse of square root of 2 \f$ \frac{1}{\sqrt{2}} \f$
    real(DP), parameter         :: root23 = sqrt(2.D0/3.D0) !< Square root of 2/3 \f$ \sqrt{2/3} \f$
    real(DP), parameter         :: root32 = sqrt(3.D0/2.D0) !< Square root of 3/2 \f$ \sqrt{3/2} \f$

    !> Generic function for conversion from radians to degrees
    interface rad2deg
        module procedure scalarRad2Deg
    end interface

    !> Generic function for conversion from degrees to radians
    interface deg2rad
        module procedure scalarDeg2Rad
    end interface

contains

      !> Conversion from radians to degrees
      elemental function scalarRad2Deg(alpha)
      real(DP):: scalarRad2Deg  !< Angle in radians
      real(DP), intent(in):: alpha
      !
           scalarRad2Deg = alpha*deg_pi
      !
      end function

      !> Conversion from degrees to radians
      elemental function scalarDeg2Rad(alpha)
      real(DP):: scalarDeg2Rad !< Angle in degrees
      real(DP), intent(in):: alpha
      !
            scalarDeg2Rad = alpha *  pi_deg
      !
      end function

      !> Calculates an angle between two vectors
      !>
      !> \returns Value zero if:
      !>          *  vectors are parallel to each other
      !>          OR
      !>          *  vectors are of different dimensionality
      !>          *  at least one of the vectors u or v has length 0
      pure real(DP) function vec_angle(u, v)
      real(DP), dimension(:), intent(in)   :: u, v
      ! Declaration section
      real(DP):: cosine
      !
            cosine = vec_cosine(u, v)
            ! calculate angle (in radians)
            vec_angle = acos(cosine)
      !
      end function

      !> Calculates a cosine of angle between two vectors. The function guarantees that
      !> the result is within range [-1:1]
      !>
      !> \returns Value 1.0 if:
      !>          *  vectors are parallel to each other
      !>          OR
      !>          *  vectors are of different dimensionality
      !>          *  at least one of the vectors u or v has length 0
      pure real(DP) function vec_cosine(u, v)
      real(DP), dimension(:), intent(in)   :: u, v
      ! Declaration section
      real(DP):: udp, vdp
      !
            vec_cosine = 1.D0
            if (size(u) /= size(v)) return
            udp = dot_product(u, u)
            vdp = dot_product(v, v)
            ! Check the conditions: neither u nor v can be of length zero
            if ((udp*vdp <= epsilon(0.D0))) return
            vec_cosine = dot_product(u, v) / sqrt(udp*vdp)
            ! Check for detrimental roundoff conditions
            if (vec_cosine >= 1.D0) then
                  vec_cosine = 1.D0
            elseif (vec_cosine <= -1.D0) then
                  vec_cosine = -1.D0
            endif
      !
      end function

      !> Rotation matrix from three Euler angles in Bunge convention (phi1, PHI, phi2).
      !>
      !> The matrix R is equivalent to superpositions of three individual
      !> rotations around Z1 (by phi1), X (by PHI) and Z2(by phi2) (in this order).
      !> In terms of matrix multiplication, the total rotation matrix R is given by:
      !> R = R_{phi2} * R_{PHI} * R_{phi1}
      !> \returns [3x3] rotation matrix R.
      pure function euler_angles_to_rotation_matrix(angles) result(mat)
      real(DP), intent(in)        :: angles(3)
      real(DP), dimension(3, 3)    :: mat
      real(DP):: cos_phi1, cos_phi2, cos_PHI, &
                 sin_phi1, sin_phi2, sin_PHI
      
            cos_phi1 = cos(angles(1))
            cos_PHI = cos(angles(2))
            cos_phi2 = cos(angles(3))
            sin_phi1 = sin(angles(1))
            sin_PHI = sin(angles(2))
            sin_phi2 = sin(angles(3))
            
            mat(1, 1) = cos_phi1*cos_phi2 - (sin_phi1*sin_phi2*cos_PHI)
            mat(1, 2) = sin_phi1*cos_phi2 + (cos_phi1*sin_phi2*cos_PHI)
            mat(1, 3) = sin_phi2*sin_PHI
            mat(2, 1) = -cos_phi1*sin_phi2 - (sin_phi1*cos_phi2*cos_PHI)
            mat(2, 2) = -sin_phi1*sin_phi2 + (cos_phi1*cos_phi2*cos_PHI)
            mat(2, 3) = cos_phi2*sin_PHI
            mat(3, 1) = sin_phi1*sin_PHI
            mat(3, 2) = -cos_phi1*sin_PHI
            mat(3, 3) = cos_PHI
      end function


    !> Three Euler angles in Bunge convention (ang) from rotation matrix (mat).
    !>
    !> The outputted Euler angles are in radians and lie within these bounds:
    !>    ang%fi1: [0, 2*pi[
    !>    ang%PHI: [0,  pi[
    !>    ang%fi2: [0, 2*pi[   note: if PHI = 0 then phi2 = 0
    pure function rotation_matrix_to_euler_angles(mat) result(ang)
        real(DP), intent(in)::  mat(3, 3)
        real(DP)::              ang(3), &
                                phi1, &
                                PHI, &
                                phi2, &
                                cos_PHI
      
        cos_PHI = mat(3, 3) / sqrt( mat(1, 3)**2+mat(2, 3)**2+mat(3, 3)**2 )
        PHI = acos(cos_PHI)  ! range: [0, pi]
        
        if (abs(cos_PHI)==1.0D0) then  ! case that PHI = 0\B0 or PHI = 180\B0
            !Set phi2 to 0.0D0, given that:
            !  (phi1;   0\B0; phi2) equivalent to (phi1+phi2;    0; 0).
            !  (phi1; 180\B0; phi2) equivalent to (phi1+phi2; 180\B0; 0).
            phi2 = 0.0D0
            phi1 = atan2(-mat(2, 1)/cos_PHI, mat(2, 2)/cos_PHI)  ! range: [-pi, pi[
        else
            phi1 = atan2(mat(3, 1), -mat(3, 2))  ! range: [-pi, pi[
            phi2 = atan2(mat(1, 3), mat(2, 3))  ! range: [-pi, pi[
        end if

        !If needed, replace Euler angles with equivalent values within proper bounds.
        if (PHI == PI)     PHI  = 0._DP       ![  0, pi] -> [0,  pi[
        if (phi1 < 0._DP) phi1 = phi1+2._DP*PI   ![-pi, pi[ -> [0, 2*pi[
        if (phi2 < 0._DP) phi2 = phi2+2._DP*PI   ![-pi, pi[ -> [0, 2*pi[

        ang(1) = phi1
        ang(2) = phi
        ang(3) = phi2
    end function

    !> The function converts the antisymmetrical rank-two tensors mat into Voigt-style vector representation.
    pure function Mat33ToVec3(mat) result(vec)
        real(DP), dimension(3, 3), intent(in)  :: mat
        real(DP), dimension(3):: vec

        vec(1) = mat(1, 2)
        vec(2) = mat(2, 3)
        vec(3) = mat(1, 3)
    end function

    !> The function converts Voigt-style vector vec into symmetrical rank-two tensor.
    pure function Vec6ToMat33(vec) result(mat)
        real(DP), dimension(6), intent(in)  :: vec
        real(DP), dimension(3, 3)            :: mat

        mat(1, 1) = vec(1)
        mat(2, 2) = vec(2)
        mat(3, 3) = vec(3)
        mat(1, 2) = vec(4)
        mat(2, 3) = vec(5)
        mat(3, 1) = vec(6)
        mat(1, 3) = mat(3, 1)
        mat(2, 1) = mat(1, 2)
        mat(3, 2) = mat(2, 3)
    end function

      !> The function converts the symmetrical rank-two tensors mat into Voigt-style vector representation.
    pure function Mat33ToVec6(mat) result(vec)
        real(DP), dimension(3, 3), intent(in)    :: mat
        real(DP), dimension(6)                  :: vec
      
        vec(1) = mat(1, 1)
        vec(2) = mat(2, 2)
        vec(3) = mat(3, 3)
        vec(4) = mat(1, 2)
        vec(5) = mat(2, 3)
        vec(6) = mat(1, 3)
    end function

      !> The function converts Voigt-style vector vec into rank-two tensor.
    pure function Vec9ToMat33(vec) result(mat)
        real(DP), dimension(9), intent(in)  :: vec
        real(DP), dimension(3, 3)   :: mat
      
        mat(1, 1) = vec(1)
        mat(2, 2) = vec(2)
        mat(3, 3) = vec(3)
        mat(1, 2) = vec(4)
        mat(2, 3) = vec(5)
        mat(3, 1) = vec(6)
        mat(2, 1) = vec(7)
        mat(3, 2) = vec(8)
        mat(1, 3) = vec(9)
    end function

    !> The function converts the rank-two tensor mat into Voigt-style vector representation.
    pure function Mat33ToVec9(mat) result(vec)
        real(DP), dimension(3, 3), intent(in)    :: mat
        real(DP), dimension(9)                  :: vec
      
        vec(1) = mat(1, 1)
        vec(2) = mat(2, 2)
        vec(3) = mat(3, 3)
        vec(4) = mat(1, 2)
        vec(5) = mat(2, 3)
        vec(6) = mat(1, 3)
        vec(7) = mat(2, 1)
        vec(8) = mat(3, 2)
        vec(9) = mat(1, 3)
    end function

    !> Calculates trace of the square n x n matrix X
    pure real(DP) function trace(X) result(res)
        real(DP), dimension(:,:), intent(in)    :: X
        integer:: i
         
        res = 0._DP
        do i = 1, minval(shape(X))
            res = res+X(i, i)
        enddo
    end function

    !> Calculate vector v that is normal to the vector AB (from point A to B).
    !> Provide the angle between the vector v and the x axis.
    !> v is obtained by a clockwise rotation by 90 degs applied to the AB vector.
    subroutine getNormalVector2D(A, B, length, v, beta)
        real(DP), intent(in):: A(2), B(2), length 
        real(DP), intent(out):: v(2)       
        !> Angle between the horizontal axis and the vector u [radians]
        !> The range of the angle is [0:2pi], thus it may vary from acute angle
        ! via obtuse angle to reflex angle.
        real(DP), intent(out)  :: beta
        
        real(DP), dimension(2):: u
        real(DP):: u_norm
    
        ! Build the secant vector
        u = b-a
        u_norm = norm2(u)
        if (u_norm > epsilon(0._DP)) then
            ! Build the normal vector. Anticlockwise rotation by 90degs
            ! gives [-u_y, u_x]. Apply the clockwise rotation by 90degs:
            u = [u(2), -u(1)]
            beta = acos(u(1) / u_norm)
            ! Let the vectors that point "downwards" have beta angle > 180deg
            if (u(2) < 0._DP) beta = 2._DP*pi-beta
            v = u/u_norm*length
        else
            ! ouups, the points C and A overlap!
            beta = 0._DP
            v = 0._DP 
        endif
    end subroutine

    !> Calculate the real roots of quadratic polynomial given in form
    !> a^2 x+b x+c = 0
    !> Provides x1 and x2. Both x1 and x2 are guaranteed to be set to a defined value, 
    !> even if no real roots exist.
    integer function solveQuadraticPolynomial(a, b, c, x) result(n_roots)
        real(DP), intent(in)   :: a, b, c
        real(DP), dimension(2), intent(out)  :: x
        real(DP):: delta
      
        ! Satisfy intent(out)
        x = 0.D0
        n_roots = 0
        if (abs(a) > tiny(0.D0)) then
              delta = b**2 - 4.D0*a * c
              if (delta >= 0) then
                    x(1) = 0.5D0 * (-b-sqrt(delta)) / a
                    x(2) = 0.5D0 * (-b+sqrt(delta)) / a
                    n_roots = 2
              endif
        else
              ! Solve linear equation b x = -c
              if (abs(a) > epsilon(0.D0)) then
                    x(1) = -c/b
                    n_roots = 1
              endif
        endif
    end function

    real(DP) pure function average(a)
        real(DP), dimension(:), intent(in):: a
        integer:: n
      
        n = size(a)
        if (n >= 1) average = sum(a) / dble(n)
    end function

end module
