!> Elementary math constants and functions
!>
!> The module provides a set of typical mathematical constants
!> that are frequently used in various subroutunes in the library.
!> It also provides some simple functions, e.g. conversions.
module criMathUtils
      use definitions
      implicit none


      real(DP), parameter         :: pi  = acos(-1.D0) !< Pi \f$ \pi \f$
      real(DP), parameter         :: pi_deg = pi / 180.D0
      real(DP), parameter         :: deg_pi = 180.D0 / pi

      real(DP), parameter         :: root2 = sqrt(2.D0) !< Square root of 2 \f$ \sqrt{2} \f$
      real(DP), parameter         :: root2i = 0.5D0 * sqrt(2.D0) !< Inverse of square root of 2 \f$ \frac{1}{\sqrt{2}} \f$
      real(DP), parameter         :: root23 = sqrt(2.D0/3.D0) !< Square root of 2/3 \f$ \sqrt{2/3} \f$
      real(DP), parameter         :: root32 = sqrt(3.D0/2.D0) !< Square root of 3/2 \f$ \sqrt{3/2} \f$

      !> Matrix form of the unit second rank tensor
      real(DP), dimension(3,3), parameter :: UNIT_SR_MATRIX = reshape([1._DP, 0._DP, 0._DP, & 
                                                                       0._DP, 1._DP, 0._DP, & 
                                                                       0._DP, 0._DP, 1._DP], [3,3])   
  
      !> Representation of Euler angles: Bunge notation
      type EulerAngles
            real(DP)  :: fi1 = 0.D0 !< \f$ \phi_1 \f$
            real(DP)  :: phi = 0.D0 !< \f$ \Phi \f$
            real(DP)  :: fi2 = 0.D0 !< \f$ \phi_2 \f$
      end type


      !> Generic function for conversion from radians to degrees
      interface rad2deg
            module procedure scalarRad2Deg
      end interface

      !> Generic function for conversion from degrees to radians
      interface deg2rad
            module procedure scalarDeg2Rad, EulerAnglesDeg2Rad
      end interface

      interface rotmat
            module procedure rotmat_triplet, rotmat_EulerAngles, rotmat_array
      end interface

contains

      !> Conversion from radians to degrees
      elemental function scalarRad2Deg(alpha)
      real(DP) :: scalarRad2Deg  !< Angle in radians
      real(DP),intent(in) :: alpha
      !
           scalarRad2Deg = alpha * deg_pi
      !
      end function

      !> Conversion from degrees to radians
      elemental function scalarDeg2Rad(alpha)
      real(DP) :: scalarDeg2Rad !< Angle in degrees
      real(DP),intent(in) :: alpha
      !
            scalarDeg2Rad = alpha *  pi_deg
      !
      end function

      !> Conversion from degrees to radians
      elemental type(EulerAngles) function EulerAnglesDeg2Rad(ang)
      type(EulerAngles), intent(in)       :: ang
      !
            EulerAnglesDeg2Rad%fi1 = deg2rad(ang%fi1)
            EulerAnglesDeg2Rad%phi = deg2rad(ang%phi)
            EulerAnglesDeg2Rad%fi2 = deg2rad(ang%fi2)
      !
      end function

      !> Trivial conversion from EulerAngles to array of rank 1, dimension 3
      pure function EulerAngles2Arr(ang) result(arr)
      real(DP),dimension(3) :: arr
      type(EulerAngles),intent(in)  :: ang
      !
            arr(1) = ang%fi1
            arr(2) = ang%phi
            arr(3) = ang%fi2
      !
      end function

      !> Trivial conversion from  array of rank 1, dimension 3 to EulerAngles
      pure function Arr2EulerAngles(arr) result(ang)
      type(EulerAngles)  :: ang
      real(DP),dimension(3),intent(in) :: arr
      !
            ang%fi1 = arr(1)
            ang%phi = arr(2)
            ang%fi2 = arr(3)
      !
      end function

      !> Calculates an angle between two vectors
      !>
      !> \returns Value zero if:
      !>          *  vectors are parallel to each other
      !>          OR
      !>          *  vectors are of different dimensionality
      !>          *  at least one of the vectors u or v has length 0
      pure real(DP) function vec_angle(u,v)
      real(DP),dimension(:),intent(in)   :: u, v
      ! Declaration section
      real(DP) :: cosine
      !
            cosine = vec_cosine(u,v)
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
      pure real(DP) function vec_cosine(u,v)
      real(DP),dimension(:),intent(in)   :: u, v
      ! Declaration section
      real(DP) :: udp, vdp
      !
            vec_cosine = 1.D0
            if (size(u) /= size(v)) return
            udp = dot_product(u,u)
            vdp = dot_product(v,v)
            ! Check the conditions: neither u nor v can be of length zero
            if ((udp*vdp <= epsilon(0.D0))) return
            vec_cosine = dot_product(u,v) / sqrt(udp*vdp)
            ! Check for detrimental roundoff conditions
            if (vec_cosine >= 1.D0) then
                  vec_cosine = 1.D0
            elseif (vec_cosine <= -1.D0) then
                  vec_cosine = -1.D0
            endif
      !
      end function

    pure function rotmat_array(angles) result(mat)
        real(DP), dimension(3), intent(in) :: angles
        real(DP), dimension(3,3) :: mat

        mat = rotmat(angles(1), angles(2), angles(3))
    end function

      !> Rotation matrix from three Euler angles in Bunge convention (phi1,PHI,phi2).
      !>
      !> The matrix R is equivalent to superpositions of three individual
      !> rotations around Z1 (by phi1), X (by PHI) and Z2(by phi2) (in this order).
      !> In terms of matrix multiplication, the total rotation matrix R is given by:
      !> R = R_{phi2} * R_{PHI} * R_{phi1}
      !> \returns [3x3] rotation matrix R.
      pure function rotmat_triplet(phi1, PHI, phi2) result(mat)
      real(DP), intent(in)        :: phi1, PHI, phi2
      real(DP), dimension(3,3)    :: mat
      !
      real(DP) :: cosphi1, cosphi2, cosPHI
      real(DP) :: sinphi1, sinphi2, sinPHI
      !
            cosphi1 = cos(phi1)
            cosPHI = cos(PHI)
            cosphi2 = cos(phi2)
            !
            sinphi1 = sin(phi1)
            sinPHI = sin(PHI)
            sinphi2 = sin(phi2)
            !
            mat(1,1) = cosphi1*cosphi2 - (sinphi1*sinphi2*cosPHI)
            mat(1,2) = sinphi1*cosphi2 + (cosphi1*sinphi2*cosPHI)
            mat(1,3) = sinphi2*sinPHI
            mat(2,1) = -cosphi1*sinphi2 - (sinphi1*cosphi2*cosPHI)
            mat(2,2) = -sinphi1*sinphi2 + (cosphi1*cosphi2*cosPHI)
            mat(2,3) = cosphi2*sinPHI
            mat(3,1) = sinphi1*sinPHI
            mat(3,2) = -cosphi1*sinPHI
            mat(3,3) = cosPHI
      !
      end function

      !> Rotation matrix from three Euler angles in Bunge convention
      pure function rotmat_EulerAngles(ang) result(mat)
      real(DP), dimension(3,3)    :: mat
      type(EulerAngles),intent(in) :: ang
      !
            mat = rotmat(ang%fi1, ang%PHI, ang%fi2 )
      !
      end function

      !> Three Euler angles in Bunge convention (ang) from rotation matrix (mat).
      !>
      !> The outputted Euler angles are in radians and lie within these bounds:
      !>    ang%fi1: [0,2*pi[
      !>    ang%PHI: [0,  pi[
      !>    ang%fi2: [0,2*pi[   note: if PHI=0 then phi2=0
      pure function EulerAnglesType(mat) result(ang)
      real(DP), dimension(3,3),intent(in) :: mat
      type(EulerAngles) :: ang
      !
      real(DP) :: phi1,PHI,phi2,cosPHI
      !
          cosPHI = mat(3,3) / sqrt( mat(1,3)**2 + mat(2,3)**2 + mat(3,3)**2 )
          PHI = acos(cosPHI) !range: [0,pi]
          !
          if (abs(cosPHI)==1.0D0) then !case that PHI=0\B0 or PHI=180\B0
              !Set phi2 to 0.0D0, given that:
              !  (phi1;   0\B0; phi2) equivalent to (phi1+phi2;    0; 0).
              !  (phi1; 180\B0; phi2) equivalent to (phi1+phi2; 180\B0; 0).
              phi2 = 0.0D0
              phi1 = atan2(-mat(2,1)/cosPHI,mat(2,2)/cosPHI) !range: [-pi,pi[
          else
              phi1 = atan2(mat(3,1),-mat(3,2)) !range: [-pi,pi[
              phi2 = atan2(mat(1,3),mat(2,3)) !range: [-pi,pi[
          end if
          
          !If needed, replace Euler angles with equivalent values within proper bounds.
          if (PHI==pi)     PHI  = 0.0D0         ![  0,pi] -> [0,  pi[
          if (phi1<0.0D0) phi1 = phi1+2.D0*pi   ![-pi,pi[ -> [0,2*pi[
          if (phi2<0.0D0) phi2 = phi2+2.D0*pi   ![-pi,pi[ -> [0,2*pi[
          
          ang%fi1= phi1
          ang%PHI= PHI
          ang%fi2= phi2
      end function

    !> Rotates the second-rank tensor S to the reference frame given by rotation R.
    pure function rotateSRTensorTo(S,R) result(Srot)
        real(DP), dimension(3,3), intent(in)    ::  S, &
                                                    R
        real(DP), dimension(3,3)                ::  Srot

        Srot = matmul(matmul(transpose(R),S),R)
    end function

    !> Rotates the second-rank tensor S back from the reference frame given by rotation R.
    pure function rotateSRTensorFrom(S,R) result(Srot)
        real(DP), dimension(3,3), intent(in)    ::  S, &
                                                    R
        real(DP), dimension(3,3)                ::  Srot
      
        Srot = matmul(matmul(R,S),transpose(R))
    end function

    !> The function converts the antisymmetrical rank-two tensors mat into Voigt-style vector representation.
    pure function Mat33ToVec3(mat) result(vec)
        real(DP),dimension(3,3),intent(in)  :: mat
        real(DP),dimension(3) :: vec

        vec(1) = mat(1,2)
        vec(2) = mat(2,3)
        vec(3) = mat(1,3)
    end function

    !> The function converts Voigt-style vector vec into symmetrical rank-two tensor.
    pure function Vec6ToMat33(vec) result(mat)
        real(DP), dimension(6), intent(in)  :: vec
        real(DP), dimension(3,3)            :: mat

        mat(1,1) = vec(1)
        mat(2,2) = vec(2)
        mat(3,3) = vec(3)
        mat(1,2) = vec(4)
        mat(2,3) = vec(5)
        mat(3,1) = vec(6)
        mat(1,3) = mat(3,1)
        mat(2,1) = mat(1,2)
        mat(3,2) = mat(2,3)
    end function

      !> The function converts the symmetrical rank-two tensors mat into Voigt-style vector representation.
    pure function Mat33ToVec6(mat) result(vec)
        real(DP), dimension(3,3), intent(in)    :: mat
        real(DP), dimension(6)                  :: vec
      
        vec(1) = mat(1,1)
        vec(2) = mat(2,2)
        vec(3) = mat(3,3)
        vec(4) = mat(1,2)
        vec(5) = mat(2,3)
        vec(6) = mat(1,3)
    end function

      !> The function converts Voigt-style vector vec into rank-two tensor.
    pure function Vec9ToMat33(vec) result(mat)
        real(DP),dimension(9),intent(in)  :: vec
        real(DP),dimension(3,3)   :: mat
      
        mat(1,1) = vec(1)
        mat(2,2) = vec(2)
        mat(3,3) = vec(3)
        mat(1,2) = vec(4)
        mat(2,3) = vec(5)
        mat(3,1) = vec(6)
        mat(2,1) = vec(7)
        mat(3,2) = vec(8)
        mat(1,3) = vec(9)
    end function

    !> The function converts the rank-two tensor mat into Voigt-style vector representation.
    pure function Mat33ToVec9(mat) result(vec)
        real(DP), dimension(3,3), intent(in)    :: mat
        real(DP), dimension(9)                  :: vec
      
        vec(1) = mat(1,1)
        vec(2) = mat(2,2)
        vec(3) = mat(3,3)
        vec(4) = mat(1,2)
        vec(5) = mat(2,3)
        vec(6) = mat(1,3)
        vec(7) = mat(2,1)
        vec(8) = mat(3,2)
        vec(9) = mat(1,3)
    end function

    !> Calculates trace of the square n x n matrix X
    pure real(DP) function trace(X) result(res)
        real(DP), dimension(:,:), intent(in)    :: X
        integer :: i
         
        res = 0._DP
        do i = 1, minval(shape(X))
            res = res + X(i,i)
        enddo
    end function

    !> Calculate vector v that is normal to the vector AB (from point A to B).
    !> Provide the angle between the vector v and the x axis.
    !> v is obtained by a clockwise rotation by 90 degs applied to the AB vector.
    subroutine getNormalVector2D(A,B,length,v,beta)
        real(DP), intent(in) :: A(2), B(2), length 
        real(DP),intent(out) :: v(2)       
        !> Angle between the horizontal axis and the vector u [radians]
        !> The range of the angle is [0:2pi], thus it may vary from acute angle
        ! via obtuse angle to reflex angle.
        real(DP),intent(out)  :: beta
        
        real(DP),dimension(2) :: u
        real(DP) :: u_norm
    
        ! Build the secant vector
        u = b - a
        u_norm = norm2(u)
        if (u_norm > epsilon(0._DP)) then
            ! Build the normal vector. Anticlockwise rotation by 90degs
            ! gives [-u_y, u_x]. Apply the clockwise rotation by 90degs:
            u = [u(2), -u(1)]
            beta = acos(u(1) / u_norm)
            ! Let the vectors that point "downwards" have beta angle > 180deg
            if (u(2) < 0._DP) beta = 2._DP*pi - beta
            v = u / u_norm * length
        else
            ! ouups, the points C and A overlap!
            beta = 0._DP
            v = 0._DP 
        endif
    end subroutine

    !> Calculate the real roots of quadratic polynomial given in form
    !> a^2 x + b x + c = 0
    !> Provides x1 and x2. Both x1 and x2 are guaranteed to be set to a defined value,
    !> even if no real roots exist.
    integer function solveQuadraticPolynomial(a, b, c, x) result(n_roots)
        real(DP),intent(in)   :: a, b, c
        real(DP),dimension(2),intent(out)  :: x
        real(DP) :: delta
      
        ! Satisfy intent(out)
        x = 0.D0
        n_roots = 0
        if (abs(a) > tiny(0.D0)) then
              delta = b**2 - 4.D0 * a * c
              if (delta >= 0) then
                    x(1) = 0.5D0 * (-b - sqrt(delta)) / a
                    x(2) = 0.5D0 * (-b + sqrt(delta)) / a
                    n_roots = 2
              endif
        else
              ! Solve linear equation b x = -c
              if (abs(a) > epsilon(0.D0)) then
                    x(1) = -c / b
                    n_roots = 1
              endif
        endif
    end function

    !> Convert 5D vector v into second-rank tensor
    pure function vec5D2tens(v) result(t)
        real(DP),dimension(5),intent(in)    :: v
        real(DP),dimension(3,3)             :: t
        real(DP),parameter ::  root6i = 1.D0/sqrt(6.D0)
      
        t(1,1) =  root2i*v(1) + root6i*v(2)
        t(2,2) = -root2i*v(1) + root6i*v(2)
        t(3,3) = -root23*v(2)
        t(2,3) =  root2i*v(3)
        t(3,1) =  root2i*v(4)
        t(1,2) =  root2i*v(5)
        ! Make tensor symmetric
        t(3,2) = t(2,3)
        t(1,3) = t(3,1)
        t(2,1) = t(1,2)
    end function

    !> Convert second-rank tensor t into 5D vector.
    !> \remark If the tensor v is not of deviatoric nature,
    !> the deviator will be extracted and used in calculations.
    pure function tens2vec5D(t) result(v)
        real(DP),dimension(3,3),intent(in)   :: t
        real(DP),dimension(5)                :: v
        real(DP),dimension(3,3)   :: x !< Temporary
        real(DP) :: p ! Pressure
    
        x = t ! set temporary
        p = (t(1,1) + t(2,2) + t(3,3)) / 3.D0
        ! Make the temporary traceless by substracting the pressure
        if (abs(p) > epsilon(0.D0)) then
            x(1,1) = t(1,1) - p
            x(2,2) = t(2,2) - p
            x(3,3) = t(3,3) - p
        endif
        v(1) =  root2i*(x(1,1) - x(2,2))
        v(2) = -root32*x(3,3)
        v(3) =  root2*x(2,3)
        v(4) =  root2*x(3,1)
        v(5) =  root2*x(1,2)
    end function

    real(DP) pure function average(a)
        real(DP),dimension(:),intent(in) :: a
        integer :: n
      
        n = size(a)
        if (n >= 1) average = sum(a) / dble(n)
    end function

    pure function cross(v1,v2)
        real(DP), intent(in), dimension(3) :: v1, v2
        real(DP), dimension(3) :: cross

        cross(1)=v1(2)*v2(3)-v1(3)*v2(2)
        cross(2)=v1(3)*v2(1)-v1(1)*v2(3)
        cross(3)=v1(1)*v2(2)-v1(2)*v2(1)
    end function
end module
