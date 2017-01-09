!
! $Id$
!
!>    \author Jerzy Gawad
!>    Email:  Jerzy.Gawad@cs.kuleuven.be
!>    \credits Philip Eyckens (EulerAnglesType)
!>
!>    Organization: Katholieke Universiteit Leuven (KU Leuven)
!>    Organization unit: Dept.Comp.Sci., TWR Group
!>    \copyright KU Leuven
!>
!>    \date Date of first release: 2010-08-02
!>    $Revision$
!>    $Date$
!>
!>    History of modifications: (see svn log)
!>
!>    \file criMathUtils.f90 
!
#include "criStdDefs.fpp"
!
!> Elementary math constants and functions
!>
!> The module provides a set of typical mathematical constants 
!> that are frequently used in various subroutunes in the library.
!> It also provides some simple functions, e.g. conversions.
module criMathUtils
      
      !>@{ \name Math constants
      
      !> Pi \f$ \pi \f$
      double precision, parameter         :: pi  = acos(-1.D0) !
      
      !> Pi / 2 
      double precision, parameter         :: pi2 = acos(0.D0) !

      double precision, parameter         :: pi_deg = pi / 180.D0

      double precision, parameter         :: deg_pi = 180.D0 / pi

      !> Square root of 2 \f$ \sqrt{2} \f$
      double precision, parameter         :: root2 = sqrt(2.D0)
      
      !> Inverse of square root of 2 \f$ \frac{1}{\sqrt{2}} \f$
      double precision, parameter         :: root2i = 0.5D0 * sqrt(2.D0)

      !> Square root of 2/3 \f$ \sqrt{2/3} \f$
      double precision, parameter         :: root23 = sqrt(2.D0/3.D0)
      
      !> Square root of 3/2 \f$ \sqrt{3/2} \f$
      double precision, parameter         :: root32 = sqrt(3.D0/2.D0)

      !> Array dimension for second-rank tensors (sr_tensor_dim x sr_tensor_dim)
      integer,parameter                   :: sr_tensor_dim = 3
      
      !> Array dimension for 3D rotation matrix (rot_matrix_dim x rot_matrix_dim)
      integer,parameter                   :: rot_matrix_dim = 3
      
      !> Array dimension for symmetric 3D second-rank tensors expressed in Voigt
      !> notation.
      integer,parameter                   :: sr_symm_voigt_dim = 6

      !> Matrix form of the unit second rank tensor
      double precision,dimension(sr_tensor_dim,sr_tensor_dim),parameter :: unit_sr_Matrix = reshape( &
           [ 1.D0, 0.D0, 0.D0,     &
             0.D0, 1.D0, 0.D0,     &
             0.D0, 0.D0, 1.D0], [ sr_tensor_dim, sr_tensor_dim ])
      !>@}

      !> Type for 2nd rank tensors in matrix notation. The matrix is initially filled 
      !> with zeros.
      type :: SRTensor
            !> matrix representation of the tensor (t stands for tensor)
            double precision,dimension(sr_tensor_dim,sr_tensor_dim) :: t = 0.D0
      end type
      
      type(SRTensor),parameter :: unit_sr_tensor = SRTensor(unit_sr_Matrix)
      
      !> Rotate the 2nd-rank tensor S to the reference frame given by rotation R.
      interface rotateSRTensorTo
            module procedure rotateSRTensorTo_matrix, rotateSRTensorTo_SRTensor
      end interface
      
      !> Rotate the 2nd-rank tensor S back from the reference frame given by rotation R.
      interface rotateSRTensorFrom
            module procedure rotateSRTensorFrom_matrix, rotateSRTensorFrom_SRTensor
      end interface
      
      !> \interface ocross_product Vector-Vector ocross product operator
      !>
      !> The result of m = ocross_product(u,v) is equivalent to:
      !> \f$ \mathbf{m} = \mathbf{u}^T \mathbf{v} \f$  where \f$\mathbf{u}\f$ and \f$\mathbf{v}\f$  are vectors.
      interface ocross_product
            module procedure ocross_product_dp, ocross_product_int
      end interface ocross_product

      !> \interface vector_product Vector-Vector ovector product operator
      interface vector_product
            module procedure vector_product_dp
      end interface vector_product
      
      !> Representation of Euler angles: Bunge notation
      type EulerAngles
            double precision  :: fi1 = 0.D0 !< \f$ \phi_1 \f$
            double precision  :: phi = 0.D0 !< \f$ \Phi \f$
            double precision  :: fi2 = 0.D0 !< \f$ \phi_2 \f$
      end type
      
      
      !> Generic function for conversion from radians to degrees
      interface rad2deg
            module procedure scalarRad2Deg,EulerAnglesRad2Deg
      end interface
      
      !> Generic function for conversion from degrees to radians
      interface deg2rad
            module procedure scalarDeg2Rad, EulerAnglesDeg2Rad
      end interface

      !> Datatype representing pair of double precision reals
      type pair_double
            double precision :: x
            double precision :: y
      end type
      
      interface rotmat
            module procedure rotmat_triplet, rotmat_EulerAngles
      end interface

      interface trace
            module procedure trace_matrix, trace_SRTensor
      end interface
      
#ifndef FORT_HAS_NORM2
      !> A substitute for the norm2 intrinsic for ifort 11.1 and older.
      !> 
      !> \note This implementation has important limitations compared to
      !> the intrinsic norm2:
      !> - only for rank 1 and 2 arrays can be handled,
      !> - only double precision reals can be used. 
      interface norm2
          module procedure vec_norm2, FrobeniusNorm
      end interface
#endif
      
contains

      !> Conversion from radians to degrees
      elemental function scalarRad2Deg(alpha)
      implicit none
      double precision :: scalarRad2Deg  !< Angle in radians
      double precision,intent(in) :: alpha
      !
           scalarRad2Deg = alpha * deg_pi
      !
      end function

      !> Conversion from degrees to radians
      elemental function scalarDeg2Rad(alpha)
      implicit none
      double precision :: scalarDeg2Rad !< Angle in degrees
      double precision,intent(in) :: alpha
      !
            scalarDeg2Rad = alpha *  pi_deg
      !
      end function

      !> Conversion from radians to degrees
      elemental type(EulerAngles) function EulerAnglesRad2Deg(ang)
      implicit none
      type(EulerAngles), intent(in)       :: ang
      !
            EulerAnglesRad2Deg%fi1 = rad2deg(ang%fi1)
            EulerAnglesRad2Deg%phi = rad2deg(ang%phi)
            EulerAnglesRad2Deg%fi2 = rad2deg(ang%fi2)
      !
      end function

      !> Conversion from degrees to radians
      elemental type(EulerAngles) function EulerAnglesDeg2Rad(ang)
      implicit none
      type(EulerAngles), intent(in)       :: ang
      !
            EulerAnglesDeg2Rad%fi1 = deg2rad(ang%fi1)
            EulerAnglesDeg2Rad%phi = deg2rad(ang%phi)
            EulerAnglesDeg2Rad%fi2 = deg2rad(ang%fi2)
      !
      end function

      !> Trivial conversion from EulerAngles to array of rank 1, dimension 3
      pure function EulerAngles2Arr(ang) result(arr) 
      implicit none
      double precision,dimension(3) :: arr
      type(EulerAngles),intent(in)  :: ang
      !
            arr(1) = ang%fi1
            arr(2) = ang%phi
            arr(3) = ang%fi2
      !
      end function

      !> Trivial conversion from  array of rank 1, dimension 3 to EulerAngles
      pure function Arr2EulerAngles(arr) result(ang) 
      implicit none
      type(EulerAngles)  :: ang
      double precision,dimension(3),intent(in) :: arr
      !
            ang%fi1 = arr(1) 
            ang%phi = arr(2) 
            ang%fi2 = arr(3)
      !
      end function



#ifdef FLAWED_IFORT_OPT
      !> Calculation of ocross_product for double precision real
      !>
      !> \implements ocross_product
      pure function ocross_product_dp(a,b)
      implicit none
      double precision,dimension(5),intent(in)        :: a,b
      double precision,dimension(5,5)                 :: ocross_product_dp
      !integer :: i
      !forall (i = 1:5)
      !       ocross_product_dp(i,:) = a(i) * b 
      !       !ocross_product_dp(i,i:) = a(i) * b(i:)
      !       !ocross_product_dp(i:,i) = ocross_product_dp(i,i:) 
      !endforall
      integer :: i,j
      !
            do i=1,size(a)
                  !do j = 1, size(b)
                  !      ocross_product_dp(i,j) = a(i) * b(j)
                  !enddo
                  ocross_product_dp(i,:) = a(i) * b(:)
            enddo
      !
      end function
 
      !> Calculation of ocross_product for integer (default kind) 
      !>
      !> \implements ocross_product
      pure function ocross_product_int(a,b)
      implicit none
      integer,dimension(5),intent(in)                 :: a,b
      integer,dimension(5,5)                          :: ocross_product_int
      integer :: i
      !
            forall (i = 1:5)
                  ocross_product_int(i,:) = a(i) * b
            endforall
      !
      end function
#else
      ! buggy until ifort is updated
      !> Calculation of ocross_product for double precision real
      !>
      !> \implements ocross_product
      pure function ocross_product_dp(a,b)
      implicit none
      double precision,dimension(:),intent(in)        :: a,b
      double precision,dimension(size(a),size(b))     :: ocross_product_dp
      integer :: i
      !
            forall (i = 1:size(a))
                  ocross_product_dp(i,:) = a(i) * b 
            !      ! ocross_product_dp(i,i:) = a(i) * b(i:)
            !      ! ocross_product_dp(i:,i) = ocross_product_dp(i,i:) 
            endforall
      !
      end function
 
      !> Calculation of ocross_product for integer (default kind) 
      !>
      !> \implements ocross_product
      pure function ocross_product_int(a,b)
      implicit none
      integer,dimension(:),intent(in)                 :: a,b
      integer,dimension(size(a),size(b))              :: ocross_product_int
      integer :: i
      !
            forall (i = 1:size(a))
                  ocross_product_int(i,:) = a(i) * b
            endforall
      !
      end function

#endif
     
      !> Calculation of the vector product of two double precision vectors with size 3.
      pure function vector_product_dp(a,b)
      implicit none
      double precision,dimension(3),intent(in)        :: a,b
      double precision,dimension(3)                   :: vector_product_dp
      integer :: i
      !
            vector_product_dp(1) = a(2)*b(3) - a(3)*b(2)
            vector_product_dp(2) = a(3)*b(1) - a(1)*b(3)
            vector_product_dp(3) = a(1)*b(2) - a(2)*b(1)            
      !
      end function
    
      !> Calculates square norm of vector v
      pure double precision function vec_norm2(v)
      double precision,dimension(:),intent(in)   :: v
      !
            vec_norm2 = sqrt(dot_product(v,v))
      !
      end function



      !> Calculates an angle between two vectors
      !>
      !> \returns Value zero if: 
      !>          *  vectors are parallel to each other
      !>          OR
      !>          *  vectors are of different dimensionality
      !>          *  at least one of the vectors u or v has length 0
      pure double precision function vec_angle(u,v)
      implicit none
      double precision,dimension(:),intent(in)   :: u, v
      ! Declaration section
      double precision :: cosine
      !
            vec_angle = 0.D0
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
      pure double precision function vec_cosine(u,v)
      implicit none
      double precision,dimension(:),intent(in)   :: u, v
      ! Declaration section
      double precision :: udp, vdp
      !
            vec_cosine = 1.D0
            if (size(u) /= size(v)) return
            udp = dot_product(u,u)
            vdp = dot_product(v,v)
            ! Check the conditions: neither u nor v can be of length zero
            if ((udp <= epsilon(0.D0)) .or.  (udp <= epsilon(0.D0))) return
            vec_cosine = dot_product(u,v) / sqrt(udp*vdp)
            ! Check for detrimental roundoff conditions
            if (vec_cosine >= 1.D0) then
                  vec_cosine = 1.D0
            elseif (vec_cosine <= -1.D0) then
                  vec_cosine = -1.D0
            endif
      !
      end function

      
      !> Rotation matrix from three Euler angles in Bunge convention (phi1,PHI,phi2).
      !>
      !> The matrix R is equivalent to superpositions of three individual
      !> rotations around Z1 (by phi1), X (by PHI) and Z2(by phi2) (in this order). 
      !> In terms of matrix multiplication, the total rotation matrix R is given by:
      !> R = R_{phi2} * R_{PHI} * R_{phi1}
      !> \returns [3x3] rotation matrix R. 
      pure function rotmat_triplet(phi1, PHI, phi2) result(mat)
      implicit none
      double precision, intent(in)        :: phi1, PHI, phi2
      double precision, dimension(rot_matrix_dim,rot_matrix_dim)    :: mat
      !
      double precision :: cosphi1, cosphi2, cosPHI
      double precision :: sinphi1, sinphi2, sinPHI
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
      implicit none
      double precision, dimension(rot_matrix_dim,rot_matrix_dim)    :: mat
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
      double precision, dimension(rot_matrix_dim,rot_matrix_dim),intent(in) :: mat
      type(EulerAngles) :: ang
      !
      double precision :: phi1,PHI,phi2,cosPHI
      !
          cosPHI = mat(3,3) / sqrt( mat(1,3)**2 + mat(2,3)**2 + mat(3,3)**2 )
          PHI = acos(cosPHI) !range: [0,pi]
          !
          if (abs(cosPHI)==1.0D0) then !case that PHI=0° or PHI=180°
              !Set phi2 to 0.0D0, given that:
              !  (phi1;   0°; phi2) equivalent to (phi1+phi2;    0; 0).
              !  (phi1; 180°; phi2) equivalent to (phi1+phi2; 180°; 0).
              phi2 = 0.0D0 
              phi1 = atan2(-mat(2,1)/cosPHI,mat(2,2)/cosPHI) !range: [-pi,pi[
          else 
              phi1 = atan2(mat(3,1),-mat(3,2)) !range: [-pi,pi[
              phi2 = atan2(mat(1,3),mat(2,3)) !range: [-pi,pi[
          end if
          !
          !If needed, replace Euler angles with equivalent values within proper bounds.
          if (PHI==pi)     PHI  = 0.0D0         ![  0,pi] -> [0,  pi[
          if (phi1<0.0D0) phi1 = phi1+2.D0*pi   ![-pi,pi[ -> [0,2*pi[
          if (phi2<0.0D0) phi2 = phi2+2.D0*pi   ![-pi,pi[ -> [0,2*pi[
          !
          ang%fi1= phi1
          ang%PHI= PHI
          ang%fi2= phi2         
      !
      end function
      
      !> Rotates the second-rank tensor S to the reference frame given by rotation R.
      !>
      !> The result is R^T S R, which is equivalent to (R^T S) R
      pure function rotateSRTensorTo_matrix(S,R) result(Srot)
      implicit none
      double precision,dimension(sr_tensor_dim,sr_tensor_dim)     :: Srot
      double precision,dimension(sr_tensor_dim,sr_tensor_dim),intent(in)     :: S
      double precision,dimension(rot_matrix_dim,rot_matrix_dim),intent(in)   :: R
      !
            Srot = matmul(matmul(transpose(R),S),R)
      !
      end function
      
      !> Rotates the second-rank tensor S back from the reference frame given by rotation R.
      !>
      !> The result is R S R^T, which is equivalent to (R S) R^T
      pure function rotateSRTensorFrom_matrix(S,R) result(Srot)
      implicit none
      double precision,dimension(sr_tensor_dim,sr_tensor_dim)     :: Srot
      double precision,dimension(sr_tensor_dim,sr_tensor_dim),intent(in)     :: S
      double precision,dimension(rot_matrix_dim,rot_matrix_dim),intent(in)   :: R
      !
            Srot = matmul(matmul(R,S),transpose(R))
      !
      end function
      
      !> Rotates the second-rank tensor S to the reference frame given by rotation R.
      !>
      !> The result is R^T S R, which is equivalent to (R^T S) R
      pure function rotateSRTensorTo_SRTensor(S,R) result(Srot)
      implicit none
      type(SRTensor)                :: Srot
      type(SRTensor),intent(in)     :: S
      double precision,dimension(rot_matrix_dim,rot_matrix_dim),intent(in)   :: R
      !
            Srot%t = rotateSRTensorTo_matrix(S%t, R)
      !
      end function
      
      !> Rotates the second-rank tensor S back from the reference frame given by rotation R.
      !>
      !> The result is R S R^T, which is equivalent to (R S) R^T
      pure function rotateSRTensorFrom_SRTensor(S,R) result(Srot)
      implicit none
      type(SRTensor)                :: Srot
      type(SRTensor),intent(in)     :: S
      double precision,dimension(rot_matrix_dim,rot_matrix_dim),intent(in)   :: R
      !
            Srot%t = rotateSRTensorFrom_matrix(S%t, R)
      !
      end function
      
      !> Calculation of the Frobenius norm (aka Hilbert–Schmidt norm) of the rectangular matrix M
      pure double precision function FrobeniusNorm(M) result(fn)
      implicit none
      double precision,dimension(:,:),intent(in) :: M !< Rectangular matrix 
      integer :: i
      !$ integer,parameter :: min_omp_size = 128*128
      !
            fn = 0.D0
            !$omp parallel do default(shared) private(i) reduction(+:fn) if (size(M) >= min_omp_size)
            do i = lbound(M,dim=2), ubound(M,dim=2)
                  fn = fn + dot_product(M(:,i),M(:,i))
            enddo
            !$omp end parallel do
            fn = sqrt(fn)
      !
      end function
      
      
      !> The function converts Voigt-style vector vec into symmetrical rank-two tensors.
      !> Ordering of the tensor terms in the vector follows the convention used in Abaqus: 
      !> 11, 22, 33, 12, 23, 13. 
      !>
      !> There is a reverse conversion available. \sa Mat33ToVec6
      pure function Vec6ToMat33(vec) result(mat)
      implicit none
      double precision,dimension(sr_symm_voigt_dim),intent(in)  :: vec
      double precision,dimension(sr_tensor_dim,sr_tensor_dim)   :: mat
      !
            mat(1,1) = vec(1)
            mat(2,2) = vec(2)
            mat(3,3) = vec(3)
            mat(1,2) = vec(4)
            mat(2,3) = vec(5)
            mat(1,3) = vec(6)
            mat(2,1) = mat(1,2)
            mat(3,1) = mat(1,3)
            mat(3,2) = mat(2,3)
      !
      end function

      
      !> The function converts the symmetrical rank-two tensors mat into Voigt-style vector representation.
      !> Ordering of the tensor terms in the vector follows the convention used in Abaqus: 
      !> 11, 22, 33, 12, 23, 13
      !>
      !> There is a reverse conversion available. \sa Vec6ToMat33
      pure function Mat33ToVec6(mat) result(vec)
      implicit none
      double precision,dimension(sr_tensor_dim,sr_tensor_dim),intent(in)  :: mat
      double precision,dimension(sr_symm_voigt_dim)                       :: vec
      !
            vec(1) = mat(1,1)
            vec(2) = mat(2,2)
            vec(3) = mat(3,3)
            vec(4) = mat(1,2)
            vec(5) = mat(2,3)
            vec(6) = mat(1,3)
      !
      end function


      !> Calculates trace of the square n x n matrix X
      pure double precision function trace_matrix(X) result(res)
      implicit none
      double precision,dimension(:,:),intent(in)    :: X
      !
      integer :: i
            res = 0.D0
            do i = 1, minval(shape(X))
                res = res + X(i,i)
            enddo
      !
      end function

    
      !> Calculates trace of second-rank tensor X
      pure double precision function trace_SRTensor(X) result(res)
      implicit none
      type(SRTensor),intent(in)    :: X
      !
           res = trace_matrix(X%t)
      !
      end function

    !
      ! Some operations on double_pair
      !
      
      !> Calculate vector v that is normal to the vector AB (from point A to B).
      !> Provide the angle between the vector v and the x axis.
      !>
      !> v is obtained by a clockwise rotation by 90 degs applied to the AB vector.
      subroutine getNormalVector2D(A,B,length,v,beta)
      implicit none
      type(pair_double), intent(in) :: A, B    !< Positions of the points: A and B
      double precision,intent(in)   :: length  !< Length of the vector v
      type(pair_double),intent(out) :: v       !< Normal vector
      !> Angle between the horizontal axis and the vector u [radians]
      !> The range of the angle is [0:2pi], thus it may vary from acute angle 
      ! via obtuse angle to reflex angle.
      double precision,intent(out)  :: beta
      !
      double precision,dimension(2) :: u
      double precision :: u_norm
      !
            ! Build the secant vector 
            u = [B%x, B%y]  - [A%x, A%y]
            u_norm = norm2(u)
            if (u_norm > epsilon(0.D0)) then
                  ! Build the normal vector. Anticlockwise rotation by 90degs 
                  ! gives [-u_y, u_x]. Apply the clockwise rotation by 90degs:
                  u = [u(2), -u(1)]
                  beta = acos(u(1) / u_norm)
                  ! Let the vectors that point "downwards" have beta angle > 180deg
                  if (u(2) < 0.D0) beta = 2.D0*pi - beta
                  u = u / u_norm * length
                  v = pair_double(u(1), u(2))
            else
                  ! ouups, the points C and A overlap!
                  beta = 0.D0
                  v = pair_double(0.D0,0.D0)
            endif
      !
      end subroutine


      !> Calculate the real roots of quadratic polynomial given in form
      !> a^2 x + b x + c = 0
      !>
      !> 
      !> Provides x1 and x2. Both x1 and x2 are guaranteed to be set to a defined value,
      !> even if no real roots exist and info /= criSuccess is returned.
      integer function solveQuadraticPolynomial(a, b, c, x) result(n_roots)
      implicit none
      double precision,intent(in)   :: a, b, c
      double precision,dimension(2),intent(out)  :: x
      !
      double precision :: delta
      !
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
      !
      end function

end module

