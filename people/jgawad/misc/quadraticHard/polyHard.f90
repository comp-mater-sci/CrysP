! #define norm2(X) sqrt(sum(X*X))

module hardFx

      type :: vectorWrapper
            double precision,dimension(:),pointer     :: vector
      end type

contains
      
      elemental double precision function swift(eps,K,n,eps0) result(sigma)
      implicit none
      double precision,intent(in) :: eps, K, n, eps0
            sigma = K * ((eps0+eps)**n)
      end function

      elemental double precision function quadratic(x,A,B,C)
      implicit none
      double precision,intent(in)   :: x, A, B, C
            quadratic = A*x*x + B*x + C
      end function 

      !> Calculates value of polynomial at x using Horner method
      elemental double precision function polynomial(x,v)
      implicit none
      double precision,intent(in)         :: x
      type(vectorWrapper),intent(in)      :: v     
      !      
      integer :: i, nterms
      !
            polynomial = 0.D0
            do i=1,size(v%vector)
                  polynomial = polynomial*x + v%vector(i)
            enddo
      !
      end function 

      
end module
      
! #include "fngStdDefs.fpp"

program quadraticHard
use KpolynomialHard
use polyApproximation
use updateData
use hardFx
use fngRuntime
use fngVec5D
implicit none

double precision,dimension(3),target   :: vEps,vSigma, vCoeff

type(polynomialHardData) :: hardApprox

integer :: info, ierr
type(defData) :: defdta
!
double precision, parameter :: swift_K = 696860000.0, swift_n = 0.28433, swift_eps0 = 0.050875
integer,parameter :: inunit = 100, outunit = 101
!
      ! Open and process defdata.dat file
      open(inunit,file='defdata.dat',status='old',iostat=ierr)
      if (ierr /= 0) then
             errmsg = 'Cannot open defdata.dat'
             call finalize(1)
      endif
      !
      open(outunit,file='elem.hard',status='replace',iostat=ierr)
      if (ierr /= 0) then
                  errmsg = 'Cannot open elem.hard for writing'
                  call finalize(1)
      endif
      !
      call readUpdateData(inunit,defdta,info)
      if (info /= 0) then
            errmsg = 'Error during processing defdata.dat'
            call finalize(1)
      endif
      !
      
      
      vEps(1) = defdta%eps_0
      vEps(3) = defdta%eps_1
      ! Set coordinate of the middle-point 
      vEps(2) = 0.5D0 * (vEps(1) + vEps(3))
      !
      if (abs(vEps(1) - vEps(3)) > 0.D0) then      
            ! Calculate stress values according  
            vSigma = swift(vEps,swift_K,swift_n,swift_eps0)
            !
            call calculateAppoximation(vEps,vSigma,vCoeff,info)
            !
            hardApprox = initPolynomialHardData(2) 
            !
            hardApprox%valid_eps_range = vEps(1:3:2)
            hardApprox%vCoeff = vCoeff
            hardApprox%vD0 = tens2vec5D(defdta%tDEps)
            hardApprox%vD0 = hardApprox%vD0 / norm2(hardApprox%vD0)
            
            call writePolynomialHardData(outunit,hardApprox,info)
            
      endif
      close(outunit)

      call test()

contains

      
      subroutine test()
      implicit none
      double precision,dimension(:),allocatable   :: vEpsTest, vSigmaSwift, vSigmaQuadratic, vSigmaHorner, vDevErr
      double precision :: deps
      integer :: ntest, i
      type(vectorWrapper) :: vCoeffWrap
            ntest = 20
      
            deps = (2.*vEps(3) - vEps(1)) / ntest
            allocate(vEpsTest(ntest),vSigmaSwift(ntest),vSigmaQuadratic(ntest),vDevErr(ntest))
            vEpsTest(1) = vEps(1)
            do i=2,ntest
                  vEpsTest(i) = vEpsTest(i-1) + deps
            enddo
      
            vSigmaSwift =  swift(vEpsTest,swift_K,swift_n,swift_eps0)
            vSigmaQuadratic = quadratic(vEpsTest,vCoeff(1),vCoeff(2),vCoeff(3))
      
            vCoeffWrap%vector => vCoeff
            vSigmaHorner = polynomial(vEpsTest,vCoeffWrap)
            
            vDevErr = (vSigmaSwift - vSigmaQuadratic) / vSigmaSwift 
            
            do i=1,ntest
                  write(*,'(4(E15.6,1X),F10.5)') vEpsTest(i), vSigmaSwift(i), vSigmaQuadratic(i), vSigmaHorner(i), vDevErr(i)
            enddo

      
      end subroutine
      

end program
      
      
      
