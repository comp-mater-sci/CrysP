module hardFx
use hardSwift
implicit none
      type :: vectorWrapper
            double precision,dimension(:),pointer     :: vector
      end type

contains
       
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
      integer :: i
      !
            polynomial = 0.D0
            do i=1,size(v%vector)
                  polynomial = polynomial*x + v%vector(i)
            enddo
      !
      end function 

      
      subroutine hardFx_test()
      implicit none
      double precision,dimension(:),allocatable   :: vEpsTest, vSigmaSwift, vSigmaQuadratic, vSigmaHorner, vDevErr
      double precision :: deps
      integer :: ntest, i
      type(vectorWrapper) :: vCoeffWrap
      
      double precision,dimension(:),allocatable   :: vEps,vSigma
      double precision,parameter :: swift_K = 696860000.0, swift_n = 0.28433, swift_eps0 = 0.050875 
            
            vEps =   [   0.D0, 0.25D0, 0.5D0  ]
            vSigma = swift(vEps,swift_K,swift_n,swift_eps0)
            
            ntest = 20
      
            deps = (2.*vEps(3) - vEps(1)) / ntest
            allocate(vEpsTest(ntest),vSigmaSwift(ntest),vSigmaQuadratic(ntest),vDevErr(ntest))
            vEpsTest(1) = vEps(1)
            do i=2,ntest
                  vEpsTest(i) = vEpsTest(i-1) + deps
            enddo
      
            vSigmaSwift =  swift(vEpsTest,swift_K,swift_n,swift_eps0)
            if (hardApprox%order == 2) then
                  vSigmaQuadratic = quadratic(vEpsTest,hardApprox%vCoeff(1),hardApprox%vCoeff(2),hardApprox%vCoeff(3))
            else
                  vSigmaQuadratic = 0.D0
            endif
      
            vCoeffWrap%vector => hardApprox%vCoeff
            vSigmaHorner = polynomial(vEpsTest,vCoeffWrap)
            
            vDevErr = (vSigmaSwift - vSigmaHorner) / vSigmaSwift 
            
            do i=1,ntest
                  write(*,'(4(E15.6,1X),F10.5)') vEpsTest(i), vSigmaSwift(i), vSigmaQuadratic(i), vSigmaHorner(i), vDevErr(i)
            enddo

      
      end subroutine
      
end module