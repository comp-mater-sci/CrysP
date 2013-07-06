
module polynomialHard
use fngConstants, only: fng_dsv_dim
implicit none

      type :: polynomialHardData
            integer                                   :: order = 0
            double precision,dimension(:),allocatable :: vCoeff
            double precision,dimension(2)             :: valid_eps_range = 0.D0
            double precision,dimension(fng_dsv_dim)   :: vD0 = 0.D0
      end type
      
contains
      
      pure function initPolynomialHardData(order)  result(res)
      type(polynomialHardData)      :: res
      integer,intent(in)            :: order
      !
      integer :: n_order
      !
            n_order = merge(order,0,order > 0)
            allocate(res%vCoeff(n_order+1))
            res%order = n_order
      !
      end function
      
      
      subroutine writePolynomialHardData(outunit,dta,info)      
      implicit none
      integer,intent(in)                        :: outunit
      type(polynomialHardData),intent(in)       :: dta
      integer,intent(out)                       :: info
      !
      integer :: i
      !
            write(outunit,fmt=101,iostat=info,err=900) dta%order
            !            
            do i=1, size(dta%vCoeff)
                  write(outunit,fmt=102,iostat=info,err=900) dta%vCoeff(i)
            enddo
            !
            write(outunit,fmt=102,iostat=info,err=900) dta%valid_eps_range(1)
            write(outunit,fmt=102,iostat=info,err=900) dta%valid_eps_range(2)
            !
            do i=1, size(dta%vD0)
                  write(outunit,fmt=102,iostat=info,err=900) dta%vD0(i)
            enddo
            !
101         format(I3)
102         format(E18.10)
            info = 0
            return
            ! Error handler
900         info =  -1           
      !
      end subroutine

      
      subroutine readPolynomialHardData(outunit,dta,info)      
      implicit none
      integer,intent(in)                        :: outunit
      type(polynomialHardData),intent(out)      :: dta
      integer,intent(out)                       :: info
      !
      integer :: i,order
      !
            read(outunit,*,iostat=info,err=900) order
            dta = initPolynomialHardData(order)
            !            
            do i=1, size(dta%vCoeff)
                  read(outunit,*,iostat=info,err=900) dta%vCoeff(i)
            enddo
            !
            read(outunit,*,iostat=info,err=900) dta%valid_eps_range(1)
            read(outunit,*,iostat=info,err=900) dta%valid_eps_range(2)
            !
            do i=1, size(dta%vD0)
                  read(outunit,*,iostat=info,err=900) dta%vD0(i)
            enddo
            !
            info = 0
            return
            ! Error handler
900         info =  -1           
      !
      end subroutine
      
      
   
      

end module

      
      
module polyApproximation

contains
      
      pure subroutine calculateAppoximation(vEps,vSigma,vCoeff,info)
      use LAPACK95
      implicit none
      double precision,dimension(:),intent(in)   :: vEps   ! {n}
      double precision,dimension(:),intent(in)   :: vSigma ! {n}
      double precision,dimension(:),intent(out)   :: vCoeff ! {n}
      integer,intent(out)                 :: info
      !
      ! mA: [n x n], mB: [n x nrhs]
      double precision,dimension(:,:),allocatable     :: mA, mB
      integer :: n, i
      !
            info = -1
            n = size(vEps)  ! Number of coefficients, so order of polynomial is n-1
            if (any([size(vSigma),size(vCoeff)] /= n)) return
            !        
            allocate(mA(n,n), mB(n,1))
            
            mB(:,1) = vSigma
            !
            forall (i=1:n)
                  mA(:,i) = vEps**(n-i)
            endforall
            !
            call gesv( mA, mB, info=info)
            vCoeff = mB(:,1)
            !
      !
      end subroutine

end module
      
      
module updateData

      type :: defData
            integer :: step,seq 
            double precision,dimension(3,3) :: tDEps
            integer :: req_texu 
            integer :: req_aniso 
            integer :: req_hard
            double precision :: eps_0, eps_1
      end type

contains

      subroutine readUpdateData(inunit,dta,info)      
      implicit none
      integer,intent(in)            :: inunit
      type(defData),intent(out)     :: dta
      integer,intent(out)           :: info
      !
      integer :: i
            info = 0
            read(inunit,*,iostat=info,err=900) dta%step, dta%seq
            do i=1,3
                  read(inunit,*,iostat=info,err=900) dta%tDeps(:,i)
            enddo
            ! Request: tex
            read(inunit,*,iostat=info,err=900) dta%req_texu
            read(inunit,*,iostat=info,err=900) dta%req_aniso
            read(inunit,*,iostat=info,err=900) dta%req_hard
            read(inunit,*,iostat=info,err=900) dta%eps_0
            read(inunit,*,iostat=info,err=900) dta%eps_1
            info = 0
            return
      900   info = -1
      !
      end subroutine
      
end module

module hardFx

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
      
end module
      

program quadraticHard
use polynomialHard
use polyApproximation
use updateData
use hardFx
use fngRuntime
use fngVec5D
implicit none



double precision,dimension(3)   :: vEps,vSigma, vCoeff

type(polynomialHardData) :: hardApprox

integer :: ntest,i, info, ierr
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
      double precision,dimension(:),allocatable   :: vEpsTest, vSigmaSwift, vSigmaQuadratic, vDevErr
      double precision :: deps
      integer :: i, ntest
      !
            ntest = 20
      
            deps = (2.*vEps(3) - vEps(1)) / ntest
            allocate(vEpsTest(ntest),vSigmaSwift(ntest),vSigmaQuadratic(ntest),vDevErr(ntest))
            vEpsTest(1) = vEps(1)
            do i=2,ntest
                  vEpsTest(i) = vEpsTest(i-1) + deps
            enddo
      
            vSigmaSwift =  swift(vEpsTest,swift_K,swift_n,swift_eps0)
            vSigmaQuadratic = quadratic(vEpsTest,vCoeff(1),vCoeff(2),vCoeff(3))
      
            vDevErr = (vSigmaSwift - vSigmaQuadratic) / vSigmaSwift 
            
            do i=1,ntest
                  write(*,'(3(E15.6,1X),F10.5)') vEpsTest(i), vSigmaSwift(i), vSigmaQuadratic(i), vDevErr(i)
            enddo

      
      end subroutine
      

end program
      
      
      
