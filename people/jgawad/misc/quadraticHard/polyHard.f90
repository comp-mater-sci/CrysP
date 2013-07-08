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
use polyHardUtils
use updateData
use hardFx
use fngRuntime
use fngVec5D
implicit none

double precision,dimension(:),allocatable   :: vEps,vSigma

type(polynomialHardData),target :: hardApprox
type(PolyHardConfig)     :: cnf

integer :: info, ierr, i, npoints
type(defData) :: def_data
!
!double precision,parameter :: swift_K = 696860000.0, swift_n = 0.28433, swift_eps0 = 0.050875
double precision :: swift_K = 0.D0 , swift_n = 0.D0, swift_eps0 = 0.D0
double precision :: deps
integer :: inpunit, inunit, outunit
!
      type(MapItem),dimension(0)  :: command_map 
      integer,parameter       :: argc_min = 1, argc_max=1, command_argpos = 0
      type(commandLine)       :: cmdline
      !
      info = 1
      !
      cmdline = commandLine('polyHard' //'$Rev$',description='Parameters: configuration_file')
      call processCommandLine(cmdline,argc_min,argc_max,command_map,command_argpos,info,terminate=.true.)
      ! Open config file
      inpunit = openOrDie(fpath=cmdline%argv(1),status='old')
      !
      ! Open config file
      inpunit = openOrDie(fpath=cmdline%argv(1),status='old')
      ! Read:
      call readPolyHardConfig(inpunit,cnf,info)
      if (info /= 0) then
            errmsg = 'Cannot read config file, general section'
            call finalize(2)
      endif
      ! Read specific data:
      read(inpunit,*,iostat=info) swift_K 
      read(inpunit,*,iostat=info) swift_n
      read(inpunit,*,iostat=info) swift_eps0
      if (info /= 0) then
            errmsg = 'Cannot read config file, Swift section'
            call finalize(2)
      endif
      !
      ! Open and process defdata.dat file
      inunit = openOrDie('defdata.dat',status='old')
      !
      !
      call readUpdateData(inunit,def_data,info)
      if (info /= 0) then
            errmsg = 'Error during processing defdata.dat'
            call finalize(1)
      endif
      !
      !
      outunit = openOrDie('elem.hard',status='replace')
      !     
      !      
      ! Prepare data points  
      hardApprox = initPolynomialHardData(cnf%polynomial_order)
      npoints = size(hardApprox%vCoeff)
      !
      call makeDatapoints(npoints,def_data%eps_0,def_data%eps_1,deps,vEps,vSigma,info)
      !
      if (info /= 0) then
            errmsg = 'Cannot make proper data points'
            call finalize(2)
      endif
      ! Calculate stress values according  
      vSigma = swift(vEps,swift_K,swift_n,swift_eps0)
      !
      call calculateAppoximation(vEps,vSigma,hardApprox%vCoeff,info)
      !
      hardApprox%valid_eps_range = [ vEps(1), vEps(size(vEps)) ]
      hardApprox%vCoeff = hardApprox%vCoeff
      hardApprox%vD0 = tens2vec5D(def_data%tDEps)
      hardApprox%vD0 = hardApprox%vD0 / norm2(hardApprox%vD0)
            
      do i = 1, size(vEps)
            write(*,fmt=600) vEps(i), vSigma(i)
      enddo      
600 format(2(E15.6,1X))

            
            call writePolynomialHardData(outunit,hardApprox,info)
      !            
      close(outunit)

      ! call test()

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
      

end program
      
      
      
