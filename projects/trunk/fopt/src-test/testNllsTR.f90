!
! $Id$
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!>    \author     Jerzy Gawad 
!>    Email:      Jerzy.Gawad@cs.kuleuven.be
!>
!>    Organization: Katholieke Universiteit Leuven
!>    Organization unit: Dept.Comp.Sci., TWR Group
!>    
!>    \date Date of initial release: 2010-10-28
!>    $Revision$
!>    $Date$
!>    History of modifications: (see SVN log).
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! 

 
module testFunctionsTest
use testFunctions
use jacobiFD
implicit none


contains

      subroutine testJacobiCalculations()
      use mkl_rci
      implicit none
      integer :: m,n
      double precision,allocatable,dimension(:)         :: vX,vF,vX0,vInvDelta
      double precision,allocatable,dimension(:,:)       :: mJ, mdJ

      double precision,allocatable,dimension(:,:)       :: mdX,mFx, mdJdX,mJA, mdFx
      double precision :: eps
      integer     :: info,res, i, j
      integer,parameter :: ounit = 6

      type(quadraticFX) :: objFunc
      ! type(quadraticAnalyticFX) :: objFunc
      
      
            !!!!!!!
            n = testFunctions_n
            m = testFunctions_m
            
            call objFunc%initFx(n,m,info)
            
            write(*,*) 'Initialization of the objective function object:', info
            !!!
            
            objFunc%jacobiAnalytical => quadraJacobiAnalytic
            !!
            !
            eps = nllsTR_jacobi_eps
            !objFunc%vParams = 2.D0
            !
            write(*,20) 'Testing Jacobi matrix calculations'

            allocate(vX(n),vX0(n),vF(m),mJ(m,n),mdJ(m,n),mFx(m,2*n),mdJdX(m,n),mJA(m,n),mdFx(m,n))


            vX0 = [ double precision :: -1.D0, 0.D0, 5.5D0 ]
            
            
            vF = 1.0D0
            mJ = 0.D0

            vX = vX0
            write(*,30) 'Objective function at vX'
            call objFunc%objectiveEval(vX,info)

            write(*,40) 'vF='
            write(*,*) objFunc%state%vF
            !
            ! Calculate jacobi matrix analytically
            call objFunc%jacobiAnalytical(vX,mJA,info)
            write(*,40) 'mJA='
            call writeMatrix(mJA,ounit)

            ! Calculate jacobi matrix by means od RCI based Jacobi solver
            call objFunc%jacobiMatrixEval(vX,info)

            write(*,30) 'Jacobi matrix by JacobiObjFx_djacobi at vX'
            write(*,40) 'mJ='
            call writeMatrix(objFunc%state%mJ,ounit)
            write(*,40) 'mJ - mJA='
            call writeMatrix(objFunc%state%mJ - mJA,ounit)


            


            ! Calculate jacobi matrix (stateful interface)
            call objFunc%jacobiMatrixEval(vX,info)

            write(*,30) 'Jacobi matrix by JacobiObjFx_djacobi at vX'
            write(*,40) 'mJ='
            call writeMatrix(objFunc%state%mJ,ounit)
            write(*,40) 'state%mJ - mJA='
            call writeMatrix(objFunc%state%mJ - mJA,ounit)
            
            ! Calculate jacobi matrix by means od RCI based Jacobi solver
            write(*,30) 'Jacobi matrix by djacobi at vX'
            vX = vX0
            res = djacobi(djquadra,n,m,mdJ,vX,nllsTR_jacobi_eps)
            write(*,40) 'mdJ='
            call writeMatrix(mdJ,ounit)

            write(*,40) 'mdJ - mJA='
            call writeMatrix(mdJ - mJA,ounit)


            ! 
            vX = vX0
            call jacobiPrepCFD(vX,eps,mdX,info)
            write(*,40) 'mdX='
            write(*,*) mdX
            ! Calculate function in loop
            do i = 1,size(mdX,dim=2)
                  call objFunc%objectiveEval(mdX(:,i),info)
                  ! Grab the state
                  mFx(:,i) = objFunc%state%vF
            enddo
            call jacobiCalcCFD(mdX,mFx,mdJdX,info)
            write(*,40) 'mdJdX='
            call writeMatrix(mdJdX,ounit)

            write(*,40) 'mdJdX - mJA='
            call writeMatrix(mdJdX - mJA,ounit)
            
            
            ! Test the alternative interface
            ! 
            vX = vX0
            mdJdX = 0.D0
            call jacobiPrepCFD(vX,eps,mdX,vInvDelta,info)
            write(*,40) 'mdX='
            write(*,*) mdX
            write(*,40) 'vInvDelta='
            write(*,*) vInvDelta
            !
            ! mF contains the values of the function 
            ! Calculate function in loop
            do i = 1,size(mdX,dim=2)
                  call objFunc%objectiveEval(mdX(:,i),info)
                  ! Get the state
                  mFx(:,i) = objFunc%state%vF
            enddo
            call jacobiCalcCFD(vInvDelta,.false.,mFx,mdJdX,info)
            write(*,40) 'mdJdX='
            call writeMatrix(mdJdX,ounit)

            write(*,40) 'mdJdX - mJA='
            call writeMatrix(mdJdX - mJA,ounit)
            !
            ! mdF contains the differences between function values
            !
            j = 1
            do i = 1,size(mdX,dim=2),2
                  call objFunc%objectiveEval(mdX(:,i),info)
                  mdFx(:,j) = objFunc%state%vF
                  call objFunc%objectiveEval(mdX(:,i+1),info)
                  mdFx(:,j) = mdFx(:,j) - objFunc%state%vF
                  j = j + 1
            enddo
            !
            call jacobiCalcCFD(vInvDelta,.true.,mdFx,mdJdX,info)
            write(*,40) 'mdJdX='
            call writeMatrix(mdJdX,ounit)

            write(*,40) 'mdJdX - mJA='
            call writeMatrix(mdJdX - mJA,ounit)
            
            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

            20 format(60('#'),/,5('#'),A50,5('#'),/,60('#'))
            30 format(10('+'),1X,A)
            40 format(4('-'),'>',1X,A)

      
      end subroutine
     
      subroutine testTROptimization()
      use mkl_rci
      use nllsTR
      use jacobiFD
      use testFunctions
      implicit none
      !
      integer :: n, m
      double precision,allocatable,dimension(:)         :: vX,vF

      double precision :: eps
      integer     :: info
      integer,parameter :: ounit = 6

      ! type(quadraticFX) :: objFunc
      type(quadraticAnalyticFX) :: objFunc
      
      type(nllsTRConf)  :: config
      double precision  :: r1,r2
      type(SolutionPoint) :: initial_guess
            !!!!!!!
            n = testFunctions_n
            m = testFunctions_m
            
            call objFunc%initFx(n,m,info)
            
            write(*,*) 'Initialization of the objective function object:', info
            !!!
            !
            eps = nllsTR_jacobi_eps
            !objFunc%vParams = 2.D0
            !
            write(*,20) 'Testing Jacobi matrix calculations'

            allocate(vX(n),vF(m))

            vX = 5.D0
            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            write(*,20) 'Testing TR optimization'
            call nlls_TR_init(ounit=6,verbose=3)
            
            write(*,40) 'vX_init='
            write(*,*) vX
            r1 = 0.D0
            r2 = 0.D0
            
            ! config%constJacobi = .true.
            write(*,30) 'Starting TR solver'
            call nlls_TR_solve(objFunc,vX,config,r1,r2,info,SolutionInitOut=initial_guess)

            write(*,*) 'r1= ',r1
            write(*,*) 'r2= ',r2
            write(*,*) 'info= ',info
            write(*,40) 'vX_final='
            write(*,*) vX
            write(*,30) 'Objective function at vX_final'
            call objFunc%objectiveEval(vX,info)
            write(*,40) 'vF='
            write(*,*) objFunc%state%vF

            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

            20 format(60('#'),/,5('#'),A50,5('#'),/,60('#'))
            30 format(10('+'),1X,A)
            40 format(4('-'),'>',1X,A)
      
      end subroutine

      
      
end module


program testNLLSTR
use testFunctionsTest
implicit none

      call testJacobiCalculations()
      
      call testTROptimization()

end program
