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
implicit none


contains

      subroutine testTROptimization()
      use mkl_rci
      use nllsTR
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

      call testTROptimization()

end program
