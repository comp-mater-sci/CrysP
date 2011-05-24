!
! $Id$
!
!>    \author Jerzy Gawad                                                
!>    Email:  Jerzy.Gawad@cs.kuleuven.be
!>
!>    Organization: Katholieke Universiteit Leuven
!>    Organization unit: Dept.Comp.Sci., TWR Group
!>                                                             
!>    \date Date of first release: 2010-11-03
!>    $Revision$
!>    $Date$
!>
!>    History of modifications: (see svn log)
!
!
!>    \file AlamSRA program allows one to track anisotropic properties  
!>          along deformation due to uniaxial tensile stress.
!>
!>    \todo AlamSRA should be extended to take cognizance of biaxial stress state, arbitraty stresses,
!>          as well as arbitrary evolution of the texture.

module alamsraHelper
contains
pure function vec_norm2(v)
implicit none
double precision :: vec_norm2
double precision,dimension(:),intent(in) :: v
vec_norm2 = sqrt(dot_product(v,v))
end function
end module


!> ALAMel Stress Response Analysis
!>
program alamsra
use AlamelSub
use alamelConfig
use nllsTR
use Kutils
use alamYLP
use alamEval, only: alamEval_objFx_call_count
use alamsraHelper
implicit none
      ! Strain rate and stress tensors in Material coordinate system and "Tensile sample"
      ! coordinate system
      double precision,dimension(3,3)           :: P,D,De, Sm,St, SmIdent, Dt
      double precision,dimension(3,3)           :: Mrot = 0.0
      !
      logical                                   :: outputRequest
      double precision                          :: angle, PNormMax, PNormIter
      double precision                          :: plast_pot, scal_s, norm_sona
      double precision                          :: rvalue, qvalue
      double precision,dimension(5)             :: vA, vS,vSonA, vSonAn, vP, vD
      double precision                          :: fi1,phi,fi2
      double precision                          :: Pnorm, normP, normD
      double precision                          :: R
      integer     :: info, iw
      integer     :: nslips = 16*6, nsteps = 1
      integer     :: i,j, simtype

      double precision,parameter ::  rad2deg = (180.D0 / acos(-1.D0)), deg2rad = (acos(-1.D0) / 180.D0)

      type(multilevelYLPConfig)     :: ylpCnf
      !
      integer                 :: argc
      integer,parameter       :: argc_min = 1, argc_max=1
      character(len=128)      :: argv(0:argc_max)
      integer                 :: ioerr
      integer,parameter       :: cnfunit = 90, ofunit = 91, histunit = 92
      !
      info = 1
      iw = 3
      !
      !
      argc = command_argument_count()
      if (argc < argc_min) then
            write(*,*) 'one parameter is required: configuration file'
            stop
      endif
      call get_command_argument(1,argv(1))
      !
      ! Introduce youself ;-)
      write(*,'(A)') 'AlamSRA, rev: $Rev$'
      !
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ! INITIALIZATION OF ALAMEL: it should be done in different way !!!!
      call initConfig(acnf,nslips,nsteps,info)
      if (info /= 0) then
            write(*,*) 'Cannot initialize alamel config'
            stop
      endif
      call initResults(ares,nsteps,info)
      if (info /= 0) then
            write(*,*) 'Cannot initialize data structure for alamel results'
            stop
      endif

      ! open and read config file      
      write(*,'(/,A,1X,A,/)') 'Processing config file', trim(argv(1))
      open(cnfunit,file=trim(argv(1)),status='old',iostat=ioerr)
      if (ioerr /= 0) then
            write(*,*) 'Cannot open config file: ', trim(argv(1))
            stop
      endif
      !
      read(cnfunit,'(I2,1X,A)',iostat=ioerr) acnf%texture%input_type
      select case(acnf%texture%input_type)
            case(1,3)     ! SMT or CUB
                  read(cnfunit,'(A)',iostat=ioerr) acnf%texture%input_fname
            case(2)       ! CUR file    
                  read(cnfunit,'(I2,1X,A)',iostat=ioerr) acnf%texture%block, acnf%texture%input_fname
            case default
                  write(*,*) 'Incorrect texture type: ', acnf%texture%input_type    
      end select
      if (ioerr /= 0) then
            write(*,*) 'Incorrect format in the configuration file'
            stop  
      endif
      read(cnfunit,fmt=*,iostat=ioerr)  simtype
      read(cnfunit,'(A)' ,iostat=ioerr) acnf%output_prefix 
      read(cnfunit,'(A)' ,iostat=ioerr) acnf%slipsystem%input_fname 
      read(cnfunit,'(A)' ,iostat=ioerr) acnf%micros_fname
      read(cnfunit,'(L)' ,iostat=ioerr) outputRequest
      read(cnfunit,fmt=*,iostat=ioerr)  angle 
      read(cnfunit,fmt=*,iostat=ioerr)  PNormMax, PNormIter 

      read(cnfunit,fmt=*,iostat=ioerr) ylpCnf%jacobi_eps, ylpCnf%linearize

      if (ioerr /= 0) then
            write(*,*) 'Incorrect format of configuration file'
            stop 
      endif
       
      ! Print configuration     
      write(*,'(I2,1X,A)',iostat=ioerr) acnf%texture%input_type, trim(acnf%texture%input_fname)
      write(*,'(A)' ,iostat=ioerr) trim(acnf%output_prefix) 
      write(*,'(A)' ,iostat=ioerr) trim(acnf%slipsystem%input_fname)
      if (simtype == 0) then
            write(*,'(A)', iostat=ioerr) 'ALAMEL'
      else
            write(*,'(A)', iostat=ioerr) 'FC Taylor'
            acnf%simulCalls(1)%rlx1 = 0
            acnf%simulCalls(1)%rlx2 = 0
      endif
      !
      write(*,'(A,1X,F10.6)') 'Orientation of the sample:', angle
      if (ylpCnf%linearize) then
            write(*,100) 'Info: the program will first attempt to linearize the identification problems.'
      else
            write(*,100) 'Info: The program will attempt to solve the nonlinear problems.'
      endif
      100 format(/,A,/)
      !
      ! Open and initialize result files
      open(unit=ofunit,file=trim(acnf%output_prefix)//'.asr',status='replace')
#define OUTHEADER 'iter','normP','Pnorm','R','plast_pot','M','scal_s','||SonA||','rvalue','qvalue'     
      write(ofunit,700) OUTHEADER ! write header line
      open(unit=histunit,file=trim(acnf%output_prefix)//'_hist.asr',status='replace')
      acnf%jobtitle = trim(acnf%output_prefix)//' alamsra'
      !
      call ALAMEL(1)
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
       
      fi1 = 0.0
      phi = 0.0
      ! Convert angle from degs to rads
      fi2 = angle * deg2rad
      
      vP = 0.D0
      Pnorm = 0.D0 ! sum||P||
      normP = 0.D0 ! ||P||
      i = 0
      do 
            write(*,900)
            !! -> Take uniaxial tensile stress, rotate it to given direction     
            St = 0.D0
            St(1,1) = dsqrt(3.D0/2.D0)
            ! Rotate from "tensile" to material coordinate system
            ! Calculate rotation matrix
            call KROTMAT(fi1,phi,fi2,Mrot)
            !
            Sm = matmul(transpose(Mrot),matmul(St,Mrot))
            !            
            call KMAT2VEC5D(Sm,vS) 
            ! Enforce unit length of vS
            vS = vS / vec_norm2(vS)
            !
            !! -> Calculate corresponding strain rate vA
            call multilevelYLP(vS,vA,vSonA,R,info,.true.,ylpCnf)
            !
            plast_pot = dot_product(vA, vSonA)
            norm_sona = vec_norm2(vSonA)
            scal_s = norm_sona / vec_norm2(vS)
            ! Calculate normalized stess
            vSonAn = vSonA / vec_norm2(vSonA) 
            write(*,('(/)'))
            write(*,200) 'Requested stress:', vS
            write(*,200) 'Identified scaled stress:',vSonAn
            write(*,201) 'Norm of stress residual:', R      
            write(*,('(/)'))
            write(*,200) 'Stress on vA:',vSonA
            write(*,201) 'Norm of stress on vA:', norm_sona 
            write(*,*)
            !
            200 format(A,T40,5F10.6)
            201 format(A,T40,F10.6)
            !
            call KVEC5D2MAT(vA,D)
            call KVEC5D2MAT(vSonAn,SmIdent)
            write(*,400)
            do j=1,3
                  write(*,401) Sm(j,:),SmIdent(j,:),D(j,:)
            enddo
            write(*,*)
            !! Calculate q and r in tensile reference frame
            ! Rotate back to the "tensile test" coordinate system   
            Dt = matmul(matmul(Mrot,D),transpose(Mrot))
            if ( abs(D(3,3)) >= epsilon(0.D0) ) then
                  rvalue =  Dt(2,2) / Dt(3,3)
                  qvalue = rvalue / (1.D0 + rvalue)
            else
                  rvalue = -1.0
                  qvalue = -1.0
            endif
            ! 
            !! -> Report the results
            write(ofunit,701) i, normP, Pnorm, R, plast_pot, ares%taylor_factors(1), scal_s, norm_sona, rvalue, qvalue
            !
            write(*,710)
            write(*,700) OUTHEADER ! write header line
            write(*,701) i, normP, Pnorm, R, plast_pot, ares%taylor_factors(1), scal_s, norm_sona, rvalue, qvalue
            write(*,710)
            !!
            ! Check termination condition
            if (PNorm > PNormMax) exit
            !
            !! -> Scale the vA in order to get ||vA|| = PNormIter
            vD = vA * (PNormIter / vec_norm2(vA)) 
            normD = vec_norm2(vD)
            write(*,'(A,1X,F12.6)') 'Norm of vD = ', normD 
            ! Calculate strain increment for texture evolution           
            call KVEC5D2MAT(vD,De)
            !
            !! -> Impose De as ALAMEL input, advance the state of texture
            !
            write(*,*) 'Strain to be imposed for texture evolution De = '
            write(*,500) De
            write(*,*)
            ! Write history of deformations
            write(histunit,500) De
            !
            ! Set input data for ALAMEL
            acnf%simulCalls(1)%dgf = De
            acnf%simulCalls(1)%keep_texture = .false.
            acnf%simulCalls(1)%do_output = outputRequest
            acnf%nSimulCalls = 1
            call ALAMEL(3)       
            !       
            !! -> Calculate total strain
            vP = vP + vD
            P = P + De  
            normP = vec_norm2(vP)
            Pnorm = Pnorm + normD
            i = i + 1 
            !
            write(*,'(A)') 'Total strain:'
            write(*,'(A,1X,F12.6)') '||P|| =', normP
            write(*,'(A,1X,F12.6)') 'sum||De|| =', Pnorm
            write(*,*) 'P='
            write(*,500) P
            !
       enddo            

      write(*,'(A,1X,I8,1X,A)') 'Objective function was called', alamEval_objFx_call_count, 'times'

      close(ofunit)
      close(histunit)
      
      400 format('| Smcoord',T40,'| SmIdent',T80,'|Dmcoord')
      401 format(3(F10.6,1X),T40,3(F10.6,1X),T80,3(F10.6,1X))

      500 format(3(3(F10.6,1X),/))
      501 format(3(F10.6,1X),/,3(F10.6,1X),/,3(F10.6,1X))
      
      700 format(1X,A5,1X,9(A12,1X))
      701 format(1X,I5,1X,9(F12.6,1X))
      710 format('|',5('-'),'|',9(12('-'),'|'))
      
      900 format(112('='))
end program