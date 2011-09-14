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
!>    \file Alamq calculates plastic anisotropic properties, expressed in terms of q-values,
!>          directly from texture data, presented in form of SMT, CUR or CUB files.
!>
!
program alamq
use AlamelSub
use alamelConfig
use nllsTR
use Kutils
use alamYLP
use alamEval, only: alamEval_objFx_call_count
use qrsTypes
implicit none
      ! Strain rate and stress tensors in Material coordinate system and "Tensile sample"
      ! coordinate system
      double precision,dimension(3,3)           :: Dmcoord, Dtcoord, Smcoord,Stcoord, SmIdent, Xmcoord_resume, Xtcoord_resume
      double precision,dimension(3,3)           :: Mrot = 0.0
      !
      double precision                          :: rho 
      double precision,dimension(5)             :: vA, vS,vSonA, vSonAn
      double precision                          :: fi1,phi,fi2
      double precision                          :: SonA_len
      double precision                          :: R
      integer     :: info, iw
      integer     :: nslips = 16*6, nsteps = 1
      integer     :: i,j, nfis, simtype
      logical     :: useVMGuess, reuse_previous, resuse_stainrate
      double precision,parameter ::  rad2deg = (180.D0 / acos(-1.D0)), deg2rad = (acos(-1.D0) / 180.D0)
      double precision :: fi2min = 0.D0, fi2max =  acos(-1.D0)
      double precision :: delta_fi2
      double precision,dimension(:),allocatable       :: residuals
      type(qrsData),dimension(:),allocatable          :: qrsvalues

      type(multilevelYLPConfig)     :: ylpCnf
      !
      integer                 :: argc
      integer,parameter       :: argc_min = 1, argc_max=1
      character(len=128)      :: argv(0:argc_max)
      integer                 :: ioerr
      integer,parameter       :: cnfunit = 90, ofunit = 91
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
      ! Print banner
      write(*,'(A)') 'Alamq: $Id$'
      !     
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
      read(cnfunit,fmt=*,iostat=ioerr)  fi2min, fi2max,  nfis 
      read(cnfunit,fmt=*,iostat=ioerr)  rho 
      read(cnfunit,fmt='(2L2)',iostat=ioerr)  reuse_previous, resuse_stainrate
      read(cnfunit,fmt=*,iostat=ioerr) ylpCnf%jacobi_eps, ylpCnf%linearize
      read(cnfunit,fmt=*,iostat=ioerr) ylpCnf%default_eps, ylpCnf%obj_func_eps
      if (ioerr /= 0) then
            write(*,*) 'Incorrect format of configuration file'
            stop 
      endif
      close(cnfunit)
      ! Validate config values
      if (nfis < 2) then
            write(*,*) 'Number of intervals cannot be smaller than 2' 
            stop
      endif
      
      ! Print configuration     
      write(*,'(I2,1X,A)',iostat=ioerr) acnf%texture%input_type, trim(acnf%texture%input_fname)

      write(*,'(A)' ,iostat=ioerr) trim(acnf%output_prefix) 
      write(*,'(A)' ,iostat=ioerr) trim(acnf%slipsystem%input_fname)
      if (simtype == 0) then
            write(*,fmt=301, iostat=ioerr) 'ALAMEL'
      else
            write(*,fmt=301, iostat=ioerr) 'FC Taylor'
            acnf%simulCalls(1)%rlx1 = 0
            acnf%simulCalls(1)%rlx2 = 0
      endif
      301 format('Multilevel model: ',A)
      write(*,fmt='(2(F8.3,1X),I3)',iostat=ioerr)  fi2min, fi2max,  nfis 
      write(*,fmt='(F8.3)',iostat=ioerr)  rho 
      !
      if (reuse_previous) then
            if (resuse_stainrate) then
                  write(*,'(A,\)') 'Strain rate'
            else
                  write(*,'(A,\)') 'Stress'
            endif
            write(*,'(1X,A)') 'from the previous solution will be re-used.'
      endif
      
      ! Convert fi2min, fi2max from degs to rads
      fi2min = fi2min * deg2rad      
      fi2max = fi2max * deg2rad      

      open(unit=ofunit,file=trim(acnf%output_prefix)//'.xqrs')
      write(ofunit,fmt=500) trim(acnf%texture%input_fname)
      write(ofunit,fmt=501)
      
      ! Apply modifications to acnf:
      acnf%jobtitle = trim(acnf%output_prefix)//' alamq'
      write(*,'(A,\)') 'Initializing the multilevel model...'
      call ALAMEL(1)
      write(*,'(1X,A)') 'Done.'
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
       
      fi1 = 0.D0
      phi = 0.D0
      fi2 = 0.D0
      ! Make space for the results      
      allocate(qrsvalues(nfis+1), residuals(nfis+1))
      residuals = 0.D0
      
      delta_fi2 = (fi2max  - fi2min) / dble(nfis) 
      ! use von Mises guess as a default
      useVMGuess = .true.



      do i = 1,nfis + 1
            write(*,'(/,A,1X,I4,1X,A,1X,F8.3,A,/)')'Point:',i,'fi2 =',fi2 * rad2deg, ' degs'
            ! Calculate rotation matrix
            call KROTMAT(fi1,phi,fi2,Mrot)
            !
            Stcoord = 0.D0
            Stcoord(1,1) = dsqrt(3.D0/2.D0)*1.D0/dsqrt(rho**2-rho+1)
            Stcoord(2,2) = rho*Stcoord(1,1)

            ! Rotate from "tensile" to material coordinate system
            Smcoord = matmul(transpose(Mrot),matmul(Stcoord,Mrot))
            !            
            call KMAT2VEC5D(Smcoord,vS) 
            if ((reuse_previous) .AND. (i > 1))  then
                  ! Reuse previously stored result in new coordinate system
                  ! Type of result (strain rate or stress) is decided in line mared with (***)
                  ! Rotate Xtcoord_resume to new coordinate system
                  Xmcoord_resume = matmul(transpose(Mrot),matmul(Xtcoord_resume,Mrot))
                  ! Set starting point
                  call KMAT2VEC5D(Xmcoord_resume,vA)
                  vA = vA / sqrt(dot_product(vA,vA))
                  ! Disable Von Mises guess in multilevelYLP: vA will be used as a starting point
                  useVMGuess = .false.
            endif
            !
            call multilevelYLP(vS,vA,vSonA,R,info,useVMGuess,ylpCnf)
            residuals(i) = R
            ! Calculate normalized stess
            SonA_len = sqrt(dot_product(vSonA,vSonA)) 
            vSonAn = vSonA / SonA_len
            write(*,('(/)'))
            write(*,'(A,T30,5F10.6)') 'Requested stress:', vS
            write(*,'(A,T30,5F10.6)') 'Identified stress:',vSonAn       
            write(*,('(/)'))
            write(*,*) 'Identified stress (before normalization):'
            write(*,'(5F10.6)') vSonA
            
            ! Convert AONSET vector to tensor form
            call KVEC5D2MAT(vA,Dmcoord)

            call KVEC5D2MAT(vSonAn,SmIdent)
            
            if (iw >= 2) then
                  write(*,400)
                  do j=1,3
                        write(*,401) Smcoord(j,:),SmIdent(j,:),Dmcoord(j,:)
                  enddo
            endif

            
            
            ! Rotate back to the "tensile test" coordinate system   
            Dtcoord = matmul(matmul(Mrot,Dmcoord),transpose(Mrot))
            !
            !(***) Prepare next iteration if re-using is requested.
            if (reuse_previous) then
                  ! Re-used data are always in "tensile" coordinate system (initial coordinate system) 
                  if (resuse_stainrate) then
                        Xtcoord_resume = Dtcoord
                  else
                        ! Rotate stresses to "tensile" coordinate system 
                        Xtcoord_resume = matmul(matmul(Mrot,SmIdent),transpose(Mrot))
                  endif
            endif
            !            
            ! Calculate output variables
            if ( abs(Dtcoord(3,3)) >= epsilon(0.D0) ) then
                  qrsvalues(i)%rvalue =  Dtcoord(2,2) / Dtcoord(3,3)
                  !qv = -Dtcoord(2,2) / Dtcoord(1,1)
                  qrsvalues(i)%qvalue = qrsvalues(i)%rvalue / (1.D0 + qrsvalues(i)%rvalue)
                  qrsvalues(i)%svalue = SonA_len
            else
                  info = 4
            endif
            
            write(*,402) 'qvalue=', qrsvalues(i)%qvalue
            write(*,402) 'rvalue=', qrsvalues(i)%rvalue
            write(*,402) '||SonA||=',qrsvalues(i)%svalue
            !
            write(ofunit,fmt=450) rad2deg * fi2, rho, qrsvalues(i), residuals(i)

            !
            ! Next step
            fi2 = fi2 + delta_fi2
      enddo            

      write(*,'(A,1X,I8,1X,A)') 'Objective function was called', alamEval_objFx_call_count, 'times'

      fi2 = 0
      do i=1,nfis+1
            write(*,fmt=450) rad2deg * fi2, rho, qrsvalues(i), residuals(i)
            fi2 = fi2 + delta_fi2
      enddo
      
      
      400 format('| Smcoord',T40,'| SmIdent',T80,'|Dmcoord')
      401 format(3(F10.6,1X),T40,3(F10.6,1X),T80,3(F10.6,1X))
      402 format(A,T10,F10.6)
      450 format(F10.6,1X,F12.8,1X,3(F12.8,1X),F12.8) ! phi2, rho, (q,r,s,), residual
      ! Format for header file
      500 format('#Material:',1X,A,/,'#Generated by Alamq $Revision$')
      ! Format for title line
      501 format('#Angle',T11,'rho',T24,'q-value',T37,'r-value',T50,'s-value',T63,'residual')
      
end program
