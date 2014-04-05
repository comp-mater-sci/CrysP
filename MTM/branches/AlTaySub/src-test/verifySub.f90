!
! $Id$
!

#include "assert.fpp"

module verifySub
use altaySub
use altayConfig
use altayRCM
use altayCurAccess

double precision,dimension(3,3),parameter :: exampleDG = reshape(       &
                                             [ 2.5D-2,  0.D0,  0.D0,    &
                                                0.D0,   0.D0,  0.D0,    &
                                                0.D0,   0.D0, -2.5D-2], &
                                             [ 3, 3 ])

contains

      


      subroutine verifyMMMmode(model_id)
      implicit none
      integer,intent(in)      :: model_id
      
      character(len=fname_len) :: modfile = 'mod402o.par'
      integer,parameter :: nunit = 150
      double precision    :: t1 = 0.0, t2 = 0.0
      integer :: i, nsteps,info
      !
            call setModelType(acnf,model_id,info)
            ASSERT(info == 0)
      
            open(unit=nunit,file=modfile,status='old')
            nsteps = 201
            
            ! Initialize the altay with the configuration data      
            call initAltay(acnf,info)
            ASSERT(info == 0)
      
            call initStepData(nsteps,astate,info)
            if (info /= 0) then
                  write(*,*) 'Cannot initialize data structure for alamel results'
                  stop
            endif      

            ! Configure steps
            do i=1,nsteps
                  astate%simulCalls(i)%input%full_model = .false.
                  read(nunit,401) astate%simulCalls(i)%input%dgf
                  call setStepType(astate%simulCalls(i)%input,modelAlamel,info)
                  ASSERT(info == 0)
            enddo
            401 format(9(F8.5,1X))
            
            call cpu_time(t1)
            call runSteps(astate,info)
            ASSERT(info == 0)
            call cpu_time(t2)

            do i = 1, nsteps
                  write(*,fmt=500) astate%simulCalls(i)%output%stress_tensor
                  write(*,fmt=501) astate%simulCalls(i)%output%taylor_factor
            enddo
            500 format(3(3(F10.6,1X),/))
            501 format('Taylor factor = ',F12.8)

      
            write(*,100) nsteps, t2 - t1 
            write(*,101) (t2 - t1) / dble(nsteps)  

            100 format('Calculation time for ',I5,' calls: ',F12.3,1X,'secs.') 
            101 format('Average: ',F6.3,1X, 'secs. per call')

     
      end subroutine

      
      
      ! This subroutine shoud reproduce the Altay example file
      subroutine verifyAltayExample(model_id)
      implicit none
      integer,intent(in)      :: model_id
      double precision    :: t1 = 0.0, t2 = 0.0, wt = 0
      integer(kind=8)     :: it1, it2, itres
      integer :: i, nsteps,info
      
      integer,parameter :: istdout = 6
      

            info = -1
            
            call setModelType(acnf,model_id,info)
            ASSERT(info == 0)

            acnf%output_config%nfile = 1 ! switch on creation of the CUR file
            acnf%output_config%nres = 1 ! switch on creation of the RES file
            
            ! Initialize the altay with the configuration data      
            call initAltay(acnf,info)
            if ((info /= 0) .or. RCM_catch(istdout)) then
                 write(*,*) 'Cannot initialize AlTay module'
                 stop
            endif
            nsteps = 4
            
            call initStepData(nsteps,astate,info)
            if ((info /= 0).or. RCM_catch(istdout)) then
                  write(*,*) 'Cannot initialize data structure for AlTay results'
                  stop
            endif      

      
            do i=1,nsteps
                  ! astate%simulCalls(i)%input%full_model = .true.
                  astate%simulCalls(i)%input%keep_texture = .false.
                  astate%simulCalls(i)%input%keep_state = .false.
                  astate%simulCalls(i)%input%dgf = exampleDG
                  call setStepType(astate%simulCalls(i)%input,acnf%model_id,info)
                  ASSERT(info == 0)
                  astate%simulCalls(i)%input%nsteps = 10
                  astate%simulCalls(i)%input%do_output_init = .true.
            enddo

            ! Patch the last call: request final CUR output
            astate%simulCalls(nsteps)%input%do_output_final = .true.
      
            call system_clock(it1, itres)
            call cpu_time(t1)

            call runSteps(astate,info)
            if ((info /= 0).or. RCM_catch(istdout)) then
                  write(*,*) 'Execution error has been detected in processing steps.'
                  stop
            endif      
    
            call cpu_time(t2)
            call system_clock(it2)

            write(*,100) nsteps, t2 - t1 
            write(*,101) (t2 - t1) / dble(nsteps)  
            wt =  dble(it2 - it1) / dble(itres)
            write(*,200) nsteps, wt
            write(*,101) wt / dble(nsteps)  

            
            
      100 format('Calculation CPU time for ',I5,' calls: ',F15.6,1X,'secs.') 
      101 format('Average: ',F15.6,1X, 'secs. per call')
      200 format('Calculation wall-clock time for ',I5,' calls: ',F15.6,1X,'secs.') 
      end subroutine
      
      
      
      ! This subroutine verifies 
      subroutine verifyAltayMultimodel()
      implicit none
      double precision    :: t1 = 0.0, t2 = 0.0
      integer :: i, k, nsteps,info
      
      integer :: imp1 = 7, istdout = 6
      
      ! integer,parameter :: nmodels = 2
      ! integer,parameter :: model_types(nmodels) = [modelFCTaylor, modelAlamel ]
      integer,parameter :: nmodels = 1
      integer,parameter :: model_types(nmodels) = [modelAlamel]
      
      
            info = -1
            ! Configuration of the test
            call setModelType(acnf,model_types(1),info)
            nsteps = 1
            !
            acnf%output_config%nfile = 1 ! switch on creation of the CUR file
            acnf%output_config%nres = 1 ! switch on creation of the RES file
            
            ! Initialize the altay with the configuration data      
            call initAltay(acnf,info)
            if ((info /= 0) .or. RCM_catch(istdout)) then
                 write(*,*) 'Cannot initialize AlTay module'
                 stop
            endif
            
            

            do k =  1, nmodels
                  
                  call setModelType(acnf,model_types(k),info)
                  ASSERT(info == 0) 
                  
                  call initStepData(nsteps,astate,info)
                  if ((info /= 0).or. RCM_catch(istdout)) then
                        write(*,*) 'Cannot initialize data structure for AlTay results'
                        stop
                  endif      

                  do i=1,nsteps
                        call setStepType(astate%simulCalls(i)%input,model_types(k),info)
                        ASSERT(info == 0)
                        astate%simulCalls(i)%input%full_model = .true.
                        astate%simulCalls(i)%input%keep_texture = .false.
                        astate%simulCalls(i)%input%dgf = exampleDG
                        astate%simulCalls(i)%input%nsteps = 1
                        astate%simulCalls(i)%input%do_output_init = .true.
                  enddo
                  call cpu_time(t1)

                  call runSteps(astate,info)
                  if ((info /= 0).or. RCM_catch(istdout)) then
                        write(*,*) 'Execution error has been detected in processing steps.'
                        stop
                  endif      
    
                  call cpu_time(t2)
                  write(*,100) nsteps, t2 - t1 
                  write(*,101) (t2 - t1) / dble(nsteps)  


                  call CURwriteBlock(imp1,info)
                  ASSERT(info == 0)
                  
            enddo
      
      


      100 format('Calculation time for ',I5,' calls: ',F15.6,1X,'secs.') 
      101 format('Average: ',F15.6,1X, 'secs. per call')
      end subroutine
      
      
      subroutine verifyMultiCall()
      implicit none
      integer :: info, i
      !
      integer,parameter :: nruns = 4
      !
      integer,dimension(nruns) :: model_ids = [modelFCTaylor, modelAlamel, modelFCTaylor, modelAlamel]
      character(len=20),dimension(nruns) :: model_outs = [ 'example_FC1','example_ALAMEL1','example_FC2','example_ALAMEL2']       
            
            do i=1,nruns
                  write(*,'(2(A,1X,I0,1X),A,1X,A)') 'Run',i, 'modelID:',model_ids(i), 'output:',model_outs(i)
                  acnf%output_prefix = model_outs(i)
                  call verifyAltayExample(model_ids(i))
                  call finalizeAltay(info)
                  ASSERT(info == 0)
            enddo
      !      
      end subroutine      
      
      
      subroutine verifyKost11Example(model_id)
      use KOST1x
      implicit none
      integer,intent(in)      :: model_id
      integer :: i, nsteps,nparunit,info
      
      integer,parameter :: istdout = 6
      

            info = -1
            
            call setModelType(acnf,model_id,info)
            ASSERT(info == 0)

            
            acnf%output_config%nfile = 1 ! switch on creation of the CUR file
            acnf%output_config%nres = 1 ! switch on creation of the RES file
            acnf%output_config%nmss = 1 ! switch on creation of the MMS file
            
            ! Set KOST11 module
            acnf%slipsystem%KOST = 11
            ! It must be a bcc material
            acnf%slipsystem%input_fname = 'bccbp.pre'
            ! Texture data:
            acnf%texture%input_fname = 'DC06F250.SMT'
            
            open(newunit=nparunit,file='PAR11.par',iostat=info)
            ASSERT(info == 0)
            info = ReadPar(nparunit,acnf%slipsystem%KOST,acnf%hardening%PEBPCnf%params)
            ASSERT(info == 0)
            
            
            ! Initialize the altay with the configuration data      
            call initAltay(acnf,info)
            if ((info /= 0) .or. RCM_catch(istdout)) then
                 write(*,*) 'Cannot initialize AlTay module'
                 stop
            endif
            
            nsteps = 2
            
            call initStepData(nsteps,astate,info)
            if ((info /= 0).or. RCM_catch(istdout)) then
                  write(*,*) 'Cannot initialize data structure for AlTay results'
                  stop
            endif      

      
            do i=1,nsteps
                  astate%simulCalls(i)%input%full_model = .true.
                  astate%simulCalls(i)%input%keep_texture = .false.
                  astate%simulCalls(i)%input%keep_state = .false.
                  call setStepType(astate%simulCalls(i)%input,acnf%model_id,info)
                  ASSERT(info == 0)
                  astate%simulCalls(i)%input%do_output_init = .true.
            enddo

            ! Patch the input:
            ! Shear with strain reversal
            astate%simulCalls(1)%input%dgf = reshape([ 0.D0,  0.D0,  0.D0,    &
                                                       2D-3,  0.D0,  0.D0,    &
                                                       0.D0,  0.D0,  0.D0], [ 3, 3 ])
            astate%simulCalls(1)%input%nsteps = 25
            astate%simulCalls(2)%input%dgf = -astate%simulCalls(1)%input%dgf
            astate%simulCalls(2)%input%nsteps = 400
            
            ! Patch the last call: request final CUR output
            astate%simulCalls(nsteps)%input%do_output_final = .true.
     

            call runSteps(astate,info)
            if ((info /= 0).or. RCM_catch(istdout)) then
                  write(*,*) 'Execution error has been detected in processing steps.'
                  stop
            endif      
      
      end subroutine
      
      
end module