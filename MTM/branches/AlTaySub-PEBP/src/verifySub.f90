module verifySub
use altaySub
use altayConfig

contains
      
      subroutine verifyMMMmode(modelId)
      implicit none
      integer,intent(in)      :: modelId
      
      character(len=fname_len) :: modfile = 'mod402o.par'
      integer,parameter :: nunit = 150
      double precision    :: t1 = 0.0, t2 = 0.0
      integer :: i, nsteps,info
      !
            open(unit=nunit,file=modfile,status='old')
            nsteps = 201

            
            
            ! Initialize the altay with the configuration data      
            call initAltay(acnf,info)
      
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
            enddo
            401 format(9(F8.5,1X))
            
            call cpu_time(t1)
            call runSteps(astate,info)
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
      subroutine verifyAltayExample()
      implicit none
      double precision    :: t1 = 0.0, t2 = 0.0
      integer :: i, nsteps,info

            acnf%simul_init%ngr = 1
      
            acnf%slipsystem%kost = 1 ! Use hardening
            
            ! Initialize the altay with the configuration data      
            call initAltay(acnf,info)
      
            nsteps = 4
            
            call initStepData(nsteps,astate,info)
            if (info /= 0) then
                  write(*,*) 'Cannot initialize data structure for alamel results'
                  stop
            endif      

      
            do i=1,nsteps
                  ! astate%simulCalls(i)%input%full_model = .true.
                  astate%simulCalls(i)%input%keep_texture = .false.
                  
                  astate%simulCalls(i)%input%dgf = reshape([ 2.5D-2, 0.D0,  0.D0,   &
                                                             0.D0,   0.D0,  0.D0,   &
                                                             0.D0,   0.D0, -2.5D-2], &
                                                            [ 3, 3 ])
                  astate%simulCalls(i)%input%rlx1 = 1
                  astate%simulCalls(i)%input%rlx1 = 1
            
                  astate%simulCalls(i)%input%nsteps = 10
            enddo

      
      
            call cpu_time(t1)

            call runSteps(astate,info)
    
            call cpu_time(t2)



      !      do i=0,nsteps
      !            write(*,'(3(3(F10.6,1X),/))') ares%stress_tensors(:,:,i) !/ ares%average_stress(i)
      !            write(*,'(F12.8)') ares%taylor_factors(i)
      !      enddo




            write(*,100) nsteps, t2 - t1 
            write(*,101) (t2 - t1) / dble(nsteps)  

      100 format('Calculation time for ',I5,' calls: ',F12.3,1X,'secs.') 
      101 format('Average: ',F6.3,1X, 'secs. per call')
      end subroutine
      
end module