!
! $Id$
!
!>    \author Jerzy Gawad
!>    Email:  Jerzy.Gawad@cs.kuleuven.be
!>
!>    Organization: Katholieke Universiteit Leuven
!>    Organization unit: Dept.Comp.Sci., TWR Group
!>
!>    \date Date of first release: 2010-10-18
!>    $Revision$
!>    $Date$
!>
!>    History of modifications: (see svn log)
!
!
!>    \file mainSub.f90 Test program for ALAMEL subroutine
!>    
!
#ifdef ALAMEL_SUBROUTINE

program alamelSubTest
use alamelConfig
use AlamelSub
implicit none

!type(alamelConfigData) :: acnf
integer :: i,j
integer :: nsteps = 11
integer :: info
integer :: nslips = 16*6

integer,parameter :: nunit = 150
character(len=fname_len) :: modfile = 'mod402o.par'
real    :: t1 = 0.0, t2 = 0.0

! Open file with definitions of strain rate tensors
open(unit=nunit,file=modfile,status='old')

! Call initConfig to create dynamic structures
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
!
! Apply modifications to acnf if needed.
acnf%output_prefix = 'example'
acnf%jobtitle = 'example job'
acnf%micros_fname = 'micro1.smt'
! configure slipsystem data
!acnf%slipsystem%input_fname = 'fcc.pre'
acnf%slipsystem%input_fname = 'bcc.pre'
!acnf%slipsystem%input_fname = 'bcc2.pre'

! configure texture data
! >> small dataset
!acnf%texture%input_fname='alum39.smt'
! >> typical dataset
acnf%texture%input_fname='micros.smt'
do i=1,nsteps

      read(nunit,401) acnf%simulCalls(i)%dgf
      
!      acnf%simulCalls(i)%dgf = 0.01 * acnf%simulCalls(i)%dgf 
!
!      do j=1,2
!            acnf%simulCalls(i)%dgf(j,j) = (1.D0/3.D0)
!      enddo
!      acnf%simulCalls(i)%dgf(3,3) = - (acnf%simulCalls(i)%dgf(1,1) + acnf%simulCalls(i)%dgf(2,2))
      ! use FC:
      ! acnf%simulCalls(i)%rlx1 = 0
      ! acnf%simulCalls(i)%rlx1 = 0
enddo
401 format(9(F8.5,1X))
call cpu_time(t1)

!! Testing initial state
! call ALAMEL(1)
! request output
! call ALAMEL(4)


call ALAMEL(1)

write(*,*) '---'

!! Modify the texture in first call
acnf%simulCalls(1)%keep_texture = .false.

!! Modify every 2 calls
!! acnf%simulCalls(1:nsteps:2)%keep_texture = .false.

call ALAMEL(3)



do i=0,nsteps
      write(*,'(3(3(F10.6,1X),/))') ares%stress_tensors(:,:,i) !/ ares%average_stress(i)
      write(*,'(F10.8)') ares%taylor_factors(i)
enddo

! request output
call ALAMEL(4)


call cpu_time(t2)

write(*,100) nsteps, t2 - t1 
write(*,101) (t2 - t1) / dble(nsteps)  

100 format('Calculation time for ',I5,' calls: ',F12.3,1X,'secs.') 
101 format('Average: ',F6.3,1X, 'secs. per call')
end program

#endif     
