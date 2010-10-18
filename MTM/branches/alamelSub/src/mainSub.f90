#ifdef ALAMEL_SUBROUTINE

program alamelSub
use alamelConfig
implicit none

type(alamelConfigData) :: acnf
integer :: i

! Call initConfig to create dynamic structures
call initConfig(acnf,1,16*6)
!
! Apply modifications to acnf if needed.
acnf%output_prefix = 'example'
acnf%jobtitle = 'example job'
acnf%micros_fname = 'micro1.smt'
! configure slipsystem data
acnf%slipsystem%input_fname = 'fcc.dat'
! configure texture data
acnf%texture%input_fname='alum39.smt'

do i=1,3
      acnf%simulCalls(1)%dgf(i,i) = (1.D0/3.D0)
enddo

! finalize setip of slip systems if necessary


call ALAMEL(acnf)


end program

#endif     
