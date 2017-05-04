!
! $Id$
!
!>    \author Jerzy Gawad
!>    Email:  Jerzy.Gawad@cs.kuleuven.be
!>
!>    Organization: Katholieke Universiteit Leuven (KU Leuven)
!>    Organization unit: Dept.Comp.Sci., TWR Group
!>    \copyright KU Leuven
!>
!>    \date Date of first release: 2012-04-20
!>    $Revision$
!>    $Date$
!>
!>    History of modifications: (see svn log)
!>
!>    \file criTimers.f90 
!
#include "criStdDefs.fpp"
!
!> Time measurements and performance counters
module criTimers

      !> Provides measurements of the elapsed walltime. 
      !>
      !> Resolution of the clock is in nanoseconds.
      type :: wallclockTimer
            integer(kind=8),dimension(0:1)      :: t = [ 0, 0 ]
            integer(kind=8)                     :: clockrate = 0
      contains
            procedure,pass(this)    :: start => wallclockTimer_start
            procedure,pass(this)    :: get => wallclockTimer_get
      end type
      
      !> Provides measurements of the elapsed CPU time, 
      !> summed up over all threads and child processes.
      type :: cpuTimer
            double precision,dimension(0:1)     :: t = [ 0.D0, 0.D0 ]
      contains
            procedure,pass(this)    :: start => cpuTimer_start
            procedure,pass(this)    :: get => cpuTimer_get
      end type

      !> Aggregated timer for measuring both wallclock and CPU time
      type :: uniTimer
            type(wallclockTimer)    :: wct
            type(cpuTimer)          :: ct
      contains
            procedure,pass(this)    :: start => uniTimer_start
            procedure,pass(this)    :: get => uniTimer_get
      end type
            
            
contains
      !> Start or restart the timer
      subroutine wallclockTimer_start(this)
      implicit none
      class(wallclockTimer)    :: this
      !
            call system_clock(this%t(0),this%clockrate)
      !
      end subroutine

      !> Provides the measurement of time that elapsed since the last call 
      !> to start()
      double precision function wallclockTimer_get(this) result(t)
      implicit none
      class(wallclockTimer)    :: this
      !
            t = -1
            if (this%clockrate > 0) then
                  call system_clock(this%t(1))
                  t = dble(this%t(1) - this%t(0)) / dble(this%clockrate)
            endif
      !
      end function

      !> Start or restart the timer
      subroutine cpuTimer_start(this)
      implicit none
      class(cpuTimer)          :: this
      !
            call cpu_time(this%t(0))
      !
      end subroutine

      
      !> Provides accumulated CPU time, expressed in seconds.
      double precision function cpuTimer_get(this) result(t)
      implicit none
      class(cpuTimer)          :: this
      !
            call cpu_time(this%t(1))
            t = this%t(1) - this%t(0)
      !
      end function

      
      !> Start or restart the timer
      subroutine uniTimer_start(this)
      implicit none
      class(uniTimer)          :: this
      !
            call this%wct%start()
            call this%ct%start()
      !
      end subroutine
     
      
      !> Provides time measurement.
      !>
      !> \returns Rank-one array of double precision. First element contains 
      !> the elapsed wallclock time, the second element contains CPU time that elapsed.
      function uniTimer_get(this) result(t)
      implicit none
      class(uniTimer)    :: this
      double precision,dimension(2) :: t
      !
            t(1) = this%wct%get()
            t(2) = this%ct%get()
      !
      end function

end module