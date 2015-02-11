! $Id$
!
!>    \author Jerzy Gawad                                                
!>    Email:  Jerzy.Gawad@cs.kuleuven.be
!>
!>    Organization: Katholieke Universiteit Leuven
!>    Organization unit: Dept.Comp.Sci., TWR Group
!>                                                             
!>    \date Date of the initial release: 2011-09-17
!>    $Revision$
!>    $Date$
!>
!>    History of modifications: (see svn log)
!
!
!> This module contains subroutines, data structures and common variables
!> for shared configuration features of all alamDMC modules
module commonConfig
implicit none

      character(len=20),parameter   :: fmtMsg2Msg   = '(A,T35,A)'
      character(len=20),parameter   :: fmtMsg2Int   = '(A,T35,I4)'
      character(len=20),parameter   :: fmtMsg2Float = '(A,T35,F12.8)'

      character(len=20),parameter   :: fmtMsg2Any   = '(A,T35)'
      character(len=20),parameter   :: fmtMsg2Other = '(A,T35,'  ! Note: user is responsible for finishing the format string      
      
contains

      !> Check exit status of IO operation
      logical function ioStatusOK(ioerr)
      use dmcUtils
      implicit none
      integer,intent(in) :: ioerr
            ! Status 
            if (ioerr /= 0) then
                  write(display_unit,*) 'An error has occured while reading the config file'
                  ioStatusOK = .false.  
            endif
            ioStatusOK = .true.
      end function

      !> Factory function that returns an instance appropriate range type depending on 
      !> the the input read from the  cnfunit IO unit
      function rangeFromConfig(cnfunit,info) result(inst)
      use criRange
      use criLinearMap
      use criNamedRange
      implicit none
      class(range_type),pointer     :: inst
      integer,intent(in)            :: cnfunit
      integer,intent(out)           :: info
      !
      !
      character(len=32) :: keyword
      type(bias_t),dimension(:),allocatable :: vBiases
      double precision :: rbegin, rend, ratio, rstep
      integer :: i, ierr, id, nranges, npoints
      double precision,dimension(:),allocatable :: vPoints
      !
            info = -1
            nullify(inst)
            keyword = ''
            id = -1
            ! Read the keyword
            read(cnfunit,*,iostat=ierr) keyword
            if (.not. ioStatusOK(ierr)) return
            if (resolveName(range_name_map,trim(keyword),id)) then
            
                  select case(id)
                  case(range_uniform_id)
                        ! Read: begin end step
                        read(cnfunit,*,iostat=ierr) rbegin, rend, rstep
                        if (.not. ioStatusOK(ierr)) return
                        allocate(uniformRange :: inst)
                        select type(inst)
                        type is (uniformRange)
                              inst = uniformRange(rbegin, rend, rstep)
                        end select
                  !
                  case(range_biased_id)
                        ! Read: begin end ratio
                        read(cnfunit,*,iostat=ierr) rbegin, rend, ratio, npoints
                        if (.not. ioStatusOK(ierr)) return
                        allocate(biasedRange :: inst)
                        select type(inst)
                        type is (biasedRange)
                              inst = biasedRange(rbegin, rend, ratio, npoints)      
                        end select
                        !
                  !
                  case(range_doublebiased_id)
                        ! Read: begin end ratio npoints
                        read(cnfunit,*,iostat=ierr) rbegin, rend, ratio, npoints
                        if (.not. ioStatusOK(ierr)) return
                        ! 
                        allocate(multiBiasedRange :: inst)
                        select type(inst)
                        type is (multiBiasedRange)
                              inst = doubleBiasedRange(rbegin, rend, ratio, npoints)      
                        end select
                  !
                  case(range_multibiased_id)
                        ! Read: begin nranges
                        read(cnfunit,*,iostat=ierr) rbegin, nranges
                        if (.not. ioStatusOK(ierr)) return
                        ! Read: definitions of biases 
                        if (nranges > 0) then
                              allocate(vBiases(nranges))
                              do i = 1, nranges
                                    read(cnfunit,*,iostat=ierr) vBiases(i)
                                    if (.not. ioStatusOK(ierr)) return
                              enddo
                        else
                              return
                        endif
                        allocate(multiBiasedRange :: inst)
                        select type(inst)
                        type is (multiBiasedRange)
                              inst = multiBiasedRange(rbegin,vBiases)      
                        end select
                  case(range_discrete_id)
                        npoints = 0
                        read(cnfunit,*,iostat=ierr) npoints
                        if ((.not. ioStatusOK(ierr)) .or. (npoints <= 0)) return
                        allocate(vPoints(npoints))
                        vPoints = 0.D0
                        read(cnfunit,*,iostat=ierr) vPoints
                        if (ierr /= 0) return
                        allocate(discreteRange :: inst)
                        select type(inst)
                        type is (discreteRange)
                              inst = discreteRange(vPoints)      
                        end select
                  !
                  end select
            endif
            
            info = 0
      !
      end function

end module
