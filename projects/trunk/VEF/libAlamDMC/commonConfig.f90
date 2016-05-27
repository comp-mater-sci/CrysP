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
use criErrcodes
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
      use criUncomment
      use criConfigReader
      implicit none
      class(range_type),pointer     :: inst
      integer,intent(in)            :: cnfunit
      integer,intent(out)           :: info
      !
      !
      type(bias_t),dimension(:),allocatable :: vBiases

      integer :: i, ierr, id, nranges, npoints
      double precision,dimension(:),allocatable :: vPoints
      double precision :: triplet(3)
      double precision :: rbegin, rend, ratio, rstep
      ! Mapping triplet members to logical view (named fields)
      ! Note: rstep an ratio are aliases for the same memory location.
      equivalence (rbegin,triplet(1)), (rend,triplet(2)), &
                  (rstep,triplet(3)), (ratio,triplet(3)) 
      !
            info = criErr_IORead
            nullify(inst)
            id = -1
            ! Read the keyword
            if (.not. readKeyword(cnfunit, range_name_map, id)) return
            select case(id)
            case(range_uniform_id)
                  ! Read: begin end step
                  if (.not. readValue(cnfunit, triplet)) return
                  allocate(inst, source=uniformRange(rbegin, rend, rstep))
            !
            case(range_biased_id)
                  ! Read: begin end ratio
                  if (.not. readValue(cnfunit, triplet)) return
                  if (.not. readValue(cnfunit, npoints)) return
                  if (npoints <= 0) return
                  allocate(inst, source=biasedRange(rbegin, rend, ratio, npoints))
                  !
            !
            case(range_doublebiased_id)
                  ! Read: begin end ratio npoints
                  if (.not. readValue(cnfunit, triplet)) return
                  if (.not. readValue(cnfunit, npoints)) return
                  if (npoints <= 0) return
                  ! 
                  allocate(inst, source=doubleBiasedRange(rbegin, rend, ratio, npoints))
            !
            case(range_multibiased_id)
                  ! Read: begin nranges
                  if (.not. readValue(cnfunit, rbegin)) return
                  if (.not. readValue(cnfunit, nranges)) return
                  if (nranges <= 0) return
                  !
                  allocate(vBiases(nranges))
                  do i = 1, nranges
                        ! We read only the elements in triplet
                        ! that are aliased by rend and ratio
                        if (.not. readValue(cnfunit, triplet(2:3))) return
                        if (.not. readValue(cnfunit, npoints)) return
                        if (npoints <= 0) return
                        vBiases(i) = bias_t(rend, ratio, npoints)
                  enddo
                  allocate(inst, source=multiBiasedRange(rbegin,vBiases))
            !
            case(range_discrete_id)
                  npoints = 0
                  if (.not. readValue(cnfunit, npoints)) return
                  if (npoints <= 0) return
                  allocate(vPoints(npoints))
                  vPoints = 0.D0
                  ! No comments allowed, but multiple lines can be read
                  read(cnfunit,*,iostat=ierr) vPoints
                  if (ierr /= 0) return
                  allocate(inst, source=discreteRange(vPoints))
            !
            case default
                  info = criErr_BadArgs
                  return
            !
            end select
            !
            if (associated(inst)) info = criSuccess
      !
      end function

end module
