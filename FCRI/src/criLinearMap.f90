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
!>    \date Date of first release: 2010-11-13
!>    $Revision$
!>    $Date$
!>
!>    History of modifications: (see svn log)
!>
!>    \file criLinearMap.f90
!
#include "criStdDefs.fpp"
!
!> Simple (and stupid) implementation of unsorted linear map.
!>
!> Note that linear map can be efficient only if the map size is very small (e.g. up to 10)
module criLinearMap
implicit none

      !> Maximal length of strings that are used as keys in the map
      integer,private,parameter     :: cMapNameLen = 32

      !> Helper data structure for resolving name-identifier pairs
      !> MB: Maps (= structure arrays constructed from MapItem) are used to look up name and find corresponding ID and vice versa
      type MapItem
            character(len=cMapNameLen)          :: Name
            integer                             :: ID
      end type

contains

      !> Return index (position) of the symbolic name in the map.
      !>
      !> \return 0 if the name doesn't match any name provided in the map.
      integer function findName(themap,name)
      character(len=*),intent(in)               :: name
      type(MapItem),dimension(1:)               :: themap
      !
      integer :: i
      character(len=cMapNameLen)       :: shortname
      !
            findName = 0
            shortname = trim(adjustl(name)) ! Trim and store (make direct comparison)
            do i=1,size(themap)
                  if (shortname == trim(themap(i)%Name)) then
                        findName = i
                        exit
                  endif
            enddo
      !
      end function

      !> Resolve symbolic name into an integer identifier.
      !>
      !> \return .true. if the name matches a name provided in the map, then id contains corresponding identifier
      !> \return .false. if the name doesn't match any map item, id and index (if present) are left unmodified.
      logical function resolveName(themap,name,id,index)
      character(len=*),intent(in)               :: name
      type(MapItem),dimension(1:),intent(in)    :: themap
      integer,intent(inout)                     :: id !MB: inout instead of only out since id only modified when name exists
      integer,intent(inout),optional            :: index !MB: inout instead of only out since index only modified when name exists
      !
      integer :: i
      character(len=cMapNameLen)       :: shortname
      !
            resolveName = .false.
            shortname = trim(adjustl(name)) ! Trim and store (make direct comparison)
            do i=1,size(themap) !MB: search for name in map
                  if (shortname == trim(themap(i)%Name)) then
                        id = themap(i)%ID
                        resolveName = .true.
                        exit
                  endif
            enddo
            if (present(index) .and. resolveName) index = i !MB: store map index when name found
      !
      end function

      !> Resolve an integer identifier into a symbolic name.
      !>
      !> \return .true. if the id matches a name provided in the map, then name contains corresponding symbolic identifier
      !> \return .false. if the id doesn't match any map item, name and index (if present) are left unmodified.
      logical function resolveId(themap,id,name,index)
      type(MapItem),dimension(1:),intent(in)    :: themap
      integer,intent(in)                        :: id
      character(len=*),intent(inout)            :: name !MB: inout instead of only out since name only modified when name exists
      integer,intent(inout),optional            :: index !MB: inout instead of only out since index only modified when name exists
      !
      integer :: i
      !
            resolveId = .false.
            do i=1,size(themap) !MB: search for id in map
                  if (id == themap(i)%ID) then
                        name = themap(i)%Name
                        resolveId = .true.
                        exit
                  endif
            enddo
            if (present(index) .and. resolveId) index = i
      !
      end function



end module
