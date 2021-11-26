!
! $Id$
!
!>    \author Jerzy Gawad
!>    Email:  Jerzy.Gawad@cs.kuleuven.be
!>
!>    Organization: Katholieke Universiteit Leuven
!>    Organization unit: Dept.Comp.Sci., TWR Group
!>
!>    \date Date of first release: 2009-10-06
!>    $Revision$
!>    $Date$
!>
!>    History of modifications: (see svn log)
!
!
!>    \file cubAccess.f90 
!>    
!

module cubAccess
	! Length of title string 
	integer,parameter                   :: ctitlelen = 40

	double precision,dimension(3,3),parameter,private :: unitMatrix = reshape( & 
														[ 1.D0, 0.D0, 0.D0,  &
														  0.D0, 1.D0, 0.D0,  &
														  0.D0, 0.D0, 1.D0], &
														[ 3, 3 ])

	! Grain description (single row in CUR/CUB file)
	type grainDesc
		double precision                    :: GEW = 0.D0
		double precision                    :: PHI1 = 0.D0, PHI = 0.D0, PHI2 = 0.D0
		double precision                    :: GAMMA = 0.D0
		double precision,dimension(3)       :: GAXES = [1.0,1.0,1.0]
		double precision,dimension(3)       :: GEULR = [0.0,0.0,0.0]
		double precision,dimension(3,3)     :: F = unitMatrix
	end type

	! microstructure description 
	type microsDesc
		character(len=ctitlelen)            :: TITLE =''    !< Title of microstructure
		integer                             :: NS = 0       !< Step number
		double precision,dimension(3)       :: GAXES = [1.0,1.0,1.0] !< Frame description   (?)
		double precision,dimension(3)       :: GEULR = [0.0,0.0,0.0]
		double precision,dimension(3,3)     :: FALG = unitMatrix     !< Frame description 2 (?)   
		integer                             :: NGRAINS = 0           !< Number of grains    
		type(grainDesc),dimension(:),allocatable  :: GRAINS !< Array of grains
	end type

	contains

	subroutine readCub(NUNIT,MICROS,ERRCODE)
	implicit none
		integer,intent(in)            :: NUNIT
		type(microsDesc),intent(out)  :: MICROS
		integer,intent(out)           :: ERRCODE         

		integer                       :: i,ngrains
		integer                       :: iuerr
		type(grainDesc)               :: tmpgrain
		!! End of declaration section
		!
		iuerr = 0
		! Set empty string as a title
		MICROS%TITLE=''
		write (*,*) 'Reading binary CUB-type-input file'
		read (NUNIT,iostat=iuerr)                                          &
			MICROS%NS,                                                   &
			ngrains,                                                     &
			MICROS%FALG,                                                 &
			MICROS%GAXES,                                                &
			MICROS%GEULR
		!
		if ((iuerr .ne. 0) .or. (ngrains <= 0) )then
			write(*,*) 'Error in header of CUB file'
			ERRCODE=(-1)
			return         
		endif
		MICROS%NGRAINS = ngrains
		
		write (*,105) MICROS%NS,MICROS%NGRAINS
		105  format (' Input step nr.',i6,3x,'  Number of crystallites',i6)
		!
		call allocateMicros(MICROS,ngrains,ERRCODE)

		!
		do 11 i=1,MICROS%NGRAINS
			! Read binary record; 
			! The only difference between CUR and CUB record format is
			! that the leading ordinal number is skipped in CUB. 
			READ(NUNIT,iostat=iuerr)                                     &
				 tmpgrain%GEW,                                           &
				 tmpgrain%PHI1,tmpgrain%PHI,tmpgrain%PHI2,               &
				 tmpgrain%GAMMA,tmpgrain%F,tmpgrain%GAXES,tmpgrain%GEULR
			if (iuerr .ne. 0) then
				  write(*,*) 'Error in CUB file record'
				  ERRCODE=(-1)
				  return         
			endif
			MICROS%GRAINS(i)=tmpgrain
			!write(*,'(3F13.5)') tmpgrain%PHI1,tmpgrain%PHI,tmpgrain%PHI2
		11   end do
		ERRCODE=0
	end subroutine readCub
	
	subroutine writeCub(NUNIT,MICROS,IFRESET_W,IFRESET_ORI,ERRCODE)
	implicit none
		integer,intent(in)            :: NUNIT
		type(microsDesc),intent(in)   :: MICROS
		logical,intent(in)            :: IFRESET_ORI
		logical,intent(in)            :: IFRESET_W
		integer,intent(out)           :: ERRCODE    
		double precision              :: GEW, F(3,3), GAXES(3), GEULR(3)     

		integer                       :: i
		integer                       :: iuerr
		!! End of declaration section
		!
		iuerr = 0
		write (*,*) 'Writing binary CUB-type-input file'
		if (IFRESET_ORI) then
		   write (NUNIT,iostat=iuerr)                                          &
			   MICROS%NS,                                                   &
			   MICROS%NGRAINS,                                                     &
			   unitMatrix,                                                 &
			   [1.D0,1.D0,1.D0],                                                &
			   [0.D0,0.D0,0.D0]
		else
		   write (NUNIT,iostat=iuerr)                                          &
			   MICROS%NS,                                                   &
			   MICROS%NGRAINS,                                                     &
			   MICROS%FALG,                                                 &
			   MICROS%GAXES,                                                &
			   MICROS%GEULR
		endif
		! 
		if (iuerr .ne. 0) then
			write(*,*) 'Error in header of CUB file'
			ERRCODE=(-1)
			return         
		endif
		write (*,105) MICROS%NS,MICROS%NGRAINS
		105  format (' Input step nr.',i6,3x,'  Number of crystallites',i6)
		!

		!
		do i=1,MICROS%NGRAINS
			! Write binary record; 
 
			if (IFRESET_W) then
			   GEW = 1.D0/3.D0
			else
			   GEW = MICROS%GRAINS(i)%GEW
			endif
			
			if (IFRESET_ORI) then
			   F = unitMatrix
			   GAXES = [1.D0,1.D0,1.D0]
			   GEULR = [0.D0,0.D0,0.D0]
			else
			   F = MICROS%GRAINS(i)%F
			   GAXES = MICROS%GRAINS(i)%GAXES
			   GEULR = MICROS%GRAINS(i)%GEULR
			endif
			
			write(NUNIT,iostat=iuerr)                                     &
				 GEW,MICROS%GRAINS(i)%PHI1,MICROS%GRAINS(i)%PHI,MICROS%GRAINS(i)%PHI2,               &
				 MICROS%GRAINS(i)%GAMMA,F,GAXES,GEULR
			if (iuerr .ne. 0) then
				  write(*,*) 'Error in CUB file record'
				  ERRCODE=(-1)
				  return         
			endif
		end do
		ERRCODE=0
	end subroutine writeCub
	
	subroutine writeFormattedCub(NUNIT,MICROS,ERRCODE)
	implicit none
		integer,intent(in)            :: NUNIT
		type(microsDesc),intent(in)   :: MICROS
		integer,intent(out)           :: ERRCODE         
		!
		integer                       :: i
		integer                       :: iuerr
		!! End of declaration section
		!
		iuerr = 0
		ERRCODE=0
		! Write title
		99  format (A)
		write (NUNIT,99) MICROS%TITLE
		! Write the header
		write (NUNIT,502)
		502  format (/,' Def. Step    ','Number of orientations',18X,           &
		2X,'F(1,1)',4X,'F(2,1)',4X,'F(3,1)',4X,                           &
		2X,'F(1,2)',4X,'F(2,2)',4X,'F(3,2)',4X,                           &
		2X,'F(1,3)',4X,'F(2,3)',4X,'F(3,3)',                              &
		7X,'a',9X,'b',9x,'c',9x,'G-phi1',4x,'G-PHI',5x,'G-phi2')
		! Write frame-specific data
		!!
		write (NUNIT,503)                                                  & ! nrstep,NPOINT,F,GAXES,GLR
			MICROS%NS,                                                   &
			MICROS%NGRAINS,                                              &
			MICROS%FALG,                                                 &
			MICROS%GAXES,                                                &
			MICROS%GEULR
		503  format(I6,7X,i5,34x,3(2X,3F10.6),2(2x,3f10.5))
		! write the subheader
		write (NUNIT,501)
		501  format (' WEIGHT ',5X,'phi1',6X,'PHI',7X,'phi2',6X,'  GAMMA')
		! Write grain records
		do 11 i=1, MICROS%NGRAINS
		!!
		   call writeFormattedCubRec(NUNIT,MICROS%GRAINS(i),iuerr)
		   if (iuerr /= 0) then
				  ERRCODE=(-1)
				  exit
		   endif 
		11  end do
	end subroutine writeFormattedCub
	
	subroutine writeFormattedCubRec(NUNIT,GRAIN,ERRCODE)
	implicit none
		integer,intent(in)           :: NUNIT
		type(grainDesc),intent(in)   :: GRAIN
		integer,intent(out)          :: ERRCODE 
		!
		integer                      :: iuerr
		!!
		ERRCODE=0
		! Write single record
		write(NUNIT,500,iostat=iuerr)                                  & 
			GRAIN%GEW,                                                   &
			GRAIN%PHI1,GRAIN%PHI,GRAIN%PHI2,                             &
			GRAIN%GAMMA,                                                 &
			GRAIN%F,GRAIN%GAXES,GRAIN%GEULR
		if (iuerr .ne. 0) then
			write(*,*) 'Error in CUR file record'
			ERRCODE=(-1)
		endif
		500  format (f8.5,2X,3f10.5,2X,f10.5,3(2X,3F10.6),2(2x,3f10.5))
	end subroutine writeFormattedCubRec

	subroutine writeCur(NUNIT,MICROS,ERRCODE)
	implicit none
		integer,intent(in)            :: NUNIT
		type(microsDesc),intent(in)   :: MICROS
		integer,intent(out)           :: ERRCODE         
		!
		integer                       :: i
		integer                       :: iuerr
		!! End of declaration section
		!
		iuerr = 0
		ERRCODE=0
		! Write title
		98  format (A)
		write (NUNIT,98) MICROS%TITLE
		! Write the header
		write (NUNIT,402)
		402  format (/,' Def. Step    ','Number of orientations',27X,           &
		2X,'F(1,1)',4X,'F(2,1)',4X,'F(3,1)',4X,                           &
		2X,'F(1,2)',4X,'F(2,2)',4X,'F(3,2)',4X,                           &
		2X,'F(1,3)',4X,'F(2,3)',4X,'F(3,3)',                              &
		6X,'a',9X,'b',9x,'c',9x,'G-phi1',4x,'G-PHI',4x,'G-phi2')
		! Write frame-specific data
		!!
		write (NUNIT,403)                                                  & ! nrstep,NPOINT,F,GAXES,GLR
			MICROS%NS,                                                   &
			MICROS%NGRAINS,                                              &
			MICROS%FALG,                                                 &
			MICROS%GAXES,                                                &
			MICROS%GEULR
		403  format(I6,5X,i8,41x,3(2X,3F10.6),2(2x,3f10.5))
		! write the subheader
		write (NUNIT,401)
		401  format (' CRYSTAL WEIGHT ',5X,'phi1',6X,'PHI',7X,'phi2',6X,'  GAMMA')
		! Write grain records
		do 11 i=1, MICROS%NGRAINS
		!!
		   call writeCurRecord(NUNIT,i,MICROS%GRAINS(i),iuerr)
		   if (iuerr /= 0) then
				  ERRCODE=(-1)
				  exit
		   endif 
		11  end do
	end subroutine writeCur

	subroutine writeCurRecord(NUNIT,ID,GRAIN,ERRCODE)
	implicit none
		integer,intent(in)           :: NUNIT
		integer,intent(in)           :: ID
		type(grainDesc),intent(in)   :: GRAIN
		integer,intent(out)          :: ERRCODE 
		!
		integer                      :: iuerr
		!!
		ERRCODE=0
		! Write single record
		write(NUNIT,400,iostat=iuerr)                                      &
			ID,                                                          & ! Ordinal number is a part of CUR file format 
			GRAIN%GEW,                                                   &
			GRAIN%PHI1,GRAIN%PHI,GRAIN%PHI2,                             &
			GRAIN%GAMMA
		if (iuerr .ne. 0) then
			write(*,*) 'Error in CUR file record'
			ERRCODE=(-1)
		endif
		400  format (I6,f10.5,2X,3f10.5,2X,f10.5)
	end subroutine writeCurRecord
	!
	! Prototype of CUB ver.2 format

	subroutine writeCubV2(NUNIT,MICROS,ERRCODE)
		implicit none
		integer,intent(in)            :: NUNIT
		type(microsDesc),intent(in)  :: MICROS
		integer,intent(out)           :: ERRCODE         
		!
		! Version specifier
		! 
		integer,parameter             :: vmajor=2, vminor=0
		!
		! Header
		!
		integer,parameter             :: hdrlen=3
		character(len=hdrlen),parameter   :: header='CUB'
		!
		integer                       :: i
		integer                       :: iuerr
		!! End of declaration section
		!
		iuerr = 0
		ERRCODE=0
		! Write header
		write(NUNIT,iostat=iuerr) header,vmajor,vminor
		! Write title 
		write(NUNIT,iostat=iuerr) MICROS%TITLE
		! Write frame-specific data
		!!
		write (NUNIT,iostat=iuerr)                                         &
			MICROS%NS,                                                   &
			MICROS%NGRAINS,                                              &
			MICROS%FALG,                                                 &
			MICROS%GAXES,                                                &
			MICROS%GEULR
		if (iuerr /= 0) then
			ERRCODE=(-1)
			return
		endif
		! Write grain records
		do 11 i=1, MICROS%NGRAINS
		!!
		   call writeCubV2Record(NUNIT,MICROS%GRAINS(i),iuerr)
		   if (iuerr /= 0) then
				  ERRCODE=(-1)
				  exit
		   endif 
		11  end do
	end subroutine writeCubV2

	subroutine writeCubV2Record(NUNIT,GRAIN,ERRCODE)
	implicit none
		integer,intent(in)           :: NUNIT
		type(grainDesc),intent(in)   :: GRAIN
		integer,intent(out)          :: ERRCODE 
		!
		integer                      :: iuerr
		!!
		ERRCODE=0
		! Write single record,skip redundant data (F,GAXES,GEULR)
		write(NUNIT,iostat=iuerr)                                          &
			GRAIN%GEW,                                                   &
			GRAIN%PHI1,GRAIN%PHI,GRAIN%PHI2,                             &
			GRAIN%GAMMA
		if (iuerr .ne. 0) then
			write(*,*) 'Error in CUR file record'
			ERRCODE=(-1)
		endif
	end subroutine writeCubV2Record

	subroutine allocateMicros(MICROS,NGRAINS,ERRCODE)
	implicit none
		type(microsDesc),intent(inout)  :: MICROS
		integer,intent(in)            :: NGRAINS
		integer,intent(out)           :: ERRCODE
		ERRCODE = 1
		if (NGRAINS >= 0) then
			MICROS%NGRAINS = NGRAINS
			! Allocate the array
			allocate(MICROS%GRAINS(MICROS%NGRAINS))
			ERRCODE = 0
		else
			MICROS%NGRAINS = NGRAINS
		endif
	end subroutine

	subroutine freeMicros(micros,istat)
	implicit none
		type(microsDesc),intent(inout)      ::    micros
		integer istat
		!
		if (ALLOCATED(micros%GRAINS)) DEALLOCATE(micros%GRAINS,stat=istat)
		micros%NGRAINS=0
	end subroutine freeMicros

end module cubAccess



