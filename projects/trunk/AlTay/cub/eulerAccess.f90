!
! $Id: eulerAccess.f90 1476 2013-08-02 14:33:41Z jgawad $
!
!>    \author Jerzy Gawad
!>    Email:  Jerzy.Gawad@cs.kuleuven.be
!>
!>    Organization: Katholieke Universiteit Leuven
!>    Organization unit: Dept.Comp.Sci., TWR Group
!>
!>    \date Date of first release: 2009-10-06
!>    $Revision: 1476 $
!>    $Date: 2013-08-02 16:33:41 +0200 (Fri, 02 Aug 2013) $
!>
!>    History of modifications: (see svn log)
!
!
!>    \file eulerAccess.f90 
!>    
!

      module eulerAccess
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
            integer(kind=4)                     :: NGRAINS = 0           !< Number of grains    
            type(grainDesc),dimension(:),allocatable  :: GRAINS !< Array of grains
      end type
      
      contains

      subroutine readEuler(FNAMEULER,MICROS,ERRCODE)
      implicit none
      character(LEN=512),intent(in) :: FNAMEULER
      type(microsDesc),intent(out)  :: MICROS
      integer,intent(out)           :: ERRCODE 
      integer,parameter             :: nunit=110  ! Unit number	  
      integer                       :: tmp
      integer(kind=4)               :: i,ngrains
      integer                       :: iuerr
      type(grainDesc)               :: tmpgrain
      !! End of declaration section
      !
      ! Set empty string as a title
      MICROS%TITLE=''
      write (*,*) 'DAMASK output .euler file'
	  open (unit=nunit,file=TRIM(FNAMEULER),status='old') 
	  ngrains = 0
	  do
	     read (nunit,*,end=10)
		 ngrains=ngrains+1
	  end do
10    write(*,*) 'Number of grains in the euler file: ', ngrains
      close (nunit)
        
      if (ngrains <= 0) then
            write(*,*) 'Error in the euler file'
            ERRCODE=(-1)
            return         
      endif
	  
      MICROS%NGRAINS = ngrains
      write (*,105) MICROS%NS,MICROS%NGRAINS
 105  format (' Input step nr.',i5,3x,'  Number of crystallites',i12)
      !
      call allocateMicros(MICROS,ngrains,ERRCODE)

      !
	  open (unit=nunit,file=TRIM(FNAMEULER),status='old')  
      do 11 i=1,MICROS%NGRAINS
			tmpgrain%GEW = 1
            READ(nunit,*,iostat=iuerr)                                     &
                 tmpgrain%PHI1,tmpgrain%PHI,tmpgrain%PHI2
            if (iuerr .ne. 0) then
                  write(*,*) 'Error in euler file record'
                  ERRCODE=(-1)
                  return         
            endif
            MICROS%GRAINS(i)=tmpgrain
 11   end do
      ERRCODE=0
	  close (nunit)
      end subroutine readEuler
	  
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
      end subroutine allocateMicros
	  
	  subroutine writeCur(NUNIT,MICROS,ERRCODE)
      implicit none
      integer,intent(in)            :: NUNIT
      type(microsDesc),intent(in)   :: MICROS
      integer,intent(out)           :: ERRCODE         
      !
      integer(kind=4)               :: i
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
 403  format(I6,5X,i7,42x,3(2X,3F10.6),2(2x,3f10.5))
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

      end module eulerAccess



