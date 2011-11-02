      module cubAccess
      ! Length of title string 
      integer,parameter                   :: ctitlelen = 40

      ! Grain description (single row in CUR/CUB file)
      type grainDesc
            double precision                    :: GEW,PHI1,PHI,PHI2,GAMMA
            double precision,dimension(3)       :: GAXES,GEULR
            double precision,dimension(3,3)     :: F
      end type
      
      ! microstructure description 
      type microsDesc
            character(len=ctitlelen)            :: TITLE =''    ! Title of microstructure
            integer                             :: NS           ! Step number
            double precision,dimension(3)       :: GAXES,GEULR  ! Frame description   (?)
            double precision,dimension(3,3)     :: FALG         ! Frame description 2 (?)   
            integer                             :: NGRAINS      ! Number of grains    
            type(grainDesc),dimension(:),allocatable  :: GRAINS ! Array of grains
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
      write (*,*) 'Binary CUB-type-input file'
      read (NUNIT,iostat=iuerr) 
     &      MICROS%NS,
     &      ngrains,
     &      MICROS%FALG,
     &      MICROS%GAXES,
     &      MICROS%GEULR
      ! 
      if ((iuerr .ne. 0) .or. (ngrains <= 0) )then
            write(*,*) 'Error in header of CUB file'
            ERRCODE=(-1)
            return         
      endif
      MICROS%NGRAINS = ngrains
      write (*,105) MICROS%NS,MICROS%NGRAINS
 105  format (' Input step nr.',i5,3x,'  Number of crystallites',i5)
      !
      call allocateMicros(MICROS,ngrains,ERRCODE)

      !
      do 11 i=1,MICROS%NGRAINS
            ! Read binary record; 
            ! The only difference between CUR and CUB record format is
            ! that the leading ordinal number is skipped in CUB. 
            READ(NUNIT,iostat=iuerr) 
     &           tmpgrain%GEW,
     &           tmpgrain%PHI1,tmpgrain%PHI,tmpgrain%PHI2,
     &           tmpgrain%GAMMA,tmpgrain%F,tmpgrain%GAXES,tmpgrain%GEULR
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
      ! Write header
      write (NUNIT,402)
 402  format (/,' Def. Step    ','Number of orientations',27X,
     1 2X,'F(1,1)',4X,'F(2,1)',4X,'F(3,1)',4X,
     2 2X,'F(1,2)',4X,'F(2,2)',4X,'F(3,2)',4X,
     3 2X,'F(1,3)',4X,'F(2,3)',4X,'F(3,3)',
     4 6X,'a',9X,'b',9x,'c',9x,'G-phi1',4x,'G-PHI',4x,'G-phi2')
      ! Write frame-specific data
      !!
      write (NUNIT,403)  ! nrstep,NPOINT,F,GAXES,GLR
     &      MICROS%NS,
     &      MICROS%NGRAINS,
     &      MICROS%FALG,
     &      MICROS%GAXES,
     &      MICROS%GEULR
 403  format(I6,5X,i5,44x,3(2X,3F10.6),2(2x,3f10.5))
      ! write subheader
      write (NUNIT,401)
 401  format (' CRYSTAL WEIGHT ',5X,'phi1',6X,'PHI',7X,'phi2',6X,
     1'  GAMMA',5X,2X,'F(1,1)',4X,'F(2,1)',4X,'F(3,1)',4X,
     2             2X,'F(1,2)',4X,'F(2,2)',4X,'F(3,2)',4X,
     3             2X,'F(1,3)',4X,'F(2,3)',4X,'F(3,3)',
     4 6X,'a',9X,'b',9x,'c',9x,'G-phi1',4x,'G-PHI',4x,'G-phi2')
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
      write(NUNIT,400,iostat=iuerr)
     &      ID,           ! Ordinal number is a part of CUR file format 
     &      GRAIN%GEW,
     &      GRAIN%PHI1,GRAIN%PHI,GRAIN%PHI2,
     &      GRAIN%GAMMA,GRAIN%F,GRAIN%GAXES,GRAIN%GEULR
      if (iuerr .ne. 0) then
            write(*,*) 'Error in CUR file record'
            ERRCODE=(-1)
      endif
 400  format (I6,f10.5,2X,3f10.5,2X,f10.5,3(2X,3F10.6),2(2x,3f10.5))
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
      write (NUNIT,iostat=iuerr) 
     &      MICROS%NS,
     &      MICROS%NGRAINS,
     &      MICROS%FALG,
     &      MICROS%GAXES,
     &      MICROS%GEULR
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
      write(NUNIT,iostat=iuerr)
     &      GRAIN%GEW,
     &      GRAIN%PHI1,GRAIN%PHI,GRAIN%PHI2,
     &      GRAIN%GAMMA
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



