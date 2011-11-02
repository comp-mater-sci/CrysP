module smtAccess
      use cubAccess

      contains

      subroutine writeSMT(NUNIT,MICROS,STYLE,ERRCODE)
      use cubAccess
      implicit none
      integer,intent(in)            :: NUNIT
      type(microsDesc),intent(in)   :: MICROS
      integer,intent(in)            :: STYLE
      integer,intent(out)           :: ERRCODE         
      !
      integer                       :: i
      integer                       :: iuerr
      !! End of declaration section
      !
      iuerr = 0
      ERRCODE=0
      ! Write header
      write (NUNIT,400)   MICROS%NGRAINS, MICROS%TITLE
      ! write grains
      do 11 i=1, MICROS%NGRAINS
      !!
            call writeSMTRecord(NUNIT,STYLE,micros%grains(i),ERRCODE) 
                 
           ! Check IO status 
           if (ERRCODE /= 0) then
                  ERRCODE=(-1)
                  exit
           endif 
  11  end do
 400  format(I5,5x,A)
      end subroutine writeSMT


      subroutine writeSMTRecord(NUNIT,STYLE,GRAIN,ERRCODE)
      implicit none
      integer,intent(in)            :: NUNIT
      integer,intent(in)            :: STYLE
      type(grainDesc),intent(in)    :: GRAIN 
      integer,intent(out)           :: ERRCODE 
      integer,parameter             :: NSTAP = 1
      double precision              :: STAP = 0.D0
      select case (STYLE)
        case(1)     
          ! use the 'bare' smt format
          write(NUNIT,401,iostat=ERRCODE)  GRAIN%PHI2,GRAIN%PHI,GRAIN%PHI1,1 , 1.0
        case(2)                       
          ! Use format with weigths, 
          write(NUNIT,402,iostat=ERRCODE) GRAIN%PHI2,GRAIN%PHI,GRAIN%PHI1,1,GRAIN%GEW
        case(3)
          ! Use full format                       
          write(NUNIT,403,iostat=ERRCODE) GRAIN%PHI2,GRAIN%PHI,GRAIN%PHI1,STAP,NSTAP,GRAIN%GEW,GRAIN%GAMMA
      end select

 401  format (3F10.3,10X,I5,5X,F10.1)  ! Bare smt record format
 402  format (3F10.3,10X,I5,5X,F10.5)
 403  format (4F10.3,I5,5X,2F10.5)     !                                  


      end subroutine



end module smtAccess


