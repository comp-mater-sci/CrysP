module smtWriter
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



end module smtWriter


program cub2smt
      use cubAccess
      use smtWriter
      implicit none
      integer,parameter             :: ncubunit=110,nsmtunit=111  ! Unit numbers      
      integer                       :: iuerr  ! Error code for I/O operations
      integer,parameter             :: pathlength=512
      character(LEN=pathlength)     :: fnamcub, fnamsmt
      integer                       :: argc,i
      type(microsDesc)              :: micros
      character(len=ctitlelen)      :: title,buf
      character(len=10)             :: stylename
      integer                       :: nstyle
      integer,parameter             :: titlearg = 4
      ! Check number of parameters, 2 are required, 
      ! the remaining parametrers are percieved as title of simulation
      argc = COMMAND_ARGUMENT_COUNT()
      if ( argc < 3 ) then
            write(*,*) 'arguments: cubfile smtfile style [title]'
            write(*,*)  'available styles: bare, plain, full'
            call exit(10)
      endif
      ! Form title
      title = ''
      do i=titlearg,argc
            call GET_COMMAND_ARGUMENT(i,buf,status=iuerr)
            if (i == titlearg ) then
                  title = trim(buf)
            else
                  title = trim(title) // ' ' // trim(buf)
            endif
      end do

      call GET_COMMAND_ARGUMENT(1,fnamcub,status=iuerr)
      call GET_COMMAND_ARGUMENT(2,fnamsmt,status=iuerr)
      call GET_COMMAND_ARGUMENT(3,stylename,status=iuerr)

      ! Interpret style name 
      nstyle = 0 
      if (trim(adjustl(stylename)) == 'bare') nstyle=1
      if (trim(adjustl(stylename)) == 'plain') nstyle=2
      if (trim(adjustl(stylename)) == 'full') nstyle=3
      if (nstyle == 0 ) then
            write(*,*) 'Bad style selector'
            call exit(10)
      endif
      ! Open files
      write(*,*) trim(fnamcub), ' => ',trim(fnamsmt)
      ! Open CUB file 
      open (unit=ncubunit,file=TRIM(fnamcub),status='old',form='UNFORMATTED')
     
      
      call readCub(ncubunit,micros,iuerr)
      if (iuerr /= 0) then
            write(*,*) 'Error reading CUB file.'
            call exit(11)
      endif
      close(ncubunit)
      ! Open CUR file
      open (unit=nsmtunit,file=TRIM(fnamsmt),status='unknown',form='FORMATTED')
      ! Mangle title
      if (len_trim(title) >= 1) then
             micros%TITLE = trim(title)
            write(*,*) 'title: ', title
      endif

      call writeSMT(nsmtunit,micros,nstyle,iuerr)     
      if (iuerr /= 0) then
            write(*,*) 'Error writing SMT file.'
            call exit(11)
      endif
      close(nsmtunit)

end program 

      
