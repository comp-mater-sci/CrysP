module smtAccess
use DYNFIL
implicit none

contains
      
      subroutine SMTreadHeader(iounit,mpoint,title,info)
      implicit none
      integer,intent(in)      :: iounit
      integer,intent(in)      :: mpoint
      character(len=*)        :: title
      integer,intent(out)     :: info
      !
      integer :: nrec, ioerr
      !
            info = -1
            nrec = 0
            read (iounit,94,iostat=ioerr) nrec,title
            if ((ioerr == 0) .and. (nrec > 0) .and. (nrec < mpoint)) then
                  filetitle = title
                  ! Pre-allocate the storage. Chances are that there will be no need to reallocate it.
                  call DYNFIL0(nrec,mpoint,.false.,info)
            endif
      94  format(I5,5x,A)
      !
      end subroutine
      
      
      subroutine SMTwriteHeader(iounit,title,info)
      implicit none
      integer,intent(in)            :: iounit
      character(len=*),intent(in)   :: title !< Title line
      integer,intent(out)           :: info
      !
            info = -1
            write(iounit,94,iostat=info) size(DFIL),title
      94  format(I5,5x,A)
      !
      end subroutine
      
      subroutine SMTwriteBlock(iounit,info)
      use dynfil
      implicit none
      integer,intent(in)      :: iounit   !< IO unit
      integer,intent(out)     :: info     !< Exit code
      !
      integer :: i,ngrains
      integer,parameter :: NSTAP = 1
      double precision,parameter :: convf = 180.D0 / acos(-1.D0)
            !
            ngrains = size(DFIL)
            do i = 1, ngrains
                  ! order: PHI2,PHI,PHI1,blank,NSTAP,GEW,blank
                  write(iounit,97,iostat=info) DFIL(i)%tfi2 * convf,     &
                                          DFIL(i)%tPHI* convf,           &
                                          DFIL(i)%tfi1* convf,           &
                                          NSTAP,                  &
                                          DFIL(i)%tGEW
                  if (info /= 0) exit
            enddo
            !
      97 format (3F10.3,10X,I5,5X,F10.5) 
      end subroutine      

      subroutine SMTreadBlock(iounit,mpoint,info)
      use dynfil
      implicit none
      integer,intent(in)      :: iounit      !< IO unit
      integer,intent(in)      :: mpoint   !< Maximal number of points in a block
      integer,intent(out)     :: info     !< Exit code
      !
      double precision,parameter :: convf =  acos(-1.D0) / 180.D0
      integer :: i,j,i0,k,NSTAP,nrec,ngrains
      double precision :: STAP = 0.D0
            !
            ! Number of records in the SMT file
            nrec = size(DFIL)
            ngrains = nrec
            i = 1
            do j = 1, nrec
                  NSTAP=1
                  STAP=0.0D0
                  ! order: PHI2,PHI,PHI1,STAP,NSTAP,GEW,GAMMA
                  read(iounit,96,iostat=info) DFIL(i)%tfi2,          &
                                          DFIL(i)%tPHI,           &
                                          DFIL(i)%tfi1,           &
                                          STAP,NSTAP,             &
                                          DFIL(i)%tGEW,           &                                   
                                          DFIL(i)%tGAM
                  if (info /= 0) exit
                  ! Convert the grain orientatios from degrees to radians
                  DFIL(i)%tfi1 = DFIL(i)%tfi1 * convf
                  DFIL(i)%tPHI = DFIL(i)%tPHI * convf
                  DFIL(i)%tfi2 = DFIL(i)%tfi2 * convf
                  call initFields(mf,DFIL(i))
                  i = i + 1
                  if (NSTAP > 1) then
                        ! More than one grain per record. This path is more complex,
                        ! but is very infrequently followed.
                        ngrains = ngrains + NSTAP - 1
                        call DYNFIL0(ngrains,mpoint,.true.,info)
                        if (info /= 0) exit
                        i0 = i - 1 ! Store the index of the "parent" grain
                        do k=1,NSTAP-1
                              DFIL(i) = DFIL(i0)
                              DFIL(i)%tfi1 = DFIL(i0)%tfi1 + dble(k)*STAP*convf
                              call initFields(mf,DFIL(i))
                              i = i + 1
                        enddo
                  endif
            enddo
  96  FORMAT (4F10.0,I5,5X,2F10.0)                                      
      !
      end subroutine
      
      


      
      
      
      
end module