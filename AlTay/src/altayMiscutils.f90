!> Container for miscellaneous utility routines.
module altayMiscutils
implicit none
      !>@{ \name Exit codes that are returned to the OS on various stop contitions

      ! This piece of code has been ported from fngRuntime

      !> OK, succsssful termination
      integer,parameter :: stopcode_OK = 0

      !> Error, input parameters are wrong
      integer,parameter :: stopcode_inputerror = 1

      !> Error, an IO operation has failed.
      integer,parameter :: stopcode_ioerror = 2

      !> Run-time error condition occured.
      integer,parameter :: stopcode_runtimeerror = 10
      !>@}


      double precision,dimension(3,3),parameter :: unitMatrix = reshape( &
           [ 1.D0, 0.D0, 0.D0,     &
             0.D0, 1.D0, 0.D0,     &
             0.D0, 0.D0, 1.D0], [ 3, 3 ])

      double precision,parameter :: pi = acos(-1.D0)

    contains

      !> Terminate the analysis and return exit code
      !>
      !> STOP statement does not necessarily set exit code.
      !> Typical use case for premature termination:
      !> call terminate(stopcode_runtimeerror)
      subroutine terminate(exit_code)
      integer,intent(in)        :: exit_code
      !
          if (exit_code /= stopcode_OK) write(*,'(A)') 'AlTay terminated due to an error.'
          call exit(exit_code)
      !
      end subroutine

      !> This subroutine extracts the first word from str, fills
      !> the remaining part with spaces and removes all leading blanks.
      subroutine stripComment(str)
      character(len=*),intent(inout) :: str
      !
      integer :: iblank
      !
            str = adjustl(str)
            ! Scan for the first blank
            iblank = index(str,' ')
            if (iblank.GT.0) then
                 str(iblank:)=' '
            end if
      !
      end subroutine


      subroutine writeMSSHeader(ounit,info)
      integer,intent(in)      :: ounit
      integer,intent(out)     :: info
      !
            write(ounit,fmt=554,iostat=info)
      554 format(T5,'Eps_vM',T21,'Eps_vM^Tot',T37,'GAMMA_H',T53,'GAMMA_H^Tot',T69,'Sigma_HvM', &
                   T90,'Sigma_11',T106,'Sigma_22',T122,'Sigma_33',T138,'Sigma_23',T154,'Sigma_31',T170,'Sigma_12', &
                   T186,'TayFac_avg',T202,'StrnRatHet')
      !
      end subroutine


      subroutine writeMSSRecord(ounit,meps,mepstot,hgamcall,hgamtot,shsam,mavg,srh,info)
      integer,intent(in)                              :: ounit
      double precision,intent(in)                     :: meps,mepstot,hgamcall,hgamtot,mavg,srh
      double precision,dimension(3,3),intent(in)      :: shsam
      integer,intent(out)                             :: info
      !
            write(ounit,fmt=555,iostat=info) meps,mepstot,hgamcall,hgamtot,   &
            sqrt(3.D0/2.D0*sum(shsam*shsam)),                                     &
            shsam(1,1),shsam(2,2),shsam(3,3), shsam(2,3),shsam(3,1),shsam(1,2), &
            mavg,srh

      555  format(5(E15.6,1X),5X,8(E15.6,1X))
      !
      end subroutine

      !> Write header line to the report file
      subroutine writeReportHeader(outunit,info)
      integer,intent(in)      :: outunit !< I/O unit number to be used for raport
      integer,intent(out)     :: info
      !
            write(outunit,fmt=100,iostat=info)
            100 format(T8,'phi1',T23,'PHI',T38,'phi2',T55'W')
      !
      end subroutine

      !> Write data line to the report file
      subroutine writeReportRecord(outunit,fi1,PHI,fi2,Wtot,info)
      integer,intent(in)            :: outunit !< I/O unit number to be used for raport
      double precision,intent(in)   :: fi1,PHI,fi2,Wtot
      integer,intent(out) :: info
      !
            write(outunit,fmt=100,iostat=info) fi1, PHI, fi2, Wtot
            100 format(3(F15.7),E15.7)
      !
      end subroutine

end module
