module altay_io
    use altay_definitions
    use altay_log

    implicit none

contains


    !> Output state-derived variables (SDV) or/and a header line.
    integer function writeSDV(unit,SDV,header) result(info)
    integer,intent(in)                :: unit
    logical,intent(in),optional       :: header
    type(StateDerivedVars),intent(in),optional :: SDV
    !
    integer :: ierr
    !
    info = VEF_IO
    if (present(header)) then
        if (header) write(unit,fmt=100,iostat=ierr)
        if (ierr /= 0) return
    endif
    if (present(SDV)) then
        write(unit,fmt=101,iostat=ierr) SDV
        if (ierr /= 0) return
    endif
    info = VEF_OK
    !
    100 format(T4,'rho_CBs',T20,'rho_CBBs',T36,'rho_polCBBs',T52,'rho_avg')
    101 format(4(E15.7,1X))
    !
    end function



      !> Perform an IO formatted read operation on StatVar
      !>
      !> \param dummy if true, the function performs a fake read operation of by simply skipping the same number of lines as the ReadSVfile would normally read. The resulting SV becomes initialized to default values.
      integer function ReadSVfile(unit,SV,dummy) result(iError)
      integer,      intent(in)  :: unit
      type(StatVar),intent(out) :: SV
      logical,optional,intent(in)   :: dummy

      !local variables declarations
      integer :: i,j
      logical :: is_dummy
      character(len=5)             :: tmpstr
      !
      is_dummy = .false.
      if (present(dummy)) is_dummy = dummy
      if (is_dummy) then !< when dummy = .true. perform fake read
            do i=1,10
                  read(unit,fmt=100,err=666,end=666) tmpstr
            enddo
      else               !< when dummy = .false. or not present (default option) read SV from file
            read(unit,fmt=101,err=666,end=666) SV%RHOcb
            do i=1,6 !one line per WALL
              read(unit,fmt=102,err=666,end=666)SV%CBB(i)%RHOwd,        &
                                                SV%CBB(i)%RHOwp,        &
                                                SV%CBB(i)%RHOwdHOM,     &
                                                SV%CBB(i)%accGAMMA_new, &
                                                SV%CBB(i)%RHOwd_ini
            end do
            read(unit,fmt=103,err=666,end=666) SV%ActiveCBB(1),SV%ActiveCBB(2)
            do i=1,2 !first line for positive sense, 2nd line for negative sense
              read(unit,fmt=104,err=666,end=666)(SV%CRSS(i,j),j=1,24)
            end do
      endif
      iError = VEF_OK
      return
100   format(A5)             ! 5 characters
101   format(   E15.8 )      ! real number in scientific notation, 15 digits total (including 1
                             ! digit for sign and 4 for exponent, 8 digits after decimal point)
102   format( 5(E15.8,1X))   ! 5 times E15.8 with 1 blank spacing in between
103   format( 2(I5,1X   ))   ! 2 5-digit integers with 1 blank spacing
104   format(24(E15.8,1X))
      !
666   iError = VEF_IO !Error in reading from file
      !
      end function ReadSVfile



      integer function WriteSVfile(unit,SV) result(iError)
      integer,      intent(in)  :: unit
      type(StatVar),intent(in)  :: SV

      !local variables declarations
      integer :: i,j

      write(unit,fmt=101,err=666) SV%RHOcb
      do i=1,6 !one line per WALL
            write(unit,fmt=102,err=666)SV%CBB(i)%RHOwd,        &
                                    SV%CBB(i)%RHOwp,        &
                                    SV%CBB(i)%RHOwdHOM,     &
                                    SV%CBB(i)%accGAMMA_new, &
                                    SV%CBB(i)%RHOwd_ini
      end do
      write(unit,fmt=103,err=666) SV%ActiveCBB(1),SV%ActiveCBB(2)
      do i=1,2 !first line for positive sense, 2nd line for negative sense
            write(unit,fmt=104,err=666)(SV%CRSS(i,j),j=1,24)
      end do
      iError = VEF_OK
      return
      !
101   format(   E15.8 )
102   format( 5(E15.8,1X))
103   format( 2(I5,1X   ))
104   format(24(E15.8,1X))
      !
666   iError = VEF_IO !Error in reading from file
      !
      end function WriteSVfile




      integer function WriteHeadSVfile(unit) result(iError)
      integer,intent(in)  :: unit
      !
      write(unit,fmt=100,err=666)"# CB         : [1]RHOcb                                                    "
      write(unit,fmt=100,err=666)"# CBB1(01-1) : [1]RHOwd [2]RHOwp [3]RHOwdHOM [4]accGAMMA_new [5]RHOwd_ini  "
      write(unit,fmt=100,err=666)"# CBB2(-101) : [1]RHOwd [2]RHOwp [3]RHOwdHOM [4]accGAMMA_new [5]RHOwd_ini  "
      write(unit,fmt=100,err=666)"# CBB3(1-10) : [1]RHOwd [2]RHOwp [3]RHOwdHOM [4]accGAMMA_new [5]RHOwd_ini  "
      write(unit,fmt=100,err=666)"# CBB4(0-1-1): [1]RHOwd [2]RHOwp [3]RHOwdHOM [4]accGAMMA_new [5]RHOwd_ini  "
      write(unit,fmt=100,err=666)"# CBB5(101)  : [1]RHOwd [2]RHOwp [3]RHOwdHOM [4]accGAMMA_new [5]RHOwd_ini  "
      write(unit,fmt=100,err=666)"# CBB6(-1-10): [1]RHOwd [2]RHOwp [3]RHOwdHOM [4]accGAMMA_new [5]RHOwd_ini  "
      write(unit,fmt=100,err=666)"# ActiveCBBs : [1]ID_ActiveCBB_highest_slip [2]ID_ActiveCBB_2ndhighest_slip"
      write(unit,fmt=100,err=666)"# CRSS+sense : [1]CRSS(1+) [2]CRSS(2+) ...  [23]CRSS(23+) [24]CRSS(24+)    "
      write(unit,fmt=100,err=666)"# CRSS-sense : [1]CRSS(1-) [2]CRSS(2-) ...  [23]CRSS(23-) [24]CRSS(24-)    "
      write(unit,fmt=100,err=666)"#--------------------------------------------------------------------------"
      write(unit,fmt=100,err=666)"# units:  RHOx:          micrometer^(-2)                                   "
      write(unit,fmt=100,err=666)"#         accGAMMA_new:  /                                                 "
      write(unit,fmt=100,err=666)"#         CRSS:          MPa                                               "
      write(unit,fmt=100,err=666)"#--------------------------------------------------------------------------"
      iError = VEF_OK
      return
      !
100   format(A76)
101   format(A26,L1)
666   iError = VEF_IO !Error in writing to file
      !
      end function WriteHeadSVfile



      integer function ReadHeadSVfile(unit) result(iError)
      integer,intent(in)  :: unit

      !local variables declarations
      integer ::  i
      character :: tmp

      do i=1,15
        read(unit,fmt=100,err=666) tmp !read 15 lines
      end do

      iError = VEF_OK
      return
      !
100   format(A76)
666   iError = VEF_IO !Error in reading from file
      !
      end function ReadHeadSVfile
end module
