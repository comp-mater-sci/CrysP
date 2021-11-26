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
!>    \date Date of the first release: 2012-03-27
!>    $Revision$
!>    $Date$
!>
!>    History of modifications: (see svn log)
!>
!>    \file criLogTemplates.fpp 
!
      ! Templatization by the parameters:
      ! TMPL_LOGEVENT_FX  - function name (typically, it contains type name)
      ! TMPL_CRILOG_TYPE - real type name
      ! TMPL_CRILOG_FMT - default format string for the type
      
      !> Write string msg and value of type TMPL_CRILOG_TYPE to the log
      integer function TMPL_LOG_FX(logunit,severity,msg,val,frmt) result(info)
      implicit none
      type(logData),intent(in)                        :: logunit
      integer,intent(in)                              :: severity
      character(len=*),intent(in)                     :: msg
      TMPL_CRILOG_TYPE,intent(in)                     :: val
      character(len=*),intent(in),optional            :: frmt
      !
      integer :: ioinfo
      info = criSuccess
      if (severity <= logunit%level) then
            if (present(frmt)) then
                  write(logunit%ounit,fmt=frmt,iostat=ioinfo) msg,val
            else
                  write(logunit%ounit,fmt=100,iostat=ioinfo) msg,val
            endif
            if (info /= 0) info = criErr_IOWrite
      endif
      100 format(A,': ',TMPL_CRILOG_FMT)
      end function

      !> Write string eveng, followed by string msg and value of type TMPL_CRILOG_TYPE 
      !> to the log
      integer function TMPL_LOGEVENT_FX(logunit,severity,event,msg,val,frmt) result(info)
      implicit none
      type(logData),intent(in)                        :: logunit
      integer,intent(in)                              :: severity
      character(len=*),intent(in)                     :: event
      character(len=*),intent(in)                     :: msg
      TMPL_CRILOG_TYPE,intent(in)                            :: val
      character(len=*),intent(in),optional            :: frmt
      !
      integer :: ioinfo
      info = criSuccess
      if (severity <= logunit%level) then
            if (present(frmt)) then
                  write(logunit%ounit,fmt=frmt,iostat=ioinfo) event,msg,val
            else
                  write(logunit%ounit,fmt=100,iostat=ioinfo) event,msg,val
            endif
            if (ioinfo /= 0) info = criErr_IOWrite
      endif
      100 format(A,': ',A,1X,TMPL_CRILOG_FMT)
      !
      end function
