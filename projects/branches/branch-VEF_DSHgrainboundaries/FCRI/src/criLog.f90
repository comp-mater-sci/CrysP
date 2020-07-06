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
!>    \date Date of first release: 2011-08-08
!>    $Revision$
!>    $Date$
!>
!>    History of modifications: (see svn log)
!>
!>    \file criLog.f90 
!
#include "criStdDefs.fpp"
!
!> Constant parameters, data structures and functions for output logging
!>
!> \todo Logging subroutines should be templatized (eg. by means of preprocessor)
!> \todo A generic name should be provided for all logging subroutines
module criLog
use criErrcodes
      !>@{ \name Logging levels defined in CRI
      !>
      !> \remark In principle, users should not rely on numerical values of the constants.
      !> To determine if an event should be logged, one should use the doLogging function.
      !> To check if an integer represents a valid log level, the isLogLevelOK function should be used.

      integer,parameter       :: criLogNone = -1      !< Disable all logging
      integer,parameter       :: criLogErr  = 0       !< Print only error messages
      integer,parameter       :: criLogWarn  = 1      !< Write warnings
      integer,parameter       :: criLogInfo  = 2      !< Information messages

      !> Detailed diagnostic messages 
      !> \details It is very likely that a huge amount of output data will be produced.
      integer,parameter       :: criLogDebug = 3     
      !>@}


      !> Helper data structure for logging features
      type logData
            integer           :: level = criLogErr    !< loglevel
            integer           :: ounit = 6            !< I/O unit (stdout is a default)
      end type

      interface logIt
            module procedure log_string, log_integer, log_logical, log_double
      end interface logIt
      
      interface logEvent
            module procedure logEvent_string, logEvent_integer, logEvent_logical, logEvent_double
      end interface logEvent
      
      interface doLogging
            module procedure doLogging_integer, doLogging_logData
      end interface doLogging
      
contains
      
      !> Informs whether events with given severity level should be logged under refLogLevel.
      pure logical function doLogging_integer(severity, refLogLevel)
      implicit none
      integer,intent(in)                              :: severity
      integer,intent(in)                              :: refLogLevel
      !
            doLogging_integer = (severity /= criLogNone) .and. (severity <= refLogLevel)
      !
      end function
      
      !> Informs whether events with given severity level should be logged by gived logunit.
      pure logical function doLogging_logData(logunit, severity)
      implicit none
      type(logData),intent(in)                        :: logunit
      integer,intent(in)                              :: severity
      !
            ! Note: ref
            doLogging_logData = doLogging_integer(severity,logunit%level)
      !
      end function

      !> Checks if the log level "severity" is within proper range.
      pure logical function isLogLevelOK(severity)
      implicit none
      integer,intent(in)                              :: severity
      !
            isLogLevelOK = ( (severity >= criLogNone) .and. (severity <= criLogDebug) )
      !
      end function
      
! Templatized functions:      
!
! Instantization of the template for: string
!
#define TMPL_LOG_FX log_string
#define TMPL_LOGEVENT_FX logEvent_string
#define TMPL_CRILOG_TYPE character(len=*)
#define TMPL_CRILOG_FMT A
#include "criLogTemplates.fpp"
#undef TMPL_LOG_FX 
#undef TMPL_LOGEVENT_FX 
#undef TMPL_CRILOG_TYPE
#undef TMPL_CRILOG_FMT
!
! Instantization of the template for: integer
!
#define TMPL_LOG_FX log_integer
#define TMPL_LOGEVENT_FX logEvent_integer
#define TMPL_CRILOG_TYPE integer
#define TMPL_CRILOG_FMT I0
#include "criLogTemplates.fpp"
#undef TMPL_LOG_FX 
#undef TMPL_LOGEVENT_FX 
#undef TMPL_CRILOG_TYPE
#undef TMPL_CRILOG_FMT
!
! Instantization of the template for: logical
!
#define TMPL_LOG_FX log_logical
#define TMPL_LOGEVENT_FX logEvent_logical
#define TMPL_CRILOG_TYPE logical
#define TMPL_CRILOG_FMT L1
#include "criLogTemplates.fpp"
#undef TMPL_LOG_FX 
#undef TMPL_LOGEVENT_FX 
#undef TMPL_CRILOG_TYPE
#undef TMPL_CRILOG_FMT
!
! Instantization of the template for: double
!
#define TMPL_LOG_FX log_double
#define TMPL_LOGEVENT_FX logEvent_double
#define TMPL_CRILOG_TYPE double precision
#define TMPL_CRILOG_FMT D10.3
#include "criLogTemplates.fpp"
#undef TMPL_LOG_FX 
#undef TMPL_LOGEVENT_FX 
#undef TMPL_CRILOG_TYPE
#undef TMPL_CRILOG_FMT
      

end module
