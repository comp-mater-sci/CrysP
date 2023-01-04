!
!> Constant parameters, data structures and functions for output logging
!>
module criLog
    use criErrcodes
    implicit none
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

    interface doLogging
          module procedure doLogging_integer, doLogging_logData
    end interface doLogging

contains

    !> Informs whether events with given severity level should be logged under refLogLevel.
    pure logical function doLogging_integer(severity, refLogLevel)
    integer,intent(in)                              :: severity
    integer,intent(in)                              :: refLogLevel
    !
          doLogging_integer = (severity /= criLogNone) .and. (severity <= refLogLevel)
    !
    end function

    !> Informs whether events with given severity level should be logged by gived logunit.
    pure logical function doLogging_logData(logunit, severity)
    type(logData),intent(in)                        :: logunit
    integer,intent(in)                              :: severity
    !
          ! Note: ref
          doLogging_logData = doLogging_integer(severity,logunit%level)
    !
    end function

end module
