!
! $Id: criRuntime.f90 3345 2020-01-23 21:31:07Z Hadi.Ghiabakloo $
!
!>    \author Jerzy Gawad
!>    Email:  Jerzy.Gawad@cs.kuleuven.be
!>
!>    Organization: Katholieke Universiteit Leuven (KU Leuven)
!>    Organization unit: Dept.Comp.Sci., TWR Group
!>    \copyright KU Leuven
!>
!>    \date Date of first release: 2011-12-24
!>    $Revision: 3345 $
!>    $Date: 2020-01-23 22:31:07 +0100 (Thu, 23 Jan 2020) $
!>
!>    History of modifications: (see svn log)
!>
!>    \file criRuntime.f90 
!
#include "criStdDefs.fpp"
!
!> Provide shared infrastructure for managing runtime in cri
!> applications.
module criRuntime
use criErrcodes
use criLinearMap
use criPath
use,intrinsic :: iso_fortran_env, only: error_unit,output_unit
implicit none

      !> Length of error message
      integer,parameter              :: errmsg_len = 1024

      !> Error message to be emitted on stop.
      character(len=errmsg_len),save :: errmsg = ''

      !>@{ \name Exit codes that are returned to the OS on various stop contitions
      
      !> OK, succsssful termination
      integer,parameter :: stopcode_OK = 0
      
      !> Error, input parameters are wrong
      integer,parameter :: stopcode_inputerror = 1
      
      !> Error, an IO operation has failed. 
      integer,parameter :: stopcode_ioerror = 2
      
      !> Run-time error condition occured.
      integer,parameter :: stopcode_runtimeerror = 10
      !>@}
      
      integer,parameter       :: description_len = 128
      
      integer,parameter       :: max_command_param_len = max_pathlen
      
      !> Data structure for parsing (and combining) the command line arguments.
      !>
      !> The structure and the procedures provided support a concept of a
      !> "command argument" in the command line, which acts as a selector of the run mode (or subprogram).
      !> The "command argument" shall be looked up in the command map (an instance of LinearMap).
      !> However, the library does not force using "command argument" in the code.
      !>
      !> \note The command line interface is very crude at the moment. 
      !> There is no actual need to improve it much because any complicated use case 
      !> shall be served by a "pythonized" interface...
      type :: commandLine
            !> Name of the program. It does not need to correspond to the name of the 
            !> executable binary. It can typically contain version info and other
            !> useful information.
            character(len=description_len)            :: progname = ''
            
            !> Program description
            character(len=description_len)            :: description = ''
            

            !> Flag: .true. if the object is properly initialized 
            logical                             :: is_initialized = .false.

            !> Actual total number of command line arguments.
            integer                             :: argc = 0

            !> Array of command line arguments. Shape [0:argc]
            character(len=:),dimension(:),allocatable   :: argv

            !> Actual number of optional command line arguments
            integer                             :: argc_opt = 0

            !> Concatenated optional command line arguments
            character(len=:),allocatable        :: args_opt

            !> Flag: .true. if the command line contains a valid "command argument" 
            logical                             :: is_command_identified = .false.

            !> Id of the command as obtained form the map used at the initialization.
            !> It contains a valid data only if is_command_identified is .true.
            integer                             :: command_id = -1
            
            
            !> Index of the "command argument" the map used at the initialization.
            !> It contains a valid data only if is_command_identified is .true.
            integer                             :: command_idx = 0
      end type

#ifdef FORT_HAS_DERIVED_TYPE_INTERFACE
      interface commandLine
            module procedure commandLine_init
      end interface
#endif
      
      interface
            subroutine callback_commandLine(outunit,cmdline,info,command_map)
            import commandLine
            import MapItem
            implicit none
            integer,intent(in)                  :: outunit !< IO unit where the function should print to.
            type(commandLine),intent(in)        :: cmdline !< Command line object
            integer,intent(out)                 :: info    !< Exit code
            type(MapItem),dimension(:),optional :: command_map !< Command map
            end subroutine
      end interface

contains

      !> Terminate execution of the program, returning stop code.
      !> 
      !> If error message is non-empty and errcode is non-zero,
      !> the message will be written to standard error output.
      !> This function should be called instead of the folowing:
      !>   * STOP
      !>   * call exit()
      subroutine finalize(errcode)
      implicit none
      integer,intent(in)      :: errcode
            !
            if ((errcode /= 0) .and. (len_trim(errmsg) > 0)) then
                  write(error_unit,fmt=9000) trim(errmsg)
                  9000 format(/,'Error:',1X,A)                                    
            endif
            !
            call exit(errcode)
      end subroutine

      !> Initialization of commandLine object and return it with progname and description defined
      function commandLine_init(progname,description) result(res)
      implicit none
      character(len=*),intent(in)        :: progname
      character(len=*),intent(in)        :: description
      type(commandLine)       :: res !MB: initialize res of type commandLine and set progname and description (other 8 components unmodified)
      !
            res%progname = progname
            res%description = description
      !
      end function
      
      
      !> Process the arguments provided in the command line.
      subroutine processCommandLine(this,argc_min,argc_max,command_map,command_argpos,info, & 
                                    terminate,command_desc,argv,prologue_fx,epilogue_fx) !MB: 11 arguments, last 5 optional
      implicit none
      type(commandLine),intent(inout)           :: this !MB: defined in criRuntime.f90
      !> Minimal number of mandatory parameters
      !> 2 default, 3 if DMC_USE_TOKENS defined
      integer,intent(in)                        :: argc_min
      !> Maximal number of parameters
      !> 2 default, 3 if DMC_USE_TOKENS defined
      integer,intent(in)                        :: argc_max
      !> Map of command strings into integer identifiers.
      !>
      !> The command argument will be verified against the map. \sa command_argpos
      type(MapItem),dimension(:),intent(in)     :: command_map
      !> Absolute position of the "command argument" in the command line.
      !>
      !> The "command argument" has a special meaning: it will be looked up in the command_map.
      !> If command_argpos is set 0, then no command argument is expected.
      integer,intent(in)                        :: command_argpos 
      !> Exit code.
      !> 
      !> The following values are returned:
      !>    * info = criSuccess on success
      !>    * info = criFailure if the command argument does not match any of commands in the command_map.
      !>    * info = criError or one of criErr_XXX on processing error.
      integer,intent(out)                       :: info
      !> Request termination of the process if the error condition occurs. Default: .false.
      logical,intent(in),optional                           :: terminate
      !> Short description of the "command arguments" that can be accepted by the program
      !>
      !> \note To give an extended description, one can use either 
      !> prologue or epilogue callback functions.
      character(len=*),dimension(:),intent(in),optional     :: command_desc
      !> User-supplied list of command line arguments. It must contain program name in the zeroth element.
      character(len=*),dimension(:),intent(in),optional     :: argv
      !> Subroutine to be called before printing the help message.
      procedure(callback_commandLine),optional              :: prologue_fx
      !> Subroutine to be called after printing the help message.
      procedure(callback_commandLine),optional              :: epilogue_fx
      !
      integer :: ierr, i
      !
            info = criError
            this%is_command_identified = .false.
            !
            ! The 'argv' argument (9th argument of processCommandLine) takes precedence over the command line:
            if (present(argv)) then !MB: not present (thus skipped) in call to processCommandLine in main.f90
                  if (allocated(this%argv)) deallocate(this%argv)
                  this%argc = size(argv) !MB: total number of command line arguments
                  allocate(character(len=max_command_param_len) :: this%argv(0:this%argc),stat=ierr)
                  this%argv(0:) = argv(:)
            else
                  ! Get the count of parameters (this%argc) and the parameters (this%argv: array of strings containing program name (alamDMC) and arguments)
                  call getArgv(this%argc,this%argv,info)
            endif
            !
            ! Attempt to identify the command argument in the first place
            if ((this%argc >= command_argpos) .and. (command_argpos > 0) .and. (size(command_map) > 0)) then
                  ! Check if the command appears in the command line
                  this%is_command_identified = resolveName(command_map,this%argv(command_argpos),this%command_id) !MB: logical function resolveName(themap,name,id[,index]) defined in criLinearMap.f90; default command_id = -1, here to be overwritten with that of argv(command_argpos)
                  if (.not. this%is_command_identified) then
                        errmsg = 'Unknown command: ' // trim(this%argv(command_argpos))
                        call finishProcessing(this,command_map,info,terminate,command_desc,prologue_fx,epilogue_fx)
                        if (info /= criSuccess) return
                  endif
            endif
            ! Pre-validate command line input
            if ( (this%argc < argc_min) .or. (this%argc > argc_max) ) then
                  errmsg = 'Insufficient number of parameters.'
                  ! Either stop or return exit code:
                  call finishProcessing(this,command_map,info,terminate,command_desc,prologue_fx,epilogue_fx)
                  if (info /= criSuccess) return
            endif
            !
            ! Process the optional parameters
            this%argc_opt = max(0,this%argc - argc_min)
            ! Allocate and initialize the args_opt
            info = criErr_MemAlloc
            allocate(character(len=max_pathlen*this%argc_opt) :: this%args_opt,stat=ierr)
            if (ierr /= 0) return
            this%args_opt  = ''
            do i = argc_min + 1, this%argc
                  this%args_opt = trim(this%args_opt) // ' ' // this%argv(i)
            enddo
            !
            this%is_initialized = .true.
            info = criSuccess
            !
      !
      end subroutine

                                    
      !> Print the help message and (optionally) terminate the program.
      !>
      !> See processCommandLine for the description of parameters. 
      !> \sa processCommandLine 
      subroutine finishProcessing(this,command_map,info,terminate,command_desc,prologue_fx,epilogue_fx)
      implicit none
      type(commandLine),intent(inout)     :: this
      type(MapItem),dimension(:),intent(in)                 :: command_map
      integer,intent(out)                 :: info
      logical,intent(in),optional         :: terminate
      character(len=*),dimension(:),intent(in),optional     :: command_desc
      !> Subroutine to be called before printing the help message.
      procedure(callback_commandLine),optional              :: prologue_fx
      !> Subroutine to be called after printing the help message.
      procedure(callback_commandLine),optional              :: epilogue_fx
      !
      logical :: do_terminate
      ! 
            info = criError
            do_terminate = .false.
            if (present(terminate)) do_terminate = terminate
            !
            call printHelpMessage(this,command_map,info,command_desc,prologue_fx,epilogue_fx)
            if (do_terminate) then
                  call finalize(stopcode_inputerror)
            else
                  return
            endif
      !
      end subroutine

      
      !> Print the help message.
      !>
      !> See processCommandLine for the description of parameters. 
      !> \sa processCommandLine 
      subroutine printHelpMessage(this,command_map,info,command_desc,prologue_fx,epilogue_fx)
      implicit none
      type(commandLine),intent(inout)     :: this
      type(MapItem),dimension(:),intent(in)                 :: command_map
      integer,intent(out)                 :: info
      character(len=*),dimension(:),intent(in),optional     :: command_desc
      !> Subroutine to be called before printing the help message.
      procedure(callback_commandLine),optional              :: prologue_fx
      !> Subroutine to be called after printing the help message.
      procedure(callback_commandLine),optional              :: epilogue_fx
      !
      integer :: i, idx
      character(len=max_command_param_len) :: name
      !
            info = criError
            if (present(prologue_fx)) call prologue_fx(error_unit,this,info,command_map)
            if (this%progname /= '') write(error_unit,'(A)') trim(this%progname)
            write(error_unit,'(A)') trim(basename(trim(this%argv(0))))//' '// trim(this%description)
            if (size(command_map) > 0) then
                  if (present(command_desc)) then
                        !
                        if ( all(shape(command_desc) == shape(command_map)) .and. (size(command_map) > 0)) then
                              ! If the user provided a valid command, give the most specific information:
                              ! the command + its description
                              if (this%is_command_identified) then
                                    if (resolveId(command_map,this%command_id,name,idx)) then
                                          write(error_unit,fmt=9000) trim(command_map(idx)%name), trim(command_desc(idx))
                                    endif
                              else
                                    write(error_unit,fmt=8000)
                                    do i = lbound(command_map,dim=1), ubound(command_map,dim=1)
                                          write(error_unit,fmt=9000) trim(command_map(i)%name), trim(command_desc(i))
                                    enddo
                              endif
                        endif
                  else
                        write(error_unit,fmt=8000)
                        do i =1, size(command_map)
                              write(error_unit,'(A)') trim(command_map(i)%name)
                        enddo
                  endif
            endif
            if (present(epilogue_fx)) call epilogue_fx(error_unit,this,info,command_map)
      !
            8000 format(/,'Available commands:')                  
            9000 format(T3,A,1X,':',1X,A)                                          
      !
      end subroutine
                                    
                                    
      !> Process the command line using Fortran intrinsic procedures (command_argument_count, get_command_argument) and put the command line arguments into an allocatable array of strings (argv)
      subroutine getArgv(argc,argv,info)
      implicit none
      !> Number of command parameters. The program name does not count as one of the command arguments.
      integer,intent(out)                                   :: argc
      !> Array of command parameters. Its shape is [0:argc]. The program name is stored under index 0.
      character(:),dimension(:),allocatable,intent(out)     :: argv
      integer,intent(out)                                   :: info     !< Exit status
      !
      integer :: i,ierr, max_param_len, param_len
      !
            info = criErr_MemAlloc
            argc = command_argument_count() !MB: Fortran intrinsic function: returns the number of command arguments available. If there are no command arguments available, the result is 0. The command name does not count as one of the command arguments.
            ! Scout for the longest parameter
            max_param_len = 0
            do i = 0, argc !MB: determine maximum length of supplied command line parameters and store in max_param_len
                  call get_command_argument(i,length=param_len) !MB: Fortran intrinsic subroutine: Returns the command line argument at position (number) i of the command that invoked the program (here: alamDMC). The command itself (here: alamDMC) is argument number 0 (zeroth position). 
                  if (param_len > max_param_len) max_param_len = param_len
            enddo
            if (max_param_len == 0) max_param_len = max_command_param_len
            !
            allocate(character(len=max_param_len) :: argv(0:argc),stat=ierr)
            argv(0:) = ''
            if (ierr /= 0) return
            ! OK, minimal conditions are satisfied.
            info = criError
            do i = 0, argc !MB: loop over command line parameters (command name + arguments) and store them in array of strings argv
                  call get_command_argument(i,argv(i),status=ierr) !MB: Fortran intrinsic function
                  if (ierr /= 0) return
            enddo
            info = criSuccess
      !
      end subroutine
            
#ifdef FORT_HAS_NEWUNIT

      !> Open file fpath in the mode given by status, or call finalize on failure.
      !> Returns IO unit of the newly opened file.
      integer function openOrDie(fpath,status,unit) result(nunit)
      implicit none
      character(len=*),intent(in)   :: fpath
      character(len=*),intent(in)   :: status !< Status of the file. The same as status in OPEN. 
      !> IO unit to be used in opening the file. Unless it is provided, a new unit will be generated.
      integer,intent(in),optional   :: unit
      !
      integer :: ierr
      !
            if (present(unit)) then
                  nunit = unit
                  open(unit=nunit,file=fpath,status=status,iostat=ierr)
            else
                  open(newunit=nunit,file=fpath,status=status,iostat=ierr)
            endif
            if (ierr /= 0) then
                  errmsg = 'Cannot open file ' // trim(fpath)
                  call finalize(stopcode_ioerror)
            endif
      !
      end function
#endif
      
end module
