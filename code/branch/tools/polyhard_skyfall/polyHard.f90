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
!>    \date Date of the initial release: 2013-07-07
!>    $Revision$
!>    $Date$
!>
!>    History of modifications: (see svn log)
!>
!
!> The program calculates approximation of hardeninv law by means of polynomial interpolation 
!> from discrete data points.
program polyHard
use fngRuntime
use hardSwift
use hardGeneric
use hardAltay
use KpolynomialHard
use polyApproximation
use polyHardUtils
use updateData
implicit none
!      
integer,parameter       :: argc_min = 2, argc_max=2, command_argpos = 1
type(commandLine)       :: cmdline
!
integer,parameter :: max_commands = 3
! Identifiers of commands
integer,parameter :: id_generic = 1, id_swift = 2, id_altay = 3
!
! Map of commands
type(MapItem),dimension(max_commands),parameter :: command_map = [            &
                                          MapItem('generic',id_generic),&
                                          MapItem('swift',id_swift),      &
                                          MapItem('altay',id_altay)]
!
double precision :: deps
double precision,dimension(:),allocatable   :: vEps,vSigma
type(defData) :: def_data
!
type(polynomialHardData),target :: hardApprox
type(PolyHardConfig)     :: cnf
!
type(genericModel) :: generic_model_cnf
type(swiftModel) :: swift_model_cnf
type(altayModel) :: altay_model_cnf
!
integer :: inpunit, info
      !
      info = fngError
      !
      cmdline = commandLine('polyHard' //'$Rev$',description='Parameters: command configuration_file')
      call processCommandLine(cmdline,argc_min,argc_max,command_map,command_argpos,info,terminate=.true.)
      ! Open config file
      inpunit = openOrDie(fpath=cmdline%argv(2),status='old')
      ! Read the general section
      call readPolyHardConfig(inpunit,cnf,info)
      if (info /= 0) then
            write(errmsg,901) '(general section)'
            call finalize(2)
      endif
      ! 
      ! Process relevant sections of the configuration file
      !
      info = fngError
      select case (cmdline%command_id) 
      case(id_generic)
            call readGenericHardConfig(generic_model_cnf, inpunit, info)
            if (info /= fngSuccess) write(errmsg,901) '(generic section)'
      !
      case(id_swift)
            call readSwiftConfig(swift_model_cnf, inpunit,info)
            if (info /= fngSuccess) write(errmsg,901) '(Swift section)'
      !
      case(id_altay)
            call readAlamelConfigSection(inpunit,altay_model_cnf%altay_cnf,info)
            if (info /= fngSuccess) write(errmsg,901) '(altay section)'
      !
      case default
            write(errmsg,900) 'internal error: incorrect execution mode'
      end select
      if (info /= 0) then
            call finalize(2)
      endif
      !
      ! Load contents to def_data, make hardApprox.
      ! This function may terminate the program.
      call prepareData(cnf,def_data,hardApprox,info)
      ! Terminate if no hardening calculations are requested
      if (def_data%req_hard == 0) call finalize(0)
      !
      ! Create data points
      call makeDatapoints(size(hardApprox%vCoeff),def_data%eps_0,def_data%eps_1,deps,vEps,vSigma,info)
      if (info /= fngSuccess) then
            write(errmsg,900) 'Cannot make proper data points.'
            call finalize(2)
      endif
      !
      ! Calculate stresses for the data points
      select case (cmdline%command_id) 
      case(id_generic)
            ! Simply override the data points
            if ( (size(vEps) /= size(generic_model_cnf%vEps)) .or. (size(vSigma) /= size(generic_model_cnf%vSigma)) ) then
                  write(errmsg,900) 'Number of data points does not match polynomial order.'
                  call finalize(2)
            endif
            vEps = generic_model_cnf%vEps
            vSigma = generic_model_cnf%vSigma
            info = fngSuccess
      !
      case(id_swift)
            ! Calculate stress values according  
            vSigma = swift(vEps,swift_model_cnf%swift_K,swift_model_cnf%swift_n,swift_model_cnf%swift_eps0)
            info = fngSuccess
      !
      case(id_altay)
            call initialize(altay_model_cnf,info)
            if (info /= 0) then
                  write(errmsg,900) 'Cannot initialize libaltay.'
                  call finalize(2)
            endif
            call getStress(altay_model_cnf,vEps,vSigma,hardApprox%vD0,info,errmsg)
      !
      end select
      !
      if (info /= fngSuccess) then
            write(errmsg,900) 'Cannot calculate stress response.' // errmsg
            call finalize(2)
      endif
      !
      ! Calculate coefficients of the interpolation polynomial
      !
      call makeApproximation(vEps,vSigma,hardApprox,info)
      if (info /= 0) then
            write(errmsg,900) 'Cannot calculate polynomial interpolation.'
            call finalize(2)
      endif
      !
      ! Write output files and exit.
      !
      call writeOutputs(cnf,hardApprox,vEps,vSigma,info)
      !
      if (info /= 0) then
            write(errmsg,900) 'failed to write the result files.'
            call finalize(2)
      endif
      !
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ! Formats for error messages
      900 format('polyHard: Error: ', A)
      901 format('polyHard: Error: an error is found in the config file ',A)       
!
end program
      
      
      
