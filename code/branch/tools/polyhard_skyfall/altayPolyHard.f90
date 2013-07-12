module runtimeHelper
use fngRuntime
contains
      
end module
      
program alTayPolyHard
use fngPath
use fngRuntime
use altayConfig
use dmcBasicModule, only: readAlamelConfigSection
use altaySub
use updateData
use runtimeHelper
use polyApproximation
use polyHardUtils
implicit none


      
type(PolyHardConfig) :: cnf
type(alTayConfigData)   :: altay_cnf
type(defData)     :: def_data

double precision,dimension(:),allocatable   :: vEps,vSigma
double precision :: deps, deltaEps_norm
integer :: i, npoints

double precision,dimension(3,3) :: tDeltaEps
type(polynomialHardData) :: hardApprox

double precision,parameter  :: unit_conversion_factor = 1.D6  ! For convension from MPa to Pa
integer :: inpunit,  info
!
      !
      type(MapItem),dimension(0)  :: command_map 
      integer,parameter       :: argc_min = 1, argc_max=1, command_argpos = 0
      type(commandLine)       :: cmdline
      !
      info = 1
      !
      cmdline = commandLine('alTayPolyHard' //'$Rev$',description='Parameters: configuration_file')
      call processCommandLine(cmdline,argc_min,argc_max,command_map,command_argpos,info,terminate=.true.)
      !
      ! Open config file
      inpunit = openOrDie(fpath=cmdline%argv(1),status='old')
      ! Read:
      call readPolyHardConfig(inpunit,cnf,info)
      if (info /= 0) then
            errmsg = 'Cannot read config file, general section'
            call finalize(1)
      endif
      !
      call readAlamelConfigSection(inpunit,altay_cnf,info)
      if (info /= 0) then
            errmsg = 'Cannot read config file, libaltay section'
            call finalize(1)
      endif
      !
      ! Load contents to def_data, make hardApprox.
      ! This function may terminate the program.
      call prepareData(cnf,def_data,hardApprox,info)
      ! Terminate if no hardening calculations are requested
      if (def_data%req_hard == 0) call finalize(0)
      !
      npoints = size(hardApprox%vCoeff)
      call makeDatapoints(npoints,def_data%eps_0,def_data%eps_1,deps,vEps,vSigma,info)
      if (info /= 0) then
            errmsg = 'Cannot make proper data points'
            call finalize(2)
      endif
      !
      ! Initialize AlTay
      altay_cnf%output_prefix = 'alTayPolyHard'
      altay_cnf%jobtitle = 'alTayPolyHard'
      !
      call initAltay(altay_cnf,info)
      if (info /= 0) then
            errmsg = 'Cannot initialize libaltay'
            call finalize(2)
      endif
      !
      ! Prepare strain increment tensor
      deltaEps_norm = norm2(def_data%tDeps)
      if (deltaEps_norm < epsilon(0.D0)) then
            errmsg = 'Input error: norm of strain increment must not be zero.'
            call finalize(2)
      endif
      !
      tDeltaEps = def_data%tDeps / deltaEps_norm * deps
      !
      ! Init and configure steps
      call initStepData(npoints,astate,info)
      if (info /= 0) then
            errmsg = 'Cannot initialize data structure for alamel results'
            call finalize(2)
      endif      
      !
      ! Configure steps
      do i=1,npoints
            astate%simulCalls(i)%input%full_model = .true.
            astate%simulCalls(i)%input%keep_texture = .false.
            astate%simulCalls(i)%input%keep_state = .false.
            astate%simulCalls(i)%input%dgf = tDeltaEps
            call setStepType(astate%simulCalls(i)%input,altay_cnf%model_id,info)
      enddo
      !      
      ! Run AlTay
      call runSteps(astate,info)
      if (info /= 0) then
           errmsg = 'Multilevel model failed.' 
           call finalize(2) 
      endif
#ifdef DIAGNOSTICS
      do i = 1, npoints
            write(*,fmt=500) astate%simulCalls(i)%output%stress_tensor
      enddo
500 format(3(3(E15.6,1X),/))
#endif
      !
      ! Get average stresses
      vSigma = astate%simulCalls(:)%output%average_stress * unit_conversion_factor
      !
      call makeApproximation(vEps,vSigma,hardApprox,info)
      !
      call finalizeAltay(info)
      !
      call writeOutputs(cnf,hardApprox,vEps,vSigma,info)
!      
end program
