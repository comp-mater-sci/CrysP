module altaySub
    use hardening_model_dsh
    use altayIOConfig
    use altaySimul
    use altayMesostructure
    use altayTexFormats
    use altayConfig
    use hardening
    use altayDynfil
    use altayMiscutils
    use altayMacroKinematic
    use altayCurAccess
    use logging
    use parameters

    implicit none

    character(*), parameter, private :: MOD_NAME = 'altaySub'

contains

    !> Initialize the module.
    !>
    !> This subroutine must be called prior to any call to other
    !> module subroutines.
    subroutine initAltay(cnf,info,errmsg)

        type(altayConfigData),intent(inout)    :: cnf      !< configuration data
        integer,intent(out)                 :: info     !< exit code (altaySub_OK on success)
        character(len=*),intent(out),optional :: errmsg !< Error message (set if info /= altaySub_OK)
        character(*), parameter :: PROC_NAME = 'initAltay'

        integer :: ierr

        if (present(errmsg)) errmsg = '' ! clear errmsg
        ierr = 0
        ! Set the singleton object to the cnf
        acnf = cnf
        ! Open input files
        ! UNIT LEC = SLIP SYSTEMS; open slip system file
        open (unit=LEC,file=trim(cnf%slipsystem%input_fname),status='old',iostat=ierr)
        if (ierr /= VEF_OK) &
            call log_error(MOD_NAME, PROC_NAME, ERR_IO, 'Cannot open slip system definition file: ' // trim(cnf%slipsystem%input_fname))

        ! Load microstructure data
        CALL GRFIL(acnf%micros_fname,acnf%simul_init%FMicro,info)
        if (info /= VEF_OK) &
            call log_error(MOD_NAME, PROC_NAME, ERR_IO, 'Cannot process the microstructure file: ' // trim(acnf%micros_fname))

        ! Get the initial texture
        call loadTexture(cnf%texture%input_type,NDAT1,trim(cnf%texture%input_fname),cnf%texture%block_id,info)
        if (info /= VEF_OK) &
            call log_error(MOD_NAME, PROC_NAME, ERR_IO, 'Cannot process the texture data file: ' // trim(cnf%texture%input_fname))

        ! Open output files
        call openOutputFiles(cnf, info, errmsg)
        if (info /= VEF_OK) &
            call log_error(MOD_NAME, PROC_NAME, ERR_IO, 'Cannot open output files.')

        ! Initialize altay modules
        !
        ! Set the data for CRSS calculations
        call parameter_set(cnf%hardening_parameters, 'n_grains', size(DFIL), fail_on_absent=.false.)
        call hardening_init(cnf%hardening_parameters)
        ! Initialisation of SIMUL
        if (present(errmsg)) errmsg = 'Initialization call to the micromechanical model failed.'
        info = VEF_ERROR
        call SIMUL0()

        if (present(errmsg)) errmsg = ''
        ! No need for the slip system definition anymore.
        close(LEC)
        info = VEF_OK
    end subroutine

    !> Finalizes the module and releases the resources.
    subroutine finalizeAltay(info)
        integer,intent(out)                 :: info     !< exit code (0 on success)

        ! Close all units.
        close(LEC)
        close(KLEC)
        close(IMP)
        close(IMP3)
        close(IMP5)
        call MICROSTR_finalize(info)
        if (info /= 0) return
        call DYNFIL_finalize(info)
        if (info /= 0) return
        call hardening_finalize()
        if (allocated(astate%simulCalls)) then
              deallocate(astate%simulCalls)
              astate%nSimulCalls = 0
        endif
    end subroutine

    subroutine openOutputFiles(cnf, info, errmsg)
        type(altayConfigData),intent(in)    :: cnf      !< configuration data
        integer,intent(out)                 :: info     !< exit code (altaySub_OK on success)
        character(len=*),intent(out),optional :: errmsg !< Error message (set if info /= altaySub_OK)

        character(len=fname_len) :: fname_prefix, fname
        character(*), parameter :: PROC_NAME = 'openOutputFiles'

        fname_prefix = cnf%output_prefix
        info = VEF_ERROR

        if (cnf%output_config%nfiltw /= 0) then
            fname = trim(fname_prefix)//'.TWN'
            open (unit=IMP3,file=fname,status='replace',err=9999)
        endif

        if (cnf%output_config%nfile /= 0) then
            fname = trim(fname_prefix)//'.CUR'
            ! IMP1=output file with successive "current situations"
            open (unit=IMP1,file=fname,status='replace',err=9999)
        endif

        if (cnf%output_config%NMSS /= 0) then
            fname = trim(fname_prefix)//'.MSS'
            ! UNIT IMP5 = homogenized strain-stress
            open (unit=IMP5,file=fname,status='replace',err=9999)
            call writeMSSHeader(IMP5,info)
        endif

        info = VEF_OK
        return

        ! Error handler
        9999 continue
        call log_error(MOD_NAME, PROC_NAME, ERR_IO, 'Cannot open file '//trim(fname))
    end subroutine

    !> Initialization of input and output data for the steps.
    subroutine initStepData(nsteps,steps,info)
        integer,intent(in)                  :: nsteps   !< Number of steps to be created
        type(altayStateData),intent(out)    :: steps    !< Definiton of the steps.
        integer,intent(out)                 :: info     !< Exit code: 0 on success
        integer :: ierr
        info = 1
        if (nsteps <= 0) return
        allocate(steps%simulCalls(nsteps),stat=ierr)
        steps%nSimulCalls = nsteps
        steps%this = 0
        ! No need to specifically initialize other components,
        ! since there are initializers provided in the datatype.
        info = ierr
    end subroutine

    !> Run the AlTay for the set of steps
    subroutine runSteps(steps,info)
        type(altayStateData),intent(inout)        :: steps !< Definiton of the steps.
        integer,intent(out)                       :: info  !< Exit code: 0 on success.
        integer :: NFILE0
        integer :: i
        logical :: input_ok
        type(DeformationRate) :: MacroDefRate
        ! Validate input
        info = VEF_ERROR
        input_ok = .false.
        if (allocated(steps%simulCalls)) input_ok = (size(steps%simulCalls) == steps%nSimulCalls)
        if (.not. input_ok) return
        ! Assign steps with astate
        astate = steps
        ! Clean exception stack from a previous (possibly unsuccessful)
        ! set of calls.
        info = VEF_ERROR
        !
        do i = 1, steps%nSimulCalls
            steps%this = i
            NFILE0 = merge(1,0,steps%simulCalls(i)%input%do_output_init)
            call Set_DeformationRate(steps%simulCalls(i)%input%dgf,MacroDefRate)
            ! Run simul.
            call SIMUL1(NFILE0,MacroDefRate)

            if (steps%simulCalls(i)%input%do_output_final) call outputCurrentState(info)
        enddo

        info = VEF_OK
    end subroutine

    !> Write out the current state variables.
    !>
    !> The call may involve IO units: IMP1 (CUR file), IMP4 (PEBP state file) and IMP5 (MSS file).
    !> Appropriate control fields in acnf%output_config are checked to decide if the data have to
    !> be actually written to corresponding IO units.
    subroutine outputCurrentState(info)
        integer,intent(out)           :: info

        info = VEF_OK
        if (acnf%output_config%nfile == 1) call CURwriteBlock(IMP1,info)
        if (info /= 0) return

        if ((acnf%output_config%nmss == 1) .and. allocated(astate%simulCalls)) then
            associate (callout => astate%simulCalls(astate%this)%output)
                  call writeMSSRecord(IMP5, &
                                      callout%effective_macro_strain, callout%effective_macro_strain_tot, &
                                      callout%homogenised_slip,callout%homogenised_slip_tot, &
                                      callout%stress_tensor, &
                                      callout%taylor_factor, callout%strain_rate_heterogeneity, &
                                      info)
            end associate
        endif
    end subroutine

end module
