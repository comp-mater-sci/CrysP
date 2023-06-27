program alamDMC
    use logging
    use dmcBasicModule
    use dmcASR
    use dmcQRS
    use dmcUDSA
    use dmcYld
    use dmcEWC
    use dmcADP

    implicit none

    character(*),                parameter  ::  MODULE_NAME     = 'alamDMC',                    &
                                                PROCEDURE_NAME  = 'main'

    integer                                 ::  info,                               &
                                                cnfunit,                            &
                                                i
    character(:), allocatable               ::  moduleName
    character(256), dimension(0:2)          ::  argv
    class(BasicModule), pointer             ::  the_module              => null()

    !> Process the command line and put the command line arguments into an allocatable array of strings (argv)
    !> Strictly 2 arguments of max 256 characters allowed.
    if (command_argument_count() /= 2) call log_error(MODULE_NAME, PROCEDURE_NAME, ERR_IO, '2 arguments required.')
    do i = 0, 2
        call get_command_argument(i, length=info)
        if (info > 256) call log_error(MODULE_NAME, PROCEDURE_NAME, ERR_IO, 'Arguments must be no longer than 256 characters.')
        call get_command_argument(i, argv(i))
    enddo

    !> Open file fpath in the mode given by status, or call finalize on failure.
    open(newunit=cnfunit, file=trim(argv(2)), status='old', iostat=info)
    if (info /= VEF_OK) call log_error(MODULE_NAME, PROCEDURE_NAME, ERR_IO, 'Can not open config file')

    ! Create a module of appropriate type:
    moduleName = trim(argv(1))
    select case(moduleName)
        case('QRS')
            allocate(QRSModule :: the_module)
        case('UDSA')
            allocate(UDSAModule :: the_module)
        case('ASR')
            allocate(ASRModule :: the_module)
        case('YLD')
            allocate(YldModule :: the_module)
        case('EWC')
            allocate(EWCModule :: the_module)
        case('ADP')
            allocate(ADPModule :: the_module)
        case default
            call log_error(MODULE_NAME, PROCEDURE_NAME, ERR_VAL, 'Unknown command.')
    end select

    ! Read the configuration file:
    if (the_module%ReadConfig(cnfunit) /= VEF_OK) call log_error(MODULE_NAME, PROCEDURE_NAME, ERR_IO, 'Could not read module config.')
    close(cnfunit)

    ! Initialize the module
    if (the_module%initialize() /= VEF_OK) call log_error(MODULE_NAME, PROCEDURE_NAME, ERR, 'Cannot initialize module.')

    ! Run the module
    call the_module%run(info)

    write(display_unit,'(A,1X,A,1X,A)',advance='no') 'Execution of module', trim(moduleName), 'finished'
    write(display_unit,'(1X,A)') merge('succesfully.','with errors.',info==0)

    ! Finalize
    if (the_module%finalize() /= VEF_OK) call log_error(MODULE_NAME, PROCEDURE_NAME, ERR, 'Error finalizing module.')

end program
