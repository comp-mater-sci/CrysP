program alamDMC
    use logging
    use dmcBasicModule
    use dmcASR
    use dmcQRS
    use dmcYld
    use dmcADP

    implicit none

    character(*), parameter:: MOD_NAME  = 'CrysP', &
                              PROC_NAME = 'main'

    integer:: cnfunit, &
              info,    &
              i
    character(:), allocatable     ::  moduleName
    character(256), dimension(0:2)::  argv
    class(BasicModule), pointer   ::  the_module              => null()

    !> Process the command line and put the command line arguments into an allocatable array of strings (argv)
    !> Strictly 2 arguments of max 256 characters allowed.
    if (command_argument_count() /= 2) call log_error(MOD_NAME, PROC_NAME, ERR_IO, '2 arguments required.')
    do i = 0, 2
        call get_command_argument(i, length = info)
        if (info > 256) call log_error(MOD_NAME, PROC_NAME, ERR_IO, 'Arguments must be no longer than 256 characters.')
        call get_command_argument(i, argv(i))
    enddo

    ! Create a module of appropriate type:
    moduleName = trim(argv(1))
    select case(moduleName)
        case('QRS')
            allocate(QRSModule:: the_module)
        case('ASR')
            allocate(ASRModule:: the_module)
        case('YLD')
            allocate(YldModule:: the_module)
        case('ADP')
            allocate(ADPModule:: the_module)
        case default
            call log_error(MOD_NAME, PROC_NAME, ERR_VAL, 'Unknown command.')
    end select

    !Determine output file name from config file name
    i=1
    do while (argv(2)(i:i) /= '.')
        i=i+1
    end do
    the_module%output_prefix = argv(2)(:i-1)

    open(newunit = cnfunit, file = trim(argv(2)), status='old', iostat = info)
    if (info /= VEF_OK) &
        call log_error(MOD_NAME, PROC_NAME, ERR_IO, 'Can not open config file')
    call the_module%initialize(cnfunit)
    close(cnfunit)

    call the_module%run(info)

    write(display_unit, '(A, 1X, A, 1X, A)',advance='no') 'Execution of module', trim(moduleName), 'finished'
    write(display_unit, '(1X, A)') merge('succesfully.','with errors.',info == 0)
end program
