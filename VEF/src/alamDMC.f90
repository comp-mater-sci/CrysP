program alamDMC
    use logging
    use dmcUtils, only: display_unit
    use dmcBasicModule
    use dmcASR
    use dmcQRS
    use dmcUDSA
    use dmcYld
    use dmcEWC
    use dmcADP
    use criLinearMap
    use criPath

    implicit none

    integer,                     parameter  ::  Q_ID            = 1,                            &
                                                UDSA_ID         = 2,                            & 
                                                ASR_ID          = 3,                            &
                                                YLD_ID          = 4,                            &
                                                EWC_ID          = 5,                            &
                                                ADP_ID          = 6
    character(*),                parameter  ::  MODULE_NAME     = 'alamDMC',                    &
                                                PROCEDURE_NAME  = 'main'
    type(MapItem), dimension(6), parameter  ::  COMMAND_MAP     =  [MapItem('QRS', Q_id),       &
                                                                    MapItem('UDSA', UDSA_id),   &
                                                                    MapItem('ASR', ASR_id),     &
                                                                    MapItem('YLD', YLD_id),     &
                                                                    MapItem('EWC', EWC_id),     &
                                                                    MapItem('ADP',ADP_id)]

    logical                                 ::  moduleFound             = .false.
    integer                                 ::  info,                               &
                                                cnfunit,                            &
                                                command_id,                         &
                                                i
    character(32)                           ::  moduleName              = ''
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
          
    ! Attempt to identify the command argument in the first place
    if (.not. resolveName(COMMAND_MAP, argv(1), command_id))    call log_error(MODULE_NAME, PROCEDURE_NAME, ERR_VAL, 'Unknown command.')
    if (.not. resolveId(COMMAND_MAP, command_id, moduleName))   call log_error(MODULE_NAME, PROCEDURE_NAME, ERR_VAL, 'Invalid module name.')
    
    !> Open file fpath in the mode given by status, or call finalize on failure.
    open(newunit=cnfunit, file=trim(argv(2)), status='old', iostat=info)
    if (info /= VEF_OK) call log_error(MODULE_NAME, PROCEDURE_NAME, ERR_IO, 'Can not open config file')

    ! Create a module of appropriate type:
    select case(command_id)
        case(Q_id)
            allocate(QRSModule :: the_module)
        case(UDSA_id)
            allocate(UDSAModule :: the_module)
        case(ASR_id)
            allocate(ASRModule :: the_module)
        case(YLD_id)
            allocate(YldModule :: the_module)
        case(EWC_id)
            allocate(EWCModule :: the_module)
        case(ADP_id)
            allocate(ADPModule :: the_module)
    end select

    if (.not. associated(the_module)) call log_error(MODULE_NAME, PROCEDURE_NAME, ERR, 'Cannot instantiate requested module.')

    ! Read the configuration file:
    if (the_module%ReadConfig(cnfunit) /= VEF_OK) call log_error(MODULE_NAME, PROCEDURE_NAME, ERR_IO, 'Could not read module config.') 
    close(cnfunit)

    ! Initialize the module
    if (the_module%initialize() /= VEF_OK) call log_error(MODULE_NAME, PROCEDURE_NAME, ERR, 'Cannot initialize module.')

    ! Show general configuration of the multilevel model
    info = the_module%printConfig(display_unit)

    ! Run the module
    call the_module%run(info)

    write(display_unit,'(A,1X,A,1X,A)',advance='no') 'Execution of module', trim(moduleName), 'finished'
    write(display_unit,'(1X,A)') merge('succesfully.','with errors.',info==0)

    ! Finalize 
    if (the_module%finalize() /= VEF_OK) call log_error(MODULE_NAME, PROCEDURE_NAME, ERR, 'Error finalizing module.')

#define MSG_GROUP_ERRORS
#include "msgFormats.inc"
#undef MSG_GROUP_ERRORS
end program
