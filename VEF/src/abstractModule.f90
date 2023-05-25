!> The module defines abstract types for computational modules
module dmcAbstractModule
    implicit none

    !> Abstract class for computational modules of the DMC
    type,abstract     :: AbstractModule
    contains
        !> Initialization of the configured object.
        !> This method has to be called prior to the invocation of "run".
        procedure(IF_AbstractModule_initialize),deferred :: initialize
        !> Read the configuration from a configuration file.
        procedure(IF_AbstractModule_readConfig),deferred :: readConfig
        !> Start the calculations.
        procedure(IF_AbstractModule_run),deferred        :: run
        !> Finalization of the object
        procedure(IF_AbstractModule_finalize),deferred   :: finalize
    end type

    abstract interface
        integer function IF_AbstractModule_initialize(this)
        import :: AbstractModule
              class(AbstractModule),intent(inout) :: this
        end function

        integer function IF_AbstractModule_readConfig(this,cnfunit)
        import :: AbstractModule
              class(AbstractModule),intent(inout) :: this
              integer,intent(in)                  :: cnfunit
        end function

        subroutine IF_AbstractModule_run(this,info)
        import :: AbstractModule
              class(AbstractModule),intent(inout) :: this
              integer,intent(out)                 :: info
        end subroutine

        integer function IF_AbstractModule_finalize(this)
        import :: AbstractModule
              class(AbstractModule),intent(inout) :: this
        end function
    end interface
end module
