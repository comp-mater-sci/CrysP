! $Id$
!
!>    \author Jerzy Gawad                                                
!>    Email:  Jerzy.Gawad@cs.kuleuven.be
!>
!>    Organization: Katholieke Universiteit Leuven
!>    Organization unit: Dept.Comp.Sci., TWR Group
!>                                                             
!>    \date Date of the initial release: 2013-02-16
!>    $Revision$
!>    $Date$
!>
!>    History of modifications: (see svn log)

!> The module defines abstract types for computational modules
module dmcAbstractModule
implicit none

      !> Abstract class for computational modules of the DMC
      type,abstract     :: AbstractModule
            
      contains

            !> Initialization of the configured object. 
            !>
            !> This method has to be called prior to the invocation of "run".
            procedure(IF_AbstractModule_initialize),deferred,pass(this)       :: initialize

            !> Read the configuration from a configuration file.
            procedure(IF_AbstractModule_readConfig),deferred,pass(this)       :: readConfig

            !> Print a nicely formatted info about the configuration.
            procedure(IF_AbstractModule_printConfig),deferred,pass(this)      :: printConfig
            
            !> Start the calculations.
            procedure(IF_AbstractModule_run),deferred,pass(this)              :: run
            
            !> Finalization of the object
            procedure(IF_AbstractModule_finalize),deferred,pass(this)         :: finalize
            
      end type

      
      abstract interface
      
            integer function IF_AbstractModule_initialize(this)
            import :: AbstractModule
                  class(AbstractModule),intent(inout) :: this
            end function
      
            integer function IF_AbstractModule_printConfig(this,outunit) 
            import :: AbstractModule
                  class(AbstractModule),intent(in)    :: this
                  integer,intent(in)                  :: outunit
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