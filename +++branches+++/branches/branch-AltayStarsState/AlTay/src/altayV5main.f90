!
! $Id$
!
program altayV5main
use criRuntime
use criPath
use altayAPI
use altaySimulation
use altayConfigReader
implicit none
!
type(SimulationConfig),target :: config
type(Simulation),target :: the_simulation
type(altayInputConfigReader) :: config_reader
character(len=max_pathlen) :: input_path
integer :: info
!
    call get_command_argument(1, input_path, status=info)
    
    info = config_reader%setInput(input_path)
    info = config_reader%read(config)
    
    info = altay_initialize()
    
    info = the_simulation%initialize(config)
    info = the_simulation%runSteps()
    
    info = altay_finalize()
!
end program
