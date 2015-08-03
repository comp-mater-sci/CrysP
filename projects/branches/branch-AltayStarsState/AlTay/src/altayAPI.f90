!> Defines API of the AlTay
!>
!>
module altayAPI
use altayConfig
use FH5
implicit none

!> Fundamental concepts in altay API:
!> *Configuration*: complete set of parameters that define the context in which the AlTay will run
!> *Simulation*: 

!> Other concepts:

!> *State*: set of state variables that is complete with respect to the Configuration
!> *Step*: Description of deformation to be imposed. This entity consists of two parts:
!   * Increment: 
!   * Output request:
! *Output variables*: Depending on resolution, any output variable falls into one of the three categories:
!   * Grain-level variables: quantities that can be requested per single crystal
!   * Cluster-level variables: quantities that can be requested per cluster
!   * Homogenized variables: quantities that can be requested per aggregate.

contains

    integer function altay_initialize() result(info)
    implicit none
    !
        info = FH5_initialize()
    !
    end function
    
    integer function altay_finalize() result(info)
    implicit none
    !
        info = FH5_finalize()
    !
    end function
    
end module
