!> This module contains the configuration parameters of IO operations
!> as well as other IO-related entities, such as IO unit numbers.
module altayIOConfig
    implicit none

    integer :: LEC = 4   !< data set with slip systems
    integer :: IMP = 3   !< printer
    integer :: IMP1 = 7  !< output-file with successive "current situations"
    integer :: IMP3 = 11 !< output-file with twinning information

    integer :: IMP5 = 111     !< output of stress-strain or slip-stress
    integer :: NDAT1 = 9      !< input texture file
    integer :: NDAT2 = 10     !< input microstructure file

end module
