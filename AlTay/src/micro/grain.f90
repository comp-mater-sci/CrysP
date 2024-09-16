module grain_module
    use utils
    use logging
    use slip_systems

    implicit none
    private

    character(*), parameter:: MOD_NAME = 'dynfil'

    !>Texture-related state variables for single grain
    type:: grain
        real(DP)::                  sum_slip = 0._DP
        real(DP), dimension(3, 3):: orientation
        type(SlipSystem), dimension(:), allocatable:: slip_systems
    end type

    integer                                    :: nrStep = 0       !<State variable: step number.

    public  ::  grain, &
                nrStep

end module
