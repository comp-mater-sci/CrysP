!> Strain-(rate) driven step
module dmcStrainDrivenStep
    use base_defs
    use altayConfig
    use altay

    implicit none

    private
    public:: StrainDrivenStep, &
             StepOutput, &
             IncrementOutput

    character(*), parameter:: MOD_NAME = 'straindrivenstep'

    !> A strain-(rate) driven step
    type:: StrainDrivenStep
        real(DP), dimension(3, 3):: velocity_gradient
        real(DP)                 :: time_increment
        logical                  :: update_state = .false.
        logical                  :: output_state = .false.
    contains
        procedure:: execute => StrainDrivenStep_execute
    end type

    !> Outputs collected per increment
    type:: IncrementOutput
        real(DP), dimension(3, 3):: velocity_gradient, &
                                    stress
    end type

    !> Outputs collected per step
    type:: StepOutput
        type(IncrementOutput), dimension(:), allocatable:: increments
    end type

contains

    !> Execute step and store the output in step_output
    !>
    !> Returns VEF_OK on success.
    integer function StrainDrivenStep_execute(this, step_output, taylor_factor) result(info)
        class(StrainDrivenStep), intent(inout)  :: this
        class(StepOutput), intent(out)               :: step_output
        real(DP), intent(out):: taylor_factor

        integer:: n_increments, i
        real(DP):: increment_size, &
                   increment_strain(3, 3), &
                   velocity_gradient(3,3), &
                   stress(3,3)

        !Step size equivalent to target accuracy
        n_increments = ceiling(norm2(this%velocity_gradient)/ ACCURACY)
        velocity_gradient = this%velocity_gradient / n_increments !Velocity gradient is equal to strain increment due to small strain
                                                       !assumption and implicit step duration of 1s.

        allocate(step_output%increments(n_increments))

        do i = 1, n_increments
            step_output%increments(i)%velocity_gradient = velocity_gradient
            call deformation_step(velocity_gradient, step_output%increments(i)%stress, taylor_factor)
        end do

        info = VEF_OK
    end function
end module
