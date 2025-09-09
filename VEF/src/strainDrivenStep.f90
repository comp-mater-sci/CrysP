#include "criMacros.fpp"

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
        real(DP), dimension(3, 3):: L, &
                                    S
        real(DP):: vm_strain_begin = 0._DP, &
                    vm_strain_end = 0._DP, &
                    vm_stress = 0._DP, &
                    plastic_work_inc = 0._DP, &
                    taylor_factor = 0._DP, &
                    plastic_slip_tot = 0._DP, &
                    vmeqstrainrate = 0._DP
    end type

    !> Outputs collected per step
    type:: StepOutput
        type(IncrementOutput), dimension(:), allocatable:: increments
    contains
        procedure, pass(this)        :: collect => StepOutput_collect
    end type

contains

    !> Execute step and store the output in step_output
    !>
    !> Returns VEF_OK on success.
    integer function StrainDrivenStep_execute(this, step_output) result(info)
        class(StrainDrivenStep), intent(inout)  :: this
        class(StepOutput), intent(out)               :: step_output

        integer:: n_increments, i
        real(DP):: increment_size, &
                   increment_strain(3, 3)

        !Step size equivalent to target accuracy
        n_increments = ceiling(norm2(this%velocity_gradient)/ ACCURACY)
        increment_strain = this%velocity_gradient / n_increments

        ! Initialize AlTay structures
        RETURN_ON_WITH(call initStepData(n_increments, astate, info), info /= 0, info = VEF_ERROR)

        ! Set-up the substeps
        do i=1,n_increments
            ! Set input data for AlTay
            associate (input => astate%simulCalls(i)%input)
                input%dgf = increment_strain
                input%keep_texture = .not. this%update_state
                input%keep_state = .not. this%update_state
                input%full_model = .true.
                input%do_output_init = .false.
                input%do_output_final = this%output_state
            end associate
        enddo

        ! Call the AlTay
        RETURN_ON_WITH(call runSteps(astate, info), info /= 0, info = VEF_ERROR)
        RETURN_IF(info /= VEF_OK, info = step_output%collect(n_increments))
    end function

    !> Collect the outputs from the AlTay simulation
    integer function StepOutput_collect(this, n_increments) result(info)
    use altayConfig
    class(StepOutput), intent(inout)     :: this
    integer, intent(in)                  :: n_increments
    !
    integer:: i, ierr, n_simulcalls
    !
        ! Check if the input and the state of libaltay correspond.
        ALLOCATED_SIZE(n_simulcalls, astate%simulCalls)
        RETURN_IF(n_simulcalls < n_increments .or. n_simulcalls /= astate%nSimulCalls, info = VEF_ERROR)
        !
        ! Allocate storage for output
        RETURN_ON_WITH(allocate(this%increments(n_increments), stat = ierr), ierr /= 0, info = VEF_ERROR)
        !
        ! collect the results
        do i = 1, n_increments
            associate (increment_output =>  this%increments(i), &
                       altay_state => astate%simulCalls(i), &
                       altay_output => astate%simulCalls(i)%output)   ! HGH: originally altay_output => altay_state%output

                increment_output%L = altay_state%input%dgf
                increment_output%S = altay_output%stress_tensor
                increment_output%vm_strain_begin = altay_output%effective_macro_strain_tot
                increment_output%vm_strain_end = altay_output%effective_macro_strain_tot_end
                increment_output%vm_stress = altay_output%effective_stress
                ! D : S
                increment_output%plastic_work_inc = increment_output%L .dot. increment_output%S !Works because S is symmetric
                !
                increment_output%taylor_factor = altay_output%taylor_factor
                increment_output%plastic_slip_tot = altay_output%homogenised_slip_tot
                increment_output%vMeqStrainRate = tensor_to_von_mises(increment_output%L)

            end associate
        enddo
        info = VEF_OK
    end function
end module
