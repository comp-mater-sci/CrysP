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
        real(DP), dimension(3, 3):: L, &
                                    S
        real(DP)::  vm_stress = 0._DP, &
                    plastic_work_inc = 0._DP
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
        call initStepData(n_increments, astate, info)
        if (info /= VEF_OK) &
            call log_error(MOD_NAME, 'execute', ERR, 'Could not initialize step data')

        ! Set-up the substeps
        do i=1,n_increments
            astate%simulCalls(i)%velocity_gradient = increment_strain
        enddo

        call runSteps(astate, info)
        if (info /= VEF_OK) &
            call log_error(MOD_NAME, 'execute', ERR, 'Error while running steps')

        info = step_output%collect(n_increments)
        if (info /= VEF_OK) return
    end function

    !> Collect the outputs from the AlTay simulation
    integer function StepOutput_collect(this, n_increments) result(info)
        class(StepOutput), intent(inout)     :: this
        integer, intent(in)                  :: n_increments
        !
        integer:: i, ierr, n_simulcalls
        !
        ! Check if the input and the state of libaltay correspond.
        if (allocated(astate%simulcalls)) then
            n_simulcalls = size(astate%simulcalls)
            if (n_simulcalls < n_increments .or. n_simulcalls /= astate%nSimulCalls) then
                 info = VEF_ERROR
                 return
            end if
        else
            call log_error(MOD_NAME, 'stepoutput_collect', ERR_VAL, 'Can not collect output if no simul calls are present')
        end if

        allocate(this%increments(n_increments))
        !
        ! collect the results
        do i = 1, n_increments
            associate (increment_output =>  this%increments(i), &
                       altay_state => astate%simulCalls(i))

                increment_output%L = altay_state%velocity_gradient
                increment_output%S = altay_state%stress
                increment_output%vm_stress = sqrt(3._DP/2._DP)*norm2(altay_state%stress)
                increment_output%plastic_work_inc = increment_output%L .dot. increment_output%S !Works because S is symmetric
            end associate
        enddo
        info = VEF_OK
    end function
end module
