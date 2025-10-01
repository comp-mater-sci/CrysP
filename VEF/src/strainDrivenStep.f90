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
        call initStepData(n_increments, astate, info)
        if (info /= VEF_OK) &
            call log_error(MOD_NAME, 'execute', ERR, 'Could not initialize step data')



        ! Set-up the substeps
        do i=1,n_increments
            ! Set input data for AlTay
            associate (input => astate%simulCalls(i)%input)
                input%dgf = increment_strain
            end associate
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
