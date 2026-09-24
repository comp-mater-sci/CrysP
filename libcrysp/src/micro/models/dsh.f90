!> Dislocation Substructural Hardening (DSH) is a family of physics-based hardening models mapping the movement of (clusters of) dislocations.
!>
!> First formulated and documented in Bart Peeters's PhD thesis:
!> 'Multiscale modelling of the induced plastic anisotropy in IF steel during sheet forming'.
!> Several variants of this model have been formulated, each of which considers different types of dislocations.
!>
!> This file contains the logic common to all variants, which is everything except for the definition of the dislocation movement vectors.
!>
!> @note
!> Only the BCC24 slip system set is supported.
!> @endnote

module dsh
    use base_defs
    use math_utils
    use constitutive_model
    use crysp_model
    use crysp_status
    use slip_systems
    use conversions
    use crysp_serialization
    use crysp_input

    implicit none

    private
    public:: CBBNORMAL, &
             ConstitutiveModelDSH

    character(*), parameter:: MOD_NAME = 'dsh'

    real(DP), parameter:: MINFRAC = 2.0D-5,   &     !! See B. Peeters: Multiscale modelling of the induced plastic anisotropy in IF steel during sheet forming (PhD thesis). NOTE original was 2.0D-3. Set to 2.0D-5 because the
                                                     !! original value was problematic for very small time increments
                          LOWFRAC = 10.0D-5        !! Idem to MINFRAC. Original value was 10.0D-3
    real(DP), dimension(6, 3), parameter:: CBBNORMAL = transpose(real(SLIP_SYSTEMS_BCC_110(:,1, 1:12:2), DP)/sqrt(2._DP)) !! Normal on the
                                                                                                                   !! cell block boundaries. See PhD thesis Peeters.
!! Different DSH models use the CBB normal vectors to derive the dislocation movement vectors

    !> Type representing cell block boundaries. See PhD thesis Peeters for the meaning of the fields.
    type:: CBBtype
        real(DP):: RHOwd = 0._DP, &
                   RHOwp = 0._DP, &
                   RHOwdHOM = 0._DP, &
                   accGAMMA_new = 0._DP, &
                   RHOwd_ini = 0._DP
    end type

    type, extends(HardeningState):: DSHState    !! Grain-bound state. See PhD thesis Peeters for the meaning of the fields.
        real(DP)                   :: RHOcb = 0._DP
        type(CBBtype), dimension(6):: CBB
        integer, dimension(2)      :: ActiveCBB = 0
    contains
        procedure:: size => dsh_state_size
        procedure:: serialize => dsh_state_serialize
        procedure:: deserialize => dsh_state_deserialize
    end type

    !Not abstract: variants call `this%ConstitutiveModelDSH%describe()`, which F2018 19.4.5 forbids on an abstract parent.
    type, extends(ConstitutiveModel):: ConstitutiveModelDSH !! Model state common to all DSH models.
                                                                     !! Each DSH model variant extends this base model.
                                                                     !! See PhD thesis Peeters for the meaning of the fields.
        real(DP):: b            !! Magnitude of Burgers vector
        real(DP):: G            !! Shear modulus
        real(DP):: alfa         !! Dislocation interaction parameter
        real(DP):: f            !! Volume fraction of cell block boundaries
        real(DP):: tau0         !! Initial critical resolved shear stress on all slip systems
        real(DP):: I            !! Immobilization coefficient of Cell Boundaries
        real(DP):: R            !! Recovery coefficient of cell boundaries
        real(DP):: Iwd          !! Immobilization coefficient of CBBs
        real(DP):: Rwd          !! Recovery coefficient of CBBs
        real(DP):: Rncg         !! Recovery coefficient of old CBBs and polarity of old CBBs
        real(DP):: beta1        !! 1st coeff. micro shear band cut-through of old CBBs
        real(DP):: beta2        !! 2nd coeff. micro shear band cut-through of old CBBs
        real(DP):: Iwp          !! Immobilization coefficient of polarity of CBBs
        real(DP):: Rwp          !! Recovery coefficient of polarity of CBBs
        real(DP):: Rrev         !! Recovery coefficient of polarity CBBs during bauschinger
        real(DP):: R2           !! Recovery coefficient of CBs due to reversal polarity flux
        real(DP):: RHOwpSAT
        real(DP):: RHOwdMIN
        real(DP):: RHOwpMIN
        real(DP):: RHOwpLOW
        real(DP), dimension(24, 6):: effslashb    = 0._DP
        real(DP), dimension(24, 6):: alfa_G_b_eff = 0._DP
    contains
        procedure:: deform                      => dsh_deform              !! Inherited from ConstitutiveModel
        procedure, nopass:: make_hardening_state => dsh_make_state
        procedure:: init_hardening_state => dsh_init_state
        procedure:: size => dsh_size
        procedure:: serialize  => dsh_serialize
        procedure:: deserialize => dsh_deserialize
        procedure:: describe => dsh_describe
        procedure:: init_common, &
                    update_crss
    end type

contains

    !> Everything the DSH variants have in common. Each variant adds its own name and description.
    function dsh_describe(this) result(desc)
        class(ConstitutiveModelDSH), intent(in):: this
        type(ModelDescriptor):: desc

        integer:: i

        desc = this%ConstitutiveModel%describe()
        desc%inputs = [Input('b', &
                             "Magnitude of the Burgers vector [m]", &
                             INPUT_REAL, &
                             lower_bound=serialize(0._C_DOUBLE), &
                             upper_bound=serialize(1.E-8_C_DOUBLE)), &
                       Input('G', &
                             "Shear modulus [MPa]", &
                             INPUT_REAL, &
                             lower_bound=serialize(1.E4_C_DOUBLE), &
                             upper_bound=serialize(5.E5_C_DOUBLE)), &
                       Input('alfa', &
                             "Dislocation interaction parameter", &
                             INPUT_REAL, &
                             lower_bound=serialize(0._C_DOUBLE), &
                             upper_bound=serialize(5._C_DOUBLE)), &
                       Input('f', &
                             "Volume fraction of cell block boundaries", &
                             INPUT_REAL, &
                             lower_bound=serialize(0._C_DOUBLE), &
                             upper_bound=serialize(1._C_DOUBLE)), &
                       Input('tau0', &
                             "Initial critical resolved shear stress on all slip systems [MPa]", &
                             INPUT_REAL, &
                             lower_bound=serialize(0._C_DOUBLE), &
                             upper_bound=serialize(1.E4_C_DOUBLE)), &
                       Input('I', &
                             "Immobilization coefficient of cell boundaries", &
                             INPUT_REAL, &
                             lower_bound=serialize(0._C_DOUBLE), &
                             upper_bound=serialize(10._C_DOUBLE)), &
                       Input('R', &
                             "Recovery coefficient of cell boundaries [m]", &
                             INPUT_REAL, &
                             lower_bound=serialize(0._C_DOUBLE), &
                             upper_bound=serialize(10._C_DOUBLE)), &
                       Input('Iwd', &
                             "Immobilization coefficient of cell block boundaries", &
                             INPUT_REAL, &
                             lower_bound=serialize(0._C_DOUBLE), &
                             upper_bound=serialize(10._C_DOUBLE)), &
                       Input('Rwd', &
                             "Recovery coefficient of cell block boundaries [m]", &
                             INPUT_REAL, &
                             lower_bound=serialize(0._C_DOUBLE), &
                             upper_bound=serialize(1.E-6_C_DOUBLE)), &
                       Input('Rncg', &
                             "Recovery coefficient of old cell block boundaries and their polarity [m]", &
                             INPUT_REAL, &
                             lower_bound=serialize(0._C_DOUBLE), &
                             upper_bound=serialize(1.E-6_C_DOUBLE)), &
                       Input('beta1', &
                             "1st coefficient of micro shear band cut-through of old cell block boundaries", &
                             INPUT_REAL, &
                             lower_bound=serialize(0._C_DOUBLE), &
                             upper_bound=serialize(1.E-6_C_DOUBLE)), &
                       Input('beta2', &
                             "2nd coefficient of micro shear band cut-through of old cell block boundaries", &
                             INPUT_REAL, &
                             lower_bound=serialize(0._C_DOUBLE), &
                             upper_bound=serialize(1.E-6_C_DOUBLE)), &
                       Input('Iwp', &
                             "Immobilization coefficient of the polarity of cell block boundaries", &
                             INPUT_REAL, &
                             lower_bound=serialize(0._C_DOUBLE), &
                             upper_bound=serialize(1.E-6_C_DOUBLE)), &
                       Input('Rwp', &
                             "Recovery coefficient of the polarity of cell block boundaries [m]", &
                             INPUT_REAL, &
                             lower_bound=serialize(0._C_DOUBLE), &
                             upper_bound=serialize(1.E-6_C_DOUBLE)), &
                       Input('Rrev', &
                             "Recovery coefficient of cell block boundary polarity during Bauschinger [m]", &
                             INPUT_REAL, &
                             lower_bound=serialize(0._C_DOUBLE), &
                             upper_bound=serialize(100._C_DOUBLE)), &
                       Input('R2', &
                             "Recovery coefficient of cell boundaries due to reversal polarity flux [m]", &
                             INPUT_REAL, &
                             lower_bound=serialize(0._C_DOUBLE), &
                             upper_bound=serialize(100._C_DOUBLE))]

        !Mirrors dsh_serialize, in the same order.
        desc%model_state = [desc%model_state, &
                            StateDescriptor("b", &
                                            "Magnitude of the Burgers vector [m]", &
                                            TYPE_REAL), &
                            StateDescriptor("G", &
                                            "Shear modulus [MPa]", &
                                            TYPE_REAL), &
                            StateDescriptor("alfa", &
                                            "Dislocation interaction parameter", &
                                            TYPE_REAL), &
                            StateDescriptor("f", &
                                            "Volume fraction of cell block boundaries", &
                                            TYPE_REAL), &
                            StateDescriptor("tau0", &
                                            "Initial critical resolved shear stress on all slip systems [MPa]", &
                                            TYPE_REAL), &
                            StateDescriptor("I", &
                                            "Immobilization coefficient of cell boundaries", &
                                            TYPE_REAL), &
                            StateDescriptor("R", &
                                            "Recovery coefficient of cell boundaries [m]", &
                                            TYPE_REAL), &
                            StateDescriptor("Iwd", &
                                            "Immobilization coefficient of cell block boundaries", &
                                            TYPE_REAL), &
                            StateDescriptor("Rwd", &
                                            "Recovery coefficient of cell block boundaries [m]", &
                                            TYPE_REAL), &
                            StateDescriptor("Rncg", &
                                            "Recovery coefficient of old cell block boundaries and their polarity [m]", &
                                            TYPE_REAL), &
                            StateDescriptor("beta1", &
                                            "1st coefficient of micro shear band cut-through of old cell block boundaries", &
                                            TYPE_REAL), &
                            StateDescriptor("beta2", &
                                            "2nd coefficient of micro shear band cut-through of old cell block boundaries", &
                                            TYPE_REAL), &
                            StateDescriptor("Iwp", &
                                            "Immobilization coefficient of the polarity of cell block boundaries", &
                                            TYPE_REAL), &
                            StateDescriptor("Rwp", &
                                            "Recovery coefficient of the polarity of cell block boundaries [m]", &
                                            TYPE_REAL), &
                            StateDescriptor("Rrev", &
                                            "Recovery coefficient of cell block boundary polarity during Bauschinger [m]", &
                                            TYPE_REAL), &
                            StateDescriptor("R2", &
                                            "Recovery coefficient of cell boundaries due to reversal polarity flux [m]", &
                                            TYPE_REAL), &
                            StateDescriptor("RHOwpSAT", &
                                            "Saturation value of the cell block boundary polarity. See PhD thesis Peeters.", &
                                            TYPE_REAL), &
                            StateDescriptor("RHOwdMIN", &
                                            "Lower bound on the cell block boundary dislocation density. See PhD thesis Peeters.", &
                                            TYPE_REAL), &
                            StateDescriptor("RHOwpMIN", &
                                            "Lower bound on the cell block boundary polarity. See PhD thesis Peeters.", &
                                            TYPE_REAL), &
                            StateDescriptor("RHOwpLOW", &
                                            "Threshold on the cell block boundary polarity. See PhD thesis Peeters.", &
                                            TYPE_REAL), &
                            StateDescriptor("effslashb", &
                                            "Per slip system and cell block boundary: effective boundary strength divided by the Burgers vector.", &
                                            TYPE_REAL_MATRIX), &
                            StateDescriptor("alfa_G_b_eff", &
                                            "Per slip system and cell block boundary: product of interaction parameter, shear modulus and effective Burgers vector.", &
                                            TYPE_REAL_MATRIX)]

        !Mirrors dsh_state_serialize, including its loop over the six cell block boundaries.
        desc%element_state = [desc%element_state, &
                              StateDescriptor("RHOcb", "Dislocation density of the cell boundaries.", TYPE_REAL)]
        do i = 1, 6
            desc%element_state = [desc%element_state, &
                                  StateDescriptor("CBB " // int_to_str(i) // ": RHOwd", &
                                                  "Dislocation density of cell block boundary " // int_to_str(i) // ".", &
                                                  TYPE_REAL), &
                                  StateDescriptor("CBB " // int_to_str(i) // ": RHOwp", &
                                                  "Polarity of cell block boundary " // int_to_str(i) // ".", &
                                                  TYPE_REAL), &
                                  StateDescriptor("CBB " // int_to_str(i) // ": RHOwdHOM", &
                                                  "Homogeneous part of the dislocation density of cell block boundary " // int_to_str(i) // ".", &
                                                  TYPE_REAL), &
                                  StateDescriptor("CBB " // int_to_str(i) // ": accGAMMA_new", &
                                                  "Slip accumulated on cell block boundary " // int_to_str(i) // " since it was formed.", &
                                                  TYPE_REAL), &
                                  StateDescriptor("CBB " // int_to_str(i) // ": RHOwd_ini", &
                                                  "Dislocation density of cell block boundary " // int_to_str(i) // " when it was formed.", &
                                                  TYPE_REAL)]
        end do
        desc%element_state = [desc%element_state, &
                              StateDescriptor("Active CBBs", &
                                              "Indices of the currently active cell block boundaries.", &
                                              TYPE_INT_ARRAY)]
    end function

    !> Convert a generic HardeningState to a pointer to a DSHState object
    !>
    !> Closest Fortran comes to type casting.
    !> A state of any other dynamic type is a defect in libCrysP rather than caller error, but is reported rather than fatal so that
    !> an embedding host survives it.
    function to_dsh_state(state, dsh_state_ptr) result(stat)
        class(HardeningState), target, intent(in):: state   !! HardeningState to be converted. Must have dynamic type DSHState.
        type(DSHState), pointer, intent(out):: dsh_state_ptr !! Pointer of type DSHState to the HardeningState
        type(Status):: stat                                 !! `CRYSP_OK`, or `CRYSP_ERR_TYPE` if the state is not a `DSHState`.

        select type (state)
            type is (DSHState)
                dsh_state_ptr => state
            class default
                dsh_state_ptr => null()
                stat = fail(MOD_NAME, 'to_dsh_state', CRYSP_ERR_TYPE, 'Hardening state is not a DSHState')
        end select
    end function

    !> Main model initialization procedure common to all variants of the DSH model family.
    !>
    !> The only difference between the variants of DSH is the interaction coefficients between dislocations and cell block
    !> boundaries. Thus, each model defines its own coefficients and calls this common initialization procedure with them.
    subroutine init_common(this, miller_indices, params, eff)
        class(ConstitutiveModelDSH), intent(inout):: this          !! DSH model variant to be initialized.
        integer, dimension(:,:,:), intent(in):: miller_indices     !! Miller indices of the deformation mechanism to be used.
        type(Parameter), dimension(:), intent(in):: params
        real(DP), dimension(24, 6), intent(in):: eff               !! 'Wall-effectivity' matrix == cosines of the angle between
                                                                   !! dislocation movement vectors and the cell block boundary normals.

        call this%ConstitutiveModel%init(miller_indices, params)

        this%b     = params(1)
        this%b = this%b * 1.e6_DP ![m] -> [um]
        this%G     = params(2)
        this%alfa  = params(3)
        this%f     = params(4)
        this%tau0  = params(5)
        this%I     = params(6)
        this%R     = params(7)
        this%R = this%R * 1.e6_dp ![m] -> [um]
        this%Iwd   = params(8)
        this%Rwd   = params(9)
        this%rwd = this%rwd * 1.e6_dp ![m] -> [um]
        this%Rncg  = params(10)
        this%rncg = this%rncg * 1.e6_dp ![m] -> [um]
        this%beta1 = params(11)
        this%beta2 = params(12)
        this%Iwp   = params(13)
        this%Rwp   = params(14)
        this%rwp = this%rwp * 1.e6_dp ![m] -> [um]
        this%Rrev  = params(15)
        this%rrev = this%rrev * 1.e6_dp ![m] -> [um]
        this%R2    = params(16)
        this%r2 = this%r2 * 1.e6_dp ![m] -> [um]

        !Calculate dependent hardening parameters
        this%RHOwdMIN = MINFRAC* (this%Iwd)**2 / (this%Rwd)**2  ! Minfrac*rho_wd_sat
        this%RHOwpSAT = (sqrt((this%Iwp/this%Rwp)**4+4._dp * (this%Iwp*this%Iwd / (this%Rwp*this%Rwd))**2) + (this%Iwp/this%Rwp)**2) / 2._dp
        this%RHOwpLOW = LOWFRAC*this%RHOwpSAT
        this%RHOwpMIN = MINFRAC*this%RHOwpSAT

        !Quantities derived from input parameters. Kept as fields to avoid having to recompute them all the time.
        this%effslashb       = eff/this%b
        this%alfa_G_b_eff    = this%alfa*this%G*this%b*eff
    end subroutine

    function dsh_make_state() result(state)
        class(HardeningState), allocatable:: state

        allocate(DSHState::state)
    end function

    !> Initialize the hardening state for a DSH grain.
    subroutine dsh_init_state(this, state)
        class(ConstitutiveModelDSH), intent(in):: this
        class(HardeningState), intent(out):: state

        call this%ConstitutiveModel%init_hardening_state(state)

        select type (state)
            type is (DSHState)
                state%RHOcb            = MINFRAC * (this%I)**2 / (this%R)**2  ! Minfrac*rho_cb_sat
                state%CBB%RHOwd        = this%RHOwdMIN
                state%CBB%RHOwp        = 0._DP
                state%CBB%RHOwdHOM     = this%RHOwdMIN
                state%CBB%accGAMMA_new = 0._DP
                state%CBB%RHOwd_ini    = this%RHOwdMIN
                state%ActiveCBB        = 0

                call this%update_crss(state)
        end select
    end subroutine

    pure function dsh_size(this) result(size)
        class(ConstitutiveModelDSH), intent(in):: this
        integer:: size

        size = this%ConstitutiveModel%size() + 22
    end function

    !> See cm_deform
    function dsh_deform(this, state, time, slip_rates) result(stat)
        class(ConstitutiveModelDSH), intent(inout)                  :: this
        class(HardeningState), target, intent(inout)                :: state
        real(DP), intent(in)                                        :: time
        real(DP), dimension(size(this%taylor_coeffs, 2)), intent(in):: slip_rates
        type(Status):: stat

        logical::  flux_reversal
        integer::  i, &
                   r(6)
        real(DP):: RHObausch   , &
                   rho_wp_a, &
                   wpflux, &
                   r_effective, &
                   sum_slip_active_cbb, &
                   sum_slip_rates_110(6)
        type(DSHState), pointer:: state_ptr  ! Pointer to grain_%hardening_state of type DSHState for easy access to DSH-specific fields

        stat = to_dsh_state(state, state_ptr)
        if (failed(stat)) return

        !Calculate quantities of slip rates and slips
        !Identify currently generated and non-currently generated walls
        do i = 1, 6
            sum_slip_rates_110(i) = sum(abs(slip_rates(2*i-1:2*i)))  !Sum of slip rates for the systems of each 110-plane
        end do

        !r(1) = plane with largest slip
        !r(2) = plane with 2nd largest slip
        !r(3:6) = remaining planes (unordered)
        r(1:2) = merge([1, 2], &
                       [2, 1], &
                       sum_slip_rates_110(1) >= sum_slip_rates_110(2))
        do i = 3, 6
            if (sum_slip_rates_110(i) > sum_slip_rates_110(r(1))) then
                r(i) = r(2)
                r(2) = r(1)
                r(1) = i
            else if (sum_slip_rates_110(i) > sum_slip_rates_110(r(2))) then
                r(i) = r(2)
                r(2) = i
            else
                r(i) = i
            end if
        end do

        !Update dislocation densities
        RHObausch = 0._DP
        !Loop over 2 currently generated walls
        do i = 1, 2
            associate(cur_cbb => state_ptr%cbb(r(i)))
                cur_cbb%RHOwd = kocks_mecking(this%b, cur_cbb%RHOwd, sum_slip_rates_110(r(i))*time, this%Iwd, this%Rwd)
                cur_cbb%RHOwdHOM = cur_cbb%RHOwd

                rho_wp_a = cur_cbb%RHOwp
                wpFLUX = this%effslashb(:,r(i)) .dot. slip_rates
                flux_reversal = wpFLUX*rho_wp_a  <  0._DP

                if (flux_reversal .and. abs(rho_wp_a) > this%RHOwpLOW) then
                    !|RHOwp| gets smaller, following analytic time integration
                    cur_cbb%RHOwp = rho_wp_a*exp(-this%Rrev*abs(wpFLUX) * time)
                    RHObausch = RHObausch+abs(rho_wp_a)
                else
                    !|RHOwp| gets larger, following numeric time integration (4th order Runge-Kutta)
                   cur_cbb%RHOwp = runge_kutta(merge(-rho_wp_a, rho_wp_a, flux_reversal), &
                                               time, &
                                               dwp_dt, & !See definition of dwp_dt for the meaning of the list of variables below
                                               [this%iwp, this%rwp, wpflux, state_ptr%CBB(r(i))%RHOwdHOM])
                end if
            end associate
        end do

        !Loop over 4 non-currently generated walls
        sum_slip_active_cbb = sum(sum_slip_rates_110(r(:2)))*time
        do i = 3, 6
            associate (cur_cbb => state_ptr%cbb(r(i)))
                if (cur_cbb%RHOwdHOM > this%RHOwdMIN) then
                    if (all(state_ptr%activecbb /= r(i))) then  ! if the wall was NOT active in prev. inc.
                        cur_cbb%accGAMMA_new = cur_cbb%accGAMMA_new+sum_slip_active_cbb
                    else
                        cur_cbb%accGAMMA_new = sum_slip_active_cbb
                        cur_cbb%RHOwd_ini = cur_cbb%RHOwdHOM
                    end if

                    cur_cbb%RHOwdHOM = cur_cbb%RHOwdHOM*exp(-this%Rncg*sum_slip_active_cbb/this%b)
                    cur_cbb%RHOwd = cur_cbb%RHOwdHOM-tanh(this%beta1*cur_cbb%accGAMMA_new)*exp(-this%beta1*cur_cbb%accGAMMA_new)*cur_cbb%RHOwd_ini*this%beta2
                    if (cur_cbb%RHOwd < this%RHOwdMIN) &
                        cur_cbb%RHOwd = this%RHOwdMIN
                else
                    cur_cbb%RHOwdHOM = this%RHOwdMIN
                    cur_cbb%RHOwd = this%RHOwdMIN
                end if

                cur_cbb%rhowp = merge(cur_cbb%RHOwp*exp(-this%Rncg*sum_slip_active_cbb/this%b), &
                                      this%RHOwpMIN*merge(1, -1, cur_cbb%rhowp >= 0._DP), &
                                      abs(cur_cbb%RHOwp) > this%RHOwpMIN)
            end associate
        end do

        if(RHObausch > 0._DP) then
            r_effective = this%R+this%R2*RHObausch / (2.D0*this%RHOwpSAT)
            if (this%I*sqrt(state_ptr%rhocb) - r_effective*state_ptr%rhocb > 0._DP) &
                state_ptr%rhocb = kocks_mecking(this%b, state_ptr%rhocb, sum(abs(slip_rates))*time, this%I, r_effective)
        else
            state_ptr%rhocb = kocks_mecking(this%b, state_ptr%rhocb, sum(abs(slip_rates))*time, this%I, this%R)
        end if

        !Update indices of active CBBs
        state_ptr%activecbb = r(:2)

        !Update CRSS of grain based on new dislocation densities
        call this%update_crss(state_ptr)
    contains
        !Unfortunately, this is the only way to formulate 'partial function application' that IFX can handle.
        !Refer to PhD thesis by Bart Peters for the meaning of this function.
        real(DP) function dwp_dt(wp, args) result(res)
            real(DP), intent(in):: wp
            real(DP), dimension(:), intent(in):: args ![iwp, rwp, fl, wd]

            res = (sign(1._DP, args(3)) * args(1)*sqrt(args(4)+abs(wp)) - args(2)*wp) * abs(args(3))
        end function
    end function

    !> Returns RHO_b, the value of RHO at the end of an interval (a, b) for the following differential equation:
    !> d(RHO)/d(g) = 1/b * (II*sqrt(RHO) - RR*RHO)
    real(DP) function kocks_mecking(b, RHO_a, delta_g, II, RR) result(kock)
        real(DP), intent(in):: b
        real(DP), intent(in):: RHO_a
        real(DP), intent(in):: delta_g
        real(DP), intent(in):: II
        real(DP), intent(in):: RR

      kock = exp(-0.5D0*RR*delta_g/b)
      kock = (II/RR * (1.D0-kock) + sqrt(RHO_a) * kock)**2
    end function

    !> Update the CRSS of a given grain.
    subroutine update_crss(this, state)
        class(ConstitutiveModelDSH), intent(in):: this !! Hardening model
        type(DSHState), intent(inout):: state          !! Grain state to be updated.

        integer:: j, s, i
        real(DP):: tau_CB, &
                   CRSS_0_CB, &
                   tau_CBB(2, size(this%taylor_coeffs, 2)), &
                   wpcontr(6), &
                   wdcontr(6)

        !Some systems are not allowed to become active
        state%crss = REAL_DP_MAX_VAL

        !CRSS within cells & CBs
        tau_CB = this%alfa*this%G*this%b*sqrt(state%RHOcb)

        !contributions from tau_0 and CBs to CRSS
        CRSS_0_CB = this%tau0 + (1.D0-this%f) * tau_CB

        !Calc. CRSS for each slip system s, for the sense of slip j
        do j = 1, 2
            do s = 1, size(state%crss, 2)
                !wp-and wd-contributions from all CBBs i
                do i = 1, 6
                    wpcontr(i)= sqrt(abs(state%CBB(i)%RHOwp)) &
                                * (-1)**(j-1) &
                                * this%alfa_G_b_eff(s, i) &
                                * sign(1.D0, state%CBB(i)%RHOwp)
                    if (wpcontr(i) < 0.0_DP) &
                        wpcontr(i) = 0._DP  ! Heaviside bracket
                    wdcontr(i)=sqrt(state%CBB(i)%RHOwd)*abs(this%alfa_G_b_eff(s, i))
                end do
                !CRSS within CBB = wp-and wd-contributions for all 6 walls
                tau_CBB(j, s)=sum(wpcontr)+sum(wdcontr)
                !CRSS for the "two-phase composite"
                state%crss(j, s)= CRSS_0_CB+this%f*tau_CBB(j, s)
            end do
        end do
    end subroutine

    function dsh_serialize(this) result(params)
        class(ConstitutiveModelDSH), target, intent(in):: this
        type(Parameter), dimension(:), allocatable:: params

        integer:: offset
        type(Parameter), dimension(:), allocatable:: base

        allocate(params(this%size()))

        base = this%ConstitutiveModel%serialize()
        offset = size(base)
        params(:offset) = base

        params(offset+1)  = this%b
        params(offset+2)  = this%G
        params(offset+3)  = this%alfa
        params(offset+4)  = this%f
        params(offset+5)  = this%tau0
        params(offset+6)  = this%I
        params(offset+7)  = this%R
        params(offset+8)  = this%Iwd
        params(offset+9)  = this%Rwd
        params(offset+10) = this%Rncg
        params(offset+11) = this%beta1
        params(offset+12) = this%beta2
        params(offset+13) = this%Iwp
        params(offset+14) = this%Rwp
        params(offset+15) = this%Rrev
        params(offset+16) = this%R2
        params(offset+17) = this%RHOwpSAT
        params(offset+18) = this%RHOwdMIN
        params(offset+19) = this%RHOwpMIN
        params(offset+20) = this%RHOwpLOW
        params(offset+21) = this%effslashb
        params(offset+22) = this%alfa_G_b_eff
    end function

    subroutine dsh_deserialize(this, params)
        class(ConstitutiveModelDSH), target, intent(inout):: this
        type(Parameter), dimension(:), intent(in):: params

        integer:: offset

        call this%ConstitutiveModel%deserialize(params)

        offset = this%ConstitutiveModel%size()

        this%b       = params(offset+1)
        this%G       = params(offset+2)
        this%alfa    = params(offset+3)
        this%f       = params(offset+4)
        this%tau0    = params(offset+5)
        this%I       = params(offset+6)
        this%R       = params(offset+7)
        this%Iwd     = params(offset+8)
        this%Rwd     = params(offset+9)
        this%Rncg    = params(offset+10)
        this%beta1   = params(offset+11)
        this%beta2   = params(offset+12)
        this%Iwp     = params(offset+13)
        this%Rwp     = params(offset+14)
        this%Rrev    = params(offset+15)
        this%R2      = params(offset+16)
        this%RHOwpSAT  = params(offset+17)
        this%RHOwdMIN  = params(offset+18)
        this%RHOwpMIN  = params(offset+19)
        this%RHOwpLOW  = params(offset+20)
        this%effslashb     = params(offset+21)
        this%alfa_G_b_eff  = params(offset+22)
    end subroutine

    pure function dsh_state_size(this) result(size)
        class(DSHState), intent(in):: this
        integer:: size

        size = this%HardeningState%size() + 32
    end function

    function dsh_state_serialize(this) result(params)
        class(DSHState), target, intent(in):: this
        type(Parameter), dimension(:), allocatable:: params

        integer:: i, offset
        type(Parameter), dimension(:), allocatable:: base

        allocate(params(this%size()))

        base = this%HardeningState%serialize()
        offset = size(base)
        params(:offset) = base

        params(offset+1) = this%rhocb
        offset = offset + 1
        do i=1,6
            associate (cbb => this%cbb(i))
                params(offset+1) = serialize(cbb%rhowd)
                params(offset+2) = serialize(cbb%rhowp)
                params(offset+3) = serialize(cbb%rhowdhom)
                params(offset+4) = serialize(cbb%accgamma_new)
                params(offset+5) = serialize(cbb%rhowd_ini)
            end associate
            offset = offset + 5
        end do
        params(offset+1) = this%activecbb
    end function

    subroutine dsh_state_deserialize(this, params)
        class(DSHState), target, intent(inout):: this
        type(Parameter), dimension(:), intent(in):: params

        integer:: i, offset

        call this%HardeningState%deserialize(params)

        offset = this%HardeningState%size()

        this%rhocb = params(offset+1)
        offset = offset + 1
        do i=1,6
            associate (cbb => this%cbb(i))
                cbb%rhowd = params(offset+1)
                cbb%rhowp = params(offset+2)
                cbb%rhowdhom = params(offset+3)
                cbb%accgamma_new = params(offset+4)
                cbb%rhowd_ini = params(offset+5)
            end associate
            offset = offset + 5
        end do
        this%activecbb = params(offset+1)
    end subroutine
end module
