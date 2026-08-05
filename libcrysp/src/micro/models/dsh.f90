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
    use parameters
    use serialization
    use logging
    use slip_systems

    implicit none

    private
    public:: CBBNORMAL, &
             ConstitutiveModelDSH

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
    end type

   type, extends(ConstitutiveModel), abstract:: ConstitutiveModelDSH !! Model state common to all DSH models.
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
        procedure, nopass:: get_parameters => dsh_get_parameters  !! Inherited from ConstitutiveModel

        procedure, nopass:: validate_parameters => dsh_validate_parameters !! Inherited from ConstitutiveModel
                procedure:: deform                      => dsh_deform              !! Inherited from ConstitutiveModel

        procedure:: init_common, &
                    update_crss
    end type

contains

    !> Convert a generic HardeningState to a pointer to a DSHState object
    !>
    !> Closest Fortran comes to type casting
    !> If the provided state is not of type dsh_state, the program crashes.
    function to_dsh_state(state) result(dsh_state_ptr)
        class(HardeningState), target, intent(in):: state   !! HardeningState to be converted. Must have dynamic type DSHState.
        type(DSHState), pointer:: dsh_state_ptr             !! Pointer of type DSHState to the HardeningState

        select type (state)
            type is (DSHState)
                dsh_state_ptr => state
            class default
                call log_error(ERR_TYPE)
        end select
    end function

!    !> See [[ConstitutiveModel:get_parameters]]
!    function dsh_get_parameters() result(params)
!        type(Parameter), allocatable    :: params(:) !! - **b**:     Magnitude of burgers vector [m]
!                                                     !! - **G**:     Shear modulus [MPa]
!                                                     !! - **alfa**:  Dislocation interaction parameter
!                                                     !! - **f**:     Volume fraction of Cell Block Boundaries
!                                                     !! - **tau0**:  Initial critical resolved shear stress on all slip systems [MPa]
!                                                     !! - **I**:     Immobilization coefficient of Cell Boundaries
!                                                     !! - **R**:     Recovery coefficient of cell boundaries [m]
!                                                     !! - **Iwd**:   Immobilization coefficient of CBBs
!                                                     !! - **Rwd**:   Recovery coefficient of CBBs [m]
!                                                     !! - **Rncg**:  Recovery coefficient of old CBBs and polarity of old CBBs [m]
!                                                     !! - **beta1**: 1st coeff. micro shear band cut-through of old CBBs
!                                                     !! - **beta2**: 2nd coeff. micro shear band cut-through of old CBBs
!                                                     !! - **Iwp**:   Immobilization coefficient of polarity of CBBs
!                                                     !! - **Rwp**:   Recovery coefficient of polarity of CBBs [m]
!                                                     !! - **Rrev**:  Recovery coefficient of polarity CBBs during bauschinger [m]
!                                                     !! - **R2**:    Recovery coefficient of CBs due to reversal polarity flux [m]
!
!        params = [parameter_init('b',     TYPE_REAL), &
!                  parameter_init('G',     TYPE_REAL), &
!                  parameter_init('alfa',  TYPE_REAL), &
!                  parameter_init('f',     TYPE_REAL), &
!                  parameter_init('tau0',  TYPE_REAL), &
!                  parameter_init('I',     TYPE_REAL), &
!                  parameter_init('R',     TYPE_REAL), &
!                  parameter_init('Iwd',   TYPE_REAL), &
!                  parameter_init('Rwd',   TYPE_REAL), &
!                  parameter_init('Rncg',  TYPE_REAL), &
!                  parameter_init('beta1', TYPE_REAL), &
!                  parameter_init('beta2', TYPE_REAL), &
!                  parameter_init('Iwp',   TYPE_REAL), &
!                  parameter_init('Rwp',   TYPE_REAL), &
!                  parameter_init('Rrev',  TYPE_REAL), &
!                  parameter_init('R2',    TYPE_REAL)]
!    end function dsh_get_parameters
!
!    !> See cm_validate_parameters
!    subroutine dsh_validate_parameters(params)
!        type(Parameter), dimension(:), target, intent(in):: params !! - **b**:     ]0.;1.E-8]
!                                                                   !! - **G**:     [1.E4; 5.E5]
!                                                                   !! - **alfa**:  ]0.;5.]
!                                                                   !! - **f**:     ]0.;1.]
!                                                                   !! - **tau0**:  [0.;1.E4]
!                                                                   !! - **I**:     [0.;1.E1]
!                                                                   !! - **R**:     ]0.;1.E-6]
!                                                                   !! - **Iwd**:   [0.;1.E1]
!                                                                   !! - **Rwd**:   ]0.;1.E-6]
!                                                                   !! - **Rncg**:  ]0.;1.E-6]
!                                                                   !! - **beta1**: [0.;1.E2]
!                                                                   !! - **beta2**: [0.;1.E2]
!                                                                   !! - **Iwp**:   [0.;1.E1]
!                                                                   !! - **Rwp**:   ]0.;1.E-6]
!                                                                   !! - **Rrev**:  ]0.;1.E-6]
!                                                                   !! - **R2**:    ]0.;1.E-6]
!
!        if ((params .find. 'n_slip_systems') /= 24)  &
!            call log_error('DSH', 'validate_parameters', ERR_VAL, 'DSH only supports BCC24 slip systems.')
!
!        call parameter_check_bounds(params .find. 'b',     0._dp,   1.e-8_dp)
!        call parameter_check_bounds(params .find. 'G',     1.e4_dp, 5.e5_dp)
!        call parameter_check_bounds(params .find. 'alfa',  0._dp,   5._dp)
!        call parameter_check_bounds(params .find. 'f',     0._dp,   1.0_dp)
!        call parameter_check_bounds(params .find. 'tau0',  0._dp,   1.e4_dp)
!        call parameter_check_bounds(params .find. 'I',     0._dp,   10._dp)
!        call parameter_check_bounds(params .find. 'Iwd',   0._dp,   10._dp)
!        call parameter_check_bounds(params .find. 'Iwp',   0._dp,   10._dp)
!        call parameter_check_bounds(params .find. 'R',     0._dp,   1.e-6_dp)
!        call parameter_check_bounds(params .find. 'Rwd',   0._dp,   1.e-6_dp)
!        call parameter_check_bounds(params .find. 'Rncg',  0._dp,   1.e-6_dp)
!        call parameter_check_bounds(params .find. 'Rwp',   0._dp,   1.e-6_dp)
!        call parameter_check_bounds(params .find. 'Rrev',  0._dp,   1.e-6_dp)
!        call parameter_check_bounds(params .find. 'R2',    0._dp,   1.e-6_dp)
!        call parameter_check_bounds(params .find. 'beta1', 0._dp,   100._dp)
!        call parameter_check_bounds(params .find. 'beta2', 0._dp,   100._dp)
!    end subroutine

    !> Main model initialization procedure common to all variants of the DSH model family.
    !>
    !> The only difference between the variants of DSH is the interaction coefficients between dislocations and cell block
    !> boundaries. Thus, each model defines its own coefficients and calls this common initialization procedure with them.
    function init_common(this, miller_indices, params, eff) result(initial_state)
        class(ConstitutiveModelDSH), intent(inout):: this          !! DSH model variant to be initialized.
        integer, dimension(:,:,:), intent(in):: miller_indices     !! Miller indices of the deformation mechanism to be used.
        character(*), target, intent(in):: params !! Model parameters. Assumed to pass dsh_validate_parameters(params)
        real(DP), dimension(24, 6), intent(in):: eff               !! 'Wall-effectivity' matrix == cosines of the angle between
                                                                   !! dislocation movement vectors and the cell block boundary normals.
        class(HardeningState), allocatable:: initial_state         !! Initial state of each grain using a DSH model.

        type(JSONParser):: parser

        allocate(DSHState:: initial_state)
        call this%base_init(miller_indices, initial_state)

        parser = params
        this%b     =  parser
        this%b     = this%b * 1.e6_DP ![m] -> [um]
        this%G     =  parser
        this%alfa  =  parser
        this%f     =  parser
        this%tau0  =  parser
        this%I     =  parser
        this%R     = parser
        this%R     = this%R * 1.e6_dp ![m] -> [um]
        this%Iwd   = parser
        this%Rwd   = parser
        this%Rwd   = this%Rwd * 1.e6_dp ![m] -> [um]
        this%Rncg  = parser
        this%Rcng  = this%rcng * 1.e6_dp ![m] -> [um]
        this%beta1 = parser
        this%beta2 = parser
        this%Iwp   = parser
        this%Rwp   = parser
        this%rwp   = this%rwp * 1.e6_dp ![m] -> [um]
        this%Rrev  = parser
        this%rrev  = this%rrev * 1.e6_dp ![m] -> [um]
        this%R2    = parser
        this%r2    = this%r2 * 1.e6_dp ![m] -> [um]

        !Calculate dependent hardening parameters
        this%RHOwdMIN = MINFRAC* (this%Iwd)**2 / (this%Rwd)**2  ! Minfrac*rho_wd_sat
        this%RHOwpSAT = (sqrt((this%Iwp/this%Rwp)**4+4._dp * (this%Iwp*this%Iwd / (this%Rwp*this%Rwd))**2) + (this%Iwp/this%Rwp)**2) / 2._dp
        this%RHOwpLOW = LOWFRAC*this%RHOwpSAT
        this%RHOwpMIN = MINFRAC*this%RHOwpSAT

        !Quantities derived from input parameters. Kept as fields to avoid having to recompute them all the time.
        this%effslashb       = eff/this%b
        this%alfa_G_b_eff    = this%alfa*this%G*this%b*eff

        !Select type is required here due to Fortran semantics even though the type is obvious
        select type (initial_state)
            type is (DSHState)
                initial_state%RHOcb            = MINFRAC * (this%I)**2 / (this%R)**2  ! Minfrac*rho_cb_sat
                initial_state%CBB%RHOwd        = this%RHOwdMIN
                initial_state%CBB%RHOwp        = 0._DP
                initial_state%CBB%RHOwdHOM     = this%RHOwdMIN
                initial_state%CBB%accGAMMA_new = 0._DP
                initial_state%CBB%RHOwd_ini    = this%RHOwdMIN
                initial_state%ActiveCBB        = 0

                call this%update_crss(initial_state)
        end select
    end function

    !> See cm_deform
    subroutine dsh_deform(this, state, time, slip_rates)
        class(ConstitutiveModelDSH), intent(inout)                  :: this
        class(HardeningState), target, intent(inout)                :: state
        real(DP), intent(in)                                        :: time
        real(DP), dimension(size(this%taylor_coeffs, 2)), intent(in):: slip_rates

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

        state_ptr => to_dsh_state(state)

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
    end subroutine

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
end module
