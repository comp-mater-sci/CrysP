module dsh
    use utils
    use hardening_model
    use altayConfig
    use parameters
    use logging
    use slip_systems
    use omp_lib

    implicit none
    private

    real(DP), parameter ::  MINFRAC = 2.0D-3,   &
                            LOWFRAC = 10.0D-3
    real(DP), dimension(6, 3), parameter, public:: CBBNORMAL = transpose(real(SLIP_SYSTEMS_BCC_110(:,1, 1:12:2), DP)/SQR2)

    character(*), parameter:: MOD_NAME = 'hardening_model_dsh'

    type:: CBBtype
        real(DP):: RHOwd = 0._DP, &
                    RHOwp = 0._DP, &
                    RHOwdHOM = 0._DP, &
                    accGAMMA_new = 0._DP, &
                    RHOwd_ini = 0._DP
    end type CBBtype

    !> State variables for single grain
    type:: StatVar
          real(DP)                    :: RHOcb = 0._DP
          type(CBBtype), dimension(6):: CBB
          integer, dimension(2)       :: ActiveCBB = 0
    end type StatVar

    !Family of physics-based hardening models mapping the movement of (clusters of) dislocations.
    !First formulated and documented in Bart Peeters's PhD thesis ('Multiscale modelling of the induced plastic anisotropy in IF
    !steel during sheet forming'). Several variants of this model have been formulated, each of which considers different types of
    !dislocations.
    !Note that only the BCC24 slip system set is supported.
    type, extends(HardeningModel), abstract:: HardeningModelDSH
        type(StatVar), dimension(:), allocatable    ::  state
        real(DP)                                    ::  b,                      &
                                                        G,                      &
                                                        alfa,                   &
                                                        f,                      &
                                                        tau0,                   &
                                                        I,                      &
                                                        R,                      &
                                                        Iwd,                    &
                                                        Rwd,                    &
                                                        Rncg,                   &
                                                        beta1,                  &
                                                        beta2,                  &
                                                        Iwp,                    &
                                                        Rwp,                    &
                                                        Rrev,                   &
                                                        R2,                     &
                                                        RHOwpSAT,               &
                                                        RHOwdMIN,               &
                                                        RHOwpMIN,               &
                                                        RHOwpLOW
        real(DP), dimension(24, 6)                   :: effslashb       = 0._DP, &
                                                        alfa_G_b_eff    = 0._DP
    contains
        procedure:: get_parameters => dsh_get_parameters
        procedure:: validate_parameters => dsh_validate_parameters
        procedure:: get_crss       => dsh_get_crss
        procedure:: update_state       => dsh_update_state
        procedure:: finalize       => dsh_finalize
    end type

    public  ::  HardeningModelDSH, &
                dsh_init

contains

    function dsh_get_parameters(this) result(params)
        class(HardeningModelDSH), intent(in)    :: this
        type(Parameter), allocatable    :: params(:)

        params = [hardening_model_get_parameters(this), &
                 [parameter_init('n_grains', TYPE_INTEGER),         &
                  parameter_init('b', TYPE_REAL),                   &   ![m]   ]0.;1.E-8]   Magnitude of burgers vector
                  parameter_init('G', TYPE_REAL),                   &   ![MPa] [1.E4; 5.E5] Shear modulus
                  parameter_init('alfa', TYPE_REAL),                &   ![/]   ]0.;5.]      Dislocation interaction parameter
                  parameter_init('f', TYPE_REAL),                   &   ![/]   [0.;1.]      Volume fraction of CBBs (Cell Block Boundaries)
                  parameter_init('tau0', TYPE_REAL),                &   ![MPa] [0.;1.E4 ]   Initial critical resolved shear stress on all slip systems
                  parameter_init('I', TYPE_REAL),                   &   ![/]   [0.;1.E1 ]   Immobilization coefficient of CBs (Cell Boundaries)
                  parameter_init('R', TYPE_REAL),                   &   ![m]   ]0.;1.E-6]   Recovery coefficient of CBs
                  parameter_init('Iwd', TYPE_REAL),                 &   ![/]   [0.;1.E1 ]   Immobilization coefficient of CBBs
                  parameter_init('Rwd', TYPE_REAL),                 &   ![m]   ]0.;1.E-6]   Recovery coefficient of CBBs
                  parameter_init('Rncg', TYPE_REAL),                &   ![m]   ]0.;1.E-6]   Recovery coefficient of old CBBs and polarity of old CBBs
                  parameter_init('beta1', TYPE_REAL),               &   ![/]   [0.;1.E2 ]   1st coeff. micro shear band cut-through of old CBBs
                  parameter_init('beta2', TYPE_REAL),               &   ![/]   [0.;1.E2 ]   2nd coeff. micro shear band cut-through of old CBBs
                  parameter_init('Iwp', TYPE_REAL),                 &   ![/]   [0.;1.E1 ]   Immobilization coefficient of polarity of CBBs
                  parameter_init('Rwp', TYPE_REAL),                 &   ![m]   ]0.;1.E-6]   Recovery coefficient of polarity of CBBs
                  parameter_init('Rrev', TYPE_REAL),                &   ![m]   ]0.;1.E-6]   Recovery coefficient of polarity CBBs during bauschinger
                  parameter_init('R2', TYPE_REAL)]]                      ![m]   ]0.;1.E-6]   Recovery coefficient of CBs due to reversal polarity flux
    end function dsh_get_parameters

    subroutine dsh_validate_parameters(this, params)
        class(HardeningModelDSH), intent(in)     :: this
        type(Parameter), dimension(:), target, intent(in):: params

        if ((params .find. 'n_slip_systems') /= 24)  &
            call log_error(MOD_NAME, 'validate_parameters', ERR_VAL, 'DSH only supports BCC24 slip systems.')

        call parameter_check_bounds(params .find. 'b',     0._dp,   1.e-8_dp)   ! [m]
        call parameter_check_bounds(params .find. 'G',     1.e4_dp, 5.e5_dp)    ! [MPa]
        call parameter_check_bounds(params .find. 'alfa',  0._dp,   5._dp)      ! [/]
        call parameter_check_bounds(params .find. 'f',     0._dp,   1.0_dp)     ! [/]
        call parameter_check_bounds(params .find. 'tau0',  0._dp,   1.e4_dp)    ! [MPa]
        call parameter_check_bounds(params .find. 'I',     0._dp,   10._dp)     ! [/]
        call parameter_check_bounds(params .find. 'Iwd',   0._dp,   10._dp)     ! [/]
        call parameter_check_bounds(params .find. 'Iwp',   0._dp,   10._dp)     ! [/]
        call parameter_check_bounds(params .find. 'R',     0._dp,   1.e-6_dp)   ! [m]
        call parameter_check_bounds(params .find. 'Rwd',   0._dp,   1.e-6_dp)   ! [m]
        call parameter_check_bounds(params .find. 'Rncg',  0._dp,   1.e-6_dp)   ! [m]
        call parameter_check_bounds(params .find. 'Rwp',   0._dp,   1.e-6_dp)   ! [m]
        call parameter_check_bounds(params .find. 'Rrev',  0._dp,   1.e-6_dp)   ! [m]
        call parameter_check_bounds(params .find. 'R2',    0._dp,   1.e-6_dp)   ! [m]
        call parameter_check_bounds(params .find. 'beta1', 0._dp,   100._dp)    ! [/]
        call parameter_check_bounds(params .find. 'beta2', 0._dp,   100._dp)    ! [/]
    end subroutine

    !Main initialization function
    subroutine dsh_init(this, params, eff)
        class(HardeningModelDSH), intent(inout)  :: this
        type(Parameter), allocatable, target, intent(in):: params(:)
        real(DP), dimension(24, 6), intent(in):: eff          !> Interaction coefficients between dislocation directions and cell
                                                              !> block boundary normals.

        integer:: i, &
                  n_grains

        call hardening_model_init(this, params)

        this%b     = (params .find. 'b')    * 1.e6_DP ![m] -> [um]
        this%G     =  params .find. 'G'
        this%alfa  =  params .find. 'alfa'
        this%f     =  params .find. 'f'
        this%tau0  =  params .find. 'tau0'
        this%I     =  params .find. 'I'
        this%R     = (params .find. 'R')    * 1.e6_dp ![m] -> [um]
        this%Iwd   =  params .find. 'Iwd'
        this%Rwd   = (params .find. 'Rwd')  * 1.e6_dp ![m] -> [um]
        this%Rncg  = (params .find. 'Rncg') * 1.e6_dp ![m] -> [um]
        this%beta1 =  params .find. 'beta1'
        this%beta2 =  params .find. 'beta2'
        this%Iwp   =  params .find. 'Iwp'
        this%Rwp   = (params .find. 'Rwp')  * 1.e6_dp ![m] -> [um]
        this%Rrev  = (params .find. 'Rrev') * 1.e6_dp ![m] -> [um]
        this%R2    = (params .find. 'R2')   * 1.e6_dp ![m] -> [um]

        !Calculate dependent hardening parameters
        this%RHOwdMIN = MINFRAC* (this%Iwd)**2 / (this%Rwd)**2  ! Minfrac*rho_wd_sat
        this%RHOwpSAT = (sqrt((this%Iwp/this%Rwp)**4+4._dp * (this%Iwp*this%Iwd / (this%Rwp*this%Rwd))**2) + (this%Iwp/this%Rwp)**2) / 2._dp
        this%RHOwpLOW = LOWFRAC*this%RHOwpSAT
        this%RHOwpMIN = MINFRAC*this%RHOwpSAT

        n_grains = params .find. 'n_grains'
        allocate(this%state(n_grains))
        this%state(1)%RHOcb            = MINFRAC * (this%I)**2 / (this%R)**2  ! Minfrac*rho_cb_sat
        this%state(1)%CBB%RHOwd        = this%RHOwdMIN
        this%state(1)%CBB%RHOwp        = 0._DP
        this%state(1)%CBB%RHOwdHOM     = this%RHOwdMIN
        this%state(1)%CBB%accGAMMA_new = 0._DP
        this%state(1)%CBB%RHOwd_ini    = this%RHOwdMIN
        this%state(1)%ActiveCBB        = 0
        this%state = this%state(1)

        this%effslashb       = eff/this%b
        this%alfa_G_b_eff    = this%alfa*this%G*this%b*eff
    end subroutine dsh_init

    subroutine dsh_update_state(this, grain, time, slip_rates)
        class(HardeningModelDSH), intent(inout)     ::  this
        integer, intent(in)                         ::  grain
        real(DP), intent(in)                        ::  time
        real(DP), dimension(this%nss), intent(in)   ::  slip_rates
        real(DP)                                    ::  RHObausch   , &
                                                        rho_wp_a, &
                                                        wpflux, &
                                                        r_effective, &
                                                        sum_slip_active_cbb
        real(DP), dimension(6)                      ::  sum_slip_rates_110
        integer, dimension(6)                       ::  r
        integer                                     ::  i
        logical:: flux_reversal

        !>Calculate quantities of slip rates and slips
        !>Identify currently generated and non-currently generated walls
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
        associate (sv => this%state(grain))
            RHObausch = 0._DP
            !Loop over 2 currently generated walls
            do i = 1, 2
                associate(cur_cbb => sv%cbb(r(i)))
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
                                                   [this%iwp, this%rwp, wpflux, this%state(grain)%CBB(r(i))%RHOwdHOM])
                    end if
                end associate
            end do

            !Loop over 4 non-currently generated walls
            sum_slip_active_cbb = sum(sum_slip_rates_110(r(:2)))*time
            do i = 3, 6
                associate (cur_cbb => sv%cbb(r(i)))
                    if (cur_cbb%RHOwdHOM > this%RHOwdMIN) then
                        if (all(sv%activecbb /= r(i))) then  ! if the wall was NOT active in prev. inc.
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
                if (this%I*sqrt(sv%rhocb) - r_effective*sv%rhocb > 0._DP) &
                    sv%rhocb = kocks_mecking(this%b, sv%rhocb, sum(abs(slip_rates))*time, this%I, r_effective)
            else
                sv%rhocb = kocks_mecking(this%b, sv%rhocb, sum(abs(slip_rates))*time, this%I, this%R)
            end if
        end associate

        !Update indices of active CBBs
        this%state(grain)%activecbb = r(:2)
    contains
        !Unfortunately, this is the only way to formulate 'partial function application' that IFX can handle.
        !Refer to PhD thesis by Bart Peters for the meaning of this function.
        real(DP) function dwp_dt(wp, args) result(res)
            real(DP), intent(in):: wp
            real(DP), dimension(:), intent(in):: args ![iwp, rwp, fl, wd]

            res = (sign(1._DP, args(3)) * args(1)*sqrt(args(4)+abs(wp)) - args(2)*wp) * abs(args(3))
        end function
    end subroutine

    !>Returns RHO_b, the value of RHO at the end of an interval (a, b) for the following differential equation:
    !>d(RHO)/d(g) = 1/b * (II*sqrt(RHO) - RR*RHO)
    real(DP) function kocks_mecking(b, RHO_a, delta_g, II, RR) result(kock)
        real(DP), intent(in):: b
        real(DP), intent(in):: RHO_a
        real(DP), intent(in):: delta_g
        real(DP), intent(in):: II
        real(DP), intent(in):: RR

      kock = exp(-0.5D0*RR*delta_g/b)
      kock = (II/RR * (1.D0-kock) + sqrt(RHO_a) * kock)**2
    end function

    function dsh_get_crss(this, grain, sum_slip) result(crss)
        class(HardeningModelDSH), intent(in)    :: this
        integer, intent(in)                     :: grain
        real(DP), intent(in)                    :: sum_slip
        real(DP), dimension(2, this%nss)         :: crss

        integer:: j, s, i
        real(DP):: tau_CB, &
                   CRSS_0_CB, &
                   tau_CBB(2, this%nss), &
                   wpcontr(6), &
                   wdcontr(6)

        !Some systems are not allowed to become active
        crss = REAL_DP_MAX_VAL

        associate (sv => this%state(grain))
            !CRSS within cells & CBs
            tau_CB = this%alfa*this%G*this%b*sqrt(SV%RHOcb)

            !contributions from tau_0 and CBs to CRSS
            CRSS_0_CB = this%tau0 + (1.D0-this%f) * tau_CB

            !Calc. CRSS for each slip system s, for the sense of slip j
            do j = 1, 2
                do s = 1, this%nss
                    !wp-and wd-contributions from all CBBs i
                    do i = 1, 6
                        wpcontr(i)= sqrt(abs(SV%CBB(i)%RHOwp)) &
                                    * (-1)**(j-1) &
                                    * this%alfa_G_b_eff(s, i) &
                                    * sign(1.D0, SV%CBB(i)%RHOwp)
                        if (wpcontr(i) < 0.0_DP) &
                            wpcontr(i) = 0._DP  ! Heaviside bracket
                        wdcontr(i)=sqrt(SV%CBB(i)%RHOwd)*abs(this%alfa_G_b_eff(s, i))
                    end do
                    !CRSS within CBB = wp-and wd-contributions for all 6 walls
                    tau_CBB(j, s)=sum(wpcontr)+sum(wdcontr)
                    !C.R.S.S. for the "two-phase composite"
                    crss(j, s)= CRSS_0_CB+this%f*tau_CBB(j, s)
                end do
            end do
        end associate
    end function

    subroutine dsh_finalize(this)
        class(HardeningModelDSH), intent(inout)    :: this

        deallocate(this%state)
    end subroutine
end module
