module hardening_model_dsh
    use utils
    use hardening_model
    use altayConfig
    use parameters
    use logging
    use slip_systems

    implicit none
    private

    real(DP), parameter ::  MINFRAC = 2.0D-3,   &
                            LOWFRAC = 10.0D-3
    !real(DP), dimension(6,3), parameter, public :: CBBNORMAL = transpose(BCC24(:,1,1:12:2)/SQR2)

  real(DP), dimension(6,3), parameter, public :: CBBNORMAL = real(transpose(reshape([ 0,  1, -1, &
                                                                                       -1,  0,  1, &
                                                                                        1, -1,  0, &
                                                                                        0, -1, -1, &
                                                                                        1,  0,  1, &
                                                                                       -1, -1,  0], [3,6])),DP)/sqrt(2._DP)


    character(*), parameter :: MOD_NAME = 'hardening_model_dsh'

    type :: CBBtype
        real(DP) :: RHOwd = 0._DP, &
                    RHOwp = 0._DP, &
                    RHOwdHOM = 0._DP, &
                    accGAMMA_new = 0._DP, &
                    RHOwd_ini = 0._DP
    end type CBBtype

    !> State variables for single grain
    type :: StatVar
          real(DP)                    :: RHOcb = 0._DP
          type(CBBtype), dimension(6) :: CBB
          integer, dimension(2)       :: ActiveCBB = 0
    end type StatVar

    type, extends(HardeningModel) :: HardeningModelDSH
        type(StatVar), dimension(:), allocatable    ::  state
        real(DP), dimension(:,:,:), allocatable     ::  crss
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
                                                        RHOcbSAT,               &
                                                        RHOwdSAT,               &
                                                        RHOwpSAT,               &
                                                        RHOcbMIN,               &
                                                        RHOwdMIN,               &
                                                        RHOwpMIN,               &
                                                        RHOwpLOW,               &
                                                        alfa_G_b
        real(DP), dimension(24,6)                   ::  eff             = 0._DP, &
                                                        effslashb       = 0._DP, &
                                                        alfa_G_b_eff    = 0._DP, &
                                                        alfa_G_b_ABSeff = 0._DP
    contains
        procedure :: get_parameters => dsh_get_parameters
        procedure :: validate_parameters => dsh_validate_parameters
        procedure :: init           => dsh_init
        procedure :: get_crss       => dsh_get_crss
        procedure :: update_state       => dsh_update_state
        procedure :: finalize       => dsh_finalize
        procedure :: initstate
        procedure :: f_crss
        procedure :: f_kocksmeck
        procedure :: upd_cur_wp
        procedure :: bp_upd_ncg_wd
        procedure :: rungeKutta
        procedure :: dwp_dt
        procedure :: upd_ncg_wp
        procedure :: upd_cb
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
                  parameter_init('G', TYPE_REAL),                   &   ![MPa] [1.E4 ;5.E5] Shear modulus
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
        type(Parameter), allocatable, intent(in) :: params(:)
        character(:), allocatable :: nss

        nss = params .find. 'n_slip_systems'

        if (nss /= 'fcc12' .and. nss /= 'bcc24')  &
            call log_error(MOD_NAME, 'validate_parameters', ERR_VAL, 'DSH only supports FCC12 and BCC24 slip systems.')

        call check_param('b',     0._dp,   1.e-8_dp)   ! [m]
        call check_param('G',     1.e4_dp, 5.e5_dp)    ! [MPa]
        call check_param('alfa',  0._dp,   5._dp)      ! [/]
        call check_param('f',     0._dp,   1.0_dp)     ! [/]
        call check_param('tau0',  0._dp,   1.e4_dp)    ! [MPa]
        call check_param('I',     0._dp,   10._dp)     ! [/]
        call check_param('Iwd',   0._dp,   10._dp)     ! [/]
        call check_param('Iwp',   0._dp,   10._dp)     ! [/]
        call check_param('R',     0._dp,   1.e-6_dp)   ! [m]
        call check_param('Rwd',   0._dp,   1.e-6_dp)   ! [m]
        call check_param('Rncg',  0._dp,   1.e-6_dp)   ! [m]
        call check_param('Rwp',   0._dp,   1.e-6_dp)   ! [m]
        call check_param('Rrev',  0._dp,   1.e-6_dp)   ! [m]
        call check_param('R2',    0._dp,   1.e-6_dp)   ! [m]
        call check_param('beta1', 0._dp,   100._dp)    ! [/]
        call check_param('beta2', 0._dp,   100._dp)    ! [/]
    contains
        subroutine check_param(name, min, max)
            character(*), intent(in)    ::  name
            real(DP), intent(in)        ::  min,    &
                                            max
            character(32)               ::  min_str, &
                                            max_str
            real(DP)                    ::  param_val
            character(*), parameter     ::  PROC_NAME = 'dsh_check_param'

            param_val = params .find. name

            if (param_val < min .or. param_val > max) then
                write (min_str, *) min
                write (max_str, *) max
                call log_error(MOD_NAME, PROC_NAME, ERR_VAL, 'Parameter ' // name // ' must lie between ' // min_str // ' and ' // max_str)
            end if
        end subroutine check_param
    end subroutine dsh_validate_parameters

    !Main initialization function
    subroutine dsh_init(this, params)
        class(HardeningModelDSH), intent(inout)  :: this
        type(Parameter), allocatable, intent(in) :: params(:)
        integer :: n_grains

        call hardening_model_init(this, params)

        this%b     = params .find. 'b'
        this%G     = params .find. 'G'
        this%alfa  = params .find. 'alfa'
        this%f     = params .find. 'f'
        this%tau0  = params .find. 'tau0'
        this%I     = params .find. 'I'
        this%R     = params .find. 'R'
        this%Iwd   = params .find. 'Iwd'
        this%Rwd   = params .find. 'Rwd'
        this%Rncg  = params .find. 'Rncg'
        this%beta1 = params .find. 'beta1'
        this%beta2 = params .find. 'beta2'
        this%Iwp   = params .find. 'Iwp'
        this%Rwp   = params .find. 'Rwp'
        this%Rrev  = params .find. 'Rrev'
        this%R2    = params .find. 'R2'

        !change of units if different in params from this (units of this are: MPa; micrometer)
        this%b    = this%b    * 1.e6_dp ![m] -> [um]
        this%R    = this%R    * 1.e6_dp ![m] -> [um]
        this%Rwd  = this%Rwd  * 1.e6_dp ![m] -> [um]
        this%Rncg = this%Rncg * 1.e6_dp ![m] -> [um]
        this%Rwp  = this%Rwp  * 1.e6_dp ![m] -> [um]
        this%Rrev = this%Rrev * 1.e6_dp ![m] -> [um]
        this%R2   = this%R2   * 1.e6_dp ![m] -> [um]

        !Calculate dependent hardening parameters
        this%RHOcbSAT = (this%I)**2 / (this%R)**2
        this%RHOwdSAT = (this%Iwd)**2 / (this%Rwd)**2
        this%RHOwpSAT = (sqrt((this%Iwp / this%Rwp)**4 + 4._dp * (this%Iwp * this%Iwd / (this%Rwp * this%Rwd))**2) + (this%Iwp / this%Rwp)**2) / 2._dp
        this%RHOcbMIN = MINFRAC * this%RHOcbSAT
        this%RHOwpLOW = LOWFRAC * this%RHOwpSAT
        this%RHOwdMIN = MINFRAC * this%RHOwdSAT
        this%RHOwpMIN = MINFRAC * this%RHOwpSAT
        this%RHOwpLOW = LOWFRAC * this%RHOwpSAT

        n_grains = params .find. 'n_grains'
        allocate(this%state(n_grains))
        allocate(this%crss(n_grains, 2, this%nss))
        this%state(1)%RHOcb               = this%RHOcbMIN
        this%state(1)%CBB%RHOwd        = this%RHOwdMIN
        this%state(1)%CBB%RHOwp        = 0._DP
        this%state(1)%CBB%RHOwdHOM     = this%RHOwdMIN
        this%state(1)%CBB%accGAMMA_new = 0._DP
        this%state(1)%CBB%RHOwd_ini    = this%RHOwdMIN
        this%state(1)%ActiveCBB        = 0
        this%state = this%state(1)
    end subroutine dsh_init

    !> Initialize state separately after model-specific initialization.
    subroutine initState(this)
        class(HardeningModelDSH) :: this
        integer :: i

        this%effslashb       = this%eff / this%b
        this%alfa_G_b        = this%alfa * this%G * this%b
        this%alfa_G_b_eff    = this%alfa_G_b * this%eff
        this%alfa_G_b_ABSeff = abs(this%alfa_G_b_eff)

        do i=1, size(this%state)
            call F_CRSS(this, i)
        end do
    end subroutine

    subroutine dsh_update_state(this, grain, time, slip_rates)
        class(HardeningModelDSH), intent(inout)     ::  this
        integer, intent(in)                         ::  grain
        real(DP), intent(in)                        ::  time
        real(DP), dimension(this%nss), intent(in)   ::  slip_rates
        type(StatVar)                               ::  SVa,                    &
                                                        SVb
        real(DP)                                    ::  SUMabsGamDot    = 0._DP, &
                                                        GAMMAdot_new    = 0._DP, &
                                                        RHObausch       = 0._DP, &
                                                        SUMabsGam       = 0._DP, &
                                                        GAMMA_new       = 0._DP
        real(DP), dimension(6)                      ::  GAMMAdot        = 0._DP, &
                                                        GAMMA           = 0._DP
        integer, dimension(6)                       ::  r
        integer                                     ::  i, &
                                                        j

        SVa = this%state(grain)
        !>Calculate quantities of slip rates and slips
        !>Identify currently generated and non-currently generated walls
        SUMabsGamDot = sum(abs(slip_rates))
        SUMabsGam = SUMabsGamDot * time

        if (SUMabsGam < epsilon(0._DP)) return


        GAMMAdot(1) = abs(slip_rates(1)) + abs(slip_rates(7))   !(01-1)-plane
        GAMMAdot(2) = abs(slip_rates(2)) + abs(slip_rates(11))  !(-101)-plane
        GAMMAdot(3) = abs(slip_rates(3)) + abs(slip_rates(6))   !(1-10)-plane
        GAMMAdot(4) = abs(slip_rates(4)) + abs(slip_rates(10))  !(0-1-1)-plane
        GAMMAdot(5) = abs(slip_rates(5)) + abs(slip_rates(8))   !(101)-plane
        GAMMAdot(6) = abs(slip_rates(9)) + abs(slip_rates(12))  !(-1-10)-plane


        !forall (i=1:6) GAMMAdot(i) = sum(abs(slip_rates(2*i-1:2*i)))   
        gamma = GAMMAdot * time

        !r(1) = plane with largest slip
        !r(2) = plane with 2nd largest slip
        !r(3:6) = remaining planes (unordered)
        r(1:2) = merge([1,2],[2,1], gammadot(1) >= gammadot(2))

        do i=3,6
            if (gammadot(i) > gammadot(r(1))) then
                r(i) = r(2)
                r(2) = r(1)
                r(1) = i
            else if (gammadot(i) > gammadot(r(2))) then
                r(i) = r(2)
                r(2) = i
            else
                r(i) = i
            end if
        end do

        SVb%ActiveCBB(1) = r(1)
        SVb%ActiveCBB(2) = r(2)

        GAMMAdot_new = GAMMAdot(r(1)) + GAMMAdot(r(2))
        GAMMA_new = GAMMAdot_new * time

       !Update dislocation densities
        RHObausch = 0._DP
        do j=1,2 !Loop over 2 currently generated walls
            SVb%CBB(r(j))%RHOwd = this%F_KocksMeck(SVa%CBB(r(j))%RHOwd, gamma(r(j)), this%Iwd, this%Rwd)
            SVb%CBB(r(j))%RHOwdHOM = SVb%CBB(r(j))%RHOwd
            call this%upd_cur_wp(r(j), SVa%CBB(r(j))%RHOwp, SVb%CBB(r(j))%RHOwp, RHObausch, slip_rates, svb, time)
        end do

        do j=3,6 !Loop over 4 non-currently generated walls
            call bp_UPD_ncg_wd(this, r(j), sva, svb, gamma_new)
            call this%UPD_ncg_wp(SVa%CBB(r(j))%RHOwp, SVb%CBB(r(j))%RHOwp, gamma_new)
        end do

        call this%upd_cb(RHObausch, SUMabsGam, SVa%RHOcb, SVb%RHOcb)

        !Calculate Critical Resolved Shear Stresses
        this%state(grain) = SVb
        call this%F_CRSS(grain)
    end subroutine

    !>Returns RHO_b, the value of RHO at the end of an interval (a,b) for the following differential equation:
    !>d(RHO)/d(g) = 1/this%b * ( II*sqrt(RHO) - RR*RHO )
    real(DP) function F_KocksMeck(this, RHO_a, delta_g, II, RR) result(kock)
        class(HardeningModelDSH), intent(in)    ::  this
        real(DP), intent(in)                    ::  RHO_a,      &
                                                    delta_g,    &
                                                    II,         &
                                                    RR

      kock = exp(-0.5D0 * RR * delta_g / this%b)
      kock = (II / RR * (1.D0 - kock) + sqrt(RHO_a) * kock)**2
    end function

    subroutine upd_cur_wp(this, rdr, RHOwp_a, RHOwp_b, RHObausch, slip_rates, svb, delta_t)
        class(HardeningModelDSH), intent(in)        ::  this
        integer, intent(in)                         ::  rdr
        real(DP), intent(in)                        ::  RHOwp_a
        real(DP), intent(out)                       ::  RHOwp_b
        real(DP), intent(inout)                     ::  RHObausch
        real(DP), dimension(this%nss), intent(in)   ::  slip_rates
        real(DP), intent(in)                        ::  delta_t
        type(StatVar), intent(inout)                ::  svb
        real(DP)                                    ::  wpFLUX,         &
                                                        fl,             &
                                                        wd
        logical                                     ::  FLUXreversal,   &
                                                        wpLOW

        wpFLUX = dot_product(this%effslashb(:,rdr), slip_rates)

        FLUXreversal = wpFLUX * RHOwp_a  <  0._DP
        wpLOW = abs(RHOwp_a) <= this%RHOwpLOW

        if (FLUXreversal .and. .not. (wpLOW)) then
            !|RHOwp| gets smaller, following analytic time integration
            RHOwp_b = RHOwp_a * exp(-this%Rrev * abs(wpFLUX) * delta_t)
            RHObausch = RHObausch + abs(RHOwp_a)
        else
            !|RHOwp| gets larger, following numeric time integration (4th order Runge-Kutta)
            fl = wpFLUX
            wd = SVb%CBB(rdr)%RHOwdHOM
            if (FLUXreversal) then
                !AFTER change of its sign, RHOwp will build up again.
                RHOwp_b = this%rungeKutta(-RHOwp_a, delta_t, fl, wd)
            else
                RHOwp_b = this%rungeKutta(RHOwp_a, delta_t, fl, wd)
            end if
        end if
    end subroutine

    !>4th order Runge-Kutta approximation of the differential equation given by d(wp)/dt = F(wp)
    real(DP) function rungeKutta(this, wpini, deltaT, fl, wd) result(rk)
        class(HardeningModelDSH), intent(in)    ::  this
        real(DP), intent(in)                    ::  wpini,  &
                                                    deltaT, &
                                                    fl,     &
                                                    wd
        real(DP), dimension(4)                  ::  K

        K(1) = deltaT * this%dwp_dt(wpini, fl, wd)
        K(2) = deltaT * this%dwp_dt(wpini + K(1) / 2.D0, fl, wd)
        K(3) = deltaT * this%dwp_dt(wpini + K(2) / 2.D0, fl, wd)
        K(4) = deltaT * this%dwp_dt(wpini + K(3), fl, wd)

        rk = wpini + (K(1) + 2.D0 * K(2) + 2.D0 * K(3) + K(4)) / 6.D0
    end function rungeKutta

    real(DP) function dwp_dt(this, wp, fl, wd) result(res)
        class(HardeningModelDSH), intent(in)    ::  this
        real(DP), intent(in)                    ::  wp,     &
                                                    fl,     &
                                                    wd

        res = (sign(1.D0, fl) * this%Iwp * sqrt(wd + abs(wp)) - this%Rwp * wp) * abs(fl)
    end function dwp_dt

    subroutine bp_UPD_ncg_wd(this, rdr,SV_a,SV_b, gamma_new)
        class(HardeningModelDSH), intent(in)    ::  this
        integer, intent(in)                     ::  rdr
        type(StatVar), intent(in)               ::  SV_a
        type(StatVar), intent(inout)            ::  SV_b
        real(DP), intent(in)                    ::  gamma_new

        real(DP)                                ::  RHOwdLOC,       &
                                                    RHOwdHOM,       &
                                                    accGAMMA_new,   &
                                                    RHOwd_ini,      &
                                                    RHOwd

        RHOwdHOM     = SV_a%CBB(rdr)%RHOwdHOM
        accGAMMA_new = SV_a%CBB(rdr)%accGAMMA_new
        RHOwd_ini    = SV_a%CBB(rdr)%RHOwd_ini

        if (RHOwdHOM > this%RHOwdMIN) then
            if (rdr  /=  SV_a%ActiveCBB(1) .and. rdr  /=  SV_a%ActiveCBB(2)) then !if the wall was NOT active in prev. inc.
                accGAMMA_new = accGAMMA_new + GAMMA_new
            else
                accGAMMA_new = GAMMA_new
                RHOwd_ini = RHOwdHOM
            end if

            RHOwdLOC = -tanh(this%beta1 * accGAMMA_new) * exp(-this%beta1 * accGAMMA_new) * RHOwd_ini * this%beta2
            RHOwdHOM = RHOwdHOM * exp(-this%Rncg * GAMMA_new / this%b)
            RHOwd = RHOwdHOM + RHOwdLOC
            if (RHOwd  <  this%RHOwdMIN) RHOwd = this%RHOwdMIN
        else
            RHOwdHOM = this%RHOwdMIN
            RHOwd = this%RHOwdMIN
        end if

        SV_b%CBB(rdr)%RHOwd         = RHOwd
        SV_b%CBB(rdr)%RHOwdHOM      = RHOwdHOM
        SV_b%CBB(rdr)%accGAMMA_new  = accGAMMA_new
        SV_b%CBB(rdr)%RHOwd_ini     = RHOwd_ini
    end subroutine

    subroutine upd_ncg_wp(this, RHOwp_a, RHOwp_b, gamma_new)
        class(HardeningModelDSH), intent(in)    ::  this
        real(DP), intent(in)                    ::  RHOwp_a,    &
                                                    gamma_new
        real(DP), intent(out)                   ::  RHOwp_b

        if (abs(RHOwp_a)  >  this%RHOwpMIN) then
            RHOwp_b = RHOwp_a * exp(-this%Rncg * GAMMA_new / this%b)
        else
            RHOwp_b =  merge(this%RHOwpMIN,-this%RHOwpMIN,RHOwp_a >= 0._DP)
        end if
    end subroutine upd_ncg_wp

    subroutine upd_cb(this, RHObausch,SUMabsGam,RHO_a,RHO_b)
        class(HardeningModelDSH), intent(in)    ::  this
        real(DP), intent(in)                    ::  RHObausch,   &
                                                    SUMabsGam
        real(DP), intent(in)                    ::  RHO_a
        real(DP), intent(out)                   ::  RHO_b
        real(DP)                                ::  Reffective

        if(RHObausch > 0._DP) then
            Reffective = this%R + this%R2 * RHObausch / (2.D0 * this%RHOwpSAT)
            RHO_b = merge(RHO_a, &
                          this%F_KocksMeck(RHO_a, SUMabsGam, this%I, Reffective), &
                          this%I * sqrt(RHO_a) - Reffective * RHO_a <= 0._DP)
        else
            RHO_b = this%F_KocksMeck(RHO_a, SUMabsGam, this%I, this%R)
        end if
    end subroutine upd_cb

    subroutine F_CRSS(this, grain)
        class(HardeningModelDSH), intent(inout)    :: this
        type(StatVar) :: SV
        integer, intent(in) :: grain
        real(DP) :: tau_CB,CRSS_0_CB
        real(DP),dimension(2,this%nss)::tau_CBB
        integer :: j,s,i
        real(DP) :: signfac
        real(DP),dimension(6)::wpcontr,wdcontr

        SV = this%state(grain)
        !Slip systems not allowed to become active retain initialization value of -1.0
        this%crss(grain,:,:) = -1.D0

        !CRSS within cells & CBs
        tau_CB = this%alfa_G_b * sqrt(SV%RHOcb)

        !contributions from tau_0 and CBs to CRSS
        CRSS_0_CB=this%tau0 + (1.D0 - this%f) * tau_CB

        !Calc. CRSS for each slip system s, for the sense of slip j
        do j=1,2
            signfac=3.D0-2.D0 * dble(j) ! 1 for j=1 ; -1 for j=2
            do s=1,this%nss
                !wp- and wd-contributions from all CBBs i
                do i=1,6
                    wpcontr(i)=sqrt(abs(SV%CBB(i)%RHOwp)) * signfac * this%alfa_G_b_eff(s,i) * sign(1.D0, SV%CBB(i)%RHOwp)
                    if (wpcontr(i) < 0.0_DP) wpcontr(i) = 0._DP ! Heaviside bracket
                    wdcontr(i)=sqrt(SV%CBB(i)%RHOwd)*this%alfa_G_b_ABSeff(s,i)
                end do
                !CRSS within CBB = wp- and wd-contributions for all 6 walls
                tau_CBB(j,s)=sum(wpcontr)+sum(wdcontr)
                !C.R.S.S. for the "two-phase composite"
                this%crss(grain,j,s)= CRSS_0_CB + this%f*tau_CBB(j,s)
            end do
        end do

    end subroutine

    function dsh_get_crss(this, grain, strain) result(crss)
        class(HardeningModelDSH), intent(in)    :: this
        integer, intent(in)                     :: grain
        real(DP), intent(in)                    :: strain
        real(DP), dimension(2,this%nss)         :: crss

        crss = this%crss(grain,:,:)
    end function

    subroutine dsh_finalize(this)
        class(HardeningModelDSH), intent(inout)    :: this

        deallocate(this%crss)
        deallocate(this%state)
    end subroutine
end module
