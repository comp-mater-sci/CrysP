*----------------------------------------------------------------------*
*                             BBC_2008_16                              *
*                                                                      *
*                                                                      *
* This program implements the identification procedure of the BBC 2008 *
* yield criterion (16 parameter version).                              *
*                                                                      *
* Note:                                                                *
*   The program calls routines from the double-precision version of    *
*   the MINPACK-1 library.                                             *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
      program BBC_2008_16
      use extendedYld
      implicit none
*----------------------------------------------------------------------*
* Symbolic constants                                                   *
*----------------------------------------------------------------------*
      integer
     &  OUT_UNIT,
     &  FL_NAM_SZ, TITL_SZ,
     &  BCC_EXP, FCC_EXP,
     &  S
      double precision
     &  S_INV,
     &  MAX_Y_INC, MAX_R_INC,
     &  ANG_STEP, SIG12_FRAC_STEP,
     &  DEG_TO_RAD
      character * (*)
     &  MECH_PARM_MSG, INI_GUESS_MSG,
     &  ERR_MSG01, ERR_MSG02, ERR_MSG03, ERR_MSG04,
     &  FMT01, FMT02, FMT03, FMT04, FMT05, FMT06, FMT07, FMT08, FMT09,
     &  FMT10, FMT11, FMT12, FMT13, FMT14, FMT15, FMT16, FMT17, FMT18,
     &  FMT19, FMT20, FMT21, FMT22, SUCCESS_MSG, FAILURE_MSG
      parameter (
     &  OUT_UNIT = 1,
     &  FL_NAM_SZ = 255, TITL_SZ = 80,
     &  BCC_EXP = 3, FCC_EXP = 4,
     &  S = 2, S_INV = 1.0d0 / S,
     &  MAX_Y_INC = 1.0d-1, MAX_R_INC = 1.0d-1,
     &  ANG_STEP = 1.25d0, SIG12_FRAC_STEP = 0.2d0,
     &  DEG_TO_RAD = 0.1745329251994d-1
     &          )
      parameter (
     &  MECH_PARM_MSG = "y_u_00 (> 0):" //
     &                  "y_u_15 (> 0):" //
     &                  "y_u_30 (> 0):" //
     &                  "y_u_45 (> 0):" //
     &                  "y_u_60 (> 0):" //
     &                  "y_u_75 (> 0):" //
     &                  "y_u_90 (> 0):" //
     &                  "y_b    (> 0):" //
     &                  "r_u_00 (> 0):" //
     &                  "r_u_15 (> 0):" //
     &                  "r_u_30 (> 0):" //
     &                  "r_u_45 (> 0):" //
     &                  "r_u_60 (> 0):" //
     &                  "r_u_75 (> 0):" //
     &                  "r_u_90 (> 0):" //
     &                  "r_b    (> 0):",
     &  INI_GUESS_MSG = "l_1_1 :" //
     &                  "l_1_2 :" //
     &                  "m_1_1 :" //
     &                  "m_1_2 :" //
     &                  "m_1_3 :" //
     &                  "n_1_1 :" //
     &                  "n_1_2 :" //
     &                  "n_1_3 :" //
     &                  "l_2_1 :" //
     &                  "l_2_2 :" //
     &                  "m_2_1 :" //
     &                  "m_2_2 :" //
     &                  "m_2_3 :" //
     &                  "n_2_1 :" //
     &                  "n_2_2 :" //
     &                  "n_2_3 :"
     &          )
      parameter (
     &  ERR_MSG01 = "** Error - Cannot open the output file",
     &  ERR_MSG02 = "** Error - Wrong input data",
     &  ERR_MSG03 = "** Error - Divergence of the identification " //
     &    "procedure",
     &  ERR_MSG04 = "** Error - Cannot generate the test data"
     &          )
      parameter (
     &  FMT01 = "(/1X, 'Type in the name of the output file ',
     &    '(up to ', I3, ' characters):')",
     &  FMT02 = "(A)",
     &  FMT03 = "(/1X, A/)",
     &  FMT04 = "(/1X, 'Type in the title of the identification ',
     &    '(up to ', I3, ' characters):')",
     &  FMT05 = "(/1X, 'Type in the material data.')",
     &  FMT06 = "(5X, A, ' ', $)",
     &  FMT07 = "(/1X, 'Specify the crystallographic structure:'//
     &    5X, 'B --> BCC'//5X, 'F --> FCC'//5X, 'U --> Undefined'
     &    //1X, 'Your selection (B/F/U)? ', $)",
     &  FMT08 = "(/1X, 'Type in the exponent k (strictly positive ',
     &    'integer number): ', $)",
     &  FMT09 = "(1X, A//1X, 'MATERIAL DATA')",
     &  FMT10 = "(/1X, 'Crystallographic structure: BCC')",
     &  FMT11 = "(/1X, 'Crystallographic structure: FCC')",
     &  FMT12 = "(/1X, 'Crystallographic structure: undefined'/5X,
     &    'Exponent k selected by the user: ', I2)",
     &  FMT13 = "(/1X, 'Type in the initial guess.')",
     &  FMT14 = "(//1X, 'INITIAL GUESS')",
     &  FMT15 = "(//1X, 'PARAMETERS OF THE BBC 2008 YIELD CRITERION'/
     &    5X, 'k    : ', I4/5X, 's    : ', I4/5X, 'w    : ',
     &    F15.10/(5X, 'l_', I1, '_1: ', F15.10/5X, 'l_', I1, '_2: ',
     &    F15.10/5X, 'm_', I1, '_1: ', F15.10/5X, 'm_', I1, '_2: ',
     &    F15.10/5X, 'm_', I1, '_3: ', F15.10/5X, 'n_', I1, '_1: ',
     &    F15.10/5X, 'n_', I1, '_2: ', F15.10/5X, 'n_', I1, '_3: ',
     &    F15.10))",
     &  FMT16 = "(//1X, 'TEST DATA'//1X, 'Normalized yield stress ',
     &    'and coefficient of plastic anisotropy associated to'/1X,
     &    'the uniaxial tension along a direction defined by the ',
     &    'angle theta'/1X, 'measured with respect to RD'//
     &    2X, 'theta', 6X, 'y_u_theta', 5X, 'r_u_theta'//2X, '[deg]'/)",
     &  FMT17 = "(2X, F5.2, 7X, F7.4, 7X, F7.4)",
     &  FMT18 = "(//1X, 'Normalized yield stress and coefficient of ',
     &    'plastic anisotropy associated to'/1X, 'the biaxial tension ',
     &    'along RD and TD'//16X, 'y_b', 11X, 'r_b'//14X, F7.4, 7X,
     &    F7.4///1X, 'Sections through the normalized yield surface')",
     &  FMT19 = "(//4X, 'sig11 / Y', 9X, 'sig22 / Y', 9X,
     &    'sig12 / Y'/)",
     &  FMT20 = "(5X, F7.4, 11X, F7.4, 11X, F7.4)",
     &  FMT21 = "(/1X,'Type the number of yield section points: ')",
     &  FMT22 = "(/1X,'point ',I0,1X,'(theta[deg], S/Y, beta[deg]): ')"
     &          )
      parameter (
     &  SUCCESS_MSG = "End of execution",
     &  FAILURE_MSG = "Execution interrupted due to an error"
     &          )
      ! JG, June 11 2013 -->>
      integer,parameter :: OUT_UNIT_PAR = 200, OUT_UNIT_RS = 201, 
     &  OUT_UNIT_YLD = 202
      ! <<--
      ! JG, July 13 2013 -->>
      integer :: step
      logical :: read_trial_sol
      ! <<--
#ifdef NEW_IDENT_SCHEME
      !
      logical :: read_weights
      integer :: nref_yld, n, m, d_work_dyn_len
      double precision :: sol_full_resnorm, sol_resnorm
      double precision,dimension(:),allocatable :: d_work_dyn, rsd_dyn
      integer,dimension(:),allocatable :: i_work_dyn
      double precision,dimension(16) :: initial_guess, sol_full
      type(yldDesc),dimension(:),allocatable :: sol_yld
#endif
      
*----------------------------------------------------------------------*
* Variables                                                            *
*----------------------------------------------------------------------*
      character
     &  out_fl_nam * (FL_NAM_SZ),
     &  titl * (TITL_SZ),
     &  struc_typ
      integer
     &  k, dbl_k, dbl_k_mns_one,
     &  info,
     &  i, j, jj,
     &  num_yld_srf_inc,
     &  i_wrk_arr(16)
      double precision
     &  w, inv_w, w_mns_one, w_mns_one_tms_w_pow_s_mns_one, inv_dbl_k,
     &  y(8), r(8), crt_y(8), crt_r(8), y_inc(8), r_inc(8), y_term(8),
     &  r_term(8), u_ang(7),
     &  bbc_parm(16),
     &  sol(16), rsd(16), d_wrk_arr(352), conv_tol,
     &  ang, max_ang, sig12_frac, max_sig12_frac,
     &  aux1, aux2, aux3
*----------------------------------------------------------------------*
* Functions called from the MINPACK-1 library                          *
*----------------------------------------------------------------------*
      double precision
     &  DPMPAR
*----------------------------------------------------------------------*
* Procedures passed as arguments                                       *
*----------------------------------------------------------------------*
      external
     &  GET_IDENT_RSD
*----------------------------------------------------------------------*
* Common blocks                                                        *
*----------------------------------------------------------------------*
      common
     &  /MAT_DATA/ y_term, r_term, u_ang
     &  /BBC_DATA/ bbc_parm, w, inv_w, w_mns_one,
     &    w_mns_one_tms_w_pow_s_mns_one, inv_dbl_k, dbl_k, dbl_k_mns_one
#ifdef DIAGNOSTIC_OUTPUT_FILE
      integer :: yld_unit, errcode
      character(len=512) :: yld_fname
#endif
*----------------------------------------------------------------------*
* Read the name of the output file from the keyboard.                  *
* Open this file.                                                      *
*----------------------------------------------------------------------*
      write (*, FMT01) FL_NAM_SZ
      read (*, FMT02) out_fl_nam
      open (OUT_UNIT, file = out_fl_nam, status = 'UNKNOWN',
     &  iostat = info)
      if (info /= 0) then
        write (*, FMT03) ERR_MSG01
        write (*, FMT03) FAILURE_MSG
        !stop
        call exit(1)
      end if
*----------------------------------------------------------------------*
* Read the material data from the keyboard.                            *
* Store this data in the output file.                                  *
*----------------------------------------------------------------------*
      write (*, FMT04) TITL_SZ
      read (*, FMT02) titl
      write (*, FMT05)
      j = 1
      do i = 1, 8
        jj = INDEX (MECH_PARM_MSG(j : ), ':') + j - 1
        do
          info = 0
          write (*, FMT06) MECH_PARM_MSG(j : jj)
          read (*, *) y(i)
          if (.not. (y(i) > 0.0d0)) then
            write (*, FMT03) ERR_MSG02
            info = 1
          end if
          if (info == 0) exit
        end do
        j = jj + 1
      end do
      do i = 1, 8
        jj = INDEX (MECH_PARM_MSG(j : ), ':') + j - 1
        do
          info = 0
          write (*, FMT06) MECH_PARM_MSG(j : jj)
          read (*, *) r(i)
          if (.not. (r(i) > 0.0d0)) then
            write (*, FMT03) ERR_MSG02
            info = 1
          end if
          if (info == 0) exit
        end do
        j = jj + 1
      end do
      ! JG, July 2014 -->>
#ifdef NEW_IDENT_SCHEME
      ! Read triplets: theta, S(theta)/Y, beta(theta)
      write (*, FMT21)
      read(*,*) nref_yld
      allocate(ref_yld(nref_yld))
      do i = 1, nref_yld
            write(*, FMT22) i
            read(*,*) ref_yld(i)%theta, ref_yld(i)%S, ref_yld(i)%beta
            ! Convert input to radians
            ref_yld(i)%theta = ref_yld(i)%theta * pi_deg
            ref_yld(i)%beta  = ref_yld(i)%beta * pi_deg
      enddo
      ! <<--
#endif      
      do
        info = 0
        write (*, FMT07)
        read (*, *) struc_typ
        if ((struc_typ /= 'B') .and. (struc_typ /= 'b') .and.
     &      (struc_typ /= 'F') .and. (struc_typ /= 'f') .and.
     &      (struc_typ /= 'U') .and. (struc_typ /= 'u')) then
          write (*, FMT03) ERR_MSG02
          info = 1
        end if
        if (info == 0) exit
      end do
      if ((struc_typ == 'B') .or. (struc_typ == 'b')) then
        k = BCC_EXP
      else if ((struc_typ == 'F') .or. (struc_typ == 'f')) then
        k = FCC_EXP
      else
        do
          info = 0
          write (*, FMT08)
          read (*, *) k
          if (k < 1) then
            write (*, FMT03) ERR_MSG02
            info = 1
          end if
          if (info == 0) exit
        end do
      end if
      write (OUT_UNIT, FMT09) titl
      j = 1
      do i = 1, 8
        jj = INDEX (MECH_PARM_MSG(j : ), ':') + j - 1
        write (OUT_UNIT, FMT06) MECH_PARM_MSG(j : jj)
        write (OUT_UNIT, *) y(i)
        j = jj + 1
      end do
      do i = 1, 8
        jj = INDEX (MECH_PARM_MSG(j : ), ':') + j - 1
        write (OUT_UNIT, FMT06) MECH_PARM_MSG(j : jj)
        write (OUT_UNIT, *) r(i)
        j = jj + 1
      end do
      if ((struc_typ == 'B') .or. (struc_typ == 'b')) then
        write (OUT_UNIT, FMT10)
      else if ((struc_typ == 'F') .or. (struc_typ == 'f')) then
        write (OUT_UNIT, FMT11)
      else
        write (OUT_UNIT, FMT12) k
      end if
      aux1 = 15.0d0 * DEG_TO_RAD
      do i = 1, 7
        u_ang(i) = (i - 1) * aux1
      end do
      w = 1.5d0 ** S_INV
      inv_w = 1.0d0 / w
      w_mns_one = w - 1.0d0
      w_mns_one_tms_w_pow_s_mns_one = 1.5d0 * inv_w * w_mns_one
      dbl_k = k + k
      dbl_k_mns_one = dbl_k - 1
      inv_dbl_k = 1.0d0 / dbl_k

      ! CERTETA, June 14 2014 -->>
*----------------------------------------------------------------------*
* Read the initial guess from the keyboard.                            *
* Store this data in the output file.                                  *
* Note: The initial guess is a set of coefficients l_1_1, l_1_2,       *
*       m_1_1, m_1_2, m_1_3, n_1_1, n_1_2, n_1_3, l_2_1, l_2_2, m_2_1, *
*       m_2_2, m_2_3, n_2_1, n_2_2, and n_2_3 used to start the        *
*       identification. Their values should be 0.5 for the first       *
*       identification. In the case of the subsequent identifications, *
*       the solution of the last successful identification should be   *
*       passed as input.                                               *
*----------------------------------------------------------------------*
      write (*, FMT13)
      j = 1
      do i = 1, 16
        jj = INDEX (INI_GUESS_MSG(j : ), ':') + j - 1
        write (*, FMT06) INI_GUESS_MSG(j : jj)
        read (*, *) bbc_parm(i)
        j = jj + 1
      end do
      write (OUT_UNIT, FMT14)
      j = 1
      do i = 1, 16
        jj = INDEX (INI_GUESS_MSG(j : ), ':') + j - 1
        write (OUT_UNIT, FMT06) INI_GUESS_MSG(j : jj)
        write (OUT_UNIT, *) bbc_parm(i)
        j = jj + 1
      end do
      call validateBBCInputParams()
      ! <<--
      ! JG, July 2014 -->>
#ifdef DIAGNOSTIC_OUTPUT_FILE
      call writeYld_fname(trim(out_fl_nam)//'_yld_init.yld', info)
      ! Write out a few control points
      call outputYldDescription(36, 0.D0, 10.D0*pi_deg, 
     &                          trim(out_fl_nam)//'_yld_init.byld', 
     &                          info)
#endif
#ifdef NEW_IDENT_SCHEME
      ! Use this identification path only if extra points are provided
      if (nref_yld > 0) then
        read_weights = .false.
        write(*,'(A)') 'Would you like to provide weights [T/F]? :'
        read(*,*) read_weights
        if (read_weights) then
              write(*,*) 'weight: uniaxial r-value: '
              read(*,*) config%wt_unirvalue
              write(*,*) 'weight: uniaxial yield stress: '
              read(*,*) config%wt_unistress
              write(*,*) 'weight: equibiaxial r-value: '
              read(*,*) config%wt_bxrvalue
              write(*,*) 'weight: equibiaxial yield stress: '
              read(*,*) config%wt_bxstress
              write(*,*) 'weight: biaxial yield stress: '
              read(*,*) config%wt_stress
              write(*,*) 'weight: biaxial beta angle: '
              read(*,*) config%wt_beta
        endif
        initial_guess = bbc_parm
        sol_full = initial_guess
        !
        conv_tol = SQRT (DPMPAR (1))
        y_term = y
        r_term = r
        ! Set scratch space for LMDIF1
        n = 16
        m = 16 + 2 * size(ref_yld) + 1
        ! Calculate the size of the scratch array needed by LMDIF1
        d_work_dyn_len = m*n+5*n+m
        allocate(d_work_dyn(d_work_dyn_len), i_work_dyn(n),rsd_dyn(m))
        !
        sol_full = initial_guess
        write(*,'(/,A,/)') 'Starting full identification algorithm'
        call LMDIF1 (GET_IDENT_RSD_EXT,m,n,sol_full,rsd_dyn, conv_tol,
     &             info,i_work_dyn, d_work_dyn, d_work_dyn_len)
        sol_full_resnorm =  norm2(rsd_dyn)
        write(*,'(A,1X,E15.6,//)') '||Residual||:', sol_full_resnorm
#ifdef DIAGNOSTIC_OUTPUT_FILE
        bbc_parm = sol_full
        call writeYld_fname(trim(out_fl_nam)//'_full.yld', info)
        call writeRS_fname(trim(out_fl_nam)//'_full.rs', info)
        call outputYldDescription(24, 0.D0, 15.D0*pi_deg, 
     &                          trim(out_fl_nam)//'_full.byld',info)
        allocate(sol_yld(size(ref_yld)))
        sol_yld%theta = ref_yld%theta
        call getYldBBCDescription(sol_yld, info)
        call outputYldDescription(sol_yld, 
     &                          trim(out_fl_nam)//'_full_sel.byld',info)
#endif
        ! Restore the initial guess for the gradual distortion algorithm
        bbc_parm = initial_guess
      endif
#endif
      ! <<--
*----------------------------------------------------------------------*
* Perform the identification of the BBC 2008 yield criterion.          *
*----------------------------------------------------------------------*
* Stage 1:  Define the quantities used to control the gradual distorsion
*           of the reference yield locus.
*
      ! JG, July 13 2013 -->>
      do step = 1,2
      ! <<--
      
        aux1 = 0.0d0
        aux2 = 0.0d0
        do i = 1, 8
          aux3 = y(i) - crt_y(i)
          y_inc(i) = aux3
          aux3 = ABS (aux3)
          if (aux1 < aux3) aux1 = aux3
          aux3 = r(i) - crt_r(i)
          r_inc(i) = aux3
          aux3 = ABS (aux3)
          if (aux2 < aux3) aux2 = aux3
        end do
        aux1 = aux1 / MAX_Y_INC
        aux2 = aux2 / MAX_R_INC
        if (aux1 > aux2) then
          num_yld_srf_inc = INT (aux1) + 1
        else
          num_yld_srf_inc = INT (aux2) + 1
        end if
        aux1 = 1.0d0 / num_yld_srf_inc
        do i = 1, 8
          y_inc(i) = y_inc(i) * aux1
          r_inc(i) = r_inc(i) * aux1
        end do
*
* Stage 2:  Set the initial guess used to start the identification.
*           The initial guess corresponds to an isotropic reference
*           yield locus in the case of the first identification, or to
*           the yield locus found by the last successful identification.
*
        if (step == 1) then

          ! CERTETA, June 14 2014 -->>
          do i = 1, 16
            sol(i) = bbc_parm(i)
          end do
          ! <<--

        else
        ! JG, July 13 2013 -->
            ! If it is the first failure, try to get 
            ! another starting point: read it from the terminal
          read_trial_sol = .false.
          info = 0
          read(*,'(L1)',iostat=info) read_trial_sol
          if (read_trial_sol .and. (info==0) ) then
            write(*,'(/,A,/)') 'SECOND ATTEMPT'


            ! CERTETA, June 14 2014 -->>
            do i=1,16
              read(*,*) bbc_parm(i)
              sol(i) = bbc_parm(i)
            enddo
            call validateBBCInputParams()
            ! <<--
          endif
          ! Otherwise, we start identification from the previously 
          ! found sol.
        endif
        ! <<--
*
* Stage 3:  Perform a gradual distorsion of the reference yield locus
*           until reaching the configuration defined by the input data.
*           Each distorsional step is followed by an identification.
*           The solution provided by this identification becomes an
*           initial guess for the next distorsional step.
*
        conv_tol = SQRT (DPMPAR (1))
        do jj = 1, num_yld_srf_inc
          do i = 1, 8
            aux1 = crt_y(i) + y_inc(i)
            crt_y(i) = aux1
            y_term(i) = - aux1
            aux2 = crt_r(i) + r_inc(i)
            crt_r(i) = aux2
            r_term(i) = - (aux2 + 1.0d0)
          end do
          call LMDIF1 (GET_IDENT_RSD, 16, 16, sol, rsd, conv_tol, info,
     &      i_wrk_arr, d_wrk_arr, 352)
      ! JG, July 2014 -->>
#ifdef EXTENDED_DIAGNOSTIC_OUTPUT_FILE
  700     format('yld_diag_step_',I0,'.yld')
          write(yld_fname,fmt=700) jj
          call writeYld_fname(yld_fname, errcode)
#endif
      ! <<--
          write(*,*) '||Residual||:', norm2(rsd)
          if (info <= 0) exit
        end do
        if ((info < 1) .or. (info > 4)) then
          ! JG, July 13 2013 -->
          if (step == 1) cycle
          ! <<-
          write (*, FMT03) ERR_MSG03
          write (OUT_UNIT, FMT03) ERR_MSG03
          write (*, FMT03) FAILURE_MSG
          write (OUT_UNIT, FMT03) FAILURE_MSG
          close (OUT_UNIT)
          ! stop
          call exit(1)
        else
          exit            
        end if

        ! JG, July 13 2013 -->
      enddo
      ! <<--
      ! JG, July 2014 -->>
      call setBBC2008Params(sol)
      
#ifdef DIAGNOSTIC_OUTPUT_FILE
        ! Report the result of the gradual distortion algorithm.
        call writeYld_fname(trim(out_fl_nam)//'_gda.yld', info)
        call writeRS_fname(trim(out_fl_nam)//'_gda.rs', info)
        call outputYldDescription(24, 0.D0, 15.D0*pi_deg, 
     &                          trim(out_fl_nam)//'_gda.byld',info)
#endif
      !
#ifdef NEW_IDENT_SCHEME
      ! If the YLD points are provided, check how the gradual distorsion algorithm
      ! compares to the new one in terms of the residual from the new 
      ! objective function. Pick the solution that offers better square norm.
      if (nref_yld > 0) then
        y_term = y
        r_term = r
        rsd_dyn = 0.D0
        call GET_IDENT_RSD_EXT(m, n, sol, rsd_dyn, info)
        if (info >= 0) then
          sol_resnorm = norm2(rsd_dyn)
          write(*,'(A,E15.6)') 
     &          'Norm of residual from gradual distortion:', sol_resnorm
          write(*,'(A,E15.6)') 
     &          'Norm of residual from new algorithm:', sol_full_resnorm
          if (sol_resnorm > sol_full_resnorm) then
            write(*,*) 'Picking the solution from the new algorithm.'
            sol = sol_full
          endif
        endif
      endif
#endif
      ! <<--
*
* Stage 4:  The identification has ended. The solution can be used to
*           generate a set of BBC 2008 parameters. Store these
*           parameters in the output file.
*
      write (OUT_UNIT, FMT15) k, S, w, ((i - 1) / 8 + 1, bbc_parm(i),
     &  i = 1, 16)
      ! JG, June 11 2013 -->>
      ! Output to bbc2008 file
      open (OUT_UNIT_PAR, file = trim(out_fl_nam)//'.bbc2008', 
     &  status = 'replace', iostat = info)
      write (OUT_UNIT_PAR, FMT15) k, S, w, ((i - 1) / 8 + 1,bbc_parm(i),
     &  i = 1, 16)
      close(OUT_UNIT_PAR)
      ! <<--
#ifdef DIAGNOSTIC_OUTPUT_FILE
      call outputYldDescription(36, 0.D0, 10.*pi_deg,
     &                          trim(out_fl_nam)//'_beta.yld', info)
#endif
*----------------------------------------------------------------------*
* Generate the test data.                                              *
* Store this data in the output file.                                  *
*----------------------------------------------------------------------*
      write (OUT_UNIT, FMT16)
      ! JG, June 11 2013 -->>
      call writeRS_unit(OUT_UNIT,info)
      call writeRS_fname(trim(out_fl_nam)//'.rs',info)
      ! <<-
      if (info /= 0) then
        write (*, FMT03) ERR_MSG04
        write (OUT_UNIT, FMT03) ERR_MSG04
        write (*, FMT03) FAILURE_MSG
        write (OUT_UNIT, FMT03) FAILURE_MSG
        close (OUT_UNIT)
        ! stop
        call exit(1)
      end if
      call GET_FG_B (aux1, aux2, info)
      if (info /= 0) then
        write (*, FMT03) ERR_MSG04
        write (OUT_UNIT, FMT03) ERR_MSG04
        write (*, FMT03) FAILURE_MSG
        write (OUT_UNIT, FMT03) FAILURE_MSG
        close (OUT_UNIT)
        ! stop
        call exit(1)
      end if
      write (OUT_UNIT, FMT18) 1.0d0 / aux1, aux1 / aux2 - 1.0d0
      ! JG, June 11 2013 -->>
      call writeYld_unit(OUT_UNIT,info)
      call writeYld_fname(trim(out_fl_nam)//'.yld',info)
      ! <<-- 
      if (info /= 0) then
        write (*, FMT03) ERR_MSG04
        write (OUT_UNIT, FMT03) ERR_MSG04
        write (*, FMT03) FAILURE_MSG
        write (OUT_UNIT, FMT03) FAILURE_MSG
      else
        write (*, FMT03) SUCCESS_MSG
        write (OUT_UNIT, FMT03) SUCCESS_MSG
      end if
      close (OUT_UNIT)
      stop
      
      contains 
      
      subroutine writeRS_fname(fpath, info)
      implicit none
      character(len=*),intent(in) :: fpath
      integer,intent(out)     :: info
      !
      integer :: out_unit
      !
      open(newunit=out_unit, file=fpath, status='replace',iostat=info)
      if (info /= 0) return
      call writeRS_unit(out_unit, info)
      close(out_unit)
      !
      end subroutine

      subroutine writeRS_unit(out_unit, info)
      implicit none
      integer,intent(in)      :: out_unit
      integer,intent(out)     :: info
      !
      double precision :: ang, max_ang, aux1, aux2
      !
      ang = 0.0d0
      max_ang = 90.0d0 + 0.5d0 * ANG_STEP
      do
        if (ang > max_ang) exit
        call GET_FG_U (ang * DEG_TO_RAD, aux1, aux2, info)
        if (info /= 0) exit
        write (out_unit, FMT17) ang, 1.0d0 / aux1, aux1 / aux2 - 1.0d0
        ang = ang + ANG_STEP
      end do
      !
      end subroutine

      subroutine writeYld_fname(fpath,info)
      implicit none
      character(len=*),intent(in) :: fpath
      integer,intent(out)     :: info
      !
      integer :: out_unit
      !
      open(newunit=out_unit, file=fpath, status='replace',iostat=info)
      if (info /= 0) return
      call writeYld_unit(out_unit,info)
      close(out_unit)
      end subroutine
      
      subroutine writeYld_unit(out_unit,info)
      implicit none
      integer,intent(in)      :: out_unit
      integer,intent(out)     :: info
      !
      double precision :: ang, max_ang, aux1, aux2, aux3
      double precision :: max_sig12_frac, sig12_frac
      !
      max_ang = 360.0d0 + 0.5d0 * ANG_STEP
      sig12_frac = 0.0d0
      max_sig12_frac = 1.0d0 - 0.5d0 * SIG12_FRAC_STEP
      do
        if (sig12_frac > max_sig12_frac) exit
        write (OUT_UNIT, FMT19)
        ang = 0.0d0
        do
          if (ang > max_ang) exit
          call GET_S11_S22_S12 (ang * DEG_TO_RAD, sig12_frac, aux1,
     &      aux2, aux3, info)
          if (info /= 0) exit
          write (out_unit, FMT20) aux1, aux2, aux3
          ang = ang + ANG_STEP
        end do
        if (info /= 0) exit
        sig12_frac = sig12_frac + SIG12_FRAC_STEP
      end do
      end subroutine
      
      
      ! Check if the BBC parameters in /BBC_DATA/ allow evaluating 
      ! uniaxial and biaxial r- values and yield stresses.
      ! Set crt_y and crt_y arrays.
      subroutine validateBBCInputParams()
      implicit none
      double precision :: aux1, aux2
      integer :: info
            do i = 1, 7
              call GET_FG_U (u_ang(i), aux1, aux2, info)
              if (info /= 0) exit
              crt_y(i) = 1.0d0 / aux1
              crt_r(i) = aux1 / aux2 - 1.0d0
            end do
            if (info /= 0) then
              write (*, FMT03) ERR_MSG02
              write (OUT_UNIT, FMT03) ERR_MSG02
              write (*, FMT03) FAILURE_MSG
              write (OUT_UNIT, FMT03) FAILURE_MSG
              close (OUT_UNIT)
              ! stop
              call exit(1)
            end if
            call GET_FG_B (aux1, aux2, info)
            if (info /= 0) then
              write (*, FMT03) ERR_MSG02
              write (OUT_UNIT, FMT03) ERR_MSG02
              write (*, FMT03) FAILURE_MSG
              write (OUT_UNIT, FMT03) FAILURE_MSG
              close (OUT_UNIT)
              ! stop
              call exit(1)
            end if
            crt_y(8) = 1.0d0 / aux1
            crt_r(8) = aux1 / aux2 - 1.0d0
      end subroutine
      
      
      end program BBC_2008_16
      
      
      subroutine setBBC2008Params(sol)
      implicit none
      double precision,dimension(16),intent(in) :: sol
      !
      integer
     &  dbl_k, dbl_k_mns_one,
     &  i
      double precision
     &  bbc_parm(16), w, inv_w, w_mns_one,
     &  w_mns_one_tms_w_pow_s_mns_one, inv_dbl_k,
     &  l_1, l_2, m_1, m_2, n_1, n_2,
     &  l, m, n,
     &  l_pls_m, l_mns_m, m_pls_n, m_mns_n,
     &  f_sum, g_sum, w_fact_1, w_fact_2
*----------------------------------------------------------------------*
* Common blocks                                                        *
*----------------------------------------------------------------------*
      common
     &  /BBC_DATA/ bbc_parm, w, inv_w, w_mns_one,
     &    w_mns_one_tms_w_pow_s_mns_one, inv_dbl_k, dbl_k, dbl_k_mns_one
      !
      do i = 1, 16
        bbc_parm(i) = sol(i)
      end do
      ! 
      if (bbc_parm(1) < 0.0d0) then
        bbc_parm(1) = - bbc_parm(1)
        bbc_parm(2) = - bbc_parm(2)
      end if
      if (bbc_parm(3) < 0.0d0) then
        bbc_parm(3) = - bbc_parm(3)
        bbc_parm(4) = - bbc_parm(4)
      end if
      if (bbc_parm(5) < 0.0d0) bbc_parm(5) = - bbc_parm(5)
      if (bbc_parm(6) < 0.0d0) then
        bbc_parm(6) = - bbc_parm(6)
        bbc_parm(7) = - bbc_parm(7)
      end if
      if (bbc_parm(8) < 0.0d0) bbc_parm(8) = - bbc_parm(8)
      if (bbc_parm(9) < 0.0d0) then
        bbc_parm(9) = - bbc_parm(9)
        bbc_parm(10) = - bbc_parm(10)
      end if
      if (bbc_parm(11) < 0.0d0) then
        bbc_parm(11) = - bbc_parm(11)
        bbc_parm(12) = - bbc_parm(12)
      end if
      if (bbc_parm(13) < 0.0d0) bbc_parm(13) = - bbc_parm(13)
      if (bbc_parm(14) < 0.0d0) then
        bbc_parm(14) = - bbc_parm(14)
        bbc_parm(15) = - bbc_parm(15)
      end if
      if (bbc_parm(16) < 0.0d0) bbc_parm(16) = - bbc_parm(16)
      end subroutine
*----------------------------------------------------------------------*
*                         GET_IDENT_RSD                                *
*                                                                      *
* This routine evaluates the residual terms of the error-function used *
* in the identification of the BBC 2008 yield criterion.               *
*                                                                      *
* Note:                                                                *
*   GET_IDENT_RSD is called by the LMDIF1 routine from the MINPACK-1   *
*   library.                                                           *
*                                                                      *
* Significance of the parameters:                                      *
*   num_rsd_term (IN) - number of residual terms (it must be 16)       *
*   num_var (IN)      - number of minimization variables(it must be 16)*
*   var (IN)          - array used to store the current values of the  *
*                       minimization variables                         *
*   rsd (OUT)         - array used to store the residual terms         *
*                       associated to the current values of the        *
*                       minimization variables                         *
*   gbl_flag (OUT)    - error flag                                     *
*                         - no error --> gbl_flag is not changed       *
*                         - error    --> gbl_flag = -1                 *
* Note:                                                                *
*   gbl_flag = -1 forces the return from the calling routine LMDIF1.   *
*----------------------------------------------------------------------*
      subroutine GET_IDENT_RSD (num_rsd_term, num_var, var, rsd,
     &  gbl_flag)
      implicit none
*----------------------------------------------------------------------*
* Parameters                                                           *
*----------------------------------------------------------------------*
      integer
     &  num_rsd_term, num_var, gbl_flag
      double precision
     &  var(16), rsd(16)
*----------------------------------------------------------------------*
* Variables                                                            *
*----------------------------------------------------------------------*
      integer
     &  dbl_k, dbl_k_mns_one,
     &  i, j,
     &  lcl_flag
      double precision
     &  y_term(8), r_term(8), u_ang(7),
     &  bbc_parm(16), w, inv_w, w_mns_one,
     &  w_mns_one_tms_w_pow_s_mns_one, inv_dbl_k,
     &  f, g
*----------------------------------------------------------------------*
* Common blocks                                                        *
*----------------------------------------------------------------------*
      common
     &  /MAT_DATA/ y_term, r_term, u_ang,
     &  /BBC_DATA/ bbc_parm, w, inv_w, w_mns_one,
     &    w_mns_one_tms_w_pow_s_mns_one, inv_dbl_k, dbl_k, dbl_k_mns_one
*----------------------------------------------------------------------*
* Check the validity of the call.                                      *
*----------------------------------------------------------------------*
      if ((num_rsd_term /= 16) .or. (num_var /= 16)) then
        gbl_flag = -1
        return
      end if
*----------------------------------------------------------------------*
* Evaluate the residual-terms used to generate the error-function.     *
*----------------------------------------------------------------------*
      do i = 1, 16
        bbc_parm(i) = var(i)
      end do
      lcl_flag = 0
      j = 1
      do i = 1, 7
        call GET_FG_U (u_ang(i), f, g, lcl_flag)
        if (lcl_flag /= 0) exit
        rsd(j) = f * y_term(i) + 1.0d0
        rsd(j + 1) = f / g + r_term(i)
        j = j + 2
      end do
      if (lcl_flag /= 0) then
        gbl_flag = -1
        return
      end if
      call GET_FG_B (f, g, lcl_flag)
      if (lcl_flag /= 0) then
        gbl_flag = -1
        return
      end if
      rsd(j) = f * y_term(8) + 1.0d0
      rsd(j + 1) = f / g + r_term(8)
      return
      end subroutine GET_IDENT_RSD
*----------------------------------------------------------------------*
*                            GET_FG_U                                  *
*                                                                      *
* This routine evaluates the quantities f and g associated to the      *
* uniaxial tension along a direction defined by the angle theta        *
* (measured with respect to RD).                                       *
*                                                                      *
* Significance of the parameters:                                      *
*   theta (IN) - angle defining the load direction (radians)           *
*   f, g (OUT) - parameters that will store the quantities f and g     *
*   flag (OUT) - error flag                                            *
*                  - no error --> flag = 0                             *
*                  - error    --> flag = 1                             *
*----------------------------------------------------------------------*
      subroutine GET_FG_U (theta, f, g, flag)
      implicit none
*----------------------------------------------------------------------*
* Parameters                                                           *
*----------------------------------------------------------------------*
      integer
     &  flag
      double precision
     &  theta, f, g
*----------------------------------------------------------------------*
* Symbolic constants                                                   *
*----------------------------------------------------------------------*
      double precision
     &  TINY
      parameter (
     &  TINY = 1.0d-30
     &          )
*----------------------------------------------------------------------*
* Variables                                                            *
*----------------------------------------------------------------------*
      integer
     &  dbl_k, dbl_k_mns_one,
     &  i
      double precision
     &  bbc_parm(16), w, inv_w, w_mns_one,
     &  w_mns_one_tms_w_pow_s_mns_one, inv_dbl_k,
     &  l_1, l_2, m_1, mns_m_2, sqr_m_3, n_1, mns_n_2, sqr_n_3,
     &  aux_l, l, aux_m, m, aux_n, n,
     &  l_pls_m, l_mns_m, m_pls_n, m_mns_n,
     &  sqr_sin_theta, sqr_cos_theta, sqr_sin_dbl_theta,
     &  f_sum, g_sum, w_fact_1, w_fact_2,
     &  aux1, aux2, aux3, aux4
*----------------------------------------------------------------------*
* Common blocks                                                        *
*----------------------------------------------------------------------*
      common
     &  /BBC_DATA/ bbc_parm, w, inv_w, w_mns_one,
     &    w_mns_one_tms_w_pow_s_mns_one, inv_dbl_k, dbl_k, dbl_k_mns_one
*----------------------------------------------------------------------*
* Evaluate the quantities f and g.                                     *
*----------------------------------------------------------------------*
      aux1 = SIN (theta)
      sqr_sin_theta = aux1 * aux1
      aux1 = COS (theta)
      sqr_cos_theta = aux1 * aux1
      sqr_sin_dbl_theta = (sqr_sin_theta + sqr_sin_theta) *
     &  (sqr_cos_theta + sqr_cos_theta)
      w_fact_1 = w_mns_one
      w_fact_2 = w_mns_one_tms_w_pow_s_mns_one
      f_sum = 0.0d0
      g_sum = 0.0d0
      do i = 1, 16, 8
        l_1 = bbc_parm(i)
        l_2 = bbc_parm(i + 1)
        m_1 = bbc_parm(i + 2)
        mns_m_2 = - bbc_parm(i + 3)
        aux1 = bbc_parm(i + 4)
        sqr_m_3 = aux1 * aux1
        n_1 = bbc_parm(i + 5)
        mns_n_2 = - bbc_parm(i + 6)
        aux1 = bbc_parm(i + 7)
        sqr_n_3 = aux1 * aux1
        aux_l = l_1 + l_2
        l = l_1 * sqr_cos_theta + l_2 * sqr_sin_theta
        aux_m = m_1 * sqr_cos_theta + mns_m_2 * sqr_sin_theta
        m = MAX (SQRT (aux_m * aux_m + sqr_m_3 * sqr_sin_dbl_theta),
     &    TINY)
        aux_m = aux_m * (m_1 + mns_m_2) / m
        aux_n = n_1 * sqr_cos_theta + mns_n_2 * sqr_sin_theta
        n = MAX (SQRT (aux_n * aux_n + sqr_n_3 * sqr_sin_dbl_theta),
     &    TINY)
        aux_n = aux_n * (n_1 + mns_n_2) / n
        l_pls_m = l + m
        l_mns_m = l - m
        m_pls_n = m + n
        m_mns_n = m - n
        aux1 = w_fact_1 * l_pls_m ** dbl_k_mns_one
        aux2 = w_fact_1 * l_mns_m ** dbl_k_mns_one
        aux3 = w_fact_2 * m_pls_n ** dbl_k_mns_one
        aux4 = w_fact_2 * m_mns_n ** dbl_k_mns_one
        f_sum = f_sum + l_pls_m * aux1 + l_mns_m * aux2 +
     &    m_pls_n * aux3 + m_mns_n * aux4
        g_sum = g_sum + (aux_l + aux_m) * aux1 +
     &    (aux_l - aux_m) * aux2 + (aux_m + aux_n) * aux3 +
     &    (aux_m - aux_n) * aux4
        w_fact_1 = w_fact_1 * w
        w_fact_2 = w_fact_2 * inv_w
      end do
      if (f_sum < TINY) then
        flag = 1
        return
      end if
      f = f_sum ** inv_dbl_k
      g = g_sum * f / f_sum
      if (ABS (g) < TINY) then
        flag = 1
      else
        flag = 0
      end if
      return
      end subroutine GET_FG_U
*----------------------------------------------------------------------*
*                            GET_FG_B                                  *
*                                                                      *
* This routine evaluates the quantities f and g associated to the      *
* biaxial tension along RD and TD.                                     *
*                                                                      *
* Significance of the parameters:                                      *
*   f, g (OUT) - parameters that will store the quantities f and g     *
*   flag (OUT) - error flag                                            *
*                  - no error --> flag = 0                             *
*                  - error    --> flag = 1                             *
*----------------------------------------------------------------------*
      subroutine GET_FG_B (f, g, flag)
      implicit none
*----------------------------------------------------------------------*
* Parameters                                                           *
*----------------------------------------------------------------------*
      integer
     &  flag
      double precision
     &  f, g
*----------------------------------------------------------------------*
* Symbolic constants                                                   *
*----------------------------------------------------------------------*
      double precision
     &  TINY
      parameter (
     &  TINY = 1.0d-30
     &          )
*----------------------------------------------------------------------*
* Variables                                                            *
*----------------------------------------------------------------------*
      integer
     &  dbl_k, dbl_k_mns_one,
     &  i
      double precision
     &  bbc_parm(16), w, inv_w, w_mns_one,
     &  w_mns_one_tms_w_pow_s_mns_one, inv_dbl_k,
     &  l_1, l_2, m_1, m_2, n_1, n_2,
     &  l, m, n,
     &  l_pls_m, l_mns_m, m_pls_n, m_mns_n,
     &  f_sum, g_sum, w_fact_1, w_fact_2,
     &  aux1, aux2, aux3, aux4
*----------------------------------------------------------------------*
* Common blocks                                                        *
*----------------------------------------------------------------------*
      common
     &  /BBC_DATA/ bbc_parm, w, inv_w, w_mns_one,
     &    w_mns_one_tms_w_pow_s_mns_one, inv_dbl_k, dbl_k, dbl_k_mns_one
*----------------------------------------------------------------------*
* Evaluate the quantities f and g.                                     *
*----------------------------------------------------------------------*
      w_fact_1 = w_mns_one
      w_fact_2 = w_mns_one_tms_w_pow_s_mns_one
      f_sum = 0.0d0
      g_sum = 0.0d0
      do i = 1, 16, 8
        l_1 = bbc_parm(i)
        l_2 = bbc_parm(i + 1)
        m_1 = bbc_parm(i + 2)
        m_2 = bbc_parm(i + 3)
        n_1 = bbc_parm(i + 5)
        n_2 = bbc_parm(i + 6)
        l = l_1 + l_2
        m = m_1 - m_2
        n = n_1 - n_2
        l_pls_m = l + m
        l_mns_m = l - m
        m_pls_n = m + n
        m_mns_n = m - n
        aux1 = w_fact_1 * l_pls_m ** dbl_k_mns_one
        aux2 = w_fact_1 * l_mns_m ** dbl_k_mns_one
        aux3 = w_fact_2 * m_pls_n ** dbl_k_mns_one
        aux4 = w_fact_2 * m_mns_n ** dbl_k_mns_one
        f_sum = f_sum + l_pls_m * aux1 + l_mns_m * aux2 +
     &    m_pls_n * aux3 + m_mns_n * aux4
        g_sum = g_sum + (l_1 + m_1) * aux1 + (l_1 - m_1) * aux2 +
     &    (m_1 + n_1) * aux3 + (m_1 - n_1) * aux4
        w_fact_1 = w_fact_1 * w
        w_fact_2 = w_fact_2 * inv_w
      end do
      if (f_sum < TINY) then
        flag = 1
        return
      end if
      f = f_sum ** inv_dbl_k
      g = g_sum * f / f_sum
      if (ABS (g) < TINY) then
        flag = 1
      else
        flag = 0
      end if
      return
      end subroutine GET_FG_B
*----------------------------------------------------------------------*
*                           GET_S11_S22_S12                            *
*                                                                      *
* This routine evaluates the coordinates of a point belonging to the   *
* normalized yield surface (the point is defined by the ratio of the   *
* normal stresses sig11 / sig22 and the fraction of the shear yield    *
* stress represented by the tangential stress sig12).                  *
*                                                                      *
* Significance of the parameters:                                      *
*   theta (IN)      - angle (expressed in radians) that defines the    *
*                     ratio sig22 / sig11 (more precisely, tan(theta)= *
*                     sig22 / sig11)                                   *
*   sig12_frac (IN) - fraction of the shear yield stress represented   *
*                     by the tangential stress sig12                   *
*   nrm_sig11,                                                         *
*   nrm_sig22,                                                         *
*   nrm_sig12 (OUT) - coordinates of the point belonging to the        *
*                     normalized yield surface (sig11 / Y, sig22 / Y,  *
*                     and sig12 / Y, respectively)                     *
*   gbl_flag (OUT)  - error flag                                       *
*                       - no error --> gbl_flag = 0                    *
*                       - error    --> gbl_flag = 1                    *
*----------------------------------------------------------------------*
      subroutine GET_S11_S22_S12 (theta, sig12_frac, nrm_sig11,
     &  nrm_sig22, nrm_sig12, gbl_flag)
      implicit none
*----------------------------------------------------------------------*
* Parameters                                                           *
*----------------------------------------------------------------------*
      integer
     &  gbl_flag
      double precision
     &  theta, sig12_frac,
     &  nrm_sig11, nrm_sig22, nrm_sig12
*----------------------------------------------------------------------*
* Symbolic constants                                                   *
*----------------------------------------------------------------------*
      integer
     &  MAX_NUM_BRACK
      double precision
     &  FRST_BRACK, BRACK_FACT,
     &  TINY
      parameter (
     &  MAX_NUM_BRACK = 1000,
     &  FRST_BRACK = 1.0d-5, BRACK_FACT = 1.25d0,
     &  TINY = 1.0d-30
     &          )
*----------------------------------------------------------------------*
* Variables                                                            *
*----------------------------------------------------------------------*
      integer
     &  dbl_k, dbl_k_mns_one,
     &  lcl_flag,
     &  i
      double precision
     &  bbc_parm(16), w, inv_w, w_mns_one,
     &  w_mns_one_tms_w_pow_s_mns_one, inv_dbl_k,
     &  w_fact_1, w_fact_2, m_3, n_3,
     &  cos_theta, sin_theta, mns_sin_theta, sig12_lvl,
     &  rho1, rho2, brack,
     &  wrk_arr(8), conv_tol,
     &  aux1, aux2
*----------------------------------------------------------------------*
* Functions called from the MINPACK-1 library                          *
*----------------------------------------------------------------------*
      double precision
     &  DPMPAR
*----------------------------------------------------------------------*
* Procedures passed as arguments                                       *
*----------------------------------------------------------------------*
      external
     &  GET_RHO_RSD
*----------------------------------------------------------------------*
* Common blocks                                                        *
*----------------------------------------------------------------------*
      common
     &  /BBC_DATA/ bbc_parm, w, inv_w, w_mns_one,
     &    w_mns_one_tms_w_pow_s_mns_one, inv_dbl_k, dbl_k,
     &    dbl_k_mns_one,
     &  /RHO_RSD_DATA/ cos_theta, sin_theta, mns_sin_theta, sig12_lvl
*----------------------------------------------------------------------*
* Establish the initial guess used for calculating the radial          *
* coordinate of the point belonging to the normalized yield surface.   *
*----------------------------------------------------------------------*
      w_fact_1 = w_mns_one
      w_fact_2 = w_mns_one_tms_w_pow_s_mns_one
      aux1 = 0.0d0
      do i = 1, 16, 8
        m_3 = bbc_parm(i + 4)
        n_3 = bbc_parm(i + 7)
        aux2 = m_3 ** dbl_k
        aux1 = aux1 + w_fact_1 * (aux2 + aux2) +
     &    w_fact_2 * ((m_3 + n_3) ** dbl_k + (m_3 - n_3) ** dbl_k)
        w_fact_1 = w_fact_1 * w
        w_fact_2 = w_fact_2 * inv_w
      end do
      if (aux1 < TINY) then
        gbl_flag = 1
        return
      end if
      cos_theta = COS (theta)
      sin_theta = SIN (theta)
      mns_sin_theta = - sin_theta
      rho1 = 0.5d0 * aux1 ** (- inv_dbl_k)
      sig12_lvl = rho1 * sig12_frac
      brack = FRST_BRACK
      lcl_flag = 0
      call GET_RHO_RSD (1, rho1, aux1, lcl_flag)
      if (lcl_flag /= 0) then
        gbl_flag = 1
        return
      end if
      rho2 = rho1 + brack
      call GET_RHO_RSD (1, rho2, aux2, lcl_flag)
      if (lcl_flag /= 0) then
        gbl_flag = 1
        return
      end if
      i = 1
      do
        if (i > MAX_NUM_BRACK) then
          lcl_flag = 1
          exit
        end if
        if ((aux1 * aux2) < 0.0d0) exit
        rho1 = rho2
        aux1 = aux2
        brack = brack * BRACK_FACT
        rho2 = rho2 + brack
        call GET_RHO_RSD (1, rho2, aux2, lcl_flag)
        if (lcl_flag /= 0) exit
        i = i + 1
      end do
      if (lcl_flag /= 0) then
        gbl_flag = 1
        return
      end if
      rho1 = 0.5d0 * (rho1 + rho2)
*----------------------------------------------------------------------*
* Solve the equation of the radial coordinate.                         *
*----------------------------------------------------------------------*
      conv_tol = SQRT (DPMPAR (1))
      call HYBRD1 (GET_RHO_RSD, 1, rho1, aux1, conv_tol, lcl_flag,
     &  wrk_arr, 8)
      if (lcl_flag /= 1) then
        gbl_flag = 1
        return
      end if
      if (rho1 < TINY) then
        gbl_flag = 1
        return
      end if
      aux1 = sig12_lvl / rho1
      if (ABS (aux1) > 1.0d0) then
        gbl_flag = 1
        return
      end if
      aux1 = rho1 * COS (ASIN (aux1))
      nrm_sig11 = aux1 * cos_theta
      nrm_sig22 = aux1 * sin_theta
      nrm_sig12 = sig12_lvl
      gbl_flag = 0
      return
      end subroutine GET_S11_S22_S12
*----------------------------------------------------------------------*
*                          GET_RHO_RSD                                 *
*                                                                      *
* This routine evaluates the residual of the equation used for         *
* calculating the radial coordinate of a point belonging to the        *
* normalized yield surface.                                            *
*                                                                      *
* Note:                                                                *
*   GET_RHO_RSD is called by the HYBRD1 routine from the MINPACK-1     *
*   library.                                                           *
*                                                                      *
* Significance of the parameters:                                      *
*   dim (IN)   - size of the arrays sol and rsd (it must be at least 1)*
*   sol (IN)   - array used to store the current approximation of the  *
*                radial coordinate                                     *
*   rsd (OUT)  - array used to store the residual associated to the    *
*                current approximation sol                             *
*   flag (OUT) - error flag                                            *
*                   - no error --> flag is not changed                 *
*                   - error    --> flag = -1                           *
* Note:                                                                *
*   flag = -1 forces the return from the calling routine HYBRD1.       *
*----------------------------------------------------------------------*
      subroutine GET_RHO_RSD (dim, sol, rsd, flag)
      implicit none
*----------------------------------------------------------------------*
* Parameters                                                           *
*----------------------------------------------------------------------*
      integer
     &  dim, flag
      double precision
     &  sol(dim), rsd(dim)
*----------------------------------------------------------------------*
* Symbolic constants                                                   *
*----------------------------------------------------------------------*
      double precision
     &  MNS_ONE, TINY
      parameter (
     &  MNS_ONE = -1.0d0, TINY = 1.0d-30
     &          )
*----------------------------------------------------------------------*
* Variables                                                            *
*----------------------------------------------------------------------*
      integer
     &  dbl_k, dbl_k_mns_one,
     &  i
      double precision
     &  bbc_parm(16), w, inv_w, w_mns_one,
     &  w_mns_one_tms_w_pow_s_mns_one, inv_dbl_k,
     &  l_1, l_2, m_1, m_2, m_3, n_1, n_2, n_3,
     &  l, m, n,
     &  cos_theta, sin_theta, mns_sin_theta, sig12_lvl,
     &  sin_phi, dbl_sin_phi, cos_phi,
     &  sum, w_fact_1, w_fact_2,
     &  rho,
     &  aux1, aux2
*----------------------------------------------------------------------*
* Common blocks                                                        *
*----------------------------------------------------------------------*
      common
     &  /BBC_DATA/ bbc_parm, w, inv_w, w_mns_one,
     &    w_mns_one_tms_w_pow_s_mns_one, inv_dbl_k, dbl_k,
     &    dbl_k_mns_one,
     &  /RHO_RSD_DATA/ cos_theta, sin_theta, mns_sin_theta, sig12_lvl
*----------------------------------------------------------------------*
* Check the validity of the call.                                      *
*----------------------------------------------------------------------*
      if (dim < 1) then
        flag = -1
        return
      end if
*----------------------------------------------------------------------*
* Evaluate the residual of the radial coordinate equation.             *
*----------------------------------------------------------------------*
      rho = sol(1)
      if (rho < TINY) then
        flag = -1
        return
      end if
      sin_phi = sig12_lvl / rho
      if (ABS (sin_phi) > 1.0d0) sin_phi = SIGN (1.0d0, sin_phi)
      dbl_sin_phi = sin_phi + sin_phi
      cos_phi = COS (ASIN (sin_phi))
      w_fact_1 = w_mns_one
      w_fact_2 = w_mns_one_tms_w_pow_s_mns_one
      sum = 0.0d0
      do i = 1, 16, 8
        l_1 = bbc_parm(i)
        l_2 = bbc_parm(i + 1)
        m_1 = bbc_parm(i + 2)
        m_2 = bbc_parm(i + 3)
        m_3 = bbc_parm(i + 4)
        n_1 = bbc_parm(i + 5)
        n_2 = bbc_parm(i + 6)
        n_3 = bbc_parm(i + 7)
        l = (l_1 * cos_theta + l_2 * sin_theta) * cos_phi
        aux1 = (m_1 * cos_theta + m_2 * mns_sin_theta) * cos_phi
        aux2 = m_3 * dbl_sin_phi
        m = SQRT (aux1 * aux1 + aux2 * aux2)
        aux1 = (n_1 * cos_theta + n_2 * mns_sin_theta) * cos_phi
        aux2 = n_3 * dbl_sin_phi
        n = SQRT (aux1 * aux1 + aux2 * aux2)
        sum = sum + w_fact_1 * ((l + m) ** dbl_k + (l - m) ** dbl_k) +
     &    w_fact_2 * ((m + n) ** dbl_k + (m - n) ** dbl_k)
        w_fact_1 = w_fact_1 * w
        w_fact_2 = w_fact_2 * inv_w
      end do
      rsd(1) = rho * sum ** inv_dbl_k + MNS_ONE
      return
      end subroutine GET_RHO_RSD
