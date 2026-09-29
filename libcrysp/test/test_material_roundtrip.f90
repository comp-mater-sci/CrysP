!> Roundtrip a material state through its C-compatible counterpart.
!>
!> A material is built through the Fortran API (crysp_new_material) with one phase per supported constitutive model (so every
!> model's and hardening state's serialization is covered), using varying slip system sets, so that the grain -> phase resolution
!> in the conversion is exercised. It is deformed before the roundtrip so that the state is non-trivial (rotated grains, hardened
!> CRSS, evolved dislocation densities). It is then converted to a [[Material_C]] and back into a second material.
!>
!> Equivalence is verified in two independent ways:
!>  1. Structurally: every field of the restored material is compared to the original, including the state of each concrete
!>     model, cluster and hardening state as produced by its `serialize`, and the phase each grain points to.
!>  2. Behaviorally: both materials are subjected to the same query and deformation and must produce the same results. This
!>     catches wiring errors the field comparison cannot see.
!>
!> Converting to C leaves the original intact, so the two materials exist side by side. Both mesoscopic models are tested since
!> they use different cluster types. The texture reported for a new material is checked against the orientations it was built
!> from.
program test_material_roundtrip
    use base_defs, only: DP, TOLERANCE
    use conversions, only: deg_to_rad, euler_to_rotation_matrix
    use libcrysp, only: crysp_new_material, crysp_strain_driven_deformation, crysp_simulate_strain_mode
    use crysp_status, only: Status, failed
    use crysp_model, only: ModelData, ModelDescriptor
    use crysp_micro, only: PhaseData, micro_get_model, &
                           HARDENING_NONE, HARDENING_VOCE, HARDENING_HOCKETT_SHERBY, HARDENING_SWIFT, &
                           HARDENING_DSH_EDGE, HARDENING_DSH_SCREW, HARDENING_DSH_LOOP, &
                           SLIP_SYSTEMS_FCC, SLIP_SYSTEMS_BCC24, SLIP_SYSTEMS_BCC48
    use crysp_constitutive_model, only: ConstitutiveModel
    use meso, only: MESO_MODEL_ALAMEL, MESO_MODEL_FCTAYLOR
    use crysp_material, only: Material
    use incrementation, only: StrainIncrement
    use crysp_macro_c, only: Material_C, assignment(=)
    use crysp_parameters
    use testing

    implicit none

    !> Uniaxial tension along x, traceless as required by the API.
    real(DP), dimension(3,3), parameter:: V_GRAD = reshape([1._DP, 0._DP, 0._DP, &
                                                            0._DP, -0.5_DP, 0._DP, &
                                                            0._DP, 0._DP, -0.5_DP], [3, 3])
    real(DP), parameter:: PRE_STRAIN = 0.02_DP   !! Strain applied before the roundtrip, so that the state is non-trivial
    real(DP), parameter:: POST_STRAIN = 0.02_DP  !! Strain applied to both materials after the roundtrip

    !> Every constitutive model gets its own phase. Extend this list (and model_parameters) when adding a model.
    integer, parameter:: MODEL_IDS(*) = [HARDENING_NONE, HARDENING_VOCE, HARDENING_HOCKETT_SHERBY, HARDENING_SWIFT, &
                                         HARDENING_DSH_EDGE, HARDENING_DSH_SCREW, HARDENING_DSH_LOOP]
    !> Even, so that ALAMEL clusters (consecutive grain pairs) never straddle two phases.
    integer, parameter:: GRAINS_PER_PHASE = 2

    !> Slip system set per phase, same order as MODEL_IDS. DSH supports bcc24 only; the rest is spread over all sets.
    integer, parameter:: SLIP_SYSTEMS(*) = [SLIP_SYSTEMS_BCC48, SLIP_SYSTEMS_BCC24, SLIP_SYSTEMS_FCC, SLIP_SYSTEMS_FCC, &
                                            SLIP_SYSTEMS_BCC24, SLIP_SYSTEMS_BCC24, SLIP_SYSTEMS_BCC24]

    call run_case(MESO_MODEL_ALAMEL, 'ALAMEL')
    call run_case(MESO_MODEL_FCTAYLOR, 'FCTaylor')

    call finish()

contains

    subroutine run_case(meso_model_id, label)
        integer, intent(in):: meso_model_id
        character(*), intent(in):: label

        type(Material), target:: original, restored
        type(Material_C):: mat_c
        type(StrainIncrement), dimension(:), allocatable:: increments_original, increments_restored
        real(DP), dimension(5):: strain_mode, stress_original, stress_restored
        integer, dimension(:,:), allocatable:: texture
        type(Status):: stat

        call make_material(meso_model_id, original)
        call check_texture(original, label)

        stat = crysp_strain_driven_deformation(original, V_GRAD, PRE_STRAIN, increments_original)
        call check_ok(stat, label // ': strain_driven_deformation before the roundtrip')

        !The last increment reports the texture of the deformed material.
        texture = original%get_texture()
        call check_equal(size(increments_original(size(increments_original))%texture, 2), size(texture, 2), &
                         label // ': increment texture has one column per grain')
        if (all(shape(increments_original(size(increments_original))%texture) == shape(texture))) &
            call check_equal(reshape(increments_original(size(increments_original))%texture, [size(texture)]), &
                             reshape(texture, [size(texture)]), label // ': last increment has the texture of the material')

        !Roundtrip. Converting to C leaves the original intact; converting back consumes mat_c.
        mat_c = original
        restored = mat_c

        call check_materials_equal(restored, original, label)

        !Behavioral equivalence: a pure query first ...
        strain_mode = [1._DP, 0._DP, 0._DP, 0._DP, 0._DP]
        stat = crysp_simulate_strain_mode(original, strain_mode, stress_original)
        call check_ok(stat, label // ': simulate_strain_mode (original)')
        stat = crysp_simulate_strain_mode(restored, strain_mode, stress_restored)
        call check_ok(stat, label // ': simulate_strain_mode (restored)')
        call check_equal(stress_restored, stress_original, label // ': simulate_strain_mode yields the same stress', &
                         tolerance=2._DP*TOLERANCE)

        !... then an evolution of the full state.
        stat = crysp_strain_driven_deformation(original, V_GRAD, POST_STRAIN, increments_original)
        call check_ok(stat, label // ': strain_driven_deformation (original)')
        stat = crysp_strain_driven_deformation(restored, V_GRAD, POST_STRAIN, increments_restored)
        call check_ok(stat, label // ': strain_driven_deformation (restored)')
        call check_equal(size(increments_restored), size(increments_original), label // ': same number of increments')
        if (size(increments_restored) == size(increments_original)) then
            associate (a => increments_original(size(increments_original)), b => increments_restored(size(increments_restored)))
                call check_equal(b%vm_strain, a%vm_strain, label // ': same final von Mises strain', tolerance=TOLERANCE)
                call check_equal(b%taylor_factor, a%taylor_factor, label // ': same final Taylor factor', &
                                 tolerance=relative(a%taylor_factor))
                call check_equal(b%stress, a%stress, label // ': same final stress tensor', tolerance=relative(maxval(abs(a%stress))))
                call check_equal(b%deformation_gradient, a%deformation_gradient, label // ': same final deformation gradient', &
                                 tolerance=TOLERANCE)
                call check_equal(reshape(b%texture, [size(b%texture)]), reshape(a%texture, [size(a%texture)]), &
                                 label // ': same final texture')
            end associate
        end if

        !The deformed states still agree field by field.
        call check_materials_equal(restored, original, label // ' (after continued deformation)')
    end subroutine

    !> The texture of a new material is the texture it was built from.
    !>
    !> Orientations are compared as rotation matrices, since Euler angles are not unique. The input angles are whole degrees, so
    !> the rounded texture reproduces them exactly, up to the choice of equivalent angles.
    subroutine check_texture(mat, label)
        type(Material), intent(in):: mat
        character(*), intent(in):: label

        integer, dimension(:,:), allocatable:: texture
        real(DP), dimension(3, GRAINS_PER_PHASE*size(MODEL_IDS)):: expected
        integer:: i

        expected = reshape(deg_to_rad(orientations_deg()), shape(expected))

        texture = mat%get_texture()
        call check_equal(size(texture, 1), 3, label // ': texture has three Euler angles per grain')
        call check_equal(size(texture, 2), size(expected, 2), label // ': texture has one column per grain')
        if (any(shape(texture) /= shape(expected))) return

        do i = 1, size(expected, 2)
            call check_equal(euler_to_rotation_matrix(deg_to_rad(real(texture(:,i), DP))), euler_to_rotation_matrix(expected(:,i)), &
                             label // ': texture matches the input orientations', tolerance=1.e-9_DP)
        end do
    end subroutine

    !> Euler angles (Bunge, degrees) per grain per phase, same order as MODEL_IDS.
    !>
    !> Generated from a fixed integer sequence rather than `random_number`, so that the test is deterministic and identical across
    !> compilers. The multipliers are coprime to the ranges, so consecutive grains are spread over orientation space. The angles are
    !> whole degrees, so that the rounded texture of a new material reproduces them exactly. `Phi` stays within [1, 179] to avoid
    !> the degenerate orientations at 0 and 180, where `phi1` and `phi2` are not unique.
    pure function orientations_deg() result(angles)
        real(DP), dimension(3, GRAINS_PER_PHASE, size(MODEL_IDS)):: angles

        integer:: i, j, k

        do i = 1, size(MODEL_IDS)
            do j = 1, GRAINS_PER_PHASE
                k = (i-1)*GRAINS_PER_PHASE + j
                angles(:,j,i) = real([mod(97*k, 360), 1 + mod(53*k, 179), mod(131*k, 360)], DP)
            end do
        end do
    end function

    !> Compare two materials field by field. The state of concrete models, clusters and hardening states is compared through
    !> their `serialize`, which is exactly what the conversion to C transports.
    subroutine check_materials_equal(actual, expected, label)
        type(Material), target, intent(in):: actual, expected
        character(*), intent(in):: label

        integer:: i, j
        character(16):: i_str, j_str

        call check_equal(actual%meso_model%id, expected%meso_model%id, label // ': meso model ID')
        call check_streams_equal(actual%meso_model%serialize(), expected%meso_model%serialize(), label // ': meso model state')

        call check_equal(size(actual%phases), size(expected%phases), label // ': number of phases')
        if (size(actual%phases) /= size(expected%phases)) return
        do i = 1, size(expected%phases)
            write(i_str, '(I0)') i
            associate (a => actual%phases(i)%model, e => expected%phases(i)%model, msg => label // ': phase ' // trim(i_str))
                call check_equal(a%id, e%id, msg // ' model ID')
                call check_equal(a%taylor_coeffs, e%taylor_coeffs, msg // ' Taylor coefficients')
                call check_equal(a%spin_coeffs, e%spin_coeffs, msg // ' spin coefficients')
                call check_equal(a%basis, e%basis, msg // ' basis')
                call check_streams_equal(a%serialize(), e%serialize(), msg // ' model state')
            end associate
        end do

        call check_equal(size(actual%clusters), size(expected%clusters), label // ': number of clusters')
        if (size(actual%clusters) /= size(expected%clusters)) return
        do i = 1, size(expected%clusters)
            write(i_str, '(I0)') i
            associate (a => actual%clusters(i), e => expected%clusters(i), msg => label // ': cluster ' // trim(i_str))
                call check(same_type_as(a, e), msg // ' has the same dynamic type')
                call check_equal(a%weight, e%weight, msg // ' weight')
                call check_streams_equal(a%serialize(), e%serialize(), msg // ' state')

                call check_equal(size(a%grains), size(e%grains), msg // ' number of grains')
                if (size(a%grains) /= size(e%grains)) cycle
                do j = 1, size(e%grains)
                    write(j_str, '(I0)') j
                    associate (ag => a%grains(j), eg => e%grains(j), gmsg => msg // ' grain ' // trim(j_str))
                        call check_equal(phase_index(ag%phase, actual), phase_index(eg%phase, expected), gmsg // ' phase')
                        call check_equal(ag%orientation, eg%orientation, gmsg // ' orientation')
                        call check_equal(ag%stress_increment, eg%stress_increment, gmsg // ' stress increment')
                        call check(same_type_as(ag%hardening_state, eg%hardening_state), &
                                   gmsg // ' hardening state has the same dynamic type')
                        call check_equal(ag%hardening_state%crss, eg%hardening_state%crss, gmsg // ' CRSS')
                        call check_streams_equal(ag%hardening_state%serialize(), eg%hardening_state%serialize(), &
                                                 gmsg // ' hardening state')
                    end associate
                end do
            end associate
        end do
    end subroutine

    !> Index of the phase of `mat` a grain points to, or 0 if it points to none of them.
    function phase_index(grain_phase, mat) result(k)
        use crysp_constitutive_model, only: Phase
        type(Phase), pointer, intent(in):: grain_phase
        type(Material), target, intent(in):: mat
        integer:: k

        do k = 1, size(mat%phases)
            if (associated(grain_phase, mat%phases(k))) return
        end do
        k = 0
    end function

    !> Tolerance for comparing quantities computed with OpenMP reductions: tiny relative to the magnitude, huge relative to any
    !> discrepancy a conversion bug would cause.
    pure function relative(magnitude) result(tol)
        real(DP), intent(in):: magnitude
        real(DP):: tol

        tol = max(abs(magnitude), 1._DP) * 1.e-8_DP
    end function

    !> Build a material with one phase per constitutive model and GRAINS_PER_PHASE grains each.
    subroutine make_material(meso_model_id, mat)
        integer, intent(in):: meso_model_id
        type(Material), target, intent(out):: mat

        type(PhaseData), dimension(size(MODEL_IDS)):: phase_data
        type(ModelData):: meso_data
        real(DP), dimension(3, GRAINS_PER_PHASE, size(MODEL_IDS)):: orientations
        real(DP), dimension(:), allocatable:: values
        character(8):: id_str
        integer:: i, j
        class(ConstitutiveModel), allocatable:: model
        type(ModelDescriptor):: desc
        type(Status):: stat

        orientations = orientations_deg()
        do i = 1, size(MODEL_IDS)
            values = model_parameters(MODEL_IDS(i))

            !Guard against the parameter table drifting from the model signatures.
            write(id_str, '(I0)') MODEL_IDS(i)
            stat = micro_get_model(MODEL_IDS(i), model)
            call check_ok(stat, 'test setup: model lookup for model ' // trim(id_str))
            if (.not. failed(stat)) then
                desc = model%describe()
                call check_equal(size(values), size(desc%inputs), &
                                 'test setup: parameter count for constitutive model ' // trim(id_str))
            end if

            phase_data(i)%model%id = MODEL_IDS(i)
            allocate(phase_data(i)%model%parameters(size(values)))
            do j = 1, size(values)
                phase_data(i)%model%parameters(j) = values(j)
            end do
            phase_data(i)%deformation_mechanism = SLIP_SYSTEMS(i)
            phase_data(i)%orientations = deg_to_rad(orientations(:,:,i))
        end do

        meso_data%id = meso_model_id
        select case (meso_model_id)
            case (MESO_MODEL_ALAMEL)
                !Grain boundary normals as (theta, phi) spherical coordinates; cycled over the clusters.
                allocate(meso_data%parameters(1))
                meso_data%parameters(1) = deg_to_rad(reshape([90._DP, 0._DP, &
                                                              45._DP, 90._DP], [2, 2]))
            case default
                allocate(meso_data%parameters(0))
        end select

        stat = crysp_new_material(meso_data, phase_data, mat)
        call check_ok(stat, 'test setup: crysp_new_material')
    end subroutine

    !> Assert that an operation reported success, printing the full status line when it did not.
    subroutine check_ok(stat, message)
        type(Status), intent(in):: stat
        character(*), intent(in):: message

        call check(.not. failed(stat), message)
        if (failed(stat)) print '(A,I0,6A)', '          code ', stat%code, ' in ', stat%mod_name, ', ', &
                                 stat%proc_name, ': ', stat%message
    end subroutine

    !> Initialization parameters per model, in signature order. Values taken from the integration test configuration.
    function model_parameters(model_id) result(values)
        integer, intent(in):: model_id
        real(DP), dimension(:), allocatable:: values

        select case (model_id)
            case (HARDENING_NONE)
                values = [real(DP)::]
            case (HARDENING_VOCE)              ! TIII1, TIIIS, TIVS, THIII1, THT
                values = [12.39_DP, 15._DP, 20._DP, 0.2_DP, 0.1_DP]
            case (HARDENING_HOCKETT_SHERBY)    ! tau_0, tau_sat, b, n
                values = [1._DP, 151.2_DP, 29.79_DP, 0.4809_DP]
            case (HARDENING_SWIFT)             ! crss0, gamma0, n
                values = [12.39_DP, 1.e-3_DP, 0.24_DP]
            case (HARDENING_DSH_EDGE, HARDENING_DSH_SCREW, HARDENING_DSH_LOOP)
                values = [2.48e-10_DP, 8.16e4_DP, 0.20_DP, 0.20_DP, 53.0_DP, 2.12e-2_DP, 8.89e-10_DP, 8.37e-1_DP, &
                          2.66e-8_DP, 2.27e-9_DP, 9.59_DP, 1.07_DP, 5.45e-2_DP, 1.44e-9_DP, 4.12e-9_DP, 8.70e-9_DP]
            case default
                error stop 'test_material_roundtrip: no parameter set defined for this constitutive model'
        end select
    end function

    !> Compare two parameter lists element by element.
    subroutine check_streams_equal(actual, expected, label)
        type(Parameter), dimension(:), intent(in):: actual, expected
        character(*), intent(in):: label

        integer:: i

        call check_equal(size(actual), size(expected), label // ': lists have equal length')
        do i = 1, min(size(actual), size(expected))
            call check_parameter_equal(actual(i), expected(i), label, i)
        end do
    end subroutine

    !> Compare two parameters for identical type, rank, shape and value.
    subroutine check_parameter_equal(actual, expected, label, index)
        type(Parameter), intent(in):: actual, expected
        character(*), intent(in):: label
        integer, intent(in):: index

        character(:), allocatable:: msg
        character(16):: index_str
        integer, dimension(2):: extents
        integer:: ia, ie
        integer, dimension(:), allocatable:: iaa, iae
        real(DP):: ra, re
        real(DP), dimension(:), allocatable:: raa, rae
        real(DP), dimension(:,:), allocatable:: ma, me
        character(:), allocatable:: sa, se

        write(index_str, '(I0)') index
        msg = label // ': parameter #' // trim(index_str)

        call check_equal(type_of(actual), type_of(expected), msg // ' has the same type')
        call check_equal(actual%rank, expected%rank, msg // ' has the same rank')
        call check_equal(shape_of(actual), shape_of(expected), msg // ' has the same shape')
        if (type_of(actual) /= type_of(expected) .or. actual%rank /= expected%rank &
            .or. any(shape_of(actual) /= shape_of(expected))) return

        extents = shape_of(expected)
        select case (type_of(expected))
            case (TYPE_INTEGER)
                select case (expected%rank)
                    case (0)
                        ia = actual
                        ie = expected
                        call check_equal(ia, ie, msg // ' (integer) has the same value')
                    case (1)
                        allocate(iaa(extents(1)), iae(extents(1)))
                        iaa = actual
                        iae = expected
                        call check_equal(iaa, iae, msg // ' (integer array) has the same value')
                    case default
                        call check(.false., msg // ' has a supported integer rank')
                end select
            case (TYPE_REAL)
                select case (expected%rank)
                    case (0)
                        ra = actual
                        re = expected
                        call check_equal(ra, re, msg // ' (real) has the same value')
                    case (1)
                        allocate(raa(extents(1)), rae(extents(1)))
                        raa = actual
                        rae = expected
                        call check_equal(raa, rae, msg // ' (real array) has the same value')
                    case (2)
                        allocate(ma(extents(1), extents(2)), me(extents(1), extents(2)))
                        ma = actual
                        me = expected
                        call check_equal(ma, me, msg // ' (real matrix) has the same value')
                    case default
                        call check(.false., msg // ' has a supported real rank')
                end select
            case (TYPE_STRING)
                sa = actual
                se = expected
                call check_equal(sa, se, msg // ' (string) has the same value')
            case default
                call check(.false., msg // ' has a known type')
        end select
    end subroutine
end program
