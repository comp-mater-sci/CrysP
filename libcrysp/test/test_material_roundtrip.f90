!> Serialize a Material, deserialize it into a fresh instance, and verify that the result is indistinguishable from the original.
!>
!> The material is built through the public API (crysp_new_material) with one phase per supported constitutive model (so every
!> model's and hardening state's serialization is covered), using varying slip system sets, so that the grain -> phase index
!> resolution in the serializer is exercised. It is deformed before serialization so that the serialized state is non-trivial
!> (rotated grains, hardened CRSS, evolved dislocation densities, cluster weights).
!>
!> Equivalence is verified in two independent ways:
!>  1. Structurally: the parameter stream produced by the restored material is compared element by element to the stream of
!>     the original. This is exhaustive over everything the serializer writes.
!>  2. Behaviorally: original and restored material are subjected to the same deformation and must produce the same stresses.
!>     This catches wiring errors that the stream comparison cannot see (e.g. a grain pointing to the wrong phase object).
!>
!> Both mesoscopic models are tested since they serialize different cluster types.
program test_material_roundtrip
    use iso_c_binding, only: C_INT
    use base_defs, only: DP, TOLERANCE
    use conversions, only: deg_to_rad
    use libcrysp, only: crysp_new_material, crysp_strain_driven_deformation, crysp_simulate_strain_mode
    use micro, only: PhaseDescriptor, micro_get_signature, &
                     HARDENING_NONE, HARDENING_VOCE, HARDENING_HOCKETT_SHERBY, HARDENING_SWIFT, &
                     HARDENING_DSH_EDGE, HARDENING_DSH_SCREW, HARDENING_DSH_LOOP, &
                     SLIP_SYSTEMS_FCC, SLIP_SYSTEMS_BCC24, SLIP_SYSTEMS_BCC48
    use meso, only: MESO_MODEL_ALAMEL, MESO_MODEL_FCTAYLOR
    use incrementation, only: StrainIncrement
    use crysp_material, only: Material
    use crysp_serialization
    use testing

    implicit none

    !> Uniaxial tension along x, traceless as required by the API.
    real(DP), dimension(3,3), parameter:: V_GRAD = reshape([1._DP, 0._DP, 0._DP, &
                                                            0._DP, -0.5_DP, 0._DP, &
                                                            0._DP, 0._DP, -0.5_DP], [3, 3])
    real(DP), parameter:: PRE_STRAIN = 0.02_DP   !! Strain applied before serialization
    real(DP), parameter:: POST_STRAIN = 0.02_DP  !! Strain applied to both materials after deserialization

    !> Every constitutive model gets its own phase. Extend this list (and model_parameters) when adding a model.
    integer, parameter:: MODEL_IDS(*) = [HARDENING_NONE, HARDENING_VOCE, HARDENING_HOCKETT_SHERBY, HARDENING_SWIFT, &
                                         HARDENING_DSH_EDGE, HARDENING_DSH_SCREW, HARDENING_DSH_LOOP]
    !> Even, so that ALAMEL clusters (consecutive grain pairs) never straddle two phases.
    integer, parameter:: GRAINS_PER_PHASE = 2

    !> Slip system set per phase, same order as MODEL_IDS. DSH supports bcc24 only; the rest is spread over all sets.
    integer, parameter:: SLIP_SYSTEMS(*) = [SLIP_SYSTEMS_BCC48, SLIP_SYSTEMS_BCC24, SLIP_SYSTEMS_FCC, SLIP_SYSTEMS_FCC, &
                                            SLIP_SYSTEMS_BCC24, SLIP_SYSTEMS_BCC24, SLIP_SYSTEMS_BCC24]

    !> Euler angles (Bunge, degrees) per grain per phase, same order as MODEL_IDS.
    !>
    !> A fixed table rather than generated values, so that the test is deterministic. Should ALAMEL deformation ever abort here
    !> ("No active slip systems found"), that is a robustness issue of the deformation code, not of the serializer this test is
    !> about: it happened before the boundary frame construction in alamel::update_relaxations was made well-conditioned.
    real(DP), parameter:: ORIENTATIONS_DEG(3, GRAINS_PER_PHASE, size(MODEL_IDS)) = reshape([ &
         37._DP, 23._DP,  53._DP,    74._DP, 46._DP, 106._DP, &   ! NONE            bcc48
        111._DP, 69._DP, 159._DP,   148._DP,  2._DP, 212._DP, &   ! VOCE            bcc24
        185._DP, 25._DP, 265._DP,   222._DP, 48._DP, 318._DP, &   ! HOCKETT_SHERBY  fcc12
        121._DP, 29._DP, 329._DP,   158._DP, 52._DP,  22._DP, &   ! SWIFT           fcc12
        269._DP, 31._DP, 181._DP,   306._DP, 54._DP, 234._DP, &   ! DSH_EDGE        bcc24
        343._DP, 77._DP, 287._DP,    20._DP, 10._DP, 340._DP, &   ! DSH_SCREW       bcc24
         57._DP, 33._DP,  33._DP,    94._DP, 56._DP,  86._DP  &   ! DSH_LOOP        bcc24
        ], [3, GRAINS_PER_PHASE, size(MODEL_IDS)])

    call run_case(MESO_MODEL_ALAMEL, 'ALAMEL')
    call run_case(MESO_MODEL_FCTAYLOR, 'FCTaylor')

    call finish()

contains

    subroutine run_case(cp_model_id, label)
        integer, intent(in):: cp_model_id
        character(*), intent(in):: label

        type(Material), target:: original, restored
        type(Parameter), dimension(:), allocatable:: reference, stream, restored_stream
        type(StrainIncrement), dimension(:), allocatable:: increments_original, increments_restored
        real(DP), dimension(5):: strain_mode, stress_original, stress_restored

        call make_material(cp_model_id, original)
        call crysp_strain_driven_deformation(original, V_GRAD, PRE_STRAIN, increments_original)

        ! Two independent streams: reading a Parameter consumes it, so the one fed to deserialize cannot be compared afterwards.
        reference = original%serialize()
        stream = original%serialize()
        call check_equal(size(stream), original%size(), label // ': serialized stream has the advertised size')

        call restored%deserialize(stream)
        call check_equal(restored%size(), original%size(), label // ': restored material advertises the same size')

        restored_stream = restored%serialize()
        call check_streams_equal(reference, restored_stream, label)

        ! Behavioral equivalence: a pure query first ...
        strain_mode = [1._DP, 0._DP, 0._DP, 0._DP, 0._DP]
        strain_mode = strain_mode / norm2(strain_mode)
        call crysp_simulate_strain_mode(original, strain_mode, stress_original)
        call crysp_simulate_strain_mode(restored, strain_mode, stress_restored)
        call check_equal(stress_restored, stress_original, label // ': simulate_strain_mode yields the same stress', &
                         tolerance=2._DP*TOLERANCE)

        ! ... then an evolution of the full state.
        call crysp_strain_driven_deformation(original, V_GRAD, POST_STRAIN, increments_original)
        call crysp_strain_driven_deformation(restored, V_GRAD, POST_STRAIN, increments_restored)
        call check_equal(size(increments_restored), size(increments_original), label // ': same number of increments')
        if (size(increments_restored) == size(increments_original)) then
            associate (a => increments_original(size(increments_original)), b => increments_restored(size(increments_restored)))
                call check_equal(b%vm_strain, a%vm_strain, label // ': same final von Mises strain', tolerance=TOLERANCE)
                call check_equal(b%taylor_factor, a%taylor_factor, label // ': same final Taylor factor', &
                                 tolerance=relative(a%taylor_factor))
                call check_equal(b%stress, a%stress, label // ': same final stress tensor', tolerance=relative(maxval(abs(a%stress))))
                call check_equal(b%deformation_gradient, a%deformation_gradient, label // ': same final deformation gradient', &
                                 tolerance=TOLERANCE)
            end associate
        end if

        ! The state after continued deformation must still be identical.
        reference = original%serialize()
        restored_stream = restored%serialize()
        call check_streams_equal(reference, restored_stream, label // ' (after continued deformation)')
    end subroutine

    !> Tolerance for comparing quantities computed with OpenMP reductions: tiny relative to the magnitude, huge relative to any
    !> discrepancy a serialization bug would cause.
    pure function relative(magnitude) result(tol)
        real(DP), intent(in):: magnitude
        real(DP):: tol

        tol = max(abs(magnitude), 1._DP) * 1.e-8_DP
    end function

    !> Build a material with one phase per constitutive model and GRAINS_PER_PHASE grains each.
    subroutine make_material(cp_model_id, mat)
        integer, intent(in):: cp_model_id
        type(Material), target, intent(out):: mat

        type(PhaseDescriptor), dimension(size(MODEL_IDS)):: phases
        type(Parameter), dimension(:), allocatable:: meso_params
        real(DP), dimension(:), allocatable:: values
        real(DP), dimension(2, 2):: boundaries
        character(8):: id_str
        integer:: i, j

        do i = 1, size(MODEL_IDS)
            phases(i)%model_id = MODEL_IDS(i)
            phases(i)%deformation_mechanism = SLIP_SYSTEMS(i)

            values = model_parameters(MODEL_IDS(i))
            write(id_str, '(I0)') MODEL_IDS(i)
            call check_equal(size(values), size(micro_get_signature(MODEL_IDS(i))), &
                             'test setup: parameter count for constitutive model ' // trim(id_str))
            allocate(phases(i)%parameters(size(values)))
            do j = 1, size(values)
                phases(i)%parameters(j) = values(j)
            end do

            phases(i)%orientations = deg_to_rad(ORIENTATIONS_DEG(:, :, i))
        end do

        select case (cp_model_id)
            case (MESO_MODEL_ALAMEL)
                ! Grain boundary normals as (theta, phi) spherical coordinates; cycled over the clusters.
                boundaries = deg_to_rad(reshape([90._DP, 0._DP, &
                                                 45._DP, 90._DP], [2, 2]))
                allocate(meso_params(1))
                meso_params(1) = boundaries
            case default
                allocate(meso_params(0))
        end select

        call crysp_new_material(cp_model_id, meso_params, phases, mat)
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


    !> Compare two parameter streams element by element. Consumes both streams.
    subroutine check_streams_equal(a, b, label)
        type(Parameter), dimension(:), intent(in):: a, b
        character(*), intent(in):: label

        integer:: i

        call check_equal(size(b), size(a), label // ': streams have equal length')
        do i = 1, min(size(a), size(b))
            call check_parameter_equal(a(i), b(i), label, i)
        end do
    end subroutine

    !> Compare two parameters for identical type, shape and value. Consumes both parameters.
    subroutine check_parameter_equal(a, b, label, index)
        type(Parameter), intent(in):: a, b
        character(*), intent(in):: label
        integer, intent(in):: index

        character(:), allocatable:: msg
        character(16):: index_str
        integer(C_INT):: type_a, type_b
        integer, dimension(:), allocatable:: shape_a, shape_b
        integer:: ia, ib
        integer, dimension(:), allocatable:: iaa, iab
        real(DP):: ra, rb
        real(DP), dimension(:), allocatable:: raa, rab
        real(DP), dimension(:,:), allocatable:: ma, mb
        character(:), allocatable:: sa, sb

        write(index_str, '(I0)') index
        msg = label // ': parameter #' // trim(index_str)

        type_a = typeof(a)
        type_b = typeof(b)
        call check(type_a == type_b, msg // ' has the same type')
        if (type_a /= type_b) return

        shape_a = shape_of(a)
        shape_b = shape_of(b)
        call check_equal(shape_b, shape_a, msg // ' has the same shape')
        if (size(shape_a) /= size(shape_b)) return
        if (any(shape_a /= shape_b)) return

        select case (type_a)
            case (TYPE_INTEGER)
                ia = a
                ib = b
                call check_equal(ib, ia, msg // ' (integer) has the same value')
            case (TYPE_INT_ARRAY)
                allocate(iaa(shape_a(1)), iab(shape_a(1)))
                iaa = a
                iab = b
                call check_equal(iab, iaa, msg // ' (int array) has the same value')
            case (TYPE_REAL)
                ra = a
                rb = b
                call check_equal(rb, ra, msg // ' (real) has the same value')
            case (TYPE_REAL_ARRAY)
                allocate(raa(shape_a(1)), rab(shape_a(1)))
                raa = a
                rab = b
                call check_equal(rab, raa, msg // ' (real array) has the same value')
            case (TYPE_REAL_MATRIX)
                ma = deserialize_real_matrix(a)
                mb = deserialize_real_matrix(b)
                call check_equal(mb, ma, msg // ' (real matrix) has the same value')
            case (TYPE_STRING)
                sa = a
                sb = b
                call check_equal(sb, sa, msg // ' (string) has the same value')
            case default
                call check(.false., msg // ' has a known type')
        end select
    end subroutine
end program
