module cluster_module
    use utils
    use grain_module
    use relaxation_module

    implicit none

    type Cluster
        type(Grain), dimension(:), allocatable:: grains
        integer, dimension(:), allocatable:: ind_basis_systems
        real(DP):: weight                                    !> Measure of importance of the cluster with respect to the whole microstructure
        real(DP), dimension(3, 3):: boundary_reference_frame !> Rotation matrix for boundary frame in ACTIVE notation (for performance)
        real(DP), dimension(:), allocatable:: rss
        real(DP), dimension(:,:), allocatable:: taylor_coeffs, &
                                                spin_coeffs, &
                                                crss, &
                                                inverse_basis
        type(Relaxation), dimension(:), allocatable:: relaxations
        integer:: n_systems


    contains
        procedure:: init => cluster_init
        procedure:: get_imposed_strain => cluster_get_imposed_strain
        procedure:: get_spin_coeffs_relaxations => cluster_get_spin_coeffs_relaxations
        procedure:: set_slip_rates => cluster_set_slip_rates
        procedure:: set_overstress => cluster_set_overstress
    end type

contains

    !Initializes cluster
    !@param this the cluster
    !@param orientations List of euler angle triplets (in radians) describing the orientations of the grains of this cluster
    !@param deformation_mechanism Miller indices of the slip system set used for all grains of this cluster
    !@param boundary Only passed for ALAMEL clusters. Contains triplet of Euler angles in radians describing the orientation of the
    !boundary between both grains of the cluster.
    subroutine cluster_init(this, orientations, deformation_mechanism, boundary, initial_deformation_gradient)
        class(Cluster), target, intent(inout):: this
        real(DP), dimension(:,:), intent(in):: orientations
        integer, dimension(:,:,:), intent(in):: deformation_mechanism
        real(DP), dimension(:), intent(in), optional:: boundary
        real(DP), dimension(:,:), intent(in), optional:: initial_deformation_gradient

        integer:: n_slip_systems_grain, &
                  start_systems, &
                  start_grain, &
                  cluster_size, &
                  i, j, k

        n_slip_systems_grain = size(deformation_mechanism, 3)

        !Temporary hack. Should move to FCTaylor and ALAMEL modules, respectively once they are available.
        if (present(boundary)) then  ! ALAMEL
            cluster_size = 2
            this%n_systems = 2*n_slip_systems_grain+2
            allocate(this%grains(2))
            allocate(this%taylor_coeffs(10, this%n_systems), source = 0._DP) !>Slip systems for 2 grains and 2 relaxations
            allocate(this%crss(2, this%n_systems), source = 0._DP)
            allocate(this%rss(this%n_systems))
            allocate(this%ind_basis_systems(10))
            allocate(this%inverse_basis(10, 10), source = 0._DP)
            this%boundary_reference_frame = matmul(initial_deformation_gradient, transpose(from_euler_angles(boundary)))
            allocate(this%relaxations(2))
            do i = 1, 2
                 call this%relaxations(i)%init(i)
            end do
        else  ! FCTaylor
            cluster_size = 1
            this%n_systems = n_slip_systems_grain
            allocate(this%grains(1))
            this%weight = 1._DP
            allocate(this%crss(2, n_slip_systems_grain))
            allocate(this%rss(n_slip_systems_grain))
            allocate(this%taylor_coeffs(5, n_slip_systems_grain)) !>Slip systems for 2 grains and 2 relaxations
        end if

        !Initialize each grain of the cluster.
        do i = 1, cluster_size
            start_grain = 5*(i-1)
            start_systems = n_slip_systems_grain*(i-1)

            call this%grains(i)%init(deformation_mechanism, &
                                     orientations(:,i), &
                                     this%rss(start_systems+1:start_systems+n_slip_systems_grain), &
                                     this%crss(:,start_systems+1:start_systems+n_slip_systems_grain), &
                                     this%taylor_coeffs(start_grain+1:start_grain+5, start_systems+1:start_systems+n_slip_systems_grain))

            this%grains(i)%taylor_coeffs=>this%taylor_coeffs(start_grain+1:start_grain+5, start_systems+1:start_systems+n_slip_systems_grain)

            do j = 1, n_slip_systems_grain
                this%grains(i)%slip_systems(j)%taylor_coeffs=>this%taylor_coeffs(start_grain+1:start_grain+5, start_systems+j)
            end do
        end do

    end subroutine

    function cluster_get_imposed_strain(this) result(imposed_strain)
        class(Cluster), intent(in):: this

        real(DP), dimension(5*size(this%grains)):: imposed_strain

        integer:: i

        do i = 1, size(this%grains)
            imposed_strain(5*(i-1)+1:5*i) = this%grains(i)%imposed_strain
        end do
    end function

    function cluster_get_spin_coeffs_relaxations(this, ind_grain) result(spin_coeffs_relaxations)
        class(Cluster), intent(in):: this
        integer, intent(in):: ind_grain
        real(DP), dimension(3, 2):: spin_coeffs_relaxations

        integer:: i

        do i = 1, 2
            spin_coeffs_relaxations(:,i) = this%relaxations(i)%spin_coeffs((ind_grain-1)*3+1:ind_grain*3)
        end do
    end function

    subroutine cluster_set_slip_rates(this, slip_rates)
        class(Cluster), intent(inout):: this
        real(DP), dimension(this%n_systems), intent(in):: slip_rates

        integer::   i, &
                    n_systems_grain


        do i = 1, size(this%grains)
            n_systems_grain = size(this%grains(i)%slip_systems)
            this%grains(i)%slip_systems%slip_rate = slip_rates((i-1)*n_systems_grain+1:i*n_systems_grain)

            if (size(this%grains) == 2) &
                this%relaxations(i)%slip_rate = slip_rates(size(slip_rates)-2+i)
        end do
    end subroutine

    subroutine cluster_set_overstress(this, overstress)
        class(Cluster), intent(inout):: this
        real(DP), dimension(this%n_systems), intent(in):: overstress

        integer:: i, &
                  n_systems_grain

        do i = 1, size(this%grains)
            n_systems_grain = size(this%grains(i)%slip_systems)
            this%grains(i)%slip_systems%overstress = overstress((i-1)*n_systems_grain+1:i*n_systems_grain)
        end do
    end subroutine

end module
