module cluster_module
    use utils
    use grain_module

    implicit none

    type Cluster
        type(Grain), dimension(:), allocatable:: grains
        integer, dimension(:), allocatable:: ind_basis_systems
        real(DP):: weight                                    !> Measure of importance of the cluster with respect to the whole microstructure
        real(DP), dimension(3, 3):: boundary_reference_frame !> Rotation matrix for boundary frame in ACTIVE notation (for performance)
        real(DP), dimension(:), allocatable:: imposed_strain, &
                                              slip_rates, &
                                              overstress, &
                                              rss
        real(DP), dimension(:,:), allocatable:: taylor_coeffs, &
                                                spin_coeffs, &
                                                crss, &
                                                inverse_basis
        real(DP), dimension(:,:,:), allocatable:: spin_coeffs_relaxations


    contains
        procedure:: init => cluster_init
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
            allocate(this%grains(2))
            allocate(this%imposed_strain(10)) !> Imposed strain in crystal frame of both grains
            allocate(this%taylor_coeffs(10, 2*n_slip_systems_grain+2), source = 0._DP) !>Slip systems for 2 grains and 2 relaxations
            allocate(this%slip_rates(2*n_slip_systems_grain+2))
            allocate(this%crss(2, 2*n_slip_systems_grain+2), source = 0._DP)
            allocate(this%overstress(2*n_slip_systems_grain+2))
            allocate(this%rss(2*n_slip_systems_grain+2))
            allocate(this%ind_basis_systems(10))
            allocate(this%inverse_basis(10, 10), source = 0._DP)
            this%boundary_reference_frame = matmul(initial_deformation_gradient, transpose(from_euler_angles(boundary)))
            allocate(this%spin_coeffs_relaxations(3, 2, 2))
        else  ! FCTaylor
            cluster_size = 1
            allocate(this%grains(1))
            this%weight = 1._DP
            allocate(this%imposed_strain(5)) !> Imposed strain in grain crystal frame
            allocate(this%slip_rates(n_slip_systems_grain))
            allocate(this%crss(2, n_slip_systems_grain))
            allocate(this%overstress(n_slip_systems_grain))
            allocate(this%rss(n_slip_systems_grain))
            allocate(this%taylor_coeffs(5, n_slip_systems_grain)) !>Slip systems for 2 grains and 2 relaxations
        end if

        !Initialize each grain of the cluster.
        do i = 1, cluster_size
            start_grain = 5*(i-1)
            start_systems = n_slip_systems_grain*(i-1)

            call this%grains(i)%init(deformation_mechanism, &
                                     orientations(:,i), &
                                     this%imposed_strain(start_grain+1:start_grain+5), &
                                     this%slip_rates(start_systems+1:start_systems+n_slip_systems_grain), &
                                     this%overstress(start_systems+1:start_systems+n_slip_systems_grain), &
                                     this%rss(start_systems+1:start_systems+n_slip_systems_grain), &
                                     this%crss(:,start_systems+1:start_systems+n_slip_systems_grain), &
                                     this%taylor_coeffs(start_grain+1:start_grain+5, start_systems+1:start_systems+n_slip_systems_grain))

            this%grains(i)%taylor_coeffs=>this%taylor_coeffs(start_grain+1:start_grain+5, start_systems+1:start_systems+n_slip_systems_grain)

            do j = 1, n_slip_systems_grain
                this%grains(i)%slip_systems(j)%taylor_coeffs=>this%taylor_coeffs(start_grain+1:start_grain+5, start_systems+j)
            end do


        end do

!        this%crss(:,1:12) = 1._DP
!        this%crss(:,13:24) = 2._DP
!
!        print *, "Cluster: "
!        print "(26f6.2)", this%crss
!
!    !    do j = 1, 10
!    !        print "(26f6.2)", this%taylor_coeffs(j, :)
!    !    end do
!
!        print *, "Grain 1: "
!        print "(12f6.2)", this%grains(1)%crss
!    !    do j = 1, 5
!    !        print "(12f6.2)", this%grains(1)%taylor_coeffs(j, :)
!    !    end do
!
!    !    do k = 1, 12
!    !        print *, "Slip system ", k, " "
!    !        print "(5f6.2)", this%grains(1)%slip_systems(k)%taylor_coeffs
!    !    end do
!
!        print *, "Grain 2: "
!        print "(12f6.2)", this%grains(2)%crss
!    !    do j = 1, 5
!    !        print "(12f6.2)", this%grains(2)%taylor_coeffs(j, :)
!    !    end do
!
!    !    do k = 1, 12
!    !        print *, "Slip system ", k, " "
!    !        print "(5f6.2)", this%grains(1)%slip_systems(k)%taylor_coeffs
!    !    end do
!
!        stop



    end subroutine



end module
