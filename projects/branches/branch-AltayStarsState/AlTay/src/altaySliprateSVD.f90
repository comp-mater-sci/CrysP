!
! $Id
!
#include "criMacros.fpp"

module altaySliprateSVD
use altaySDVTypes
use altayDeformationMechanismData_preconfigured
use altayDeformationMechanism
use altayCRSSTypes
use altayMacroKinematic
use altayPancake, only: Pancak2_tolerance
use criErrcodes
implicit none

!> Maximum number of potentially active slip systems that can be 
!> processed by this module.
integer, parameter :: n_active_max = 8

contains

    subroutine sliprateSVD(solution, MacroDefRate, Pancak2_input, crss_data, DM_data, fallback, info)
    implicit none
    !
    type(DeformationRateSDV),intent(out)        :: solution
    type(DeformationRate),intent(in)            :: MacroDefRate
    type(LinearProgrammingSDV),intent(in)       :: Pancak2_input
    type(CRSSData),intent(in)                   :: crss_data
    type(DeformationMechanismData),intent(in)   :: DM_data
    !>Fallback-scenario (i.e. take pancak2-solution) utilized or not
    logical,intent(out)                         :: fallback
    integer,intent(out)                         :: info
    !
    integer :: i, j !<Running indices
    !>The sign of slip (1.d0 or -1.d0) as found by Pancak2
    double precision,dimension(DM_max_systems) :: sgnn = 0.D0
    !>Alternative A1-matrix: the columns corresponding to negative slip 
    !>as found by Pancak2, have reversed sign. Consequently, all slip systems
    !>associated to A1_sgnn are supposed to have positive slip.
    double precision,dimension(DM_dev_dims,DM_max_systems) :: A1_sgnn = 0.D0
    !>Number of active slip systems retained in solution
    integer :: n_active = 0
    !>Active deformation systems that are retained in solution
    integer,dimension(n_active_max) :: activesystems = 0
    !>Deformation rates (in absolute values) corresponding to activesystems
    double precision,dimension(n_active_max) :: activerates = 0.D0
    logical :: NegligeableDeformation = .FALSE. , SVDalgorithmconverged = .FALSE.
    !
        !Allocate the (allocatable components of) solution
        call ShearRateData_init(solution%shearrate, DM_data%n_systems, info)
        if ( info /= criSuccess ) return
        !
        !Set sgnn & A1_sgnn
        do concurrent (i=1:Pancak2_input%nactiv)
            j = Pancak2_input%indact(i)
            if (Pancak2_input%taurlp(i) < 0.0D0) then
                sgnn(j) = -1.D0
            else
                sgnn(j) = 1.D0
            end if
            A1_sgnn(:,j) = sgnn(j) * DM_data%A1(:,j)
        end do
        !
        !Check whether magnitude of local deformation is practically zero
        !OLD:!NegligeableDeformation = sum(abs(Pancak2_input%sliplp(1:Pancak2_input%nactiv))) < Pancak2_tolerance
        NegligeableDeformation = norm2(Pancak2_input%localstrainrate) < Pancak2_tolerance
        !
        !Set component solution%shearrate
        if (.not.(NegligeableDeformation)) then 
            !Initializations
            n_active = Pancak2_input%nactiv
            activesystems = Pancak2_input%indact
            call SVDalgorithm(n_active, activesystems, activerates, SVDalgorithmconverged, info)
            if (info /= CriSuccess) return
            if (SVDalgorithmconverged) then
                solution%shearrate%shearrate(activesystems(1:n_active)) = &
                    sgnn(activesystems(1:n_active)) * activerates(1:n_active) * &
                    MacroDefRate%vMeqStrainRate
            end if
        end if
        if (NegligeableDeformation .or. (.not. SVDalgorithmconverged)) then
            !Fall back scenario: use pancak2_input for solution
            Fallback = .TRUE.
            solution%shearrate%shearrate(Pancak2_input%indact(1:Pancak2_input%nactiv)) = &
                Pancak2_input%sliplp(1:Pancak2_input%nactiv) * MacroDefRate%vMeqStrainRate
        else
            Fallback = .FALSE.
        end if
        !
        !Set all remaining components of the solution
        solution%totalshearrate = sum(abs(solution%shearrate%shearrate))
        solution%taylorfactor = solution%totalshearrate / MacroDefRate%vMeqStrainRate
        call altayCRSSTypes_CalcWorkRate(solution%workrate, crss_data, &
                                         solution%shearrate, info)
        solution%vMeqStress = solution%workrate / MacroDefRate%vMeqStrainRate
        !
        info = criSuccess
        return
    
    contains
    
        !>Algorithm for finding the active deformation systems and corresponding
        !>deformation rates amongst n potentially active deformation systems, 
        !>using Singular Value Decomposition (SVD), thereby minimizing the 
        !>sum-of-squares of deformation rates.
        !>The algorithm is based on the paper:
        !>  Mánik, T., & Holmedal, B. (2014). Review of the Taylor ambiguity and
        !>  the relationship between rate-independent and rate-dependent 
        !>  full-constraints Taylor models. International Journal of Plasticity,
        !>  55, 152-181.
        !>in particular section 2.5.3 and Appendix B.
        recursive subroutine SVDalgorithm(n, systems, rates, converged, info)
        implicit none
        !
        !>Number of potentially active deformation systems
        integer,intent(inout)                                   :: n
        !>Potentially active deformation systems
        integer,intent(inout),dimension(n_active_max)           :: systems
        !>Deformation rates (in absolute values) corresponding to systems
        double precision,intent(out),dimension(n_active_max)    :: rates
        !>State of convergence of this algorithm
        logical,intent(out)                                     :: converged
        integer,intent(out)                                     :: info
        !
        !>System to be excluded, corresponding to the smallest of negative rates found
        integer :: i = 0
        !> Solution found by solvesystem_pseudoinverseSVD
        logical :: solution_ok = .FALSE.
            !
            converged = .TRUE. !<Exceptions will set value to .FALSE.
            !
            !Calculate rates for n potentially active deformation systems
            call solvesystem_pseudoinverseSVD(rates(1:n), &
                A1_sgnn(:,systems(1:n)), &
                Pancak2_input%localstrainrate, &
                solution_ok, info)
            RETURN_IF_WITH(info /= CriSuccess .or. (.not. solution_ok), converged = .FALSE.)
            !
            !Check if the smallest rate is negative, in which case the system 
            !with smallest rate is excluded in subsequent iteration
            if (minval(rates(1:n)) < 0.D0) then 
                RETURN_IF_WITH(n == 1, converged = .FALSE.)
                i = minloc(rates(1:n),dim=1)
                if (i<n_active_max) systems(i:n_active_max-1) = systems(i+1:n_active_max)
                systems(n_active_max) = 0
                rates(n_active_max) = 0.D0
                n = n - 1
                call SVDalgorithm(n, systems, rates, converged, info)
            end if
            
            return
        
        end subroutine
    
    end subroutine
    
    !>This procedure solves the linear system with unknown vector X(n):
    !>    A(m,n) * X(n) = B(m)
    !>using A+, the pseudoinverse of Singular Value Decomposition (SVD), as:
    !>    X(n) = A+(n,m) * B(m)
    !>Let the SVD be given as:
    !>    A(m,n) = U(m,m) * SIGMA(m,n) * transpose(V(n,n))
    !>in which U and V are orthogonal, and SIGMA is zero except for its min(m,n) 
    !>diagonal elements that are the singular values of A.
    !>Then, A+ is given by
    !>    A+(n,m) = V(n,n) * SIGMA+(n,m) * transpose(U(m,m))
    !>with SIGMA+ being zero except for the components for which SIGMA is nonzero; 
    !>the nonzero components of SIGMA+ equal the reciprocal values of 
    !>the corresponding components of SIGMA.
    subroutine solvesystem_pseudoinverseSVD(x, a, b, acceptablesolution, info)
    use lapack95, only: gesvd
    double precision,intent(out),dimension(:)   :: x !<Unknown vector X(n)
    double precision,intent(in),dimension(:,:)  :: a !<Matrix A(m,n)
    double precision,intent(in),dimension(:)    :: b !<Right-hand side B(m)
    !>State of convergence of Singular Value Decomposition
    logical,intent(out)                         :: acceptablesolution
    integer,intent(out)                         :: info
    !
    integer :: m, n, min_mn !< Dimensions
    integer :: ierr !< Info on allocation
    integer :: infoSVD !< Info on SVD
    integer :: i !< Running index
    !> Local copy of a that gets re-/un-defined as argument in gesvd call
    double precision,dimension(:,:),allocatable  :: acopy
    !> Orthogonal m*m-matrix of left singular vectors
    double precision,dimension(:,:),allocatable :: u(:,:)
    !> The min(m,n) singular values, in descending order
    double precision,dimension(:),allocatable   :: s(:)
    !> Transpose of orthogonal n*n-matrix of right singular vectors
    double precision,dimension(:,:),allocatable :: vt(:,:)
    !> A+(n,m), the pseudoinverse of Singular Value Decomposition (SVD)
    double precision,dimension(:,:),allocatable :: aplus(:,:)
    !>Lower threshold for double precision variables for which the reciprocal 
    !>can be represented in double precision without overflow exception.
    double precision,parameter                  :: eps_recip = 1.E-6
    !>Relative threshold value below which the solution X of equation A*X=B is considered acceptable
    double precision,parameter                  :: eps_TH = 1.E-6
    !
        acceptablesolution = .FALSE.
        
        !Set and check the dimensions of input variables
        m = size(a, dim=1)
        n = size(a, dim=2)
        min_mn = min(m,n)
        RETURN_IF_WITH(m /= size(b) .or. n /= size(x) .or. min_mn <= 0, info = criErr_BadDims)
        
        !Allocations of local allocatable variables
        RETURN_ON_WITH(allocate(acopy,source=a,stat=ierr), ierr/=0, info=criErr_MemAlloc)
        RETURN_ON_WITH(allocate(u(m,m), s(min_mn), vt(n,n), aplus(n,m), stat=ierr) &
                        , ierr/=0, info=criErr_MemAlloc)
        
        !Singular Value Decomposition
        call gesvd(acopy, s, u, vt, job = 'N', info = infoSVD)
        RETURN_IF_WITH(infoSVD > 0, info = criSuccess) !returns with: acceptablesolution = .FALSE.
        RETURN_IF_WITH(infoSVD < 0, info = criErr_BadArgs)
        
        !Calculate A+(n,m) = V(n,n) * SIGMA+(n,m) * transpose(U(m,m))
        aplus = 0.D0
        do concurrent (i=1:min_mn)
            if (s(i) > eps_recip) then
                aplus(:,i) = vt(i,:) / s(i)
            end if
        end do
        aplus = matmul(aplus,transpose(u))
        
        !Calculate X(n) = A+(n,m) * B(m)
        x = matmul(aplus, b)
        
        !Check if the solution X satisfies equation A(m,n) * X(n) = B(m) 
        ! with acceptable accuracy
        if( norm2(matmul(a,x)-b)/norm2(b) < eps_TH) acceptablesolution = .TRUE.
        
        !Deallocations
        deallocate(acopy, u, s, vt, aplus)
        
        info = criSuccess
        return
    
    end subroutine
    
end module
