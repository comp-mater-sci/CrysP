! $Id$

!> Definitions of deformation mechanisms: slip systems and twinning systems.
!>
!> Public constants defined in this module are prefixed with `DM_`, which is
!> an abbreviation from Deformation Mechanism.
module altayDeformationMechanism
!
use criErrcodes
use criMathUtils
use altayMillerIndices
use altayDeformationMechanismData_preconfigured
!use altayAlgorithms
implicit none
private

    !> Public identifyers for initialization of DeformationMechanismData object
    !> through altayDeformationMechanismData_preconfigured objects
    integer,parameter,public :: DM_fcc12 = 1, &
                                DM_bcc24 = 2, &
                                DM_bcc48 = 3, &
                                DM_user = 99
    
    !> Public identifyers for initialization of DeformationMechanismData object
    !> from file of specific file format.
    integer,parameter,public :: DM_format_pre = 1, &
                                DM_format_dat = 2

    
    !> Description of deformation mechanism given in a way suitable for
    !> the linear programming.
    !>
    !> The data fields include all content of slip system files 
    !> in both the DAT- and PRE-format.
    type,public :: DeformationMechanismData
        
        character(len=DM_title_length)      :: description = ''
        
        !> Unit vectors of unit cell of crystal lattice
        double precision,dimension(3,3)     :: unitcell = 0.D0
        
        !> Number of slip systems.
        integer                             :: n_slip_systems = 0
        
        !> Number of twinning systems.
        integer                             :: n_twinning_systems = 0
        
        !> Number of deformation systems, comprising slip and twinning systems.
        integer                             :: n_systems = 0
        
        !> Miller indices of deformation plane, for all deformation systems.
        !> Shape is [n_systems]
        type(MillerIndices),dimension(:),allocatable :: plane_Miller
        
        !> Miller indices of deformation direction, for all deformation systems.
        !> Shape is [n_systems]        
        type(MillerIndices),dimension(:),allocatable :: direction_Miller       
        
        !> Vector normal to deformation plane, of unit length, 
        !> for all deformation systems.
        !> Shape is [DM_dir_dims x n_systems]
        double precision,dimension(:,:),allocatable :: plane_vector
        
        !> Vector along deformation direction, of unit length, 
        !> for all deformation systems.
        !> Shape is [DM_dir_dims x n_systems]        
        double precision,dimension(:,:),allocatable :: direction_vector 
        
        !> Indices of a set of DM_dev_dims independent deformation systems. 
        !> This is a suitable starting set in the linear problem solving.
        integer,dimension(DM_dev_dims)      :: set0 = 0
        
        !> Inverse matrix of the sub-matrix of A1 consisting of the set0 deformation systems.
        !> This matrix is expected to be useful at start of linear problem solving (i.e. when
        !> starting set in component %set0 is considered).
        double precision,dimension(DM_dev_dims,DM_dev_dims) :: B = 0.D0
        
        !> Matrix with columns of the vector-formatted (symmetric) Schmid tensor
        !> for all deformation systems.
        !> Shape is [DM_dev_dims x n_systems]
        double precision,dimension(:,:),allocatable :: A1
        
        !> Matrix with columns of the vector-formatted 'anti-symmetric Schmid tensor'
        !> for all deformation systems.
        !> Shape is [DM_dir_dims x n_systems]
        double precision,dimension(:,:),allocatable :: B1
        
        !> Relevant only for twinning.
        !> Matrix with columns of the vector-formatted components of a 2nd-ranked tensor
        !> involved in calculation of twin crystal orientation, for all the twinning systems.
        !> Note: the columns of this variable depend only on plane normal of the respective 
        !>    twinning system. 
        !> Shape is [DM_twin_dims x n_twinning_systems]
        double precision,dimension(:,:),allocatable :: B2
        
        !> Relevant only for twinning.
        !> Twinning shear for all twinning systems.
        !> Shape is [n_twinning_systems]
        double precision,dimension(:),allocatable :: G
        
        
    end type

    !> Upper limit of allowable deformation systems.
    !> The value in inherited from altay-V4.
    integer,parameter,public :: DM_max_systems = 96!
    
    !> Near-0 threshold for determinant of matrix.
    !> The value is taken from pretay program.
    double precision, parameter,private :: D_threshold = 1.D-6 
    
    !> Empty components of DeformationMechanismData object
    !> for initialization without twinning systems.
    double precision,dimension(0),parameter,private :: empty_1D = [double precision::]
    double precision,dimension(DM_twin_dims,0),parameter,private :: empty_2D = [double precision::]    
    
    
    !> Initialization procedures for DeformationMechanismData type
    interface DeformationMechanismData_init
        module procedure DeformationMechanismData_initEmpty, & 
                         DeformationMechanismData_initFromFile, &
                         DeformationMechanismData_initFromPreconfigured
    end interface
    
    public DeformationMechanismData_init
 

   
contains

    subroutine DeformationMechanismData_initEmpty(this,nslip,ntwin,info)
    implicit none
    type(DeformationMechanismData),intent(out)  :: this
    integer,intent(in)                          :: nslip
    integer,intent(in)                          :: ntwin
    integer,intent(out)                         :: info
    !
    integer :: memerr
    !
        info = criErr_BadArgs
        if ((nslip < 0) .or. (ntwin < 0) .or. (nslip+ntwin <= 0)) return
        !
        ! Set meta-data
        this%n_slip_systems = nslip
        this%n_twinning_systems = ntwin
        this%n_systems = nslip+ntwin
        ! Allocate memory
        info = criErr_MemAlloc
        !
        ! Apparently, this works fine:
        allocate(this%plane_Miller(this%n_systems), &
                 this%direction_Miller(this%n_systems), &
                 this%plane_vector(DM_dir_dims,this%n_systems), &
                 this%direction_vector(DM_dir_dims,this%n_systems), &
                 this%A1(DM_dev_dims,this%n_systems),  &
                 this%B1(DM_dir_dims,this%n_systems),  &
                 stat=memerr)
        if (memerr /= 0) return
        !
        this%plane_Miller(:)%index(1) = 0 !"A component cannot be an array if the encompassing structure is an array"
        this%plane_Miller(:)%index(2) = 0
        this%plane_Miller(:)%index(3) = 0
        this%direction_Miller(:)%index(1) = 0
        this%direction_Miller(:)%index(2) = 0
        this%direction_Miller(:)%index(3) = 0 !"A component cannot be an array if the encompassing structure is an array"
        this%plane_vector = 0.D0
        this%direction_vector = 0.D0
        this%A1 = 0.D0
        this%B1 = 0.D0
        !
        if (this%n_twinning_systems > 0) then
            allocate(this%B2(DM_twin_dims, this%n_twinning_systems), &
                     this%G(this%n_twinning_systems), &
                     stat=memerr)
            this%B2 = 0.D0
            this%G = 0.D0
        else
            this%B2 = empty_2D
            this%G = empty_1D
        endif
        ! Check status of the last allocation
        if (memerr == 0) info = criSuccess
    !
    end subroutine


    subroutine DeformationMechanismData_initFromFile(this,path,fmt,info)
    implicit none
    type(DeformationMechanismData),intent(out)  :: this
    character(len=*),intent(in)                 :: path
    integer,intent(in)                          :: fmt
    integer,intent(out)                         :: info
    !
    integer :: iounit, ierr
    !
        info = criErr_IOOpen
        open(newunit=iounit, file=path, status='old', iostat=ierr)
        if (ierr /= 0) return
        select case(fmt)
        !
        case(DM_format_dat)
            call DeformationMechanismData_readDat(this, iounit, info)
            if (info /= CriSuccess) return
            call DeformationMechanismData_completeFromDATformat(this, info)
            if (info /= CriSuccess) return
        case(DM_format_pre)
            !This branch allows backwards compatibility. 
            !It is to be avoided if possible, as it leads to an incomplete
            !DeformationMechanismData object.
            call DeformationMechanismData_readPre(this, iounit, info)
            if (info /= CriSuccess) return
        case default
            info = criErr_BadArgs
        end select
        close(iounit)
        info = criSuccess
    !
    end subroutine

    subroutine DeformationMechanismData_initFromPreconfigured(this,struct_id,info)
    implicit none
    type(DeformationMechanismData),intent(out)  :: this
    integer,intent(in)                          :: struct_id
    integer,intent(out)                         :: info
    !
    !
        info = criSuccess
        ! Step 1: perform partial initialization
        select case(struct_id)
        case(DM_fcc12)
            !
            this = DeformationMechanismData(description=fcc12_description, &
                                        unitcell = fcc12_unitcell, &
                                        n_slip_systems = fcc12_n_slip_systems, &
                                        n_twinning_systems = fcc12_n_twinning_systems, &
                                        plane_Miller = fcc12_plane_Miller, &
                                        direction_Miller = fcc12_direction_Miller, &
                                        plane_vector = fcc12_plane_vector, &
                                        direction_vector = fcc12_direction_vector, &
                                        set0 = fcc12_set0, &
                                        A1 = fcc12_A1, & 
                                        B1 = fcc12_B1, &
                                        B  = fcc12_B,  &
                                        B2 = empty_2D, &
                                        G =  empty_1D)
        !
        case(DM_bcc24)
            !
            this = DeformationMechanismData(description=bcc24_description, &
                                        unitcell = bcc24_unitcell, &
                                        n_slip_systems = bcc24_n_slip_systems, &
                                        n_twinning_systems = bcc24_n_twinning_systems, &
                                        plane_Miller = bcc24_plane_Miller, &
                                        direction_Miller = bcc24_direction_Miller, &
                                        plane_vector = bcc24_plane_vector, &
                                        direction_vector = bcc24_direction_vector, &
                                        set0 = bcc24_set0, &
                                        A1 = bcc24_A1, & 
                                        B1 = bcc24_B1, &
                                        B  = bcc24_B,  &
                                        B2 = empty_2D, &
                                        G =  empty_1D)
        !
        case(DM_bcc48)
            !
            this = DeformationMechanismData(description=bcc48_description, &
                                        unitcell = bcc48_unitcell, &
                                        n_slip_systems = bcc48_n_slip_systems, &
                                        n_twinning_systems = bcc48_n_twinning_systems, &
                                        plane_Miller = bcc48_plane_Miller, &
                                        direction_Miller = bcc48_direction_Miller, &
                                        plane_vector = bcc48_plane_vector, &
                                        direction_vector = bcc48_direction_vector, &
                                        set0 = bcc48_set0, &
                                        A1 = bcc48_A1, & 
                                        B1 = bcc48_B1, &
                                        B  = bcc48_B,  &
                                        B2 = empty_2D, &
                                        G =  empty_1D)
        !
        case default
            info = criErr_BadArgs
        end select
        !
        ! Finalize the initialization (relevant for all structures)
        if (info == criSuccess) then
            !
            this%n_systems = this%n_slip_systems + this%n_twinning_systems
        endif
    !
    end subroutine
    
    subroutine DeformationMechanismData_readDat(this, inunit, info) 
    implicit none
    type(DeformationMechanismData),intent(out)  :: this
    integer,intent(in)                          :: inunit
    integer,intent(out)                         :: info
    !
    type(DeformationMechanismData) :: tmp
    integer :: i, j, k
    !
        info = criError
        !
        read(inunit,fmt=100,err=900,end=900) tmp%description
        !
        do i=1,3
            read(inunit,fmt=110,err=900,end=900) (tmp%unitcell(i,j),j=1,3)
        end do
        !Input error checks:
        ! Is the unitcell matrix non-invertible?
        if( abs(determinant(tmp%unitcell,3)) < D_threshold ) then
            info = criErr_BadArgs
            return
        end if 
        !
        read (inunit,fmt=111,err=900,end=900) tmp%n_slip_systems, &
                                              tmp%n_twinning_systems
        !
        ! Attempt to construct the output
        call DeformationMechanismData_initEmpty(this,tmp%n_slip_systems, &
                                                tmp%n_twinning_systems, info)
        if (info /= criSuccess) return
        ! Post the data from tmp
        this%description = tmp%description
        this%unitcell = tmp%unitcell
        !
        do i=1,this%n_systems
            !
            read(inunit,fmt=112,err=900,end=900) &
                    (this%plane_Miller(i)%index(j),j=1,3), &
                    (this%direction_Miller(i)%index(k),k=1,3)
            !
            !Input error checks:
            ! (i)   Are the Miller indices of the deformation plane (0,0,0)?
            ! (ii)  Are the Miller indices of the deformation direction (0,0,0)?
            ! (iii) Is the system non-orthogonal?            
            if( this%plane_Miller(i)%is0() .or. &
                this%direction_Miller(i)%is0() .or. &
                dot_product(this%plane_Miller(i)%index(:), &
                            this%direction_Miller(i)%index(:)) /= 0 ) then
                !
                info = criErr_BadArgs
                return
            end if 
            !
        end do
        !
        do i=1,this%n_twinning_systems
            read(inunit,fmt=113,err=900,end=900) this%g(i)
            !
            !Check positivity
            if (this%g(i) <= 0) then 
                !Twinning systems should be arranged in such 
                !a way that the twinning shear is positive.
                info = criErr_BadArgs
                return
            end if
            !
        end do
        !
        info = criSuccess
        return
        
900     info = criErr_IORead
        return  
        
    100 format(A)
    110 format(3F20.0)
    111 format(2I3)
    112 format(6I3)
    113 format(F20.0)
        
    end subroutine
    
    subroutine DeformationMechanismData_completeFromDATformat(this, info)
    implicit none
    type(DeformationMechanismData),intent(inout)  :: this
    integer,intent(out)                           :: info
    !
    integer :: s, i, j
    double precision, dimension(3) :: vec = 0.D0
    double precision, dimension(3,3) :: velgrad = 0.D0
    integer, dimension(DM_dev_dims) :: trialset = 0.D0
    double precision, dimension(DM_dev_dims,DM_dev_dims) ::trialmatrix
    double precision :: D = 0.0D0 !matrix' determinant 
    logical :: allsetsdone = .false.
    logical :: set0found = .false.
    double precision, dimension(3,3) :: unitcell_inv = 0.D0
        !    
        !Set the %plane_vector and %direction_vector components
        call invertmatrix(this%unitcell,3,unitcell_inv,info)
        if (info /= criSuccess) return
        do s = 1,this%n_systems
            !Set the %plane_vector component
            vec = matmul(unitcell_inv,dfloat(this%plane_Miller(s)%index(:)))
            this%plane_vector(:,s) = vec / norm2(vec)
            !
            !Set the %direction_vector component
            vec = matmul(dfloat(this%direction_Miller(s)%index(:)),this%unitcell)
            this%direction_vector(:,s) = vec / norm2(vec)  
        end do
        !    
        !Set %A1 and %B1 components
        do s = 1,this%n_systems
            !Construct the (normalized) velocity gradient of this deformation system
            !by o-cross product (=tensor product) .
            !> TODO: generalize 'ocross_product' of criMathUtils; it currently only 
            !> accepts 5D-vectors (in one implementation set by precompilor flag)
            !velgrad = ocross_product(this%plane_vector(:,s),this%direction_vector(:,s))
            do i=1,3
                do j=1,3
                    velgrad(i,j)=this%plane_vector(i,s)*this%direction_vector(j,s)
                end do
            end do
            !
            !construct A1-column of this deformation system
            !this%A1(:,s) = SymMat33ToVec5(velgrad)
            call STR5_(this%A1(:,s),velgrad)
            !
            !construct B1-column of this deformation system
            !this%B1(:,s) = AntiSymMat33ToVec3(velgrad)
            this%B1(1,s)=(this%direction_vector(3,s)*this%plane_vector(2,s)-this%direction_vector(2,s)*this%plane_vector(3,s))/2.D0
            this%B1(2,s)=(this%direction_vector(1,s)*this%plane_vector(3,s)-this%direction_vector(3,s)*this%plane_vector(1,s))/2.D0
            this%B1(3,s)=(this%direction_vector(2,s)*this%plane_vector(1,s)-this%direction_vector(1,s)*this%plane_vector(2,s))/2.D0
            !
        end do
        !
        !Set %B2 component (for twinning systems) 
        do s=1,this%n_twinning_systems
            !construct B2-column
            this%B2(1,s)=2.D0*this%plane_vector(1,s)**2.0D0 - 1.D0
            this%B2(2,s)=2.D0*this%plane_vector(2,s)*this%plane_vector(1,s)
            this%B2(3,s)=2.D0*this%plane_vector(3,s)*this%plane_vector(1,s)
            this%B2(4,s)=2.D0*this%plane_vector(2,s)**2.0D0 - 1.D0
            this%B2(5,s)=2.D0*this%plane_vector(3,s)*this%plane_vector(2,s)
            this%B2(6,s)=2.D0*this%plane_vector(3,s)**2.0D0 - 1.D0
            !
        end do
        !
        !Set %B component
        call nextset(trialset, allsetsdone, this%n_systems) !Get the first trialset
        set0found = .false.
        do while (.not. (set0found .or. allsetsdone))
            !
            trialmatrix = this%A1(:,trialset)
            !
            D = determinant(trialmatrix,5)
            !
            if (abs(D) > D_threshold) then
                set0found = .true.
            else
                call nextset(trialset, allsetsdone)
            end if
            !
        end do
        if (set0found) then
            !
            !Set %set0 component
            this%set0 = trialset
            !Set %B component
            call invertmatrix(trialmatrix,DM_dev_dims,this%B,info)
            if (info /= criSuccess) then
                !This branch should not have been executed, as positivity
                ! of the determinant was checked beforehand
                info = criError
                return
            end if
        else !allsetsdone=.true.
            !Output error message to console  
            write(0,*) 'ERROR: The proposed set of deformation systems (slip+twinning)'
            write(0,*) 'can not accomodate arbitrary strains:'
            write(0,*) 'There was no non-singular 5x5 partial matrix found'
            write(0,*) 'amongst all combinations of 5 deformation systems.'
            info = criErr_BadArgs
            return
        end if
        !
        info = criSuccess
        return
        
    contains
    
        subroutine nextset(Y,alldone,Ymax_input)
        implicit none
        integer,dimension(DM_dev_dims),intent(inout) :: Y
        logical,intent(out)                          :: alldone
        integer,intent(in),optional                  :: Ymax_input
        !
        integer,save :: Ymax = 0 !maximum Y-value (in casu for the largest/right-most index);
                                 ! it is SAVEd until next initialization
        integer :: i = 0 !current index
        integer :: Yi_max = 0 !maximum Y-value of current index
            !
            !Providing the Ymax_input optional parameter initializes the algorithm.
            if (present(Ymax_input)) then
                !For 
                !For invalid Ymax_input:
                !  - alldone=.true. is returned
                if (Ymax_input >= DM_dev_dims) then
                    !Valid Ymax_input:
                    !   - the 'initial Y' is returned
                    !   - local variable Ymax (with SAVE attribute) is set
                    alldone = .false.
                    Y = (/ (i, i=1,DM_dev_dims) /) !'initial Y': [1,2,3,4,5]
                    Ymax = Ymax_input
                else
                    !Invalid Ymax_input:
                    !   - alldone=.true. is returned
                    alldone = .true.
                    Y = (/ (0, i=1,DM_dev_dims) /) ![0,0,0,0,0]
                    Ymax = 0
                end if
                !
                return
            end if
            !
            !initializations for the 'next-Y' algorithm
            i = DM_dev_dims
            Yi_max = Ymax
            !
            !determine a first index to change, searching from right to left
            do while (Y(i)==Yi_max)
                i = i-1
                Yi_max = Yi_max-1
                if (i==0) then
                    alldone = .true.
                    return
                end if
            end do
            alldone = .false.
            !
            !make the first (left-most) update of Y
            Y(i) = Y(i)+1
            !            
            !make additional updates of Y (more to the right) 
            ! until (and including) the one most to the right
            i=i+1
            do while (i<=DM_dev_dims)
                Y(i) = Y(i-1)+1
                i = i+1
            end do
            !
        end subroutine
    
    end subroutine
    
    subroutine DeformationMechanismData_readPre(this, inunit, info)
    implicit none
    type(DeformationMechanismData),intent(out)  :: this
    integer,intent(in)                          :: inunit
    integer,intent(out)                         :: info
    !
    type(DeformationMechanismData) :: tmp
    ! explicit temporary just to avoid warning from runtime checks
    double precision,dimension(size(this%B,dim=2)) :: tmp_arr 
    integer :: i, itmp
    !
        read(inunit,fmt=100,err=900,end=900) tmp%description
        read(inunit,fmt=101,err=900,end=900) itmp, &    ! Dummy
                                             tmp%n_slip_systems, &
                                             tmp%n_twinning_systems, &
                                             tmp%set0
        ! Attempt to construct the output
        call DeformationMechanismData_initEmpty(this,tmp%n_slip_systems, &
                                                tmp%n_twinning_systems, info)
        if (info /= criSuccess) return
        ! Post the data from tmp
        this%set0 = tmp%set0
        this%description = tmp%description
        !
        ! Read A1 and B1
        do i = 1,this%n_systems
            read(inunit,fmt=102,err=900,end=900) itmp,this%A1(:,i), this%B1(:,i)
        enddo
        ! Read B
        do i=1,dm_dev_dims
            read(inunit,fmt=103,err=900,end=900)  itmp, tmp_arr
            this%B(i,:) = tmp_arr !Note: !transpose(B) is read
        enddo
        ! Read data for twinning systems
        do i=1, this%n_twinning_systems
            read(inunit,fmt=104,err=900,end=900)  itmp, this%B2(:,i), this%G(i)
        enddo
        !
    100 format(A)
    101 format(8I4)
    102 format (I4,8F20.16) 
    103 format (I4,5D23.16)
    104 format (I4,7F20.16)

         return
    900 info = criErr_IORead

    end subroutine

    !Calculates the inverse of square matrix A as Ainv
    ! If Ainv doesn't exist, info /= criSuccess is returned.
    ! The allowed dimension of A is n=1,2,..,n_max 
    subroutine invertmatrix(A,n,Ainv,info)
    implicit none
    integer,intent(in)                           :: n
    double precision, dimension(n,n),intent(in)  :: A
    double precision, dimension(n,n),intent(out) :: Ainv
    integer,intent(out)                          :: info
    !
    integer :: j
    double precision :: D
    !> Near-0 threshold for determinant of matrix.
    !> The value is taken from pretay program.
    double precision, parameter :: D_threshold = 1.D-6
    integer, parameter :: n_max = 5
        !
        if (n > n_max) then
            info = criError
            Ainv = 0.D0
            return
        end if
        !
        !Calculate the determinant of A
        D = determinant(A,n)
        !
        if (abs(D) < D_threshold) then
            info = criError
            Ainv = 0.D0
        else
            info = criSuccess
            Ainv = adjoint(A,n) / D
        end if
        !
    end subroutine    
    
    !Calculates the determinant for square matrix A.
    ! The allowed dimension of A is n=1,2,..,n_max
    double precision pure function determinant(A,n) result(D)
    implicit none
    integer,intent(in)                          :: n
    double precision, dimension(n,n),intent(in) :: A
    !
    integer :: j
    integer, parameter :: n_max = 5
        !
        select case (n)
        case (1)
            D = A(1,1)
        case (2)
            D = A(1,1)*A(2,2)-A(1,2)*A(2,1)
        case (3)
            D = A(1,1) * (A(2,2)*A(3,3)-A(3,2)*A(2,3)) &
               -A(1,2) * (A(2,1)*A(3,3)-A(3,1)*A(2,3)) &
               +A(1,3) * (A(2,1)*A(3,2)-A(3,1)*A(2,2))
        case (4:n_max)
            D = 0.D0
            ! determinant development along first row
            do j=1,n
                D = D + A(1,j)*cofactor(A,n,1,j)
            enddo
        case default
            D = 0.D0 !shoudl be NaN or so
        end select
        !
    end function
    
    double precision pure function cofactor(A,n,i,j)
    implicit none
    integer,intent(in)                          :: n
    double precision, dimension(n,n),intent(in) :: A
    integer,intent(in)                          :: i,j
    !
    integer,dimension(n-1) :: ii,jj
    double precision, dimension(n-1,n-1) :: Ared 
        !
        !reduce A: remove row i and column j
        ii = [1:i-1,i+1:n]
        jj = [1:j-1,j+1:n]
        Ared = A(ii,jj)
        !
        !calculate cofactor
        cofactor = (-1.D0)**float(i+j) * determinant(Ared,n-1)
        !
    end function
    
    double precision function adjoint(A,n)
    implicit none
    integer,intent(in)                          :: n
    double precision, dimension(n,n),intent(in) :: A
    dimension                                   :: adjoint(n,n)
    !
    integer :: i,j
        !
        forall (i=1:n,j=1:n) adjoint(j,i) = cofactor(A,n,i,j)
        !
        !do (i=1,n)
        !    do (j=1,n)
        !        adjoint(j,i) = cofactor(A,n,i,j)
        !    end do
        !end do
        !
    end function

     
      Subroutine STR5_(D5,VGRAD)
      IMPLICIT none! double precision (A-H,O-Z)
      double precision :: D5, VGRAD, c1, c2, c3
      dimension D5(5),VGRAD(3,3)
      data c1/1.3660254037844390D0/,&
           c2/0.3660254037844387D0/,&
           c3/0.7071067811865476D0/
      D5(1)=c1*VGRAD(2,2)+c2*VGRAD(3,3)
      D5(2)=c2*VGRAD(2,2)+c1*VGRAD(3,3)
      D5(3)=c3*(VGRAD(2,3)+VGRAD(3,2))
      D5(4)=c3*(VGRAD(3,1)+VGRAD(1,3))
      D5(5)=c3*(VGRAD(1,2)+VGRAD(2,1))
      return
      end    
       
end module
