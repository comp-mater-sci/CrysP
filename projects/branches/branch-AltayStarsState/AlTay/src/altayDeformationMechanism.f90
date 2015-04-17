! $Id$

!> Definitions of deformation mechanisms: slip systems and twinning systems.
!>
!> Public constants defined in this module are prefixed with `DM_`, which is
!> an abbreviation from Deformation Mechanism.
module altayDeformationMechanism
use criErrcodes
implicit none
private

    integer,parameter,public :: DM_fcc12 = 1, &
                                DM_bcc24 = 2, &
                                DM_bcc48 = 3, &
                                DM_user = 99


    integer,parameter,public :: DM_title_length = 72

    integer,parameter,public :: DM_max_systems = 96
    
    integer,parameter,public :: DM_dev_dims = 5
    
    integer,parameter,public :: DM_dir_dims = 3
    
    integer,parameter,public :: DM_twin_dims = 6
    
    
    !> Description of deformation mechanism given in a way suitable for
    !> the linear programming.
    !>
    !> The data fields of the type generally corresponds to the content
    !> of slip system files in the "PRE" format.
    type,public :: DeformationMechanismData
        
        character(len=DM_title_length)      :: description = ''
        
        !> Number of slip systems.
        integer                             :: n_slip_systems = 0
        
        !> Number of twinning systems.
        integer                             :: n_twinning_systems = 0
        
        !> Number of deformation systems, comprising slip and twinning systems.
        integer                             :: n_systems = 0
        
        !> Indices of a set of DM_dev_dims independent deformation systems. 
        !> This is a suitable starting set in the linear problem solving.
        integer,dimension(DM_dev_dims)      :: DI = 0
        
        !> Inverse matrix of the sub-matrix of A1 consisting of the DI deformation systems.
        !> This matrix is expected to be useful at start of linear problem solving (i.e. when
        !> starting set DI is considered). Having this component within this derived type might 
        !> be considered obsolete, as it can be readily derived from DI and A1 when required. 
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
        !>    twinning system. Should the twin plane normal be included in the 
        !>    DeformationMechanismData type, it can readily replace functionality of this 
        !>    component, thereby rendering this one obsolete.
        !> Shape is [DM_twin_dims x n_twinning_systems]
        double precision,dimension(:,:),allocatable :: B2
        
        !> Relevant only for twinning.
        !> Twinning shear for all twinning systems.
        !> Shape is [n_twinning_systems]
        double precision,dimension(:),allocatable :: G
        
        
    end type

    !> Initialization procedures for DeformationMechanismData type
    interface DeformationMechanismData
        module procedure DeformationMechanismData_initEmpty, & 
                         DeformationMechanismData_initFromFile, &
                         DeformationMechanismData_initFromPreconfigured
    end interface
    
    !> Format of slipsystem files
    integer,parameter,public:: DM_format_pre = 1, DM_format_dat = 2
    
    public DeformationMechanismData_readPre
    !
    ! Private data corresponding to the PRE files
    !
    
    integer,parameter   ::  DM_fcc12_nsystems = 12, &
                            DM_bcc24_nsystems = 24, &
                            DM_bcc48_nsystems = 48
    
    
    !==========================================================================
    ! Definition for the FCC materials
    !
    ! Origin of the data: fcc.pre
    !==========================================================================
    character(len=DM_title_length),parameter :: fcc12_description = &
                'FCC - FOR HIGH STACKING FAULT ENERGY F.C.C. METALS'
    
    integer,parameter :: fcc12_n_slip_systems = 12, fcc12_n_twinning_systems = 0
    
    integer,dimension(DM_dev_dims),parameter :: fcc12_DI = [1, 2, 4, 5, 8]
    
    double precision,dimension(DM_dev_dims,DM_fcc12_nsystems),parameter :: fcc12_A1 = &
        reshape([double precision :: &
                0.4082482904638631D0,-0.4082482904638631D0, 0.0000000000000000D0,-0.2886751345948130D0, 0.2886751345948130D0, &
                0.1494292453613423D0, 0.5576775358252053D0, 0.2886751345948130D0, 0.0000000000000000D0,-0.2886751345948130D0, &
               -0.5576775358252053D0,-0.1494292453613423D0,-0.2886751345948130D0, 0.2886751345948130D0, 0.0000000000000000D0, &
                0.4082482904638631D0,-0.4082482904638631D0, 0.0000000000000000D0, 0.2886751345948130D0,-0.2886751345948130D0, &
                0.1494292453613423D0, 0.5576775358252053D0, 0.2886751345948130D0, 0.0000000000000000D0, 0.2886751345948130D0, &
                0.5576775358252053D0, 0.1494292453613423D0, 0.2886751345948130D0, 0.2886751345948130D0, 0.0000000000000000D0, &
               -0.4082482904638631D0, 0.4082482904638631D0, 0.0000000000000000D0, 0.2886751345948130D0, 0.2886751345948130D0, &
                0.1494292453613423D0, 0.5576775358252053D0,-0.2886751345948130D0, 0.0000000000000000D0, 0.2886751345948130D0, &
               -0.5576775358252053D0,-0.1494292453613423D0, 0.2886751345948130D0, 0.2886751345948130D0, 0.0000000000000000D0, &
                0.4082482904638631D0,-0.4082482904638631D0, 0.0000000000000000D0, 0.2886751345948130D0, 0.2886751345948130D0, &
               -0.1494292453613423D0,-0.5576775358252053D0, 0.2886751345948130D0, 0.0000000000000000D0, 0.2886751345948130D0, &
               -0.5576775358252053D0,-0.1494292453613423D0, 0.2886751345948130D0,-0.2886751345948130D0, 0.0000000000000000D0],&
                shape=[DM_dev_dims, DM_fcc12_nsystems])
    
    double precision,dimension(DM_dir_dims,DM_fcc12_nsystems),parameter :: fcc12_B1 = &
        reshape([double precision :: &
                -0.4082482904638631D0, 0.2041241452319316D0, 0.2041241452319316D0, &
                 0.2041241452319316D0,-0.4082482904638631D0, 0.2041241452319316D0, &
                 0.2041241452319316D0, 0.2041241452319316D0,-0.4082482904638631D0, &
                -0.4082482904638631D0,-0.2041241452319316D0,-0.2041241452319316D0, &
                 0.2041241452319316D0, 0.4082482904638631D0,-0.2041241452319316D0, &
                -0.2041241452319316D0, 0.2041241452319316D0,-0.4082482904638631D0, &
                -0.4082482904638631D0,-0.2041241452319316D0, 0.2041241452319316D0, &
                -0.2041241452319316D0,-0.4082482904638631D0,-0.2041241452319316D0, &
                -0.2041241452319316D0, 0.2041241452319316D0, 0.4082482904638631D0, &
                 0.4082482904638631D0,-0.2041241452319316D0, 0.2041241452319316D0, &
                 0.2041241452319316D0,-0.4082482904638631D0,-0.2041241452319316D0, &
                -0.2041241452319316D0,-0.2041241452319316D0,-0.4082482904638631D0],&
                 shape=[DM_dir_dims, DM_fcc12_nsystems])

    
    !>
    !> \remark The PRE file contains transposed form of B. Thus the `order`
    !> parameter must be added to `shape`
    double precision,dimension(DM_dev_dims,DM_dev_dims),parameter :: fcc12_B = &
        reshape([double precision :: &
                 0.9659258262890680D+00,-0.2588190451025207D+00,-0.6784122650808748D-33,-0.1732050807568877D+01, 0.1113284554428872D-16, &
                 0.7071067811865472D+00, 0.7071067811865474D+00, 0.8966940261212418D-16,-0.1732050807568877D+01,-0.1732050807568877D+01, &
                 0.9659258262890680D+00,-0.2588190451025207D+00,-0.6784122650808750D-33, 0.1732050807568877D+01, 0.1113284554428872D-16, &
                -0.2190088388420719D-16, 0.8033938098117588D-16, 0.1732050807568877D+01, 0.1732050807568877D+01, 0.1732050807568877D+01, &
                 0.7071067811865472D+00, 0.7071067811865475D+00,-0.1732050807568877D+01, 0.4859048780290499D-16,-0.9576896948901857D-17],&
                 shape=[DM_dev_dims, DM_dev_dims], order=[2,1])

    !==========================================================================
    ! Definition for the BCC materials, 24 slip systems
    !
    ! Origin of the data: bcc.pre
    !==========================================================================
    character(len=DM_title_length),parameter :: bcc24_description = &
                'BCC - SLIP ON (110) AND (112) PLANES IN <111> DIRECTIONS.'
    
    integer,parameter :: bcc24_n_slip_systems = 24, bcc24_n_twinning_systems = 0
    
    integer,dimension(DM_dev_dims),parameter :: bcc24_DI = [1, 2, 4, 5, 7]
    
    double precision,dimension(DM_dev_dims,DM_bcc24_nsystems),parameter :: bcc24_A1 = &
        reshape([double precision :: &
                 0.4082482904638631D0,-0.4082482904638631D0, 0.0000000000000000D0,-0.2886751345948130D0, 0.2886751345948130D0, &
                 0.1494292453613423D0, 0.5576775358252053D0, 0.2886751345948130D0, 0.0000000000000000D0,-0.2886751345948130D0, &
                -0.5576775358252053D0,-0.1494292453613423D0,-0.2886751345948130D0, 0.2886751345948130D0, 0.0000000000000000D0, &
                 0.4082482904638631D0,-0.4082482904638631D0, 0.0000000000000000D0, 0.2886751345948130D0, 0.2886751345948130D0, &
                 0.1494292453613423D0, 0.5576775358252053D0,-0.2886751345948130D0, 0.0000000000000000D0,-0.2886751345948130D0, &
                -0.5576775358252053D0,-0.1494292453613423D0, 0.2886751345948130D0,-0.2886751345948130D0, 0.0000000000000000D0, &
                 0.4082482904638631D0,-0.4082482904638631D0, 0.0000000000000000D0, 0.2886751345948130D0,-0.2886751345948130D0, &
                 0.1494292453613423D0, 0.5576775358252053D0, 0.2886751345948130D0, 0.0000000000000000D0, 0.2886751345948130D0, &
                -0.5576775358252053D0,-0.1494292453613423D0,-0.2886751345948130D0,-0.2886751345948130D0, 0.0000000000000000D0, &
                 0.4082482904638631D0,-0.4082482904638631D0, 0.0000000000000000D0,-0.2886751345948130D0,-0.2886751345948130D0, &
                 0.1494292453613423D0, 0.5576775358252053D0,-0.2886751345948130D0, 0.0000000000000000D0, 0.2886751345948130D0, &
                -0.5576775358252053D0,-0.1494292453613423D0, 0.2886751345948130D0, 0.2886751345948130D0, 0.0000000000000000D0, &
                -0.4082482904638632D0,-0.4082482904638632D0,-0.3333333333333335D0, 0.1666666666666667D0, 0.1666666666666667D0, &
                 0.5576775358252053D0,-0.1494292453613424D0, 0.1666666666666667D0,-0.3333333333333335D0, 0.1666666666666667D0, &
                -0.1494292453613424D0, 0.5576775358252053D0, 0.1666666666666667D0, 0.1666666666666667D0,-0.3333333333333335D0, &
                -0.4082482904638632D0,-0.4082482904638632D0, 0.3333333333333335D0,-0.1666666666666667D0, 0.1666666666666667D0, &
                 0.5576775358252053D0,-0.1494292453613424D0,-0.1666666666666667D0, 0.3333333333333335D0, 0.1666666666666667D0, &
                -0.1494292453613424D0, 0.5576775358252053D0,-0.1666666666666667D0,-0.1666666666666667D0,-0.3333333333333335D0, &
                -0.4082482904638632D0,-0.4082482904638632D0,-0.3333333333333335D0,-0.1666666666666667D0,-0.1666666666666667D0, &
                 0.5576775358252053D0,-0.1494292453613424D0, 0.1666666666666667D0, 0.3333333333333335D0,-0.1666666666666667D0, &
                -0.1494292453613424D0, 0.5576775358252053D0, 0.1666666666666667D0,-0.1666666666666667D0, 0.3333333333333335D0, &
                -0.4082482904638632D0,-0.4082482904638632D0, 0.3333333333333335D0, 0.1666666666666667D0,-0.1666666666666667D0, &
                 0.5576775358252053D0,-0.1494292453613424D0,-0.1666666666666667D0,-0.3333333333333335D0,-0.1666666666666667D0, &
                -0.1494292453613424D0, 0.5576775358252053D0,-0.1666666666666667D0, 0.1666666666666667D0, 0.3333333333333335D0],&
                shape=[DM_dev_dims, DM_bcc24_nsystems])
    
    double precision,dimension(DM_dir_dims,DM_bcc24_nsystems),parameter :: bcc24_B1 = &
        reshape([double precision :: &
                 0.4082482904638631D0,-0.2041241452319316D0,-0.2041241452319316D0, &
                -0.2041241452319316D0, 0.4082482904638631D0,-0.2041241452319316D0, &
                -0.2041241452319316D0,-0.2041241452319316D0, 0.4082482904638631D0, &
                -0.4082482904638631D0, 0.2041241452319316D0,-0.2041241452319316D0, &
                 0.2041241452319316D0,-0.4082482904638631D0,-0.2041241452319316D0, &
                 0.2041241452319316D0, 0.2041241452319316D0, 0.4082482904638631D0, &
                 0.4082482904638631D0, 0.2041241452319316D0, 0.2041241452319316D0, &
                -0.2041241452319316D0,-0.4082482904638631D0, 0.2041241452319316D0, &
                -0.2041241452319316D0, 0.2041241452319316D0,-0.4082482904638631D0, &
                -0.4082482904638631D0,-0.2041241452319316D0, 0.2041241452319316D0, &
                 0.2041241452319316D0, 0.4082482904638631D0, 0.2041241452319316D0, &
                 0.2041241452319316D0,-0.2041241452319316D0,-0.4082482904638631D0, &
                 0.0000000000000000D0,-0.3535533905932739D0, 0.3535533905932739D0, &
                 0.3535533905932739D0, 0.0000000000000000D0,-0.3535533905932739D0, &
                -0.3535533905932739D0, 0.3535533905932739D0, 0.0000000000000000D0, &
                 0.0000000000000000D0, 0.3535533905932739D0, 0.3535533905932739D0, &
                -0.3535533905932739D0, 0.0000000000000000D0,-0.3535533905932739D0, &
                 0.3535533905932739D0,-0.3535533905932739D0, 0.0000000000000000D0, &
                 0.0000000000000000D0, 0.3535533905932739D0,-0.3535533905932739D0, &
                 0.3535533905932739D0, 0.0000000000000000D0, 0.3535533905932739D0, &
                -0.3535533905932739D0,-0.3535533905932739D0, 0.0000000000000000D0, &
                 0.0000000000000000D0,-0.3535533905932739D0,-0.3535533905932739D0, &
                -0.3535533905932739D0, 0.0000000000000000D0, 0.3535533905932739D0, &
                 0.3535533905932739D0, 0.3535533905932739D0, 0.0000000000000000D0],&
                shape=[DM_dir_dims, DM_bcc24_nsystems])

    
    !>
    !> \remark The PRE file contains transposed form of B. The `order`
    !> parameter of `shape` makes the things right.
    double precision,dimension(DM_dev_dims,DM_dev_dims),parameter :: bcc24_B = &
        reshape([double precision :: &
                0.9659258262890680D+00,-0.2588190451025207D+00,-0.1113284554428872D-16,-0.1732050807568877D+01, 0.4439807896328139D-16, &
                0.7071067811865472D+00, 0.7071067811865475D+00, 0.1732050807568877D+01,-0.4859048780290499D-16,-0.4859048780290499D-16, &
                0.7071067811865474D+00, 0.7071067811865472D+00, 0.9576896948901852D-17, 0.1732050807568877D+01, 0.1732050807568877D+01, &
                0.7071067811865472D+00, 0.7071067811865475D+00,-0.1732050807568877D+01,-0.4859048780290499D-16,-0.4859048780290499D-16, &
                0.2588190451025206D+00,-0.9659258262890680D+00,-0.2070974249319058D-16, 0.3679511122872680D-17,-0.1732050807568877D+01],&
                 shape=[DM_dev_dims, DM_dev_dims], order=[2,1])

    
    
    !==========================================================================
    ! Definition for the BCC materials, 48 slip systems
    !
    ! Origin of the data: bcc2.pre
    !==========================================================================
    character(len=DM_title_length),parameter :: bcc48_description = &
                'BCC - SLIP ON {110}, {112} AND {123} PLANES IN <111> DIRECTIONS.'
    
    integer,parameter :: bcc48_n_slip_systems = 48, bcc48_n_twinning_systems = 0
    
    integer,dimension(DM_dev_dims),parameter :: bcc48_DI = [1, 2, 4, 5, 7]
    
    double precision,dimension(DM_dev_dims,DM_bcc48_nsystems),parameter :: bcc48_A1 = &
        reshape([double precision :: &
                 0.4082482904638631D0,-0.4082482904638631D0, 0.0000000000000000D0,-0.2886751345948130D0, 0.2886751345948130D0, &
                 0.1494292453613423D0, 0.5576775358252053D0, 0.2886751345948130D0, 0.0000000000000000D0,-0.2886751345948130D0, &
                -0.5576775358252053D0,-0.1494292453613423D0,-0.2886751345948130D0, 0.2886751345948130D0, 0.0000000000000000D0, &
                 0.4082482904638631D0,-0.4082482904638631D0, 0.0000000000000000D0, 0.2886751345948130D0, 0.2886751345948130D0, &
                 0.1494292453613423D0, 0.5576775358252053D0,-0.2886751345948130D0, 0.0000000000000000D0,-0.2886751345948130D0, &
                -0.5576775358252053D0,-0.1494292453613423D0, 0.2886751345948130D0,-0.2886751345948130D0, 0.0000000000000000D0, &
                 0.4082482904638631D0,-0.4082482904638631D0, 0.0000000000000000D0, 0.2886751345948130D0,-0.2886751345948130D0, &
                 0.1494292453613423D0, 0.5576775358252053D0, 0.2886751345948130D0, 0.0000000000000000D0, 0.2886751345948130D0, &
                -0.5576775358252053D0,-0.1494292453613423D0,-0.2886751345948130D0,-0.2886751345948130D0, 0.0000000000000000D0, &
                 0.4082482904638631D0,-0.4082482904638631D0, 0.0000000000000000D0,-0.2886751345948130D0,-0.2886751345948130D0, &
                 0.1494292453613423D0, 0.5576775358252053D0,-0.2886751345948130D0, 0.0000000000000000D0, 0.2886751345948130D0, &
                -0.5576775358252053D0,-0.1494292453613423D0, 0.2886751345948130D0, 0.2886751345948130D0, 0.0000000000000000D0, &
                -0.4082482904638632D0,-0.4082482904638632D0,-0.3333333333333335D0, 0.1666666666666667D0, 0.1666666666666667D0, &
                 0.5576775358252053D0,-0.1494292453613424D0, 0.1666666666666667D0,-0.3333333333333335D0, 0.1666666666666667D0, &
                -0.1494292453613424D0, 0.5576775358252053D0, 0.1666666666666667D0, 0.1666666666666667D0,-0.3333333333333335D0, &
                -0.4082482904638632D0,-0.4082482904638632D0, 0.3333333333333335D0,-0.1666666666666667D0, 0.1666666666666667D0, &
                 0.5576775358252053D0,-0.1494292453613424D0,-0.1666666666666667D0, 0.3333333333333335D0, 0.1666666666666667D0, &
                -0.1494292453613424D0, 0.5576775358252053D0,-0.1666666666666667D0,-0.1666666666666667D0,-0.3333333333333335D0, &
                -0.4082482904638632D0,-0.4082482904638632D0,-0.3333333333333335D0,-0.1666666666666667D0,-0.1666666666666667D0, &
                 0.5576775358252053D0,-0.1494292453613424D0, 0.1666666666666667D0, 0.3333333333333335D0,-0.1666666666666667D0, &
                -0.1494292453613424D0, 0.5576775358252053D0, 0.1666666666666667D0,-0.1666666666666667D0, 0.3333333333333335D0, &
                -0.4082482904638632D0,-0.4082482904638632D0, 0.3333333333333335D0, 0.1666666666666667D0,-0.1666666666666667D0, &
                 0.5576775358252053D0,-0.1494292453613424D0,-0.1666666666666667D0,-0.3333333333333335D0,-0.1666666666666667D0, &
                -0.1494292453613424D0, 0.5576775358252053D0,-0.1666666666666667D0, 0.1666666666666667D0, 0.3333333333333335D0, &
                 0.2521277539490177D0,-0.5193889958614422D0,-0.1091089451179962D0,-0.2182178902359925D0, 0.3273268353539886D0, &
                 0.0413454580117595D0,-0.5758679418366084D0,-0.2182178902359925D0,-0.1091089451179962D0, 0.3273268353539886D0, &
                -0.5193889958614422D0, 0.2521277539490177D0,-0.1091089451179962D0, 0.3273268353539886D0,-0.2182178902359925D0, &
                -0.5758679418366084D0, 0.0413454580117595D0,-0.2182178902359925D0, 0.3273268353539886D0,-0.1091089451179962D0, &
                 0.3237401878875908D0, 0.4780435378496827D0, 0.3273268353539886D0,-0.1091089451179962D0,-0.2182178902359925D0, &
                 0.4780435378496827D0, 0.3237401878875908D0, 0.3273268353539886D0,-0.2182178902359925D0,-0.1091089451179962D0, &
                 0.2521277539490177D0,-0.5193889958614422D0, 0.1091089451179962D0, 0.2182178902359925D0, 0.3273268353539886D0, &
                 0.0413454580117595D0,-0.5758679418366084D0, 0.2182178902359925D0, 0.1091089451179962D0, 0.3273268353539886D0, &
                 0.5193889958614422D0,-0.2521277539490177D0,-0.1091089451179962D0, 0.3273268353539886D0, 0.2182178902359925D0, &
                 0.5758679418366084D0,-0.0413454580117595D0,-0.2182178902359925D0, 0.3273268353539886D0, 0.1091089451179962D0, &
                -0.3237401878875908D0,-0.4780435378496827D0, 0.3273268353539886D0,-0.1091089451179962D0, 0.2182178902359925D0, &
                -0.4780435378496827D0,-0.3237401878875908D0, 0.3273268353539886D0,-0.2182178902359925D0, 0.1091089451179962D0, &
                -0.2521277539490177D0, 0.5193889958614422D0,-0.1091089451179962D0, 0.2182178902359925D0, 0.3273268353539886D0, &
                -0.0413454580117595D0, 0.5758679418366084D0,-0.2182178902359925D0, 0.1091089451179962D0, 0.3273268353539886D0, &
                -0.5193889958614422D0, 0.2521277539490177D0, 0.1091089451179962D0, 0.3273268353539886D0, 0.2182178902359925D0, &
                -0.5758679418366084D0, 0.0413454580117595D0, 0.2182178902359925D0, 0.3273268353539886D0, 0.1091089451179962D0, &
                -0.3237401878875908D0,-0.4780435378496827D0, 0.3273268353539886D0, 0.1091089451179962D0,-0.2182178902359925D0, &
                -0.4780435378496827D0,-0.3237401878875908D0, 0.3273268353539886D0, 0.2182178902359925D0,-0.1091089451179962D0, &
                -0.2521277539490177D0, 0.5193889958614422D0, 0.1091089451179962D0,-0.2182178902359925D0, 0.3273268353539886D0, &
                -0.0413454580117595D0, 0.5758679418366084D0, 0.2182178902359925D0,-0.1091089451179962D0, 0.3273268353539886D0, &
                 0.5193889958614422D0,-0.2521277539490177D0, 0.1091089451179962D0, 0.3273268353539886D0,-0.2182178902359925D0, &
                 0.5758679418366084D0,-0.0413454580117595D0, 0.2182178902359925D0, 0.3273268353539886D0,-0.1091089451179962D0, &
                 0.3237401878875908D0, 0.4780435378496827D0, 0.3273268353539886D0, 0.1091089451179962D0, 0.2182178902359925D0, &
                 0.4780435378496827D0, 0.3237401878875908D0, 0.3273268353539886D0, 0.2182178902359925D0, 0.1091089451179962D0],&
                shape=[DM_dev_dims, DM_bcc48_nsystems])
    
    double precision,dimension(DM_dir_dims,DM_bcc48_nsystems),parameter :: bcc48_B1 = &
        reshape([double precision :: &
                 0.4082482904638631D0,-0.2041241452319316D0,-0.2041241452319316D0, &
                -0.2041241452319316D0, 0.4082482904638631D0,-0.2041241452319316D0, &
                -0.2041241452319316D0,-0.2041241452319316D0, 0.4082482904638631D0, &
                -0.4082482904638631D0, 0.2041241452319316D0,-0.2041241452319316D0, &
                 0.2041241452319316D0,-0.4082482904638631D0,-0.2041241452319316D0, &
                 0.2041241452319316D0, 0.2041241452319316D0, 0.4082482904638631D0, &
                 0.4082482904638631D0, 0.2041241452319316D0, 0.2041241452319316D0, &
                -0.2041241452319316D0,-0.4082482904638631D0, 0.2041241452319316D0, &
                -0.2041241452319316D0, 0.2041241452319316D0,-0.4082482904638631D0, &
                -0.4082482904638631D0,-0.2041241452319316D0, 0.2041241452319316D0, &
                 0.2041241452319316D0, 0.4082482904638631D0, 0.2041241452319316D0, &
                 0.2041241452319316D0,-0.2041241452319316D0,-0.4082482904638631D0, &
                 0.0000000000000000D0,-0.3535533905932739D0, 0.3535533905932739D0, &
                 0.3535533905932739D0, 0.0000000000000000D0,-0.3535533905932739D0, &
                -0.3535533905932739D0, 0.3535533905932739D0, 0.0000000000000000D0, &
                 0.0000000000000000D0, 0.3535533905932739D0, 0.3535533905932739D0, &
                -0.3535533905932739D0, 0.0000000000000000D0,-0.3535533905932739D0, &
                 0.3535533905932739D0,-0.3535533905932739D0, 0.0000000000000000D0, &
                 0.0000000000000000D0, 0.3535533905932739D0,-0.3535533905932739D0, &
                 0.3535533905932739D0, 0.0000000000000000D0, 0.3535533905932739D0, &
                -0.3535533905932739D0,-0.3535533905932739D0, 0.0000000000000000D0, &
                 0.0000000000000000D0,-0.3535533905932739D0,-0.3535533905932739D0, &
                -0.3535533905932739D0, 0.0000000000000000D0, 0.3535533905932739D0, &
                 0.3535533905932739D0, 0.3535533905932739D0, 0.0000000000000000D0, &
                 0.3857583749052300D0,-0.3086066999241840D0,-0.0771516749810460D0, &
                 0.3086066999241840D0,-0.3857583749052300D0, 0.0771516749810460D0, &
                -0.3857583749052300D0, 0.0771516749810460D0, 0.3086066999241840D0, &
                -0.3086066999241840D0,-0.0771516749810460D0, 0.3857583749052300D0, &
                -0.0771516749810460D0, 0.3857583749052300D0,-0.3086066999241840D0, &
                 0.0771516749810460D0, 0.3086066999241840D0,-0.3857583749052300D0, &
                -0.3857583749052300D0, 0.3086066999241840D0,-0.0771516749810460D0, &
                -0.3086066999241840D0, 0.3857583749052300D0, 0.0771516749810460D0, &
                -0.3857583749052300D0, 0.0771516749810460D0,-0.3086066999241840D0, &
                -0.3086066999241840D0,-0.0771516749810460D0,-0.3857583749052300D0, &
                -0.0771516749810460D0, 0.3857583749052300D0, 0.3086066999241840D0, &
                 0.0771516749810460D0, 0.3086066999241840D0, 0.3857583749052300D0, &
                 0.3857583749052300D0, 0.3086066999241840D0,-0.0771516749810460D0, &
                 0.3086066999241840D0, 0.3857583749052300D0, 0.0771516749810460D0, &
                 0.3857583749052300D0, 0.0771516749810460D0,-0.3086066999241840D0, &
                 0.3086066999241840D0,-0.0771516749810460D0,-0.3857583749052300D0, &
                -0.0771516749810460D0,-0.3857583749052300D0,-0.3086066999241840D0, &
                 0.0771516749810460D0,-0.3086066999241840D0,-0.3857583749052300D0, &
                -0.3857583749052300D0,-0.3086066999241840D0,-0.0771516749810460D0, &
                -0.3086066999241840D0,-0.3857583749052300D0, 0.0771516749810460D0, &
                 0.3857583749052300D0, 0.0771516749810460D0, 0.3086066999241840D0, &
                 0.3086066999241840D0,-0.0771516749810460D0, 0.3857583749052300D0, &
                -0.0771516749810460D0,-0.3857583749052300D0, 0.3086066999241840D0, &
                 0.0771516749810460D0,-0.3086066999241840D0, 0.3857583749052300D0],&
                shape=[DM_dir_dims, DM_bcc48_nsystems])

    
    !>
    !> \remark The PRE file contains transposed form of B. Thus the `order`
    !> parameter must be added to `shape`
    double precision,dimension(DM_dev_dims,DM_dev_dims),parameter :: bcc48_B = &
        reshape([double precision :: &
                0.9659258262890680D+00,-0.2588190451025207D+00,-0.1113284554428872D-16,-0.1732050807568877D+01, 0.4439807896328139D-16, &
                0.7071067811865472D+00, 0.7071067811865475D+00, 0.1732050807568877D+01,-0.4859048780290499D-16,-0.4859048780290499D-16, &
                0.7071067811865474D+00, 0.7071067811865472D+00, 0.9576896948901852D-17, 0.1732050807568877D+01, 0.1732050807568877D+01, &
                0.7071067811865472D+00, 0.7071067811865475D+00,-0.1732050807568877D+01,-0.4859048780290499D-16,-0.4859048780290499D-16, &
                0.2588190451025206D+00,-0.9659258262890680D+00,-0.2070974249319058D-16, 0.3679511122872680D-17,-0.1732050807568877D+01],&
                shape=[DM_dev_dims, DM_dev_dims], order=[2,1])
    
    double precision,dimension(0),parameter,private :: empty_1D = [double precision::]
    double precision,dimension(DM_twin_dims,0),parameter,private :: empty_2D = [double precision::]
    
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
        allocate(this%A1(DM_dev_dims,this%n_systems),  &
                 this%B1(DM_dir_dims,this%n_systems),  &
                 stat=memerr)
        if (memerr /= 0) return
        !
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
        case(DM_format_pre)
            call DeformationMechanismData_readPre(this, iounit, info)
        case default
            info = criErr_BadArgs
        end select
        close(iounit)
    !
    end subroutine

    subroutine DeformationMechanismData_initFromPreconfigured(this,struct_id,info)
    implicit none
    type(DeformationMechanismData),intent(out)  :: this
    integer,intent(in)                          :: struct_id
    integer,intent(out)                         :: info
    !
    integer,dimension(2) :: dims
    integer :: memerr

    !
        info = criSuccess
        ! Step 1: perform partial initialization
        select case(struct_id)
        case(DM_fcc12)
            !
            this = DeformationMechanismData(description=fcc12_description, &
                                        n_slip_systems = fcc12_n_slip_systems, &
                                        n_twinning_systems = fcc12_n_twinning_systems, &
                                        A1 = fcc12_A1, & 
                                        B1 = fcc12_B1, &
                                        B  = fcc12_B,  &
                                        B2 = empty_2D, &
                                        G =  empty_1D)
        !
        case(DM_bcc24)
            !
            this = DeformationMechanismData(description=bcc24_description, &
                                        n_slip_systems = bcc24_n_slip_systems, &
                                        n_twinning_systems = bcc24_n_twinning_systems, &
                                        A1 = bcc24_A1, & 
                                        B1 = bcc24_B1, &
                                        B  = bcc24_B,  &
                                        B2 = empty_2D, &
                                        G =  empty_1D)
        !
        case(DM_bcc48)
            !
            this = DeformationMechanismData(description=bcc48_description, &
                                        n_slip_systems = bcc48_n_slip_systems, &
                                        n_twinning_systems = bcc48_n_twinning_systems, &
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
    
    
    
    subroutine DeformationMechanismData_readPre(this, inunit, info)
    implicit none
    type(DeformationMechanismData),intent(out)  :: this
    integer,intent(in)                          :: inunit
    integer,intent(out)                         :: info
    !
    type(DeformationMechanismData) :: tmp
    integer :: i, itmp
    !
        read(inunit,fmt=100,err=900,end=900) tmp%description
        read(inunit,fmt=101,err=900,end=900) itmp, &    ! Dummy
                                             tmp%n_slip_systems, &
                                             tmp%n_twinning_systems, &
                                             tmp%DI
        ! Attempt to construct the output
        call DeformationMechanismData_initEmpty(this,tmp%n_slip_systems, &
                                                tmp%n_twinning_systems, info)
        if (info /= criSuccess) return
        ! Post the data from tmp
        this%DI = tmp%DI
        this%description = tmp%description
        !
        ! Read A1 and B1
        do i = 1,this%n_systems
            read(inunit,fmt=102,err=900,end=900) itmp,this%A1(:,i), this%B1(:,i)
        enddo
        ! Read B
        do i=1,dm_dev_dims
            read(inunit,fmt=103,err=900,end=900)  itmp, this%B(i,:)
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
    
      
      
end module
