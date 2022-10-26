!> Constants commonly used in libAlamYLP and in projects that depend on it.
module alamYLPConstants
implicit none

    !> Dimensionality of th search space for vector representation (Stress/Strain rate)
    integer,parameter :: alamEval_vSD_dim = 5
    !> Dimensions of second-rank tensor representation of Stress and Strain rate
    integer,parameter :: alamEval_tSD_dim = 3

end module
