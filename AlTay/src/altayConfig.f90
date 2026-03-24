!> Basic configuration of AlTay in a form of formalized data structures.

module altayConfig
    use base_defs
    use parameters

    implicit none

   !> Root-level configuration structure of Altay
   type:: altayConfigData
        integer                                     :: model_id = 2
        character(len = fname_len)                  :: output_prefix = 'alamel'
        character(len = fname_len)                  :: jobtitle      = 'alamel'
        character(len = fname_len)                  :: micros_fname  = 'equiaxed.smt'
        character(len = fname_len)                  :: texture_input_fname = ''
        integer:: hardening_model_id
        type(Parameter), allocatable:: hardening_parameters(:)
        integer:: deformation_mechanism
        integer                                   :: nfile = 0
   end type

    ! Definition of the singleton objects
    type(altayConfigData):: acnf
end module
