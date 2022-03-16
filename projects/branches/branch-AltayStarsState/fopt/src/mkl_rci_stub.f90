! $Id: mkl_rci_stub.f90 2739 2016-08-26 14:14:49Z jgawad $

!> \file mkl_rci_stub.f90 This is a workaround module that provides a missing
!> component of MKL: Fortran module file (.mod) of mkl_rci to be used in
!> dependent code.

! Provide modules: MKL_RCI_TYPE and MKL_RCI
include 'mkl_rci.f90'

