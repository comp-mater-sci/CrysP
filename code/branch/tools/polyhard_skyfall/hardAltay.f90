!
! $Id$
!
!>    \author Jerzy Gawad
!>    Email:  Jerzy.Gawad@cs.kuleuven.be
!>
!>    Organization: Katholieke Universiteit Leuven (KU Leuven)
!>    Organization unit: Dept.Comp.Sci., TWR Group
!>    \copyright KU Leuven
!>
!>    \date Date of the initial release: 2013-09-13
!>    $Revision$
!>    $Date$
!>
!>    History of modifications: (see svn log)
!>
!
!> The module implements aquisition of hardening data from the libAlTay 
module hardAltay
use updateData
use dmcBasicModule, only: readAlamelConfigSection
use altaySub
use altayConfig
use fngVec5D

implicit none

      type :: altayModel
            type(alTayConfigData)   :: altay_cnf
            double precision        :: dEps_default = 0.001
            !> Conversion factor. The stresses from the AlTay will be multiplied 
            !> by this number.
            double precision        :: unit_conversion_factor = 1.0D6
      end type
      
contains
      
      !> Wrapper for initialization of the libaltay.
      !> This routine has to be called before the first call to getStress()
      subroutine initialize(this,info)
      implicit none
      type(altayModel),intent(inout)      :: this
      integer,intent(out)                 :: info
      !      
            ! Initialize AlTay
            this%altay_cnf%output_prefix = 'alTayPolyHard'
            this%altay_cnf%jobtitle = 'alTayPolyHard'
            !
            call initAltay(this%altay_cnf,info)
            !
      !      
      end subroutine
      
      !> 
      subroutine getStress(this,vEps,vSigma,vStrainMode,info,errmsg)
      implicit none
      type(altayModel),intent(inout)            :: this
      double precision,dimension(:),intent(in)  :: vEps
      double precision,dimension(:),intent(out) :: vSigma
      double precision,dimension(fng_dsv_dim),intent(in)   :: vStrainMode
      integer,intent(out)                       :: info
      character(len=*),intent(out)              :: errmsg
      !
      double precision :: deps, deltaEps_norm
      double precision,dimension(3,3) :: tDeltaEps
      double precision,dimension(5)   :: vDeltaEps
      integer :: npoints, i
      !
            info = fngErr_BadDims
            errmsg = ''
            if (size(vEps) /= size(vSigma)) return
            !
            npoints = size(vEps)
            ! Prepare strain increment tensor
            ! The strain increments are given as von Mises equivalent strain, 
            ! so to get the scaled strain increment right, we have to multiply deltaEps_norm by sqrt(2/3)
            deltaEps_norm = norm2(vStrainMode) * root23     
            if (deltaEps_norm < epsilon(0.D0)) then
                  info = fngErr_BadArgs
                  errmsg = 'Input error: norm of strain direction vector must not be zero.'
                  return
            endif
            !
            ! Init and configure steps
            call initStepData(npoints,astate,info)
            if (info /= 0) then
                  info = fngError
                  errmsg = 'Cannot initialize data structure for alamel results'
                  return
            endif      
            !
            ! Configure steps
            deps = this%dEps_default
            do i=1,npoints
                  !
                  ! Set the strain increment. 
                  ! For the last step use the previous value
                  if (i /= npoints) deps = abs(vEps(i+1) - vEps(i))
                  vDeltaEps = vStrainMode / deltaEps_norm * deps
                  tDeltaEps = vec5D2tens(vDeltaEps)
                  !                          
                  astate%simulCalls(i)%input%full_model = .true.
                  astate%simulCalls(i)%input%keep_texture = .false.
                  astate%simulCalls(i)%input%keep_state = .false.
                  astate%simulCalls(i)%input%dgf = tDeltaEps
                  call setStepType(astate%simulCalls(i)%input,this%altay_cnf%model_id,info)
            enddo
            !      
            ! Run AlTay
            call runSteps(astate,info)
            if (info /= 0) then
                 errmsg = 'Multilevel model failed.' 
                 info = fngError
                 return
            endif
#ifdef DIAGNOSTICS
            do i = 1, npoints
                  write(*,fmt=500) astate%simulCalls(i)%output%stress_tensor
            enddo
            500 format(3(3(E15.6,1X),/))
#endif
            !
            ! Get average stresses
            vSigma = astate%simulCalls(:)%output%equivalent_stress * this%unit_conversion_factor
      !
      end subroutine
      
end module