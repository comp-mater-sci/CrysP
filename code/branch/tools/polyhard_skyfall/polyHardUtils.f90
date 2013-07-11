module polyHardUtils
use fngPath
use fngRuntime
use fngVec5D
use fngUncomment
use updateData
use KPolynomialHard
use polyApproximation
implicit none

      type :: PolyHardConfig
            character(len=max_pathlen)    :: input_fname = ''
            character(len=max_pathlen)    :: output_fname = ''
            character(len=max_pathlen)    :: track_output_fname =''
            integer                       :: polynomial_order = 0
      end type
      
contains
      
      subroutine readPolyHardConfig(inpunit,cnf,info)
      integer,intent(in)                  :: inpunit
      type(PolyHardConfig),intent(out)    :: cnf
      integer,intent(out)                 :: info
      !
            read(inpunit,'(A)',iostat=info,err=900) cnf%input_fname     ! defdata.dat
            call stripComment(cnf%input_fname)
            read(inpunit,'(A)',iostat=info,err=900) cnf%output_fname    ! .hard
            call stripComment(cnf%output_fname)
            read(inpunit,'(A)',iostat=info,err=900) cnf%track_output_fname
            call stripComment(cnf%track_output_fname)
            read(inpunit,*,iostat=info,err=900) cnf%polynomial_order
            return
            ! IO error handler
            900 continue
      !      
      end subroutine
      
      subroutine makeDatapoints(npoints,eps_0,eps_1,deps,vEps,vSigma,info)
      implicit none
      integer,intent(in)                              :: npoints
      double precision,intent(in)                     :: eps_0
      double precision,intent(in)                     :: eps_1
      double precision,intent(out)                    :: deps
      double precision,dimension(:),allocatable,intent(out) :: vEps, vSigma
      integer,intent(out)                             :: info
      !
      integer :: i
      !
            info = -1
            if (npoints <= 0) return
            allocate(vEps(npoints),vSigma(npoints))
            !
            deps = (eps_1 - eps_0) / dble(npoints-1)
            ! Interpolation points must be distinguishable
            if (deps < epsilon(0.D0)) return
            do i=1,npoints
                  vEps(i) = eps_0 + dble(i-1)*deps
            enddo
            vSigma = 0.D0
            info = 0
      !
      end subroutine
      
      
      subroutine makeApproximation(vEps,vSigma,hardApprox,info)
      implicit none
      double precision,dimension(:),intent(in)        :: vEps
      double precision,dimension(:),intent(in)        :: vSigma
      type(polynomialHardData),intent(inout)          :: hardApprox
      integer,intent(out)                             :: info
      !
            info = -1
            call calculateAppoximation(vEps,vSigma,hardApprox%vCoeff,info)
            if (info /= 0) return
            !
            hardApprox%valid_eps_range = [ vEps(1), vEps(size(vEps)) ]
            hardApprox%vCoeff = hardApprox%vCoeff
            info = 0
      !
      end subroutine
      
      
      subroutine prepareData(cnf,def_data,hardApprox,info)
      implicit none
      type(PolyHardConfig),intent(in)                 :: cnf
      type(defData),intent(out)                       :: def_data
      type(polynomialHardData),intent(out)            :: hardApprox
      integer,intent(out)                             :: info
      !
      integer :: inpunit
      double precision :: D0_norm
      !
            ! Open and process defdata.dat file
            inpunit = openOrDie(fpath=cnf%input_fname,status='old')
            call readUpdateData(inpunit,def_data,info)
            if (info /= 0) then
                  errmsg = 'Cannot read file ' // trim(cnf%input_fname)
                  call finalize(1)
            endif
            !
            info = -1
            ! Prepare data points  
            hardApprox = initPolynomialHardData(cnf%polynomial_order)
            !
            hardApprox%vD0 = tens2vec5D(def_data%tDEps)
            D0_norm = norm2(hardApprox%vD0)
            if (D0_norm < epsilon(0.D0)) return
            hardApprox%vD0 = hardApprox%vD0 / D0_norm
            !
            info = 0
      !
      end subroutine

      subroutine writeOutputs(cnf,hardApprox,vEps,vSigma,info)
      implicit none
      type(PolyHardConfig),intent(in)                 :: cnf
      double precision,dimension(:),intent(in)        :: vEps
      double precision,dimension(:),intent(in)        :: vSigma
      type(polynomialHardData),intent(in)             :: hardApprox
      integer,intent(out)                             :: info
      !
      integer :: outunit, i
            ! Open and write .hard file
            outunit = openOrDie(fpath=cnf%output_fname,status='replace')
            call writePolynomialHardData(outunit,hardApprox,info)
            if (info /= 0) then
                  errmsg = 'Cannot write output file.'
                  call finalize(2)
            endif
            close(outunit)      
            ! Write strain-stress points
            if (trim(cnf%track_output_fname) /= '') then
                  open(newunit=outunit,file=cnf%track_output_fname,status='replace',iostat=info)
                  if (info == 0) then
                        do i = 1, size(vEps)
                              write(outunit,fmt=600) vEps(i), vSigma(i)
                        enddo      
                        600 format(2(E18.10,1X))
                  endif
                  close(outunit)
            endif
            info = 0
      !
      end subroutine

      
                  
                  
      
      

                  
end module
