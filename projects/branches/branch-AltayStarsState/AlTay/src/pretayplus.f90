
!> From input DAT-file (1st console argument), the pretayplus program:
!>   1) Loads an object of type DeformationMechanismData
!>   2) Outputs (partly) the object in PRE-file format 
!>       (obsolete feature; useful for verification and backward compatibility)
!>       -> internal procedure: DeformationMechanismData_writePre 
!>   3) Outputs the object in f90-file format, suitable for addition to 
!>       altayDeformationMechanismdata_preconfigured module. 
!>       -> internal procedure: DeformationMechanismData_writef90 
!>   4) If DAT-filename corresponds to configuration present in 
!>       altayDeformationMechanismdata_preconfigured (fcc.dat;bcc.dat;bcc2.dat), 
!>       then additinaly an object from preconfiguration is loaded and outputted
!>       as another, 'verification' f90-file. 
!>       -> internal procedure: DeformationMechanismData_writef90
program pretayplus
!
use criErrcodes
use altayDeformationMechanismData_preconfigured
use altayDeformationMechanism
!use altayIOConfig !preferred not to use these as units 5 and 6 are preconnected units.
                   !cf. Visual Fortran Compilor documentation: WRITE statement.
!
implicit none
!
!>IO unit numbers
integer, parameter :: ipre = 21, if90 = 22, iverif_f90 = 22
!
character(len=50) :: cmd_arg
character(len=54) :: fname_dat, fname_pre, fname_f90
character(len=60) :: fname_verif_f90
integer :: io_stat = 0, info = criError
character(len=200) :: errormessage
character(12) :: code
type(DeformationMechanismData)  :: DMdata, DMdata_preconfigured
!>Variable to check if input filename corresponds to existing preconfiguration
integer :: struct_id = -1
    !
    !retrieve 1st command-line argument
    call getarg(1,cmd_arg,io_stat)
    if(io_stat < 1 ) then
        write(0,*) 'ERROR: The single command-line argument must be the DAT-filename, excluding extension.'
        stop
    end if
    !
    !Set filenames
    fname_dat = trim(cmd_arg)//'.DAT'
    fname_pre = trim(cmd_arg)//'.PRE'
    fname_f90 = trim(cmd_arg)//'.f90'
    fname_verif_f90 = trim(cmd_arg)//'_verif.f90'
    !
    !Set code and struct_id, the case-default should be executed when the input 
    !does not correspond to an existing preconfiguration.
    select case (trim(cmd_arg))
    case ('FCC','fcc','Fcc')
        code = 'fcc12       '
        struct_id = DM_fcc12
    case ('BCC','bcc','Bcc')
        code = 'bcc24       '
        struct_id = DM_bcc24
    case ('BCC2','bcc2','Bcc2')
        code = 'bcc48       '
        struct_id = DM_bcc48
    case default
        code = trim(cmd_arg)
        struct_id = -1
    end select
    !
    !Loading DM_data from DAT-file
    call DeformationMechanismData_init(DMdata,fname_dat,DM_format_dat,info)
    if (info /= criSuccess) then
        write(0,*) 'The call to DeformationMechanismData_init returned error code: info=', info
        write(0,*) 'Passed path for DAT-file: ', fname_dat
        stop
    end if
    !
    !Outputting DM_data in PRE-file format.
    open(unit=ipre, file=fname_pre, iostat=io_stat, iomsg=errormessage, status='replace')
    if (io_stat /= 0) then
        write(0,*) 'Opening the PRE-file returned error code: io_stat=', io_stat
        write(0,*) errormessage
        write(0,*) 'Passed path for PRE-file: ', fname_pre
        stop
    end if
    call DeformationMechanismData_writePre(DMdata, ipre, info)
    if (info /= criSuccess) then
        write(0,*) 'The call to DeformationMechanismData_writePre returned error code: info=', info
        stop
    end if
    close(ipre)
    !
    !Outputting DM_data in f90-file format.
    !Note: All components of DM_data are outputted.
    open(unit=if90, file=fname_f90, iostat=io_stat, iomsg=errormessage, status='replace')
    if (io_stat /= 0) then
        write(0,*) 'Opening the f90-file returned error code: io_stat=', io_stat
        write(0,*) errormessage
        write(0,*) 'Passed path for f90-file: ', fname_f90
        stop
    end if
    call DeformationMechanismData_writef90(DMdata, if90, info, trim(fname_dat), trim(code))
    if (info /= criSuccess) then
        write(0,*) 'The call to DeformationMechanismData_writef90 returned error code: info', info
        stop
    end if
    close(if90)
    !
    !If a preconfigured DMdata corresponding to the inputted pre-file exists,
    !initialize from preconfiguration, and output in f90-format for verification purposes.
    if (struct_id > 0) then
        call DeformationMechanismData_init(DMdata_preconfigured,struct_id,info)
        open(unit=iverif_f90, file=fname_verif_f90, iostat=io_stat, iomsg=errormessage, status='replace')
        if (io_stat /= 0) then
            write(0,*) 'Opening the verification f90-file returned error code: io_stat=', io_stat
            write(0,*) errormessage
            write(0,*) 'Passed path for f90-file: ', fname_verif_f90
            stop
        end if
        call DeformationMechanismData_writef90(DMdata_preconfigured, iverif_f90, info, trim(fname_dat), trim(code))
        if (info /= criSuccess) then
            write(0,*) 'The call to DeformationMechanismData_writef90 returned error code: info', info
            stop
        end if
        close(iverif_f90)
    end if
    
    !
    stop
    !
    contains

    
    !>Output of inputted DeformationMechanismData object to file, in ´PRE´-format.
    !>Note: not all components of the object 'this' are outputted.
    subroutine DeformationMechanismData_writePre(this, inunit, info)
    implicit none
    type(DeformationMechanismData),intent(in)   :: this
    integer,intent(in)                          :: inunit
    integer,intent(out)                         :: info
    !
    integer :: i, counter = 0
    !
        write(inunit,fmt=100,err=900) this%description
        write(inunit,fmt=101,err=900) counter, &
                                      this%n_slip_systems, &
                                      this%n_twinning_systems, &
                                      this%set0
        counter = counter+1
        ! write A1 and B1
        do i = 1,this%n_systems
            write(inunit,fmt=102,err=900) counter,this%A1(:,i), this%B1(:,i)
            counter = counter+1
        enddo
        ! write B
        do i=1,dm_dev_dims
            write(inunit,fmt=103,err=900)  counter, this%B(i,:) !transpose(B) is written
            counter = counter+1
        enddo
        ! write data for twinning systems
        do i=1, this%n_twinning_systems
            write(inunit,fmt=104,err=900)  counter, this%B2(:,i), this%G(i)
            counter = counter+1
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
    
    
    !>Output of inputted DeformationMechanismData object to file, in a format suitable 
    !>for preconfigured (hard-coded) parametric data.
    !>Note: All components of inputted object are supposed to be outputted, except 
    !>component n_systems (which is initialized as n_slip_systems+n_twinning_systems).
    !>Currently, components associated with twinning (B2, G) are not outputted.
    subroutine DeformationMechanismData_writef90(this, inunit, info, fname_in, code_in)
    implicit none
    type(DeformationMechanismData),intent(in)   :: this
    !>IO unit number
    integer,intent(in)                          :: inunit
    integer,intent(out)                         :: info
    !>name of pre-file
    character(len=*),optional                   :: fname_in
    !>identifyer for preconfigured dataset
    character(len=*),optional                   :: code_in
    !
    integer :: i, l
    !>format specifyer to output row/column of array
    integer :: fmt_arr = 0
    character(:), allocatable :: fname, code
    !>difference of %unitcell to unity matrix
    double precision,dimension(DM_dir_dims,DM_dir_dims) :: dif 
        !
        info = criError
        !
        !Set fname
        if(.NOT.present(fname_in)) then
            allocate(character(5)::fname)
            fname = '#####'
        else
            allocate(character(len(trim(fname_in)))::fname)
            fname = fname_in
        end if
        !
        !Set code
        if(.NOT.present(code_in)) then
            allocate(character(3)::code)
            code = '###'
        else
            allocate(character(len(trim(code_in)))::code)
            code = code_in
        end if
        !
        !header
        write(inunit,fmt=201,err=900)
        write(inunit,fmt=202,err=900)        
        write(inunit,fmt=203,err=900)        
        write(inunit,fmt=204,err=900) fname
        write(inunit,fmt=201,err=900)
        !
        !description
        write(inunit,fmt=205,err=900) code
        write(inunit,fmt=206,err=900) trim(this%description)
        !
        !unitcell
        dif = this%unitcell
        do i =1,DM_dir_dims
            dif(i,i) = dif(i,i) - 1.D0
        end do
        if (norm2(dif)<1.D-6) then
            write(inunit,fmt=301,err=900) code
        else
            write(inunit,fmt=302,err=900) code
            write(inunit,fmt=303,err=900)
            assign 304 to fmt_arr
            do l=1,DM_dir_dims
                if(l==DM_dir_dims) assign 3049 to fmt_arr
                write(inunit,fmt=fmt_arr,err=900) (this%unitcell(i,l),i=1,DM_dir_dims)
            end do
            write(inunit,fmt=305,err=900)
        end if
        !
        !n_slip_systems, n_twinning_systems
        write(inunit,fmt=207,err=900) code, this%n_slip_systems, code, this%n_twinning_systems
        !
        !plane_Miller
        write(inunit,fmt=312,err=900) code, code
        assign 311 to fmt_arr
        do l=1,this%n_systems
            if(l==this%n_systems) assign 3119 to fmt_arr
            write(inunit,fmt=fmt_arr,err=900) (this%plane_Miller(l)%index(i),i=1,DM_dir_dims), l
        end do
        !
        !direction_Miller
        write(inunit,fmt=322,err=900) code, code
        assign 311 to fmt_arr
        do l=1,this%n_systems
            if(l==this%n_systems) assign 3119 to fmt_arr
            write(inunit,fmt=fmt_arr,err=900) (this%direction_Miller(l)%index(i),i=1,DM_dir_dims), l
        end do
        !
        !plane_vector
        write(inunit,fmt=332,err=900) code, code
        write(inunit,fmt=214,err=900)
        assign 215 to fmt_arr
        do l=1,this%n_systems
            if(l==this%n_systems) assign 2159 to fmt_arr
            write(inunit,fmt=fmt_arr,err=900) (this%plane_vector(i,l),i=1,DM_dir_dims), l
        end do
        write(inunit,fmt=216,err=900) code
        !
        !direction_vector
        write(inunit,fmt=342,err=900) code, code
        write(inunit,fmt=214,err=900)
        assign 215 to fmt_arr
        do l=1,this%n_systems
            if(l==this%n_systems) assign 2159 to fmt_arr
            write(inunit,fmt=fmt_arr,err=900) (this%direction_vector(i,l),i=1,DM_dir_dims), l
        end do
        write(inunit,fmt=216,err=900) code
        !
        !set0
        write(inunit,fmt=208,err=900) code, (this%set0(i),i=1,DM_dev_dims)
        !
        !A1
        write(inunit,fmt=209,err=900) code, code
        write(inunit,fmt=210,err=900)
        assign 211 to fmt_arr
        do l=1,this%n_systems
            if(l==this%n_systems) assign 2119 to fmt_arr
            write(inunit,fmt=fmt_arr,err=900) (this%A1(i,l),i=1,DM_dev_dims), l
        end do
        write(inunit,fmt=212,err=900) code
        !
        !B1
        write(inunit,fmt=213,err=900) code, code
        write(inunit,fmt=214,err=900)
        assign 215 to fmt_arr
        do l=1,this%n_systems
            if(l==this%n_systems) assign 2159 to fmt_arr
            write(inunit,fmt=fmt_arr,err=900) (this%B1(i,l),i=1,DM_dir_dims), l
        end do
        write(inunit,fmt=216,err=900) code
        !
        !B
        write(inunit,fmt=220,err=900) code
        write(inunit,fmt=221,err=900)     
        assign 222 to fmt_arr
        do l=1,DM_dev_dims
            if(l==DM_dev_dims) assign 2229 to fmt_arr
            write(inunit,fmt=fmt_arr,err=900) (this%B(i,l),i=1,DM_dev_dims) !(l,i) would result in transpose(B)
        end do
        write(inunit,fmt=223,err=900)
        !
        !Twinning-components (B2, G) are not written out..
        if(this%n_twinning_systems > 0) then
            write(inunit,401)
        end if    
        !
        info = criSuccess
        return
        !
900     info = criErr_IORead
        return
    !format: header
201 format(4X,'!==========================================================================')
202 format(4X,'! Definition for #####')
203 format(4X,'!')
204 format(4X,'! Data generated by pretayplus from ',A)
    !format: description
205 format(4X,'character(len=DM_title_length),parameter :: ',A,'_description = &')
206 format(4X,12X,'''',A,'''',/)
    !format: n_slip_systems, n_twinning_systems
207 format(4X,'integer,parameter :: ',A,'_n_slip_systems =',I3,', ',A,'_n_twinning_systems =',I3,/)
    !format: set0
208 format(4X,'integer,dimension(DM_dev_dims),parameter :: ',A,'_set0 = [',4(I3,','),(I3,']'),/)
    !format: A1
209 format(4X,'double precision,dimension(DM_dev_dims,DM_',A,'_nsystems),parameter :: ',A,'_A1 = &')
210 format(4X,4X,'reshape([double precision :: &')
211 format(4X,12X,5(D32.24,','),' & !system:',I3)
2119 format(4X,12X,4(D32.24,','),D32.24,'] & !system:',I3)
212 format(4X,12X,', shape=[DM_dev_dims, DM_',A,'_nsystems])',/)
    !format: B1
213 format(4X,'double precision,dimension(DM_dir_dims,DM_',A,'_nsystems),parameter :: ',A,'_B1 = &')
    !format: plane_vector; direction_vector; B1    
214 format(4X,4X,'reshape([double precision :: &')
215 format(4X,12X,3(D32.24,','),' & !system:',I3)
2159 format(4X,12X,2(D32.24,','),D32.24,'] & !system:',I3)
216 format(4X,12X,', shape=[DM_dir_dims, DM_',A,'_nsystems])',/)
    !format: B
220 format(4X,'double precision,dimension(DM_dev_dims,DM_dev_dims),parameter :: ',A,'_B = &')
221 format(4X,'    reshape([double precision :: &')
222 format(4X,12X,5(D32.24,','),' &')
2229 format(4X,12X,4(D32.24,','),D32.24,'] &')    
223 format(4X,12X,', shape=[DM_dev_dims, DM_dev_dims])',/)
    !format: unitcell
301 format(4X,'double precision,dimension(DM_dir_dims,DM_dir_dims),parameter :: ',A,'_unitcell = unit_sr_Matrix',/)
302 format(4X,'double precision,dimension(DM_dir_dims,DM_dir_dims),parameter :: ',A,'_unitcell = &')
303 format(4X,'    reshape([double precision :: &')
304 format(4X,12X,3(D32.24,','),' &')
3049 format(4X,12X,2(D32.24,','),D32.24,'] &')    
305  format(4X,12X,', shape=[DM_dir_dims, DM_dir_dims])',/)
    !format: plane_Miller
312 format(4X,'type(MillerIndices),dimension(DM_',A,'_nsystems),parameter :: ',A,'_plane_Miller = [&')
    !format: direction_Miller
322 format(4X,'type(MillerIndices),dimension(DM_',A,'_nsystems),parameter :: ',A,'_direction_Miller = [&')
    !format: plane_Miller; direction_Miller
311 format(4X,12X,'MillerIndices([',I3,',',I3,',',I3,']), & !system:',I3)
3119 format(4X,12X,'MillerIndices([',I3,',',I3,',',I3,'])]   !system:',I3,/)
    !format: plane_vector
332 format(4X,'double precision,dimension(DM_dir_dims,DM_',A,'_nsystems),parameter :: ',A,'_plane_vector = &')
    !format: direction_vector
342 format(4X,'double precision,dimension(DM_dir_dims,DM_',A,'_nsystems),parameter :: ',A,'_direction_vector = &')
    !
401 format('NOTE: Writing out the components associated to twinning is not implemented - sorry.')

    end subroutine
    
    
end program