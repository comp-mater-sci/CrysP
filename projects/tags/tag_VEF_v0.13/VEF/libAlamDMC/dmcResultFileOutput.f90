!
! $Id$
!
!>    \author Jerzy Gawad                                                
!>    Email:  Jerzy.Gawad@cs.kuleuven.be
!>
!>    Organization: Katholieke Universiteit Leuven
!>    Organization unit: Dept.Comp.Sci., TWR Group
!>                                                             
!>    \date Date of the initial release: 2015-12-18
!>    $Revision$
!>    $Date$
!>
!>    History of modifications: (see svn log)
!

#include "criMacros.fpp"

!> Common set of procedures for producing plain text column-based output of numerical data.
module dmcResultFileOutput
use criErrcodes
use criAlgorithm
implicit none

    integer,parameter,private :: fmt_string_length = 128

    !> \fixme  max_int_digits is platform dependent. To be replaced by a constant 
    !> expression that computes how many decimal digits are needed to store 2^digits(1)
    integer,parameter,private ::  max_int_digits = 10
    
contains

!> \todo Move TADJUSTL macro to some more suitable place (FCRI?)
#define TADJUSTL(str) trim(adjustl(str))
    

    
    !> Driver function for writing numerical data
    integer function writeResultFile(iounit, data, column_names, column_widths, &
                                     use_column_numbers, data_formats) result(info)
                                    !, colnames_formats, data_formats, )
    implicit none
    integer,intent(in)                              :: iounit
    double precision,dimension(:,:),intent(in)      :: data !< Shape: [ncolumns x nrows]
    character(len=*),dimension(:),intent(in)        :: column_names
    !> Either: shape is [1] (single-element array) that contains width of every column, or
    !> column widths, shape is [ncolumns]
    integer,dimension(:),intent(in)                 :: column_widths

    logical,optional,intent(in)                     :: use_column_numbers
    character(len=*),dimension(:),optional          :: data_formats
    !
    character(len=fmt_string_length) :: fmt_string ! TODO: make it allocatable
    !
        info = criSuccess
        ! Write column numbers
        if (optionalDefault(use_column_numbers, .true.)) then
            info = writeColumnNumbers(iounit, size(column_names), column_widths)
            if (info /= criSuccess) return
        endif
        ! Write column labels
        info = writeColumnNames(iounit, column_names, column_widths)
        if (info /= criSuccess) return
        ! 
        ! Write the data
        if (present(data_formats)) then
            info = writeData(iounit, data, column_widths, data_formats)
        else
            ! \todo replace with data_formats (either generated or provided)
            ! \fixme broken feature: column_widths are ignored!
            fmt_string = '(' // 'E' // trim(tostring(column_widths(1),max_int_digits)) // &
                         '.' // trim(tostring(column_widths(1)-5,max_int_digits)) // ',1X)'
            info = writeData(iounit, data, column_widths, [fmt_string])
        endif
    !
    end function


    !> Write centered column numbers spaced according to column_widths
    integer function  writeColumnNumbers(iounit, ncolumns, column_widths) result(info)
    implicit none
    integer,intent(in)                      :: iounit
    integer,intent(in)                      :: ncolumns
    integer,dimension(:),intent(in)         :: column_widths
    !
    character(len=fmt_string_length) :: fmt_string
    integer :: i, ierr, column_width
    !
        if (size(column_widths) == 1) then
            column_width = column_widths(1)
            fmt_string = '("#",1X,' // trim(tostring(ncolumns,max_int_digits)) // &
                         '(A'// TADJUSTL(tostring(column_width,max_int_digits)) // ',1X))'
            write(iounit,fmt=fmt_string,iostat=ierr) (centered(i,column_width), i = 1, ncolumns)
            CHOOSE(info, ierr == 0, criSuccess, criErr_IOWrite)
            !
        elseif(size(column_widths) == ncolumns) then
            info = criError ! not yet implemented
        else
            info = criError
        endif
    !
    end function
    
    
    integer function  writeColumnNames(iounit, column_names, column_widths) result(info)
    implicit none
    integer,intent(in)                      :: iounit
    character(len=*),dimension(:)           :: column_names
    integer,dimension(:),intent(in)         :: column_widths
    !
    integer :: ncolumns
    character(len=fmt_string_length) :: fmt_string

    integer :: i, ierr
    !
        ncolumns = size(column_names)
        if (size(column_widths) == 1) then
            ! Format: two leading spaces, followed by columns
            fmt_string = '(2X,'// trim(tostring(ncolumns,10)) // '(A,1X))'
            write(iounit,fmt=fmt_string,iostat=ierr) (centered(column_names(i)), i = 1, ncolumns)
            CHOOSE(info, ierr==0, criSuccess, criErr_IOWrite)
            !
        elseif(size(column_widths) == ncolumns) then
            
            info = criError ! Not yet implemented
        else
            info = criErr_BadArgs
        endif
    !
    end function
    
    !> Write out standard header: two lines: #1: column numbers, #2 column names
    integer function writeStandardHeader(iounit, column_names, column_widths) result(info)
    implicit none
    integer,intent(in)                      :: iounit !< Output IO unit
    character(len=*),dimension(:)           :: column_names ! Names of columns
    integer,dimension(:),intent(in)         :: column_widths ! Widths of columns
    !
        info = writeColumnNumbers(iounit, size(column_names), column_widths)
        if (info /= criSuccess) return
        info = writeColumnNames(iounit, column_names, column_widths)
    !
    end function


    integer function writeData(iounit, data, column_widths, data_formats) result(info)
    implicit none
    integer,intent(in)                              :: iounit
    double precision,dimension(:,:),intent(in)      :: data !< Shape: [ncolumns x nrows]
    !> Specification of column widths
    !>
    !> Shape is either:
    !> - [1] (single-element array): the width of each column is set to column_widths(1)
    !> - [ncolumns]: the width of each i-th column will be set according to column_widths(i)
    integer,dimension(:),intent(in)                 :: column_widths
    !> Specification of data format used to format the columns
    !>
    !> Shape is either:
    !> - [1] (single-element array): data_formats(1) will be used to format each column. Note
    !>   that the format for a single element must be provided.
    !> - [ncolumns]: width of each column will be treated accodingly
    !> column widths, shape is [ncolumns]
    !> \todo Another choice could be to use one statement for all columns.
    character(len=*),dimension(:),intent(in)        :: data_formats
    !
    integer :: i, ierr, ncolumns, nrows
    character(len=fmt_string_length) :: fmt_string ! TODO: make it allocatable
    !
        ncolumns = size(data, dim=1)
        nrows = size(data, dim=2)
        !
        if (size(column_widths) == 1) then
            fmt_string = '(' // TADJUSTL(tostring(ncolumns,max_int_digits)) // &
                                TADJUSTL(data_formats(1)) // ')'
            do i = 1, nrows
                write(iounit,fmt=fmt_string,iostat=ierr) data(:,i)
                if (ierr /= 0) exit
            enddo
            CHOOSE(info, ierr==0, criSuccess, criErr_IOWrite)
            !
        elseif(size(column_widths) == ncolumns) then
            
            info = criError ! Not yet implemented
        else
            info = criErr_BadArgs
        endif

       
    !
    end function
    
    
    !function makeFormatString_double(specifier,width,n_repeat,sep)

    
    !end function
#undef TADJUSTL
    
end module
