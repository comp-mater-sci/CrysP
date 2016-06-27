!****************************************************************************
!
!  PROGRAM: HardeningExponent
!
!  PURPOSE: to calculate hardening exponent according to norm ISO10275
!
!****************************************************************************

program HardeningExponent

implicit none
character (LEN=40)    :: InputfileName, ResultfileName, buffer
character (LEN=60)    :: datafilename, dumchar
integer				  :: i, j, n_data, n_files, status, dumint
real                  :: dumfloat1, dumfloat2, hardexp
integer, parameter    :: unit_in=11, unit_data=12, unit_out=13
real, dimension(:), allocatable     :: x, y


call getarg(1,InputfileName,status) 
call getarg(2,ResultfileName,status)


open(unit_in,FILE=trim(InputfileName),ERR=500,STATUS='OLD',ACCESS='SEQUENTIAL',ACTION='READ')
open(unit_out,FILE=trim(ResultfileName),ERR=500,ACCESS='SEQUENTIAL',ACTION='WRITE')

!! 1st line of inputfile: n_data = number of rows of data
read(UNIT=unit_in,FMT=9997,ERR=500,ADVANCE='YES') n_data
!	
if (n_data <= 2) goto 500 !< n_data should be at least 3
!	
allocate(x(1:n_data))
allocate(y(1:n_data))

!! 2nd line of inputfile: n_data = number of rows of data
read(UNIT=unit_in,FMT=9997,ERR=500,ADVANCE='YES') n_files

!!
do i=1,n_files
    !read data from datafilename
    read(UNIT=unit_in,FMT=9998,ERR=500,ADVANCE='YES') datafilename
    open(unit_data,FILE=trim(datafilename),ERR=501,STATUS='OLD',ACCESS='SEQUENTIAL',ACTION='READ')
    read(UNIT=unit_data,FMT=9998,ERR=502,ADVANCE='YES') dumchar !< skip 1st line = comment line
    read(UNIT=unit_data,FMT=9998,ERR=502,ADVANCE='YES') dumchar !< skip 2nd line = comment line
    read(UNIT=unit_data,FMT=9998,ERR=502,ADVANCE='YES') dumchar !< skip 3rd line = 1st dataline with eps=0 to be skipped [ln(0) = -Inf]
    do j=1,n_data 
	    read(UNIT=unit_data,FMT=9999,ERR=502,ADVANCE='YES') dumint, dumfloat1, dumfloat2, x(j), y(j)
    end do
    close(unit_data)
    !
    !do calculations
    x=log(x)
    y=log(abs(y)) ! ensure positive y-values, even for ND-compression cases
    hardexp = (n_data*sum(x*y)-sum(x)*sum(y)) / (n_data*sum(x*x)-sum(x)*sum(x))
    !
    !output the result
    write(UNIT=unit_out,FMT=9996,ERR=500,ADVANCE='YES') datafilename, hardexp 
end do

close(unit_in)
close(unit_out)

goto 999 !end program

500 print *,'Read/write error in CalcHardRate!' 
501 print *,'Error opening a datafile'
502 print *,'Error reading from datafile'
    
9996  format (A60,X,f16.8)
9997  format (i5)
9998  format (A60)      
9999  format (i10,4(f19.2))

999 end program 

