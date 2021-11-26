!
! $Id$
!
!>    \author Jerzy Gawad
!>    Email:  Jerzy.Gawad@cs.kuleuven.be
!>
!>    Organization: Katholieke Universiteit Leuven
!>    Organization unit: Dept.Comp.Sci., TWR Group
!>
!>    \date Date of first release: 2011-06-01
!>    $Revision$
!>    $Date$
!>
!>    History of modifications: (see svn log)
!
!
!>    \file mergeCubs.f90 
!>    
!

!
! $Id$
!

module mergeCubs
use cubAccess
implicit none

	integer,parameter             :: pathlength=512
	integer,parameter             :: ncubunit=110


	integer,parameter :: maxCurGrains = 11200

	contains
      

	subroutine loadCubs(inputFnames,inputMicros,info)
	implicit none
		character(LEN=pathlength),dimension(:),intent(in)     :: inputFnames
		type(microsDesc),dimension(:),intent(inout)   :: inputMicros
		integer,intent(out)                             :: info

		integer :: i, iuerr, nInputs

		nInputs = size(inputMicros)
		! Process all input cub files
		info = 1
		do i=1,nInputs
			  ! Open & process the CUB file 
			  open (unit=ncubunit,file=trim(inputFnames(i)),status='old',form='UNFORMATTED',iostat=iuerr)
			  if (iuerr /= 0) then
					write(*,'(A,1X,A)') 'Cannot open CUB file',trim(inputFnames(i))
					exit
			  endif
			  call readCub(ncubunit,inputMicros(i),iuerr)        
			  close(ncubunit)
			  if(iuerr /= 0) then
					write(*,'(A,1X,A)') 'Error reading CUB file',trim(inputFnames(i))
					exit
			  endif
			  write(*,300) i, trim(inputFnames(i)), inputMicros(i)%ngrains
		enddo
		if (iuerr == 0) info = 0
		!
		  300 format('Dataset ',I3,': ',A,1X,I6,1X,'grains')
	end subroutine

	subroutine combineCubs(inputMicros,inputWeights,outfmt,mrgtype,outputMicro,rep,info)
	use ifport
	use altayAlgorithms
	use altayMacroKinematic
	
	implicit none
		type(microsDesc),dimension(:),intent(in)   :: inputMicros
		double precision,dimension(:),intent(in)   :: inputWeights
		character(len=3),intent(in)                :: outfmt
		character(len=6),intent(in)                :: mrgtype
		type(microsDesc),intent(out)               :: outputMicro
		integer,allocatable,intent(inout)          :: rep(:)
		integer,intent(out)                        :: info
		!
		integer :: i,j,k,ng,totGrains,nmicros,ngrains,nselgrains, ipiv(3)
		integer,dimension(:),allocatable  :: vProbes, Iselgrains
		double precision,dimension(:),allocatable :: vWeights, Dselgrains
		integer,dimension(:),allocatable :: allgrains
		double precision  :: w_int, DefGrad(3,3),DefGrad_inverse(3,3), CIJ(3,3), TG(3,3), work(3)
		
		!
		info = 1
		nmicros = size(inputMicros)
		
		! Normalize weights
		allocate(vWeights(nmicros))
		vWeights = inputWeights / sum(inputWeights)
		
		! TODO: check dimensions
		!
		outputMicro%NS = inputMicros(1)%NS
	
		! Determine total number of grains
		if (mrgtype=='random') then
			totGrains = inputMicros(1)%ngrains
			do i=2,nmicros
				if (inputMicros(i)%ngrains /= totGrains) then
					write(*,*) 'The number of grains in the input files are different'
					write(*,*) 'The output will have ',totGrains,' grains which is equal to that of the first input'
					exit
				endif
			enddo
		else
			totGrains = 0
			do i=1,nmicros
				  totGrains = totGrains + inputMicros(i)%ngrains
			enddo
		endif
		allocate(rep(totGrains))

		!
		if (outfmt=='cub') then
		   outputmicro%ngrains = totGrains
		else
		   outputmicro%ngrains = min(maxCurGrains,totGrains)
		endif
		write(*,'(A,1X,I6,1X,A)') 'Merging datasets, the output will contain',outputmicro%ngrains,'grains'
		! Prepare data structure
		allocate(outputMicro%grains(outputmicro%ngrains))
		if (mrgtype=='append') then
			if (totGrains < maxCurGrains .or. outfmt=='cub') then
				  ! Simple merge of the curfiles
				  j=1
				  do i=1,nmicros
						if (inputMicros(i)%NS < outputMicro%NS) outputMicro%NS = inputMicros(i)%NS
						ng = inputMicros(i)%ngrains
						write(*,300) i, 'transfering',ng
						! Transfer grains
						outputMicro%grains(j:j+ng-1) = inputMicros(i)%grains(1:ng)
						j = j + ng 
				  enddo
			else
				  allocate(vProbes(nmicros))
				  ! 
				  vProbes(:) = int(dble(maxCurGrains) * vWeights(:))
				  ng = sum(vProbes)
				  ! fill up the last slot
				  vProbes(nmicros) = vProbes(nmicros) + (maxCurGrains - ng)
				  ! Select at random 
				  j = 1
				  do i=1,nmicros
						if (inputMicros(i)%NS < outputMicro%NS) outputMicro%NS = inputMicros(i)%NS
						write(*,300) i, 'probing',vProbes(i)
						do k=1,vProbes(i)
							  outputMicro%grains(j) =inputMicros(i)%grains(irandom(1,inputMicros(i)%ngrains))
							  j = j + 1
						enddo
				  enddo
				  deallocate(vProbes)
			endif
		elseif (mrgtype=='random') then
			ngrains = nmicros*totGrains/2
			
			nselgrains = totGrains/2
			allocate(Dselgrains(nselgrains))
			call random_number(Dselgrains)
			
			do i=1,nselgrains
				w_int = 0
				do j=1,nmicros
				    w_int = w_int + vWeights(j)
					if (Dselgrains(i) < w_int) exit					
				enddo
				outputMicro%grains(2*i-1:2*i) = inputMicros(j)%grains(2*i-1:2*i)
				rep(2*i-1:2*i) = j
			enddo
		endif
		! Transfer other components
		outputMicro%FALG = 0.d0
		do i=1,nmicros
			  outputMicro%FALG = outputMicro%FALG + inputMicros(i)%FALG * vWeights(i)
		enddo

		if (norm2(outputMicro%FALG) <= 1.0D0) then
		    call MatrixExponentSmallNorm(outputMicro%FALG,DefGrad,DefGrad_inverse,info)
		else
		    DefGrad_inverse = outputMicro%FALG
			call dgetrf(3,3,DefGrad_inverse,3,ipiv,info)
            call dgetri(3,DefGrad_inverse,3,ipiv,work,3,info)
		endif
		CIJ = unitMatrix
        call UPDATC(CIJ,DefGrad_inverse) 
        call GETANG(CIJ,outputMicro%GAXES,outputMicro%GEULR,TG) !(CIJ,prval,GEULR,TMAT)	
		
		do i=1,outputmicro%ngrains
			outputMicro%grains(i)%F = outputMicro%FALG
			outputMicro%grains(i)%GAXES = outputMicro%GAXES
			outputMicro%grains(i)%GEULR = outputMicro%GEULR
		enddo
        outputMicro%GEULR = rad2deg(outputMicro%GEULR)		
		
		deallocate(vWeights)
		info = 0
		!
		300 format('Dataset ',I3, ', ',A,1X,I5,' grains')
	end subroutine

	integer function irandom(range_min,range_max)
	implicit none
		integer,intent(in)      :: range_min,range_max
		real :: rnd
		call random_number(rnd)
		irandom = range_min + int(dble(rnd)*dble(1 + range_max - range_min))
		if (irandom > range_max) irandom = range_max
	end function
	
	subroutine shuffle(x)
	use ifport
	implicit none
		integer :: x(:)
		integer :: i, n, tmp, k
		double precision :: R

		n = size(x)
		i = n
		do 
			R = rand()
			k = int(R*i)+1
			tmp = x(i)
			x(i) = x(k)
			x(k) = tmp
			i = i - 1
			if (i==1) exit
		enddo
	end subroutine

end module

program mergeCubsMain
use cubAccess
use mergeCubs
implicit none

	integer,parameter             :: noutunit=111,ncnfunit=112, nrepunit=113  ! Unit numbers      
	integer                       :: iuerr  ! Error code for I/O operations
	character(LEN=pathlength),dimension(:),allocatable     :: fnamcubs
	character(LEN=pathlength)     :: fnamout,fnamcnf,buf
	integer                       :: argc,i,offst
	logical                       :: isConfigFile, ifreset, ifreport
	character(len=ctitlelen)      :: title
	character(len=3)              :: outfmt
	character(len=5)              :: reset, tmp
	character(len=6)              :: report
	character(len=6)              :: mrgtype
	

	integer :: nInputs
	integer :: nOutGrains

	type(microsDesc),dimension(:),allocatable   :: inputMicros
	type(microsDesc)                            :: outputMicro
	double precision,dimension(:),allocatable   :: inputWeights
	integer, allocatable :: rep(:)
	integer                                     :: info
	! Check number of parameters, at least 3 are required 
	!
	ifreset = .false.
	ifreport = .false.
	argc = command_argument_count()
	! Valid command line: 1 argument or at least 4 arguments
	if ( (argc == 0) .or. (argc /= 1 .and. argc < 5) ) then
		write(*,*) 'arguments: input1.cub weight1 input2.cub weight2 [...] output cub {append,random} [reset] [report]'
		write(*,*) 'or:'
		write(*,*) 'arguments: input1.cub weight1 input2.cub weight2 [...] output cur {append,random}'
		write(*,*) 'or:'
		write(*,*) 'arguments: config_file'
		  call exit(10)
	endif
	! Determine if the program is config-driven or command line-driven
	isConfigFile = .false. 
	
	if (argc == 1) isConfigFile = .true. 
	!

	if (isConfigFile) then
		 ! Open and process config file: ger nInputs  
		  call get_command_argument(1,fnamcnf,status=iuerr)
		  open (unit=ncnfunit,file=TRIM(fnamcnf), status='unknown',form='FORMATTED',iostat=iuerr)
		  if (iuerr /= 0) then
				write(*,*) 'Cannot open config file ',trim(fnamcnf)
				call exit(1)
		  endif
		  read(ncnfunit,*,iostat=iuerr) nInputs
		  if (iuerr /= 0) then
				write(*,*) 'Unknown format of config file ',trim(fnamcnf)
				call exit(1)
		  endif
	else
		  offst = 0
		  call get_command_argument(argc,report,status=iuerr)
		  if (report=='report') offst = offst+1
		  
		  call get_command_argument(argc-offst,reset,status=iuerr)
	      if (reset=='reset') offst = offst+1
		  
		  call get_command_argument(argc-offst,mrgtype,status=iuerr)		  
		  if (mrgtype/='append' .and. mrgtype/='random') then
		      write(*,*) 'Unknown merge type ', mrgtype,'; only append and random types are supported'
			  call exit(10)
		  endif
		  if (report=='report') then
			  if (mrgtype=='random') then
				  ifreport=.true.
			  else
				  write(*,*) 'The report option is valid only for random merging'
			  endif
		  endif
		  
		  call get_command_argument(argc-offst-1,outfmt,status=iuerr)
		  if (outfmt/='cub' .and. outfmt/='cur') then
		      write(*,*) 'Unknown output format ', outfmt,'; only cub and cur formats are supported'
			  call exit(10)
		  endif
		  if (reset=='reset') then
			  if (outfmt=='cub') then
				  ifreset=.true.
			  else
				  write(*,*) 'The reset option is valid only for cub outputs; merging to cur file without resetting any values'
			  endif
		  endif
		  
		  
		  call get_command_argument(argc-offst-2,fnamout,status=iuerr)
		  nInputs = (argc-offst-3)/2
	      
	endif
	!
	allocate(inputMicros(nInputs),inputWeights(nInputs),fnamcubs(nInputs))
	inputWeights = 1.D0
	!
	if (isConfigFile) then
		  do i=1,nInputs
				read(ncnfunit,fmt='(A,/,F10.5)',iostat=iuerr)  fnamcubs(i), inputWeights(i)
				if (iuerr /= 0) exit
		  enddo
		  if (iuerr /= 0) then
				write(*,*) 'Bad format of config file'
				call exit(1)
		  endif
		  read(ncnfunit,*,iostat=iuerr) nOutGrains
		  read(ncnfunit,fmt='(A)',iostat=iuerr) fnamout
		  if (iuerr /= 0) then
				write(*,*) 'Bad format of config file'
				call exit(1)
		  endif
	else
		  ! Construct array of filenames
		  info = 0
		  do i=1,nInputs
				info = 1
				call get_command_argument(2*i-1,fnamcubs(i),status=iuerr)
				if (iuerr /= 0) exit
				call get_command_argument(2*i,buf,status=iuerr)
				read(buf,*) inputWeights(i)
				if (iuerr /= 0) exit
		  end do
		  
		  nOutGrains = maxCurGrains 
	endif
	!
	if (iuerr /= 0) then
		  write(*,*) 'Initial processing failed.'
		  call exit(1)
	endif

	! Process the cubs
	call loadCubs(fnamcubs,inputMicros,info)
	if (info /= 0) then
		  write(*,*) 'An error has occured during processing'
		  call exit(1)
	endif
	! Merge the data
	call combineCubs(inputMicros,inputWeights,outfmt,mrgtype,outputMicro,rep,info)
	!

	if (info == 0) then
		if (outfmt=='cub') then
			! open CUB file
			open (unit=noutunit,file=TRIM(fnamout), status='REPLACE',form='UNFORMATTED',iostat=iuerr)
			if (iuerr /= 0) then
				write(*,*) 'Cannot open output file'
				call exit(11)
			endif
			call writeCub(noutunit,outputMicro,.false.,ifreset,info)     
			if (info /= 0) then
				write(*,*) 'Error writing CUB file ',trim(fnamout)
				call exit(11)
			endif
			if (ifreport) then
			    do i=len_trim(fnamout),1,-1
					if (fnamout(i:i) == '/' .or. fnamout(i:i) == '\') exit
				enddo
				
				if (i == 1) then
				    open (unit=nrepunit,file='merging_report.txt', status='REPLACE',iostat=iuerr)
				else
				    open (unit=nrepunit,file=fnamout(1:i)//'merging_report.txt', status='REPLACE',iostat=iuerr)
				endif
				write(nrepunit,*) size(rep)
				write(nrepunit,*) 'grain #      original texture #'
				do i=1,size(rep)
				    write(tmp,'(i5)') i
					write(nrepunit,'(1x,A,7x,i2)') adjustl(trim(tmp)),rep(i)
				enddo
				close(nrepunit)
			endif
		else
			! Open CUR file 
			open (unit=noutunit,file=TRIM(fnamout), status='unknown',form='FORMATTED',iostat=iuerr)
			if (iuerr /= 0) then
				write(*,*) 'Cannot open output file'
				call exit(11)
			endif
			! Mangle title
			title =  'Merged CUR' ! TODO: more informative title
			outputMicro%TITLE = title
			call writeCur(noutunit,outputMicro,info)     
			if (info /= 0) then
				write(*,*) 'Error writing CUR file ',trim(fnamout)
				call exit(11)
			endif
		endif
		close(noutunit)
	else
		!
		write(*,*) 'Cannot merge the cub files'
		call exit(11)
	endif
end program
