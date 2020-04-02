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

	subroutine combineCubs(inputMicros,inputWeights,outfmt,outputMicro,info)
	implicit none
		type(microsDesc),dimension(:),intent(in)   :: inputMicros
		double precision,dimension(:),intent(in)   :: inputWeights
		character(len=3),intent(in)                :: outfmt
		type(microsDesc),intent(out)               :: outputMicro
		integer,intent(out)                        :: info
		!
		integer :: i,j,k,ng,totGrains,nmicros,ngrains
		integer,dimension(:),allocatable  :: vProbes
		double precision,dimension(:),allocatable :: vWeights
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
		totGrains = 0
		do i=1,nmicros
			  totGrains = totGrains + inputMicros(i)%ngrains
		enddo
		!
		if (outfmt=='cub') then
		   outputmicro%ngrains = totGrains
		else
		   outputmicro%ngrains = min(maxCurGrains,totGrains)
		endif
		write(*,'(A,1X,I6,1X,A)') 'Merging datasets, the output will contain',outputmicro%ngrains,'grains'
		! Prepare data structure
		allocate(outputMicro%grains(outputmicro%ngrains))
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
		! Transfer other componenst
		do i=1,nmicros
			  outputMicro%GAXES = outputMicro%GAXES + inputMicros(i)%GAXES * vWeights(i)
			  outputMicro%GEULR = outputMicro%GEULR + inputMicros(i)%GEULR * vWeights(i)
			  outputMicro%FALG = outputMicro%FALG + inputMicros(i)%FALG * vWeights(i)
		enddo
		
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

end module

program mergeCubsMain
use cubAccess
use mergeCubs
implicit none

	integer,parameter             :: noutunit=111,ncnfunit=112  ! Unit numbers      
	integer                       :: iuerr  ! Error code for I/O operations
	character(LEN=pathlength),dimension(:),allocatable     :: fnamcubs
	character(LEN=pathlength)     :: fnamout,fnamcnf
	integer                       :: argc,i
	logical                       :: isConfigFile, ifreset
	character(len=ctitlelen)      :: title
	character(len=3)              :: outfmt
	character(len=5)              :: reset
	

	integer :: nInputs
	integer :: nOutGrains

	type(microsDesc),dimension(:),allocatable   :: inputMicros
	type(microsDesc)                            :: outputMicro
	double precision,dimension(:),allocatable   :: inputWeights
	integer                                     :: info
	! Check number of parameters, at least 3 are required 
	!
	ifreset = .false.
	argc = command_argument_count()
	! Valid command line: 1 argument or at least 4 arguments
	if ( (argc == 0) .or. (argc /= 1 .and. argc < 4) ) then
		  write(*,*) 'arguments: input1.cub input2.cub [...] output cub [reset]'
		  write(*,*) 'or:'
		  write(*,*) 'arguments: input1.cub input2.cub [...] output cur'
		  write(*,*) 'or:'
		  write(*,*) 'arguments:  config_file'
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
		  call get_command_argument(argc,outfmt,status=iuerr)
		  if (outfmt=='cub' .or. outfmt=='cur') then
		     call get_command_argument(argc-1,fnamout,status=iuerr)
		     nInputs = argc-2
	      else
		     call get_command_argument(argc,reset,status=iuerr)
			 call get_command_argument(argc-1,outfmt,status=iuerr)
			 if (reset=='reset' .and. argc>=5) then
			    if (outfmt=='cub' .or. outfmt=='cur') then
			       call get_command_argument(argc-2,fnamout,status=iuerr)
			       nInputs = argc-3
				   if (outfmt=='cub') then
				      ifreset=.true.
				   else
				      write(*,*) 'The reset option is valid only for cub outputs; merging to cur file without resetting any value'
				   endif
				else
				   write(*,*) 'Unknown output format ', outfmt,'; only cub and cur formats are supported'
				   call exit(10)
				endif
			 else
			    write(*,*) 'arguments: input1.cub input2.cub [...] output cub [reset]'
		        write(*,*) 'or:'
		        write(*,*) 'arguments: input1.cub input2.cub [...] output cur'
		        write(*,*) 'or:'
		        write(*,*) 'arguments:  config_file'
		        call exit(10)
			 endif
		  endif
	endif
	!
	allocate(inputMicros(nInputs),inputWeights(nInputs),fnamcubs(nInputs))
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
				call get_command_argument(i,fnamcubs(i),status=iuerr)
				if (iuerr /= 0) exit
		  end do
		  inputWeights = 1.D0
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
	call combineCubs(inputMicros,inputWeights,outfmt,outputMicro,info)
	!

	if (info == 0) then
		if (outfmt=='cub') then
			! open CUB file
			open (unit=noutunit,file=TRIM(fnamout), status='REPLACE',form='UNFORMATTED',iostat=iuerr)
			if (iuerr /= 0) then
				write(*,*) 'Cannot open output file'
				call exit(11)
			endif
			call writeCub(noutunit,outputMicro,ifreset,info)     
			if (info /= 0) then
				write(*,*) 'Error writing CUB file ',trim(fnamout)
				call exit(11)
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
