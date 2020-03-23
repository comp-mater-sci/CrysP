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

	subroutine combineCubs(inputMicros,inputWeights,mrgtype,outputMicro,info)
	implicit none
		type(microsDesc),dimension(:),intent(in)   :: inputMicros
		double precision,dimension(:),intent(in)   :: inputWeights
		character(len=*),intent(in)                :: mrgtype
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
		
		if (mrgtype == 'average') then
		
			ngrains=inputMicros(1)%ngrains
			outputmicro%ngrains = ngrains
			write(*,'(A,1X,I6,1X,A)') 'Merging datasets, the output will contain',ngrains,'grains'
			! Prepare data structure
			allocate(outputMicro%grains(ngrains))
			
			outputMicro%GAXES = 0.d0
			outputMicro%GEULR = 0.d0
			outputMicro%FALG = 0.d0
			do j=1,ngrains
				outputMicro%grains(j)%GEW = 0.d0
				outputMicro%grains(j)%PHI1 = 0.d0
				outputMicro%grains(j)%PHI = 0.d0
				outputMicro%grains(j)%PHI2 = 0.d0
				outputMicro%grains(j)%GAMMA = 0.d0
				outputMicro%grains(j)%GAXES = 0.d0
				outputMicro%grains(j)%GEULR = 0.d0
				outputMicro%grains(j)%F = 0.d0
			enddo

			! Transfer other componenst
			do i=1,nmicros
				if (inputMicros(i)%NS < outputMicro%NS) outputMicro%NS = inputMicros(i)%NS
				outputMicro%GAXES = outputMicro%GAXES + inputMicros(i)%GAXES * vWeights(i)
				outputMicro%GEULR = outputMicro%GEULR + inputMicros(i)%GEULR * vWeights(i)
				outputMicro%FALG = outputMicro%FALG + inputMicros(i)%FALG * vWeights(i)

				do j=1,ngrains
					outputMicro%grains(j)%GEW = outputMicro%grains(j)%GEW + inputMicros(i)%grains(j)%GEW * vWeights(i)
					outputMicro%grains(j)%PHI1 = outputMicro%grains(j)%PHI1 + inputMicros(i)%grains(j)%PHI1 * vWeights(i)
					outputMicro%grains(j)%PHI = outputMicro%grains(j)%PHI + inputMicros(i)%grains(j)%PHI * vWeights(i)
					outputMicro%grains(j)%PHI2 = outputMicro%grains(j)%PHI2 + inputMicros(i)%grains(j)%PHI2 * vWeights(i)
					outputMicro%grains(j)%GAMMA = outputMicro%grains(j)%GAMMA + inputMicros(i)%grains(j)%GAMMA * vWeights(i)
					outputMicro%grains(j)%GAXES = outputMicro%grains(j)%GAXES + inputMicros(i)%grains(j)%GAXES * vWeights(i)
					outputMicro%grains(j)%GEULR = outputMicro%grains(j)%GEULR + inputMicros(i)%grains(j)%GEULR * vWeights(i)
					outputMicro%grains(j)%F = outputMicro%grains(j)%F + inputMicros(i)%grains(j)%F * vWeights(i)
				enddo
			enddo

		else
		
			! Determine total number of grains
			totGrains = 0
			do i=1,nmicros
				  totGrains = totGrains + inputMicros(i)%ngrains
			enddo
			!
			outputmicro%ngrains = min(maxCurGrains,totGrains)
			write(*,'(A,1X,I6,1X,A)') 'Merging datasets, the output will contain',outputmicro%ngrains,'grains'
			! Prepare data structure
			allocate(outputMicro%grains(outputmicro%ngrains))
			if (totGrains < maxCurGrains) then
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

		endif
		
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
	logical                       :: isConfigFile
	character(len=ctitlelen)      :: title
	character(len=3)              :: outfmt
	character(len=7)              :: mrgtype

	integer :: nInputs
	integer :: nOutGrains

	type(microsDesc),dimension(:),allocatable   :: inputMicros
	type(microsDesc)                            :: outputMicro
	double precision,dimension(:),allocatable   :: inputWeights
	integer                                     :: info
	! Check number of parameters, at least 3 are required 
	!
	argc = command_argument_count()
	! Valid command line: 1 argument or at least 5 arguments
	if ( (argc == 0) .or. (argc /= 1 .and. argc < 5) ) then
		  write(*,*) 'arguments: input1.cub input2.cub [...] output output-format(cub or cur) merge-type(append or average)'
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
		  nInputs = argc-3
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
		  call get_command_argument(argc-2,fnamout,status=iuerr)
		  call get_command_argument(argc-1,outfmt,status=iuerr)
		  call get_command_argument(argc,mrgtype,status=iuerr)
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
	call combineCubs(inputMicros,inputWeights,mrgtype,outputMicro,info)
	!

	if (info == 0) then
		if (outfmt=='cub') then
			! open CUB file
			open (unit=noutunit,file=TRIM(fnamout), status='unknown',form='UNFORMATTED',access='STREAM',iostat=iuerr)
			if (iuerr /= 0) then
				write(*,*) 'Cannot open output file'
				call exit(11)
			endif
			call writeCub(noutunit,outputMicro,info)     
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
