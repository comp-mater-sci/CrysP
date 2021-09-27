alamDMC/main
	! Create a module of appropriate type:
	select case(cmdline%command_id)
	case(Q_id) ! dmcQRS
		allocate(QRSModule :: the_module) !MB: create object the_module of type QRSModule
	case(UDSA_id) ! dmcUDSA
		allocate(UDSAModule :: the_module)
	case(ASR_id) ! dmcASR
		allocate(ASRModule :: the_module)
	case(YLD_id) ! dmcYld
		allocate(YldModule :: the_module)
	case(EWC_id) ! dmcEWC
		allocate(EWCModule :: the_module)
	case(ADP_id) ! dmcSD
		allocate(ADPModule :: the_module)
	end select
	info=the_module%ReadConfig(cnfunit) ! dmcUDSA::UDSAModule_readConfig(this,cnfunit) result(info) type-bound subroutine defined in dmc<module>.f90
		info=this%StressDrivenEvolutionModule%readConfig(cnfunit)  ! stressDrivenModule::StressDrivenModule_readConfig(this,cnfunit) result(info)
			info = this%BasicModule%readConfig(cnfunit)  ! basicModule::BasicModule_readConfig(this,cnfunit) result(info)
				basicModule::readOutputConfigSection(cnfunit,this%output,info) !MB: top 3 lines after comment header of config file
					if (.not. readValue(cnfunit, cnf%outputPrefix)) return
					if (.not. readValue(cnfunit, cnf%outputRequest)) return
					if (.not. readValue(cnfunit, cnf%verbosity)) return
				basicModule::readAlTayConfigSection(cnfunit,this%altay,info) !MB: read configuration of texture, microstructure and hardening into this%altay (root-level configuration structure)
					if (.not. readValue(cnfunit, cnf%texture%input_fname)) return
					select case(cnf%texture%input_type) ! input_type is deduced from input_fname
					case(TF_SMT,TF_CUB)     ! SMT or CUB
						continue
					case(TF_CUR)       ! CUR file, the only multi-block file now.'
					   if (.not. readValue(cnfunit, cnf%texture%block_id)) return !MB: read block id for CUR format
					end select
					if (.not. readKeyword(cnfunit, model_types, model_id)) return  ! Determine crystal plasticity model type
					if (.not. readKeyword(cnfunit, slipsystem_types, dm_id)) return  ! Determine slip system file
					use_default_microstructure = .true.
					if (.not. readValue(cnfunit, use_default_microstructure)) return  ! Process advanced microstructure characterization
					if (.not. use_default_microstructure) then 
						if (.not. readValue(cnfunit, cnf%micros_fname)) return !MB: read <microstructure>.smt filename
						do i=1,3
							if (.not. readValue(cnfunit, cnf%simul_init%Fmicro(:,i))) return !MB: read deformation gradient
						enddo
					else
						basicModule::incurMicrostructureFile(cnf%micros_fname, info) !MB: load default microstructure
							basicModule::getDataPath('equiaxed.smt', micros_fname, info)
								prefix = getVEFDataDir()
								path = pathjoin(prefix, fname)
					endif
					basicModule::readHardeningSection(cnfunit, cnf%hardening, info)  ! Process hardening model section
						use_default_hardening = .true.
						if (.not. readValue(cnfunit, use_default_hardening)) return !MB: read default hardening flag
						if (.not. use_default_hardening) then
							if (.not. readValue(cnfunit, hardening%HardLawID)) return !MB: read hardening law ID
							select case(hardening%HardLawID)
							case(hard_none)
								! no action needed
							case(hard_Voce)
								if (readValue(cnfunit, tmp(1:5))) then ! Read one line
									hardening%VoceCnf = VoceConfig(tmp(1), tmp(2), tmp(3), tmp(4), tmp(5))
								endif
							case(hard_SwiftK)
								if (readValue(cnfunit, tmp(1:3))) then ! Read one line
									hardening%SwiftKCnf = SwiftKConfig(tmp(1),tmp(2), tmp(3))
								endif
							case(hard_SwiftS)
								if (readValue(cnfunit, tmp(1:3))) then ! Read one line
									hardening%SwiftSCnf = SwiftSConfig(tmp(1),tmp(2), tmp(3))
								endif
							case(hard_BP,hard_PEBPscrew,hard_PEBPloop)                 
								basicModule::readPEPBhardening(cnfunit,hardening%HardLawID,hardening%PEBPCnf,info) !MB: read PEBP hardening model configuration into hardening%PEBPCnf (definition in basicModule.f90)
									if (.not. readValue(cnfunit, tmp_fname)) return !MB: read BP parameter file name
									open(newunit=nparunit,file=tmp_fname,status='old',iostat=ioerr)
									info = ReadPar(nparunit,kost,hc%params) !MB: read the BP parameter file
									! Read PEBP state variable file path and block ID. 3 lines in config file
									if (.not. readValue(cnfunit, hc%read_state)) return !MB: read read_state flag; default read_state=.false.
									if (hc%read_state) then
										  if (.not. readValue(cnfunit,hc%input_fname)) return !MB: read state variable file name; default input_fname=''
										  if (.not. readValue(cnfunit,hc%block_id)) return !MB: read no. of state blocks to skip in state file; default block_id=0
									endif
							end select
						endif
					AlTay/src/altayConfig::setModelType(cnf,model_id,info)  ! the keyword is mapped to a proper model_id, we can instantly set it.
						select case(modelId)
						case(modelFCTaylor)
							  cnf%simul_init%ngr = 1
						case(modelAlamel)
							  cnf%simul_init%ngr = 2
						case(modelMASAL)
							  cnf%simul_init%ngr = 3
						end select
						! OK, supported model
						cnf%model_id = modelId
					basicModule::incurSlipsystemFile(dm_id, cnf%slipsystem%input_fname, info)  ! Let's map DM_id to a file
						select case(dm_id)
						case(DM_fcc12)
							  fname = 'fcc.pre'
						case(DM_bcc24)
							  fname = 'bcc.pre'
						case(DM_bcc48)
							  fname = 'bcc2.pre'
						case default
							  return
						end select
						basicModule::getDataPath(fname, slipsystem_path, info)
							prefix = getVEFDataDir()
							path = pathjoin(prefix, fname)
			stressDrivenModule::readYLPConfigSection(cnfunit,this%ylp,info)  ! Read multilevelYLP configuration
				use_default_solver_settings = .true.
				use_advanced_settings = .false.
				if (.not. readValue(cnfunit, use_default_solver_settings)) return
				if (.not. use_default_solver_settings) then
					if (.not. readValue(cnfunit, cnf%jacobi_eps)) return
					if (.not. readValue(cnfunit, cnf%linearize)) return
					! read default_eps and obj_func_eps
					if (.not. readValue(cnfunit,tmp)) return
					cnf%default_eps = tmp(1)
					cnf%obj_func_eps = tmp(2)
					! read flag for advanced settings (placeholder at the moment)
					if (.not. readValue(cnfunit, use_advanced_settings)) return
				endif
		! Read parameters specific for the UDSAModule program
        if (.not. readKeyword(cnfunit, sample_orientation_types, this%orientation_type_id)) return
        select case(this%orientation_type_id)
        case(sample_orientation_inplane_id)
			this%ptr_orientation_range => rangeFromConfig(cnfunit, info)
        case(sample_orientation_ND_id)
            ! No sub-options
			this%ptr_orientation_range => rangeFactory_extended('zero') ! One-element range
        case(sample_orientation_arbitrary_id)
			this%ptr_orientation_range => rangeFactory_extended('zero') ! One-element range   
            ! Read Euler angles
            if (.not. readValue(cnfunit, arr_euler)) return
            this%sample_orientation = Arr2EulerAngles(arr_euler)
        end select
        ! Read incrementation settings
        IncrementationControlSettings_read(this%control, cnfunit, info)
        if (.not. readKeyword(cnfunit, stress_states, this%stress_state_id)) return
        use_default_settings = .true.
        if (.not. readValue(cnfunit, use_default_settings)) return
        if (.not. use_default_settings) then
			if (.not. readValue(cnfunit, this%rho)) return
        endif
	! The configuration stage has been finished. Initialize the module
	the_module%initialize() ! StressDrivenModule_initialize(this) result(info)
		info = this%BasicModule%initialize()  ! basicModule::BasicModule_initialize(this) result(info)
			! Finish the configuration:
            this%altay%output_config%nfile = merge(1,0,this%output%outputRequest)
            this%altay%output_prefix = trim(this%output%outputPrefix)
            this%altay%jobtitle = trim(this%output%outputPrefix)
            if (this%output%outputRequest) then
				  select case(this%altay%hardening%HardLawID)
                  case(hard_BP,hard_PEBPscrew,hard_PEBPloop) '
						this%altay%output_config%npebp = 1
                  case default '
						this%altay%output_config%npebp = 0
                  end select
            endif
			AlTay/src/altaySub::initAltay(this%altay,ierr,errmsg)
				! UNIT LEC = SLIP SYSTEMS; open slip system file
				open (unit=LEC,file=trim(cnf%slipsystem%input_fname),status='old',iostat=ierr)
				! Load microstructure data
				AlTay/src/microstr::GRFIL(acnf%micros_fname,acnf%simul_init%FMicro,info) ! GRFIL(fnam,F_mic,ierr)  !> Reading of "microstructure" (Euler angles defining grain boundary segments) in SMT-format
					open (unit=NDAT2,file=fnam,status='old',iostat=ierr) !HGH: in HMS-prepared MAIN.CTL file, this is 'equiaxed.smt'
					read (NDAT2,94) NGrElm,TitMic ! number of grains and title
					do IGrElm=1,NGrElm
						read (NDAT2,96) EulGB%fi2,EulGB%PHI,EulGB%fi1
						T = rotmat(deg2rad(EulGB)) the transformation matrix T
						!TmatGr(1:3,i,IGrElm) for i=1,2 holds two non-parallel vectors within the initial GB (grain boundary) plane.      
						!TmatGr(1:3,i,IGrElm) for i=3 holds a vector out of the initial GB plane (not necessarily perpendicular to the GB plane).
						TmatGr(:,:,IGrElm)=matmul(F_mic,transpose(T)) !F_mic is a deformation gradient that conceptually 'deforms' a spherical grain into an ellipsoidal shape
					enddo
				! Get the initial texture
				AlTay/src/texFormats::loadTexture(cnf%texture%input_type,NDAT1,trim(cnf%texture%input_fname),cnf%texture%block_id,info) 
				!AlTay/src/texFormats::loadTexture(texfmt,nunit,fname,iblock,info)
					cubAccess::CUBreadTitle(nunit,filetitle,info)  ! CUBreadTitle(iounit,title,info)
					cubAccess::CUBreadBlock(nunit,info)  ! CUBreadBlock(iounit,info)
						read(iounit,iostat=info) NRSTEP,npoint,mf%FALG,mf%GAXES,mf%GEULR
						mf%GEULR = mf%GEULR * convf
						mf%TAX0 = rotmat(mf%GEULR(1),mf%GEULR(2),mf%GEULR(3))  ! Check it!!!
						Alg2::Transf(mf%GAXES,mf%CIJ0,mf%TAX0)  ! Check it!!! calculates the CIJ matrix of an ellipsoid with half axes stored in Gaxes. T defines the orientation of the axes.
						do i=1,npoint ! 5000
							read(iounit,iostat=info) DFIL(i)%tGEW,DFIL(i)%tfi1,DFIL(i)%tPHI,DFIL(i)%tfi2,DFIL(i)%tGAM,((DFIL(i)%tF(ii,jj),ii=1,3),jj=1,3),(DFIL(i)%tAXES(jj),jj=1,3),DFIL(i)%tEULR
							! Convert the grain orientatios from degrees to radians
							DFIL(i)%tfi1 = DFIL(i)%tfi1 * convf
							DFIL(i)%tPHI = DFIL(i)%tPHI * convf
							DFIL(i)%tfi2 = DFIL(i)%tfi2 * convf
							Dynfil::initFields(mf,DFIL(i)) !> Set the computed fields in grain structure.
								gr%tT = rotmat(gr%tfi1,gr%tPHI,gr%tfi2)
								! Backward compatibility with type(gr): initialize the remaining components with mf data...
								gr%tAXES = mf%GAXES 
								gr%tEULR = mf%GEULR
								gr%tF   = mf%FALG
								gr%tCIJ = mf%CIJ0
								gr%tTAX = mf%TAX0
								! ... and zero all the rest.
								gr%tZERO = 0.D0
								gr%tRHO  = 0.D0
						enddo
				! Open output files
				AlTay/src/altaySub::openOutputFiles(cnf, info, errmsg)  ! openOutputFiles(cnf, info, errmsg)
					... !long!
				! Initialize altay modules
				! Set the data for CRSS calculations
				AlTay/src/altayHard::InitModuleAltayHard(cnf%hardening, info) ! InitModuleAltayHard_config(config,info)
					select case(config%HardLawID)
					case(hard_none)
						info = 0
					case(hard_voce)
						altayHardLawSimple::InitModuleAltayHardLaw_Simple(config%VoceCnf,info) ! Just for non-hardening and isotropic, Voce-type hardening
						! altayHardLawSimple::init_voce(c,info)
							p%THIII=c%THIII1/(1.D0-c%TIII1/c%TIIIS)     
							p%ETA=c%THT/p%THIII
							p%GAMMAT=-c%TIIIS*LOG(p%ETA*c%TIIIS/(c%TIIIS-c%TIII1))/p%THIII !transition-gamma
							p%TAUT=c%TIIIS-(c%TIIIS-c%TIII1)*exp(-p%THIII*p%GAMMAT/c%TIIIS) !transition TAU
							p%THIV=c%THT/(1.D0-p%TAUT/c%TIVS) !theta-IV-0
							p%TIV0=c%TIVS+(p%TAUT-c%TIVS)*exp(p%THIV*p%GAMMAT/c%TIVS) !TAU-IV-0
							p%TIII1= c%TIII1     
							p%TIIIS= c%TIIIS     
							p%TIVS = c%TIVS
							vocePar=p
					case(hard_swiftK)
						altayHardLawSimple::InitModuleAltayHardLaw_Simple(config%swiftKCnf,info) ! Swift-K hardening
						!altayHardLawSimple::init_swiftK(c,info)
							p%K=c%K
							p%gamma0=c%gamma0
							p%n=c%n
							configured_law_id = hard_swiftK
							! Save the trial parameter set p
							SwiftPar=p
					case(hard_swiftS)
						altayHardLawSimple::InitModuleAltayHardLaw_Simple(config%swiftSCnf,info) ! Swift-S hardening
						!altayHardLawSimple::init_swiftS(c,info)
							p%K=c%crss0/(c%gamma0**c%n)
							p%gamma0=c%gamma0
							p%n=c%n
							configured_law_id = hard_swiftS
							! Save the trial parameter set p
							SwiftPar=p
					case(hard_BP,hard_PEBPscrew,hard_PEBPloop)
						info = InitModuleAltayHardLaw_DSH(config%PEBPCnf%params,config%HardLawID,LEC)
						!AlTay/src/altayHardLawDSH::Init_PAR(Ptry,KOSTtry,LEC) result(iError)
							... !long!
					end select
					if (info /= 0) return
					! Finalize the configuration: Set the module members
					HardLawID = config%HardLawID
					crss_ratios = config%crss_ratios
					
				! Initialisation of SIMUL
				AlTay/src/Alg0A::SIMUL(0,1)
					... !long! refer to AlTay/src/altay_HMS_inflow.for
				select case(cnf%hardening%HardLawID)
				case(hard_BP,hard_PEBPscrew,hard_PEBPloop)
					info = KS_initState(size(DFIL)) ! AlTay/src/altayDSHState::KS_initState(norient) result(info)
						if (norient > 0) allocate(KS_state(norient),stat=info)
						! All elements of the KS_state array must have the same initial state.
						AlTay/src/altayHardLawDSH::GetInitStatVar(KS_state(1),info) ! GetInitStatVar(SV0,iError)
							SV0%RHOcb               = P%RHOcbMIN
							SV0%CBB(:)%RHOwd        = P%RHOwdMIN
							SV0%CBB(:)%RHOwp        = 0.
							SV0%CBB(:)%RHOwdHOM     = P%RHOwdMIN
							SV0%CBB(:)%accGAMMA_new = 0.
							SV0%CBB(:)%RHOwd_ini    = P%RHOwdMIN
							SV0%ActiveCBB(:)        = 0
							SV0%CRSS                = F_CRSS(SV0)
						do i = 2, norient
							  KS_state(i) = KS_state(1)
						enddo
					if (acnf%hardening%PEBPCnf%read_state) then 
						! Load state variables
						info = KS_openStateFile(IPEBPSTAT,acnf%hardening%PEBPCnf%input_fname, mode='r')
						!AlTay/src/altayHardLawDSH::KS_openStateFile(iounit,fname,mode,use_header) result(info)
							select case(mode)
							case('r')
								  open(unit=iounit,file=fname,status='old',iostat=ierr) !MB: open existing file (status='old')
								  if (is_header) info = ReadHeadSVfile(iounit)
									...
							case('w')
								  open(unit=iounit,file=fname,status='replace',iostat=ierr) !MB: replace if already existing
								  if (is_header) info = WriteHeadSVfile(iounit)
									...
							end select
						info = KS_readState(IPEBPSTAT,acnf%hardening%PEBPCnf%block_id)
							...
					endif
				endselect  
            ! Output the initial state variables (texture etc) if requested.
            if (this%output%outputRequest) then
				commonUtils::outputTexture(ierr)
					AlTay/src/altaySub::outputCurrentState
						if (acnf%output_config%nfile == 1) then
							  AlTay/src/curAccess::CURwriteBlock(IMP1,info)
								...
						endif
						select case(acnf%hardening%HardLawID)
						case(hard_BP,hard_PEBPscrew,hard_PEBPloop)
							if (acnf%output_config%npebp == 1) then
								  info = KS_writeState(IMP4)
							endif
						endselect
						if ((acnf%output_config%nmss == 1) .and. allocated(astate%simulCalls)) then
							associate (callout => astate%simulCalls(astate%this)%output)
								AlTay/src/miscutils::writeMSSRecord(IMP5, &
											callout%effective_macro_strain, callout%effective_macro_strain_tot, &
											callout%homogenised_slip,callout%homogenised_slip_tot, &
											callout%stress_tensor, &
											callout%taylor_factor, callout%strain_rate_heterogeneity, &
											info)
									...
							end associate
						endif
            endif
		! Allocate and possibly populate the result cache
        allocate(this%ptr_db, stat = ierr)
		! Try to load data
        this%ptr_db%load(trim(this%output%outputPrefix)//'.rtdb')
	! Show general configuration of the multilevel model
    info = the_module%printConfig(display_unit) ! UDSAModule_printConfig(this,outunit) result (info)
		info = this%StressDrivenEvolutionModule%printConfig(outunit) ! StressDrivenModule_printConfig(this,outunit) result(info)
			...
		...
	! Run the module
    the_module%run(info) ! UDSAModule_run(this,info)
		this%StressDrivenEvolutionModule%run(info)  ! BasicModule_run(this,info)
			info = criSuccess  ! only this!
		! Prepare the input data      
		! Take uniaxial/{slightly biaxial} tensile stress, to be rotated to the given sample orientation.
		!> Uniaxial stress state. Negative value denotes compressive state; non-negative values are used for tensile state.
		sigma_t%t = 0.D0
		stress_direction = merge(-1.D0,1.D0,(this%stress_state_id == compression_state))
		sigma_t%t(1,1) = stress_direction * sqrt(3.D0/2.D0)/dsqrt(this%rho**2-this%rho+1)
		sigma_t%t(2,2) = this%rho*sigma_t%t(1,1)
		!
		! Loop over test set
		test_run = 0
		n_test_runs = this%ptr_orientation_range%size()
		test_run_loop: do while (this%ptr_orientation_range%next(angle))
			test_run = test_run + 1
			! Come back to the initial material state if needed
			! Re-initialize altay 
			if (n_test_runs > 1) then
				! Re-initialize AlTay
				info = this%reinitializeLibAltay(this%outputPrefix(angle))
					this%finalizeLibAltay()
					! Reconfigure:
					!  - Set new prefix
					if (present(output_prefix)) this%altay%output_prefix = output_prefix
					AlTay/src/altaySub::initAltay(this%altay,ierr)
						! UNIT LEC = SLIP SYSTEMS; open slip system file
						open (unit=LEC,file=trim(cnf%slipsystem%input_fname),status='old',iostat=ierr)
						! Load microstructure data
						AlTay/src/microstr::GRFIL(acnf%micros_fname,acnf%simul_init%FMicro,info)
							...
						! Get the initial texture
						AlTay/src/texFormats::loadTexture(cnf%texture%input_type,NDAT1,trim(cnf%texture%input_fname),cnf%texture%block_id,info)
							...
						! Open output files
						AlTay/src/altaySub::openOutputFiles(cnf, info, errmsg)
							...
						! Initialize altay modules
						! Set the data for CRSS calculations
						AlTay/src/altayHard::InitModuleAltayHard(cnf%hardening, info) ! InitModuleAltayHard_config(config,info)
							...
						! Initialisation of SIMUL
						AlTay/src/Alg0A::SIMUL(0,1)
							...
						select case(cnf%hardening%HardLawID)
						case(hard_BP,hard_PEBPscrew,hard_PEBPloop)
							info = KS_initState(size(DFIL))
								...
							if (acnf%hardening%PEBPCnf%read_state) then 
								! Load state variables
								info = KS_openStateFile(IPEBPSTAT,acnf%hardening%PEBPCnf%input_fname, mode='r')
									...
								info = KS_readState(IPEBPSTAT,acnf%hardening%PEBPCnf%block_id)
									...
							endif
						endselect 
			endif
			!
			! Set sample orientation and make rotation matrix
			select case(this%orientation_type_id)
			case(sample_orientation_inplane_id)
				sample_orientation = EulerAngles(0.D0, 0.D0, angle)
			case(sample_orientation_ND_id)
				sample_orientation = EulerAngles(90.D0, 90.D0, 90.D0)
			case(sample_orientation_arbitrary_id)
				sample_orientation = this%sample_orientation
			end select
			sample_orientation = deg2rad(sample_orientation)
			!
			! Rotate stress from "tensile" to material coordinate system
			! Calculate rotation matrix
			Mrot = rotmat(sample_orientation)
			sigma = rotateSRTensorTo(sigma_t, Mrot)
			!
			! Open and initialize result files
			if (n_test_runs > 1) then
				info = this%createOutputFile(ofunit, angle)
			else
				info = this%createOutputFile(ofunit)
			endif
			
			info = this%calculateStressPath(sigma, this%control, output, Mrot)  
			! StressDrivenEvolutionModule_calculateStressPath(this, sigma, control, outputs, rotmat, incrementation_control, use_icv_as_is) result(info)
			!> Main loop of incremental stress driven state evolution
				! Trick: allow the increment to "stretch" a bit. The trick is used in the stop condition of the loop to prevent starting a new increment because stop_control_variable - control%step_size gives some
				! small positive value. The trick does not eliminate the main cause of that drift, which is the accumulation of increment tensor components of different sign.
				stretch = stretch_ratio * control%increment_size
				!
				! Follow the evolution line along S in the main loop over deformation increments
				do  ! HGH: loop over the 40 increments in uniaxial tensile stress mode (UDSA)
					! Event handler in calculateStressPath: invoked at the begining of each increment
					this%onIncrementStart(control, icv, info) !  StressDrivenEvolutionModule_onIncrementStart(this, control, icv, info)
					! Calculate the strain rate mode
					info = this%findSolution(sigma, D, ylp, is_acceptable=acceptable_point)  
					! StressDrivenModule_findSolution(this,sigma, D, ylp_result, vM_guess, is_acceptable, pretry) result(info)
						obj_func%ptr_db => this%ptr_db
						use_vM_guess = optionalDefault(vM_guess, .true.)
						! Pre-try if requested and no explicit initial quess is provided
						use_pretry = optionalDefault(pretry, .true.) .and. use_vM_guess
						! Convert input to the 5D space and make the unit vector(s). This also makes sure it is deviatoric.
						vS = tens2vec5D(sigma%t)
						ylp_result = YLPResult(vS)  ! YLPResult::YLPResult_init(vS) result(res) ! Create YLPResult from arbitrary vS and performs normalization.
							res%vS_length = norm2(vS)
							if (res%vS_length > 0.D0) res%vS = vS / res%vS_length
						is_pretry_acceptable = .false.
						if (use_pretry) then ! HGH: this is executed because use_pretry = .true.
							ylp_result_pretry = ylp_result
							info = this%ptr_db%get(ylp_result_pretry%vS,ylp_result_pretry%vA,max_angle=pretry_search_angle) 
							! resultTable::get(this, S, A, max_angle) result(info)
							!> Find item in the database that has the smallest angle between S and item%vSonA, optionally restricting the choice to acceptable angles smaller than max_angle. Unless criSuccess is returned, the argument A is undefined.
								npoints = size(this%table)
								if (npoints > 0) then
									allocate(angles(npoints))
									do concurrent (i = 1:npoints)
										angles(i) = vec_angle(S, this%table%values(i)%vSonA) !Calculates an angle between two vectors
									enddo
									min_idx_a = minloc(angles)
									if (present(max_angle)) then
										CHOOSE(info, angles(min_idx) > max_angle, criFailure, criSuccess)
									else
										info = criSuccess
									endif
									if (info == criSuccess) A = this%table%values(min_idx)%vA
								endif
							if (info == criSuccess) then
								! Use special settings for pre-try
								ylp_pretry = this%ylp
								ylp_pretry%linearize = .true.
								ylp_pretry%nonlinear = .false.
								! get the solution (HGH: strain rate)
								info = this%search(ylp_pretry, ylp_result_pretry, .false., obj_func) ! 
								! StressDrivenModule_search(this, ylp_config, ylp_result, use_vM_guess, obj_func) result(info)    ! Wrapper around multilevelYLP that uses YLPResult for communicating with the caller. The wrapper applies settings provided as members of StressDrivenModule. It provides a ready-to-use ylp_result on non-error info code.
									libAlamYLP/alamYLP::multilevelYLP(ylp_result%vS,ylp_result%vA,ylp_result%vSonA,ylp_result%R,info, &
											useVMGuess=use_vM_guess,YLPconfig=ylp_config,verbose=this%output%verbosity,objective_function=obj_func)
									!multilevelYLP(vS,vA,vSonA,R,info,useVMGuess,YLPconfig,outunit,verbose,objective_function) ! Calculates plastic strain rate corresponding to given deviatoric stress
										if (present(YLPconfig)) config = YLPconfig ! Override the defaults by the user's settings
										! Set the objective function
										if (present(objective_function)) then
										  objFunc => objective_function
										else
										  allocate(objective_function_local)
										  objFunc => objective_function_local
										endif
										! Configure objective function      
										objFunc%initFx(alamEval_vSD_dim,alamEval_vSD_dim,ierr) ! fopt::objectiveFx::objectiveFunction_initFx(this,n_X_dim,m_F_dim,info)
											this%state%vX = 0.D0
											this%state%vF = 0.D0
											this%state%mJ = 0.D0
										!Get normalized stress vector
										norm = norm2(vS)
										objFunc%vSn = vS / norm
										objFunc%full_model = config%search_full_model
										ounit = stdout
										if (present(outunit))  ounit = outunit
										! Initialize TR solver 
										! (note: outunit argument has "optional" modifier in both the caller and callee)
										call nlls_TR_init(outunit,tr_verbose)
										! Use von Mises guess
										if (use_vmGuess) then
											vX = vS
										else
											vX = vA
										endif
										r1 = 0.D0; r2 = 0.D0
										! TODO: more reliable lower limit, it should lead to tr(d) > 1.e-7
										tr_config%lo_limit = -10.0
										tr_config%up_limit = 10.0
										tr_config%init_step = 100.0
										! Impose configuration settings
										! Tr:
										tr_config%eps = config%default_eps   !<< beware!
										tr_config%eps(2) = config%obj_func_eps  ! Norm of F: ||F||_2
										! Obj func:
										objFunc%jacobi_eps = config%jacobi_eps
										attempt_linearized = config%linearize
										! Run linearized problem if requested
										if (attempt_linearized) then
											tr_config%constJacobi = .true.
											vX_lin = vX    
											! Start the TR solver for linearized problem
											call nlls_TR_solve(objFunc,vX_lin,tr_config,r1_lin,r2_lin,ierr,TR_res,SolutionInitOut=initState)
											!objFunc: Objective function to minimize
											!vX_lin: Design vector
											!r1_lin: Initial residual of the solution
											!r2_lin: Final residual of the solution
											R = r2_lin
											! do checks if the solution is OK:
											! Stop criterion: magic number "3" means: ||F(x)||_2 < eps(2)
											if ( (r2_lin <= r1_lin) .and. (TR_res%stop_criterion == 3) .and. (r2_lin <= tr_config%eps(2)) ) then
												  vX = vX_lin
												  linearized_successful = .true.
											endif
										endif
										! The linearized analysis is either not done or failed.
										if (.not. linearized_successful .and. config%nonlinear) then
											! Set non-linear analysis
											tr_config%constJacobi = .false.
											! initState is invalid if nlls_TR_solve in the "if (attempt_linearized)" branch above returns ierr /= 0
											if (attempt_linearized .and. (ierr == 0)) then
												  ! Profit from the initial point stored by the solver for the linearized problem
												  ierr = objFunc%state%copy(initState)
												  tr_config%use_init_state = (ierr == 0)
											endif
											! start TR solver
											call nlls_TR_solve(objFunc,vX,tr_config,r1,r2,ierr)
											R = r2
											!TODO: check exit status of the solver
											if (attempt_linearized) then
												  ! choose better of non-linear and linearized solution
												  if (r2 > r2_lin) then
														vX = vX_lin
														R = r2_lin
												  endif
											endif
										endif
										call initState%finalize()
										! Set output strain rate
										norm = norm2(vX)
										vA = vX / norm
										! Call objective function again to get corresponding yield stress and other quantities.
										objFunc%full_model = config%evaluate_full_model
										call objFunc%objectiveEval(vA,ierr) ! libAlamYLP/alamEval::objectiveEval_NV5DComp(this,vX,info)
											alamEval_objFx_call_count = alamEval_objFx_call_count + 1
											! Transfer normalized vX into second rank tensor.
											norm = norm2(vX)
											vXn = vX/norm
											Atens = vec5D2tens(vXn) 
											! Set Atens as current value for processing 
											! Re-initialize with a request for just one single step
											AlTay/src/altaySub::initStepData(istp,astate,info) !initStepData(nsteps,steps,info)
												allocate(steps%simulCalls(nsteps),stat=ierr)
												steps%nSimulCalls = nsteps
												steps%this = 0
											associate (input => astate%simulCalls(istp)%input)
												input%dgf = Atens
												input%keep_texture = .true.
												input%keep_state = .true.
												input%full_model = this%full_model
												input%do_output_init = .false.
												input%do_output_final = .false.
												AlTay/src/altayConfig::setStepType(input,acnf%model_id,info) !  setStepType(stp,modelId,info)
												! Configure the stp object for using selected model type (FCTaylor, ALAMEL, MASAL).
													select case(modelId)
													case(modelFCTaylor)
														stp%rlx1 = .false. 
														stp%rlx2 = .false.
													case(modelAlamel,modelMASAL) 
														stp%rlx1 = .true. 
														stp%rlx2 = .true.
													end select
											end associate
											! Call the simulation
											AlTay/src/altaySub::runSteps(astate,info) !runSteps(steps,info)
												! Assign steps with astate
												astate = steps
												! Clean exception stack from a previous (possibly unsuccessful) set of calls.
												AlTay/src/altayRCM::RCM_clean()
													RCM_stack_top = 0
												do i = 1, steps%nSimulCalls
													steps%this = i
													if (steps%simulCalls(i)%input%do_output_init) then
														NFILE0 = 1
													else
														NFILE0 = 0
													endif
													!Set the macro velocity gradient in module MacroKinematic
													AlTay/src/macroKinematic::Set_DeformationRate(steps%simulCalls(i)%input%dgf,MacroDefRate)
														...
													! Run simul.
													AlTay/src/Alg0A::SIMUL(1,NFILE0,MacroDefRate)
														...
													if (RCM_signal()) then
														RCM_RAISE(info,'runSteps','SIMUL has thrown exception',RCM_RTN) 
													endif
													if (steps%simulCalls(i)%input%do_output_final) then
														AlTay/src/altaySub::outputCurrentState(info)
															...
													endif
												enddo
											! Retrieve output stress into 5D vector
											vS = tens2vec5D(astate%simulCalls(istp)%output%stress_tensor)
											! Transfer vS to vSml
											this%vSml = vS     
											! Normalize vS
											norm = norm2(vS)
											if (norm > 0.D0) then
												  vS = vS / norm
												  this%state%vF = this%vSn - vS
											else
												 ! norm is zero, so vS=0
												 this%state%vF = this%vSn
											endif
										vSonA = objFunc%vSml
										if (R > config%obj_func_eps) then
										  info = criFailure
										else
										  info = criSuccess
										endif
								! Accept the solution only if it reached the requested quality
								if (info == criSuccess .and. (ylp_result_pretry%R < this%ylp%obj_func_eps)) then
									is_pretry_acceptable = .true.
									ylp_result = ylp_result_pretry
								endif
							endif
							!
						endif
						!
						if (.not. is_pretry_acceptable) then
							if (.not. use_vM_guess) then ! HGH: this is not executed because vM_guess is not supplied and use_vM_guess = optionalDefault(vM_guess, .true.) == true
								ylp_result%vA = tens2vec5D(D%t)
								vA_norm = norm2(ylp_result%vA)
								if (vA_norm < epsilon(0.D0)) return
							endif
							! Calculate the corresponding strain rate vA
							info = this%search(this%ylp, ylp_result, use_vM_guess, obj_func)
							! StressDrivenModule_search(this, ylp_config, ylp_result, use_vM_guess, obj_func) result(info)    ! Wrapper around multilevelYLP that uses YLPResult for communicating with the caller. The wrapper applies settings provided as members of StressDrivenModule. It provides a ready-to-use ylp_result on non-error info code.
								...
							if (info == criFailure .and. associated(this%ptr_db)) then
								! Try another starting point
								! Set the re-try point
								ylp_result_retry = ylp_result
								if (this%ptr_db%get(ylp_result_retry%vS, ylp_result_retry%vA) == criSuccess) then
									! get new solution
									info = this%search(this%ylp, ylp_result_retry, .false., obj_func)
									! StressDrivenModule_search(this, ylp_config, ylp_result, use_vM_guess, obj_func) result(info)    ! Wrapper around multilevelYLP that uses YLPResult for communicating with the caller. The wrapper applies settings provided as members of StressDrivenModule. It provides a ready-to-use ylp_result on non-error info code.
										...
									! Use the better of the two
									if (ylp_result_retry%R < ylp_result%R) ylp_result = ylp_result_retry
								endif
							endif
							! Rare case: normal search and re-try cannot improve over pre-try
							if (info /= criSuccess .and. use_pretry) then
								if (ylp_result_pretry%R < ylp_result%R) ylp_result = ylp_result_pretry
							endif
						endif
						if (present(is_acceptable)) then
							is_acceptable = checkYLPResult(ylp_result, this%solution_tolerance, this%ylp%obj_func_eps)
								! YLPResult::checkYLPResult(ylp_result, tolerance, target_residual) result(val)
								val = (ylp_result%R < tolerance%residual_tolerance_factor * target_residual) &
									.and. &
									(vec_angle(ylp_result%vS, ylp_result%vSonA) < deg2rad(tolerance%angular_tolerance))
						endif
						D%t = vec5D2tens(ylp_result%vA)
						
					if ((info /= criSuccess) .or. .not. acceptable_point) then
						! Re-attempt, try A from the previous increment as the starting point
						! Pick the most recent converged solution
						do i = icv%increment, 1, -1
							if (tmp_output%values(i)%R < this%ylp%obj_func_eps) then
								D_retry = tmp_output%values(i)%A
								exit
							endif
						enddo
						acceptable_point_retry = .false.
						! Check post-condition of the loop: i > 0 means we have such a solution:
						if (i > 0) then
							info = this%findSolution(sigma, D_retry, ylp_retry, vM_guess=.false., is_acceptable=acceptable_point_retry)
							! Accept the solution only if it is better than the original one
							if (acceptable_point .and. (ylp_retry%R < ylp%R)) then 
								D = D_retry
								ylp = ylp_retry
							endif
						endif
					endif
					!
					! Nasty hack: drilling a hole to libaltay to get the Taylor factor
					commonUtils::getTaylorFactor(1,taylor_factor,info) !  getTaylorFactor(stepid,M,info)
						M = astate%simulCalls(stepid)%output%taylor_factor
					!
					! Make output record and prepare variables for updating icv
					tmp_record = IncrementOutputRecord(icv%IncrementationControlVariables,ylp,SRTensor(),SRTensor(),taylor_factor)
						! evolutionOutputRecord::IncrementOutputRecord_init(icv, ylp, De, Se, taylor_factor) result(this)
						this%vm_strain = root23 * norm2(icv%vP_step)
						this%vm_strain_total = root23 * norm2(icv%vP_total)
						this%norm_P_abs = norm2(icv%vP_abs)
						this%dotWonA = ylp%dotWonA
						this%scal_s = ylp%scal_s
						this%norm_SonA = norm2(ylp%vSonA)
						this%R = ylp%R
						this%taylor_factor = taylor_factor
						this%A%t = vec5D2tens(ylp%vA)
						this%SonA%t = vec5D2tens(ylp%vSonA)
						this%P_inc_evol = De
						this%S_evol = Se
						this%icv = icv
					!
					! Check if we start a/another increment
					stop_flag = .false.
					select case(control%scaling_type)
					case(scalingStrainTensor, scalingStrainTensorIncrement)
						stop_control_variable = norm2(icv%vP_step)
					case(scalingPlasticWork)
						stop_control_variable = icv%plastic_work_total
					case(scalingStrainTensorComponent)
						! Get total plastic strain in appropriate reference frame and check the tensor component of interest.
						X_tmp%t = vec5D2tens(icv%vP_step)
						if (present(rotmat)) X_tmp = rotateSRTensorFrom(X_tmp ,rotmat)
						X_tmp_voigt = Mat33ToVec6(X_tmp%t)
						stop_control_variable = abs(X_tmp_voigt(control%selected_tensor_component))
					case default
						! Make sure it stops immediately
						stop_flag = .true.
						stop_control_variable = control%step_size + control%increment_size
					end select
					stop_flag = stop_flag .or.(stop_control_variable + stretch > control%step_size)
					if (.not. stop_flag) then
						! Calculate incrementation control variables
						! Calculate increment of plastic strain to be imposed for texture evolution: 
						select case(control%scaling_type)
						case(scalingStrainTensorIncrement)
							control_variable = norm2(ylp%vA)
						case(scalingStrainTensor)
							! Find scaling factor x such as ||vP_step - x vA|| - ||vP_step|| = increment_size   (*)
							n_roots = solveQuadraticPolynomial(a=dot_product(ylp%vA, ylp%vA),b=2*dot_product(ylp%vA, icv%vP_step),c=dot_product(icv%vP_step, icv%vP_step) - (control%increment_size + norm2(icv%vP_step))**2, x=xi)
							! Up to two roots; we pick the largest one;
							if (n_roots > 0) control_variable = control%increment_size / maxval(xi(1:n_roots))
							! If control variable is negative (the only way to satisfy (*) is to decrease the strain), fall back to a less accurate scheme.
							if (control_variable < 0.D0) control_variable = norm2(ylp%vA)
						case(scalingPlasticWork)
							control_variable = ylp%dotWonA
						case(scalingStrainTensorComponent)
							if (present(rotmat)) then
								X_tmp = rotateSRTensorFrom(D,rotmat)
							else
								X_tmp = D
							endif
							X_tmp_voigt = Mat33ToVec6(X_tmp%t)
							control_variable = abs(X_tmp_voigt(control%selected_tensor_component))
						end select
						scaling_factor = (control%increment_size / control_variable)
						! Calculate strain increment for material state evolution
						vDe = ylp%vA * scaling_factor
						tmp_record%P_inc_evol%t = vec5D2tens(vDe)
						! Update material state
						commonUtils::makeTextureUpdateStep(tmp_record%P_inc_evol%t,tmp_record%S_evol%t,taylor_factor,this%output%outputRequest, info)
						! makeTextureUpdateStep(D,S,M,output_flag,info)
							AlTay/src/altaySub::initStepData(istp,astate,info) !initStepData(nsteps,steps,info)
								allocate(steps%simulCalls(nsteps),stat=ierr)
								steps%nSimulCalls = nsteps
								steps%this = 0
							! Set input data for AlTay
							associate (input => astate%simulCalls(istp)%input)
								  input%dgf = D
								  input%keep_texture = .false.
								  input%keep_state = .false.
								  input%full_model = .true.
								  input%do_output_init = .false.
								  input%do_output_final = output_flag
								  call setStepType(input,acnf%model_id,info)
							end associate
							AlTay/src/altaySub::runSteps(astate,info) !runSteps(steps,info)
								! Assign steps with astate
								astate = steps
								! Clean exception stack from a previous (possibly unsuccessful) set of calls.
								AlTay/src/altayRCM::RCM_clean()
									RCM_stack_top = 0
								do i = 1, steps%nSimulCalls
									steps%this = i
									if (steps%simulCalls(i)%input%do_output_init) then
										NFILE0 = 1
									else
										NFILE0 = 0
									endif
									!Set the macro velocity gradient in module MacroKinematic
									AlTay/src/macroKinematic::Set_DeformationRate(steps%simulCalls(i)%input%dgf,MacroDefRate)
										...
									! Run simul.
									AlTay/src/Alg0A::SIMUL(1,NFILE0,MacroDefRate)
										...
									if (RCM_signal()) then
										RCM_RAISE(info,'runSteps','SIMUL has thrown exception',RCM_RTN) 
									endif
									if (steps%simulCalls(i)%input%do_output_final) then
										AlTay/src/altaySub::outputCurrentState(info)
											...
									endif
								enddo
							! Get the result
							S = astate%simulCalls(istp)%output%stress_tensor(:,:)
							M = astate%simulCalls(istp)%output%taylor_factor
						vSe = tens2vec5D(tmp_record%S_evol%t)
					else
						vDe = 0.D0
						vSe = 0.D0
					endif
					! Append the output record
					info = xVector_push(tmp_output, tmp_record)
					! Update icv
					icv%update(vDe, vSe, info)  ! incrementalControl::IncrementationControl_update(this, vDe, vSe, info)
						! Plastic work in the current increment
						this%plastic_work_inc = dot_product(vDe,vSe)
						! Plastic work in the current step
						this%plastic_work_step = this%plastic_work_step + this%plastic_work_inc
						! Total plastic work
						this%plastic_work_total = this%plastic_work_total + this%plastic_work_inc
						! Increment of plastic strain
						this%vP_inc = vDe
						! Total plastic strain in the current step
						this%vP_step = this%vP_step + this%vP_inc
						! Total plastic strain:
						this%vP_total = this%vP_total + this%vP_inc
						! Sum of absolute plastic strain increments:
						this%vP_abs = this%vP_abs + abs(this%vP_inc)
						this%increment = this%increment  + 1
					this%onIncrementEnd(control, icv, tmp_record, info) ! StressDrivenEvolutionModule_onIncrementEnd(this, control, icv, output_record, info)
					!> Event handler in calculateStressPath: invoked at the end of each increment	
				enddo
			
			! Process the output evolution path and produce result file
			do increment = 1, size(output%values)
				! Total plastic strain at the _begining_ of the inrement.
				associate(v => output%values(increment))
					! Rotate back to the "tensile test" coordinate system   
					D_t = rotateSRTensorFrom(v%A, Mrot)
					S_t = rotateSRTensorFrom(v%SonA, Mrot)
					! Total deviatoric strain (Note: the total, not per-step) 
					P_t%t = vec5D2tens(v%icv%vP_total) ! at the beginning of the increment
					P_t_end%t = P_t%t + v%P_inc_evol%t ! at the end of the increment
					P_t = rotateSRTensorFrom(P_t, Mrot)
					P_t_end = rotateSRTensorFrom(P_t_end, Mrot)
					!
					! Calculate output variables
					! Calculate q and r in tensile reference frame
					! Write out the result
					outrec = UDSAOutputRecord(increment = increment, &
											  vm_strain = v%vm_strain, &
											  norm_P_abs = v%norm_P_abs, &
											  TNorm = abs(P_t%t(1,1)), & ! Tensile strain
											  TSigma = S_t%t(1,1) - S_t%t(3,3), & ! Tensile total stress
											  TSNorm = abs(S_t%t(1,1)), & ! Tensile deviatoric stress
											  plastic_work_total = v%icv%plastic_work_total, &
											  dotWonA = v%dotWonA, &
											  taylor_factor = v%taylor_factor, &
											  instantaneous_qrsvalue = calculateQRS(D_t%t,v%scal_s), &
											  cummulative_qrsvalue = calculateQRS(P_t_end%t, v%norm_SonA), &
											  residual =  v%R)
					info = this%outputFile(iounit=ofunit, data_record=outrec)
				end associate
			enddo
		end do test_run_loop
	! Finalize the module
    info = the_module%finalize()