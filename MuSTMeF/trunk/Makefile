
.PHONY: all build clean mrproper

build:
	$(MAKE) -C vumat $@
	$(MAKE) -C vumat_plane_stress $@

mrproper: 
	$(MAKE) -C vumat cleandeep
	$(MAKE) -C FCRI $@
	$(MAKE) -C fng $@
	$(MAKE) -C gmetex $@
	$(MAKE) -C vumat_plane_stress $@

	
