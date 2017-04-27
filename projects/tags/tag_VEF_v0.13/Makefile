# $Id$


# Canned recipe for top-level build set:
define make-targets
$(ECHO) "Performing make $@"
$(MAKE) -C FCRI $@ 
$(MAKE) -C AlTay $@
$(MAKE) -C fopt $@
$(MAKE) -C fng $@
$(MAKE) -C abaqus/vumats $@
$(MAKE) -C MTM-FHMport $@
$(MAKE) -C slis $@
$(MAKE) -C VEF $@
# $(MAKE) -C hms $@
endef
#

ECHO=@echo -e

BRANCH_ROOT=$(shell pwd)
export BRANCH_ROOT
#

.PHONY: all clean info

info:
	$(ECHO) "Build all top-level targets. \n"
	$(ECHO) "Targets:"
	$(ECHO) "\t all \n\t clean \n\t mrproper"
	$(ECHO) "\n"
	$(ECHO) "Special flags:\n" 
	$(ECHO) "\tDEBUG: set DEBUG=1 to build the debug versions"
	$(ECHO) "\tVERBOSE: set VERBOSE=1 to see detaied compilation info on the terminal" 
	$(ECHO) "\n"

all clean mrproper:
	$(make-targets)


