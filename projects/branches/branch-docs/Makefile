# $Id$

#
# Top-level projects as phony targets
#
ALL_PROJECTS := FCRI AlTay fopt fng abaqus/vumats MTM-FHMport slis VEF hms


ECHO=@echo -e

# Canned recipe for top-level projects
define make-target
$(MAKE) -C $1 $@;
endef

#
# Provide certain variables to the sub-makes
#
BRANCH_ROOT := $(shell pwd)
export BRANCH_ROOT

#
# Top-level projects are phony targets
#
.PHONY: $(ALL_PROJECTS)
.PHONY: all clean info

all: $(ALL_PROJECTS)

#
# Dependencies between the top-level projects
#

AlTay: FCRI
fng: FCRI fopt AlTay
abaqus/vumats: FCRI fng
VEF: FCRI fopt slis AlTay fng
hms: abaqus/vumats AlTay fng VEF

#
# Targets
#

info:
	$(ECHO) "Build all top-level targets. \n"
	$(ECHO) "Targets:"
	$(ECHO) "\t all \n\t clean \n\t mrproper"
	$(ECHO) "\n"
	$(ECHO) "Special flags:\n" 
	$(ECHO) "\tDEBUG: set DEBUG=1 to build the debug versions"
	$(ECHO) "\tVERBOSE: set VERBOSE=1 to see detaied compilation info on the terminal" 
	$(ECHO) "\n"

$(ALL_PROJECTS):
	$(ECHO) "\nBuilding $@ \n"
	$(MAKE) -C $@

clean mrproper:
	$(foreach project, $(ALL_PROJECTS), $(call make-target, $(project)))

