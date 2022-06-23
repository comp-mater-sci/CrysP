# $Id$

#
# Top-level projects as phony targets
#
ALL_PROJECTS := FCRI AlTay fopt fng VEF

#
# Top-level generic targets
#
GENERIC_TARGETS := all clean info doc
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
.PHONY: $(GENERIC_TARGETS)

#
# Default target
#
all: $(ALL_PROJECTS)

#
# Dependencies between the top-level projects
#

AlTay: FCRI
VEF: fopt AlTay

#
# Targets
#

info:
	$(ECHO) "Build all top-level targets.\n"
	$(ECHO) "Default target: all\n"
	$(ECHO) "Generic targets:"
	$(ECHO) $(foreach target,$(GENERIC_TARGETS),"\t$(target)\n")
	$(ECHO) "Top-level targets:"
	$(ECHO) $(foreach target,$(ALL_PROJECTS),"\t$(target)\n")
	$(ECHO) "Special flags:\n" 
	$(ECHO) "\tDEBUG: set DEBUG=1 to build the debug versions"
	$(ECHO) "\tVERBOSE: set VERBOSE=1 to see detaied compilation info on the terminal" 
	$(ECHO) "\n"

$(ALL_PROJECTS):
	$(ECHO) "\nBuilding $@ \n"
	$(MAKE) -C $@

clean mrproper doc:
	$(foreach project, $(ALL_PROJECTS), $(call make-target, $(project)))
