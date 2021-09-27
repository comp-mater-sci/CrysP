# $Id: bootstrap.makefile 3235 2018-01-18 16:02:03Z jgawad $
# ============================
# Bootstrap Makefile for CMake
# ============================ 
# Two basic configurations are provided: debug and release
#
# Possible targets:
# - all, build, install, clean, mrproper, info
#
# How to customize user's Makefile
# --------------------------------
# 
# Typical Makefile:
# # Path to the top-level
# ROOT_DIR=
#
# # Optional custom build
# custom : all
#     actions-to-be-taken-after-all
# # Optional set of CMakeFiles
# CMAKELISTS=
#
# include $(ROOT_DIR)/build/cmake-common/bootstrap.makefile
#
ifdef DEBUG
ConfigurationName=debug
else
ConfigurationName=release
endif

OutDir=$(ConfigurationName)
IntDir=$(ConfigurationName)/build
ProjectDir=$(CURDIR)

#
# Common tools
#
CAT=cat
CD=cd
CP=cp
ECHO=@echo -e
ECHO_RAW=echo -e
DOXYGEN=doxygen
MKDIR=mkdir
RM=rm

INFOFILE=build-info.md

CMAKELISTS ?= CMakeLists.txt src/CMakeLists.txt

GENERIC_TARGETS := all install clean mrproper info doc

.PHONY : $(GENERIC_TARGETS)

install all : $(CMAKELISTS) | $(OutDir) $(IntDir)
	cd $(IntDir) && cmake -DCMAKE_BUILD_TYPE=$(ConfigurationName) $(CMAKE_FLAGS) $(ProjectDir) && $(MAKE) $@

$(OutDir) $(IntDir) :
	-$(MKDIR) -p $@
	
clean	:
	$(info Cleaning $(IntDir)) 
	${MAKE} -C $(IntDir) $@

mrproper	:
	$(info Purging $(OutDir)) 
	-@$(RM) -rf $(OutDir)

info	:
	$(ECHO) "Available generic targets:"
	$(ECHO) $(foreach target,$(GENERIC_TARGETS),"\t$(target)\n")
	-@[ -f $(INFOFILE) ] && $(CAT) $(INFOFILE) || $(ECHO_RAW) "\n(No project specific info available)" 

doc	:
	@$(DOXYGEN)


