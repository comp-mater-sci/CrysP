# $Id$
#
# Bootstrap Makefile for CMake. 
# It offers two basic configurations: debug and release
#
# Possible targets:
# - all, build, install, clean, mrproper, info
#
ifdef DEBUG
ConfigurationName=debug
else
ConfigurationName=release
endif

OutDir=$(ConfigurationName)
IntDir=$(ConfigurationName)/build
ProjectDir=$(CURDIR)

MKDIR=mkdir
RM=rm
CAT=cat
ECHO=echo -e
DOXYGEN=doxygen


INFOFILE=build-info.md

CMAKELISTS ?= CMakeLists.txt src/CMakeLists.txt

.PHONY : all clean mrproper info doc

all build install : $(CMAKELISTS) | $(OutDir) $(IntDir)
	cd $(IntDir) && cmake $(ProjectDir) -DCMAKE_BUILD_TYPE=$(ConfigurationName) && $(MAKE) install

$(OutDir) $(IntDir) :
	-$(MKDIR) -p $@
	
clean	:
	$(info Cleaning $(IntDir)) 
	${MAKE} -C $(IntDir) $@

mrproper	:
	$(info Purging $(OutDir)) 
	-@$(RM) -rf $(OutDir)

info	:
	-@$(ECHO) "Available generic targets:\n- all\n- build\n- install\n- clean\n- mrproper\n- info"
	-@[ -f $(INFOFILE) ] && $(CAT) $(INFOFILE) || $(ECHO) "\n(No project specific info available)" 

doc	:
	@$(DOXYGEN)


