# $Id$
#
# Bootstrap Makefile for CMake. 
# It offers two basic configurations: debug and release
#
ifdef DEBUG
ConfigurationName=debug
else
ConfigurationName=release
endif

OutDir=$(ConfigurationName)

MKDIR=mkdir
RM=rm

CMAKELISTS=CMakeLists.txt src/CMakeLists.txt

.PHONY : all clean mrproper

all : $(CMAKELISTS) | $(ConfigurationName) 
	cd $(ConfigurationName) && cmake .. -DCMAKE_BUILD_TYPE=$(ConfigurationName) && $(MAKE)

$(OutDir) :
	-$(MKDIR) -p $(OutDir)
	
clean	:
	${MAKE} -C $(OutDir) clean

mrproper	:
	-$(RM) -rf $(OutDir)

