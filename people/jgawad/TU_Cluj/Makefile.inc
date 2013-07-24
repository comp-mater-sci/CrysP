#
# $Id$
#
# Toolset
FC=ifort
AR=xiar

#
# Check type of build
#
ABAQUS_VERSION=612
FCFLAGS += -fPIC -fpp -traceback 
ARFLAGS=

ifdef DEBUG
$(info Debug build)
#
# Settings for debug build
#
Configuration=debug
OBJDIR=debug
# Compiler flags that enable insight diagnostic and run-time checks
#FFDIAGS=  -WB -check all -g  -warn all -inline-debug-info  -debug-parameters all -debug full  -ftrapuv
WARNLEVEL = -warn all 
# Uncomment to see detailed parameters that are passed to the compiler by the driver
# WARNLEVEL += -watch 
FFDIAGS= 
FDFLAGS = -g -O0 -debug full -check all -debug-parameters all -ftrapuv $(WARNLEVEL) -D_DEBUG -DDEBUG -DVUMAT_DIAGS
FCFLAGS+= $(FDFLAGS)
else
#
# Settings for normal build
#
Configuration=release
OBJDIR=release
WARNLEVEL = -warn interfaces
# Optimization settings 
#FFOPTFLAGS = -O0 
FFOPTFLAGS = -O3  # -Winline
FDFLAGS = $(WARNLEVEL) -vec-report0 -WB $(FFDIAGS)
FCFLAGS+= $(FFOPTFLAGS)
endif
## Append command line extra parameters
ifdef EXTRAFLAGS
FCFLAGS += $(EXTRAFLAGS)
endif
####################################################################################################
#
TargetDir=$(Configuration)/
INCLUDEDIR=$(OBJDIR)/include
OUTDIRS = $(OBJDIR) $(INCLUDEDIR)
FCFLAGS += -module $(INCLUDEDIR) -gen-interfaces
#
#
# Includes
#
VUMAT_ROOTDIR=$(HOME)/jgprojects/TWRMTMProject/code/trunk/vumat
FNG_COREDIR=$(VUMAT_ROOTDIR)/fng/projects/libfngCore
FNG_CRIDIR=$(VUMAT_ROOTDIR)/fng/projects/libfngCore
VUMAT_DIR=$(VUMAT_ROOTDIR)/$(Configuration)$(ABAQUS_VERSION)
FCFLAGS += \
 -I$(VUMAT_DIR)/include \
 -I$(FNG_COREDIR)/include \
 -I$(FNG_CRIDIR)/include

## Build-up of the durectory structure
# Canned recipe for creation of a directory:
define create-dir
echo $@
[ -d $@ ] || mkdir -p $@
endef
###################################################################################################
define printCompilationInfo
@echo Compiling $< ' --> ' $@
endef

.PHONY	: clean cleandeep dirstructure info help quite

all	: $(LIB) $(SLIB)
#
# Targets and implicit targets
#

$(OBJDIR)/%.o : %.f90 | $(OUTDIRS)
	$(printCompilationInfo)
	$(FC) -c $(FCFLAGS) $< -o $@


$(OBJDIR)/%.o : %.for | $(OUTDIRS)
	$(printCompilationInfo)
	$(FC) -c $(FCFLAGS) $< -o $@

$(SLIB)	:	$(LIBOBJS)
	@echo Building static library 
	$(AR) $(ARFLAGS) rcs $@ $^

$(LIB)  : 	$(LIBOBJS)
	@echo Building dynamic library
	$(FC) -o $@ -shared -Wl,-soname,$(@F) -O3 -fPIC -Bdynamic -i-dynamic $^

$(OUTDIRS) :
	-$(create-dir)

clean	:
	echo  $(LIBOBJS) $(INCLUDEDIR)/* $(SLIB) $(LIB)	
	-$(RM) $(LIBOBJS) $(INCLUDEDIR)/* $(SLIB) $(LIB)	
