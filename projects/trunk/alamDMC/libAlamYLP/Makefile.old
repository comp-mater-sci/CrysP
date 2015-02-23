#
# $Id$
#
#### Selection of toolset
## Note that only one toolset can be used 
######################################################################

ifndef ROOTDIR
ROOTDIR=../../../../..
endif

#### Toolset:
include $(SolutionDir)/Makefile.tools

#
######################################################################
OBJDIR=$(ConfigurationName)/obj
INCLUDEDIR=$(ConfigurationName)/include

OUT_DIRS = $(LIBDIR) $(OBJDIR) $(INCLUDEDIR)

$(info $(OUT_DIRS))

SRCS= \
alamYLP.f90 \
vumat_libvec5.for


LIB=$(ConfigurationName)/libalamYLP.a

# Build the list of the library objects:
OBJS =  $(call fxx2opaths,$(SRCS),$(OBJDIR))


.PHONY : clean dirstructure

#
$(LIB)	: $(OBJS) | dirstructure
	echo $?
	$(AR) rcs $@ $(OBJS)

dirstructure : $(OUT_DIRS)

$(OUT_DIRS)	:
	$(create-dir)
	

$(OBJDIR)/%.o	: %.f90 | dirstructure
	$(FC) -c $< $(FCFLAGS) -o $@

$(OBJDIR)/%.o	: %.for | dirstructure
	$(FC) -c $< -fixed $(FCFLAGS) -o $@

clean	: FORCE
	-rm -f $(OBJS) $(LIB) $(INCLUDEDIR)/*.mod
FORCE	:


$(OBJDIR)/alamYLP.o 	:  $(OBJDIR)/vumat_libvec5.o $(LIBOPTPATH)

$(LIBOPTPATH)		:
	make -C $(LIBOPTROOT) $(TRANSCONFIG)
