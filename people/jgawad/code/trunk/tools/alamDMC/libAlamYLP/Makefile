#
# $Id$
#
#### Selection of toolset
## Note that only one toolset can be used 
######################################################################
#### Toolset:
ROOTDIR=../../..
TOOLSDIR=../..
include ../Makefile.tools
#
######################################################################

ifdef DEBUG
ConfigurationName=debug
LIBDIR=debug
DEBUGFLAGS= -O0 -g -debug full -check all
FCFLAGS+= $(DEBUGFLAGS)
TRANSCONFIG=DEBUG=1
else
ConfigurationName=release
LIBDIR=release
OPTFLAGS=-O3 -fast
FCFLAGS+= $(OPTFLAGS)
endif
#
######################################################################
OBJDIR=$(ConfigurationName)/obj
INCLUDEDIR=$(ConfigurationName)/include

SRCS= \
alamYLP.f90 \
vumat_libvec5.for

define create-dir
echo $(dir $@)
[ -d $@ ] || mkdir -p $(dir $@)
touch $@
endef

LIB=$(ConfigurationName)/libalamYLP.a

# Build list of objects:
# Substitute .for extension
OBJLISTA=$(SRCS:.f90=.o)
OBJLIST=$(OBJLISTA:.for=.o)
# Library objects
OBJS = $(patsubst %, $(OBJDIR)/%, $(OBJLIST))
$(info $(OBJS))

.PHONY : clean

#
$(LIB)	: .dirstructure $(OBJS)
	echo $?
	$(AR) rcs $@ $(OBJS)

$(OBJDIR)/%.o	: %.f90
	$(FC) -c $< $(FCFLAGS) -o $@

$(OBJDIR)/%.o	: %.for
	$(FC) -c $< -fixed $(FCFLAGS) -o $@

clean	: FORCE
	-rm -f $(OBJS) $(LIB) $(INCLUDEDIR)/*.mod
FORCE	:

.dirstructure : $(addsuffix /.dirstructure, $(LIBDIR) $(OBJDIR) $(INCLUDEDIR))
	touch $@

$(addsuffix /.dirstructure, $(LIBDIR) $(OBJDIR) $(INCLUDEDIR)) : 
	$(create-dir)

$(OBJDIR)/alamYLP.o 	:  $(OBJDIR)/vumat_libvec5.o $(LIBOPTPATH)

$(LIBOPTPATH)		:
	make -C $(LIBOPTROOT) $(TRANSCONFIG)
