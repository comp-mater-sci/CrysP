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
main.f90 \
qrsTypes.f90 \
alamUtils.f90 \
commonConfig.f90 \
alamASR.f90 \
alamQ.f90 \
alamTSA.f90 

# Build list of objects:
# Substitute .for extension
OBJLISTA=$(SRCS:.f90=.o)
OBJLIST=$(OBJLISTA:.for=.o)
# Library objects
OBJS = $(patsubst %, $(OBJDIR)/%, $(OBJLIST))
$(info $(OBJS))
#

ALMYLPDIR=../libAlamYLP/$(ConfigurationName)
ALAMYLPLIB=libalamYLP.a
FCFLAGS+= -I$(ALMYLPDIR)/include
FLDFLAGS+= -L$(ALMYLPDIR) -lalamYLP

FLDFLAGS+= $(LIBOPTPATH) $(MKLLDFLAGS)
# Test program
PROG=alamDMC

$(ConfigurationName)/$(PROG)	: $(OBJDIR) $(INCLUDEDIR) $(OBJS)
	$(FC) $(FCFLAGS) -I$(INCLUDEDIR) $(OBJS) -o $@  $(FLDFLAGS) 

$(OBJDIR)/%.o	: %.f90
	$(FC) -c $< $(FCFLAGS) -o $@

$(OBJDIR)/%.o	: %.for
	$(FC) -c $< -fixed $(FCFLAGS) -o $@

clean	: FORCE
	-rm -f $(OBJS) $(LIB) $(INCLUDEDIR)/*.mod $(PROG) 
FORCE	:

$(LIBDIR) :
	[ -d $@ ] || mkdir -p $@

$(OBJDIR) :
	[ -d $@ ] || mkdir -p $@

$(INCLUDEDIR) :
	[ -d $@ ] || mkdir -p $@

$(OBJDIR)/main.o	:  $(addsuffix .o, $(addprefix $(OBJDIR)/, commonConfig alamQ alamASR alamTSA qrsTypes)) $(LIBOPTPATH) msgFormats.inc
$(OBJDIR)/commonConfig.o	:  $(addsuffix .o, $(addprefix $(OBJDIR)/, alamUtils )) $(LIBOPTPATH) msgFormats.inc
$(OBJDIR)/alamQ.o	:  $(addsuffix .o, $(addprefix $(OBJDIR)/, alamUtils commonConfig qrsTypes)) $(LIBOPTPATH) msgFormats.inc
$(OBJDIR)/alamASR.o	:  $(addsuffix .o, $(addprefix $(OBJDIR)/, alamUtils commonConfig qrsTypes)) $(LIBOPTPATH) msgFormats.inc
$(OBJDIR)/alamTSA.o	:  $(addsuffix .o, $(addprefix $(OBJDIR)/, alamUtils commonConfig qrsTypes)) $(LIBOPTPATH) msgFormats.inc

