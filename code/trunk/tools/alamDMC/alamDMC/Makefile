#
# $Id$
#
#### Selection of toolset
## Note that only one toolset can be used 
######################################################################
ifndef ROOTDIR
ROOTDIR=../../../../..
endif

ifndef SolutionDir
SolutionDir=..
endif

#### Toolset:
include $(SolutionDir)/Makefile.tools

#
######################################################################
OBJDIR=$(ConfigurationName)/obj
INCLUDEDIR=$(ConfigurationName)/include

OUT_DIRS = $(LIBDIR) $(OBJDIR) $(INCLUDEDIR)

SRCS= \
main.f90 \
dmcASR.f90 \
dmcQRS.f90 \
dmcUDSA.f90 \
dmcYld.f90

# Build list of objects:
OBJS=$(call fxx2opaths,$(SRCS),$(OBJDIR))

LIBALAMDMCROOT=../libAlamDMC
LIBALAMDMCDIR=$(LIBALAMDMCROOT)/$(ConfigurationName)
LIBALAMDMC=libalamDMC.a
LIBALAMDMCLIB=$(LIBALAMDMCDIR)/$(LIBALAMDMC)
FCFLAGS+= -I$(LIBALAMDMCDIR)/include -I$(LIBALAMDMCROOT)
FLDFLAGS+= -L$(LIBALAMDMCDIR) -lalamDMC

ALMYLPDIR=../libAlamYLP/$(ConfigurationName)
ALAMYLP=libalamYLP.a
ALAMYLPLIB=$(ALMYLPDIR)/$(ALAMYLP)
FCFLAGS+= -I$(ALMYLPDIR)/include
FLDFLAGS+= -L$(ALMYLPDIR) -lalamYLP

FLDFLAGS+= $(LIBOPTPATH) $(MKLLDFLAGS)
# Name of the executable binary program
PROGNAME=alamDMC

PROG=$(ConfigurationName)/$(PROGNAME)

.PHONY : clean dirstructure

$(PROG)	: $(OBJDIR) $(INCLUDEDIR) $(OBJS) $(ALAMYLPLIB) $(LIBALAMDMCLIB)
	$(FC) $(FCFLAGS) -I$(INCLUDEDIR) $(OBJS) -o $@  $(FLDFLAGS) 

$(OBJDIR)/%.o	: %.f90 | dirstructure
	$(FC) -c $< $(FCFLAGS) -o $@

clean	: FORCE
	-rm -f $(OBJS) $(LIB) $(INCLUDEDIR)/*.mod $(PROG) 
FORCE	:

$(LIBDIR) $(OBJDIR) $(INCLUDEDIR) :
	[ -d $@ ] || mkdir -p $@

$(OBJDIR)/main.o	:  $(addsuffix .o, $(addprefix $(OBJDIR)/, commonConfig commonUtils dmcQRS dmcASR dmcUDSA dmcYld qrsTypes)) $(LIBOPTPATH) $(LIBALAMDMCROOT)/msgFormats.inc
$(OBJDIR)/dmcQRS.o	:  $(addsuffix .o, $(addprefix $(OBJDIR)/, basicModule alamUtils commonConfig commonUtils qrsTypes)) $(LIBOPTPATH) $(ALAMYLPLIB) $(LIBALAMDMCROOT)/msgFormats.inc
$(OBJDIR)/dmcASR.o	:  $(addsuffix .o, $(addprefix $(OBJDIR)/, basicModule dmcUtils commonConfig commonUtils qrsTypes)) $(LIBOPTPATH) $(ALAMYLPLIB) $(LIBALAMDMCROOT)/msgFormats.inc
$(OBJDIR)/dmcUDSA.o	:  $(addsuffix .o, $(addprefix $(OBJDIR)/, basicModule dmcUtils commonConfig commonUtils qrsTypes)) $(LIBOPTPATH) $(ALAMYLPLIB) $(LIBALAMDMCROOT)/msgFormats.inc
$(OBJDIR)/dmcYld.o	:  $(addsuffix .o, $(addprefix $(OBJDIR)/, basicModule dmcUtils commonConfig commonUtils)) $(LIBOPTPATH) $(ALAMYLPLIB) $(LIBALAMDMCROOT)/msgFormats.inc

