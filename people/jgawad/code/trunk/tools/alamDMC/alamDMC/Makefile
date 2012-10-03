#
# $Id$
#
#### Selection of toolset
## Note that only one toolset can be used 
######################################################################
#### Toolset:
ROOTDIR=../../../../..

include ../Makefile.tools

#
######################################################################
OBJDIR=$(ConfigurationName)/obj
INCLUDEDIR=$(ConfigurationName)/include

OUT_DIRS = $(LIBDIR) $(OBJDIR) $(INCLUDEDIR)

SRCS= \
main.f90 \
qrsTypes.f90 \
alamUtils.f90 \
commonConfig.f90 \
commonUtils.f90 \
alamASR.f90 \
alamQ.f90 \
alamTSA.f90 \
alamYld.f90

# Build list of objects:
OBJS=$(call fxx2opaths,$(SRCS),$(OBJDIR))

ALMYLPDIR=../libAlamYLP/$(ConfigurationName)
ALAMYLP=libalamYLP.a
ALAMYLPLIB=$(ALMYLPDIR)/$(ALAMYLP)
FCFLAGS+= -I$(ALMYLPDIR)/include
FLDFLAGS+= -L$(ALMYLPDIR) -lalamYLP

FLDFLAGS+= $(LIBOPTPATH) $(MKLLDFLAGS)
# Test program
PROG=alamDMC

.PHONY : clean dirstructure


$(ConfigurationName)/$(PROG)	: $(OBJDIR) $(INCLUDEDIR) $(OBJS) $(ALAMYLPLIB)
	$(FC) $(FCFLAGS) -I$(INCLUDEDIR) $(OBJS) -o $@  $(FLDFLAGS) 

$(OBJDIR)/%.o	: %.f90 | dirstructure
	$(FC) -c $< $(FCFLAGS) -o $@

clean	: FORCE
	-rm -f $(OBJS) $(LIB) $(INCLUDEDIR)/*.mod $(PROG) 
FORCE	:

$(LIBDIR) $(OBJDIR) $(INCLUDEDIR) :
	[ -d $@ ] || mkdir -p $@

$(OBJDIR)/main.o	:  $(addsuffix .o, $(addprefix $(OBJDIR)/, commonConfig commonUtils alamQ alamASR alamTSA alamYld qrsTypes)) $(LIBOPTPATH) msgFormats.inc
$(OBJDIR)/commonConfig.o	:  $(addsuffix .o, $(addprefix $(OBJDIR)/, alamUtils )) $(LIBOPTPATH) $(ALAMYLPLIB) msgFormats.inc
$(OBJDIR)/alamQ.o	:  $(addsuffix .o, $(addprefix $(OBJDIR)/, alamUtils commonConfig commonUtils qrsTypes)) $(LIBOPTPATH) $(ALAMYLPLIB) msgFormats.inc
$(OBJDIR)/alamASR.o	:  $(addsuffix .o, $(addprefix $(OBJDIR)/, alamUtils commonConfig commonUtils qrsTypes)) $(LIBOPTPATH) $(ALAMYLPLIB) msgFormats.inc
$(OBJDIR)/alamTSA.o	:  $(addsuffix .o, $(addprefix $(OBJDIR)/, alamUtils commonConfig commonUtils qrsTypes)) $(LIBOPTPATH) $(ALAMYLPLIB) msgFormats.inc
$(OBJDIR)/alamYld.o	:  $(addsuffix .o, $(addprefix $(OBJDIR)/, alamUtils commonConfig commonUtils)) $(LIBOPTPATH) $(ALAMYLPLIB) msgFormats.inc

