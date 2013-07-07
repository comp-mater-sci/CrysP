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
qrsTypes.f90 \
dmcUtils.f90 \
commonConfig.f90 \
commonUtils.f90 \
abstractModule.f90 \
basicModule.f90

# Build list of objects:
OBJS=$(call fxx2opaths,$(SRCS),$(OBJDIR))

ALMYLPDIR=../libAlamYLP/$(ConfigurationName)
ALAMYLP=libalamYLP.a
ALAMYLPLIB=$(ALMYLPDIR)/$(ALAMYLP)
FCFLAGS+= -I$(ALMYLPDIR)/include
FLDFLAGS+= -L$(ALMYLPDIR) -lalamYLP

FLDFLAGS+= $(LIBOPTPATH) $(MKLLDFLAGS)
# Name of the executable binary program
LIBNAME=libalamDMC.a

LIB=$(ConfigurationName)/$(LIBNAME)

.PHONY : clean dirstructure

$(LIB)	: $(OBJS)  | $(OBJDIR) $(INCLUDEDIR)
		$(AR) rcs $@ $(OBJS)

$(OBJDIR)/%.o	: %.f90 | dirstructure
	$(FC) -c $< $(FCFLAGS) -o $@

clean	: FORCE
	-rm -f $(OBJS) $(LIB) $(INCLUDEDIR)/*.mod $(LIB) 
FORCE	:

$(LIBDIR) $(OBJDIR) $(INCLUDEDIR) :
	[ -d $@ ] || mkdir -p $@

$(OBJDIR)/commonConfig.o	:  $(addsuffix .o, $(addprefix $(OBJDIR)/, dmcUtils )) $(LIBOPTPATH) $(ALAMYLPLIB) msgFormats.inc
$(OBJDIR)/basicModule.o	:  $(addsuffix .o, $(addprefix $(OBJDIR)/, abstractModule commonConfig dmcUtils)) $(LIBOPTPATH) $(ALAMYLPLIB) msgFormats.inc
