#
# $Id$
#
FC=ifort



FCFLAGS=-fpp -extend-source 132 
ifdef DEBUG
ConfigurationName=debug
LIBDIR=debug
DEBUGFLAGS= -O0 -g -debug full -check all
FCFLAGS+= $(DEBUGFLAGS)
else
ConfigurationName=release
LIBDIR=release
OPTFLAGS=-O3 -fast
FCFLAGS+= $(OPTFLAGS)
endif
#
OBJDIR=$(ConfigurationName)/obj
INCLUDEDIR=$(ConfigurationName)/include
BINDIR=$(ConfigurationName)/bin

FCFLAGS+= -module $(INCLUDEDIR) # -stand f03 

SRCS=cub2cur.f90 \
cub2cubV2.f90 \
cubAccess.f90 \
smtAccess.f90 \
cub2smt.f90 \
mergeCubs.f90 


# Build list of objects:
# Substitute .for extension
OBJLISTA=$(SRCS:.f90=.o)
OBJLIST=$(OBJLISTA:.for=.o)
# Library objects
OBJS = $(patsubst %, $(OBJDIR)/%, $(OBJLIST))
$(info $(OBJS))

PROG1=cub2cur
PROG2=cub2cubV2
PROG3=cub2smt
PROG4=mergeCubs
PROG5=ori2smt

ALLPROGS= $(addprefix $(BINDIR)/, $(PROG1) $(PROG2) $(PROG3) $(PROG4) $(PROG5))

all	: structure $(ALLPROGS) $(OBJS)

.PHONY=clean structure

structure : $(BINDIR) $(OBJDIR) $(INCLUDEDIR)

$(OBJDIR)/cub2cur.o   :     $(OBJDIR)/cubAccess.o

$(OBJDIR)/cub2cubV2.o :     $(OBJDIR)/cubAccess.o

$(OBJDIR)/mergeCubs.o :     $(OBJDIR)/cubAccess.o

$(OBJDIR)/smtAccess.o   :     $(OBJDIR)/cubAccess.o

$(OBJDIR)/cub2smt.o	:  $(OBJDIR)/cubAccess.o $(OBJDIR)/smtAccess.o

$(OBJDIR)/ori2smt.o	:  $(OBJDIR)/cubAccess.o $(OBJDIR)/smtAccess.o


$(BINDIR)/$(PROG1)	: $(addprefix $(OBJDIR)/, cubAccess.o cub2cur.o)
	$(FC) -o $@ $^

$(BINDIR)/$(PROG2)	: $(addprefix $(OBJDIR)/, cubAccess.o cub2cubV2.o)
	$(FC) -o $@ $^

$(BINDIR)/$(PROG3)	: $(addprefix $(OBJDIR)/, cubAccess.o smtAccess.o cub2smt.o)
	$(FC) -o $@ $^ 

$(BINDIR)/$(PROG4)	: $(addprefix $(OBJDIR)/, cubAccess.o  mergeCubs.o)
	$(FC) -o $@ $^

$(BINDIR)/$(PROG5)	: $(addprefix $(OBJDIR)/, cubAccess.o smtAccess.o ori2smt.o)
	$(FC) -o $@ $^ 

clean :
	rm -f  $(ALLPROGS) $(OBJS) $(INCLUDEDIR)/*

$(BINDIR) :
	[ -d $@ ] || mkdir -p $@

$(OBJDIR) :
	[ -d $@ ] || mkdir -p $@

$(INCLUDEDIR) :
	[ -d $@ ] || mkdir -p $@


$(OBJDIR)/%.o : %.for
	$(FC) -c $(FCFLAGS) $< -o $@

$(OBJDIR)/%.o : %.f90
	$(FC) -c $(FCFLAGS) $< -o $@

