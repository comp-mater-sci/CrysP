#
# $Id$
#
#### Selection of toolset
## Note that only one toolset can be used 
######################################################################
#### Toolset:
# 
ROOTDIR=$(shell readlink -f "../../../..")

SolutionDir=$(shell pwd)

$(info $(ROOTDIR))


#### Toolset:
include Makefile.tools

export ROOTDIR 
export SolutionDir

#
######################################################################
#
ALAMDMCDIR=alamDMC
ALAMDMC=$(ALAMDMCDIR)/$(ConfigurationName)/alamDMC
LIBALAMDMCDIR=libAlamDMC
LIBALAMDMC=$(LIBALAMDMCDIR)/$(ConfigurationName)/libalamDMC.a
LIBALAMYLPDIR=libAlamYLP
LIBALAMYLP=$(LIBALAMYLPDIR)/$(ConfigurationName)/libalamYLP.a

.PHONY : clean  $(LIBALAMYLP) $(ALAMDMC)

all	: $(LIBALAMYLP) $(LIBALAMDMC) $(ALAMDMC)

$(ALAMDMC)	: $(LIBALAMYLP) $(LIBALAMDMC) $(LIBOPTPATH)
	$(MAKE) -C $(ALAMDMCDIR) $(TRANSCONFIG)

$(LIBALAMDMC)  : $(LIBALAMYLP)
	$(MAKE) -C $(LIBALAMDMCDIR) $(TRANSCONFIG)

$(LIBALAMYLP)	: $(LIBOPTPATH)
	$(MAKE) -C libAlamYLP $(TRANSCONFIG)

$(LIBOPTPATH)		:
	$(MAKE) -C $(LIBOPTROOT) $(TRANSCONFIG)

clean	:
	$(MAKE) -C $(ALAMDMCDIR) $@
	$(MAKE) -C $(LIBALAMYLPDIR) $@
	$(MAKE) -C $(LIBALAMDMCDIR) $@