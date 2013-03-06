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
LIBALAMYLPDIR=libAlamYLP
LIBALAMYLP=$(LIBALAMYLPDIR)/$(ConfigurationName)/libalamYLP.a

.PHONY : clean  $(LIBALAMYLP) $(ALAMDMC)

all	: $(LIBALAMYLP) $(ALAMDMC)

$(ALAMDMC)	: $(LIBALAMYLP) $(LIBOPTPATH)
	$(MAKE) -C $(ALAMDMCDIR) $(TRANSCONFIG)

$(LIBALAMYLP)	: $(LIBOPTPATH)
	$(MAKE) -C libAlamYLP $(TRANSCONFIG)

$(LIBOPTPATH)		:
	$(MAKE) -C $(LIBOPTROOT) $(TRANSCONFIG)

clean	:
	$(MAKE) -C $(ALAMDMCDIR) $@
	$(MAKE) -C $(LIBALAMYLPDIR) $@
