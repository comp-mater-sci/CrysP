#
# $Id$
#
#### Selection of toolset
## Note that only one toolset can be used 
######################################################################
#### Toolset:
# 
ROOTDIR=../..
TOOLSDIR=..
include Makefile.tools
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
	make -C $(ALAMDMCDIR) $(TRANSCONFIG)

$(LIBALAMYLP)	: $(LIBOPTPATH)
	make -C libAlamYLP $(TRANSCONFIG)

$(LIBOPTPATH)		:
	make -C $(LIBOPTROOT) $(TRANSCONFIG)

clean	:
	make -C $(ALAMDMCDIR) $@
	make -C $(LIBALAMYLPDIR) $@
