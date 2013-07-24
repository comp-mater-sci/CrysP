#
# $Id$
#

define make-all
$(MAKE) -C BBC2008Ident $(OPTIONS)
$(MAKE) -C libHMSIntegration $(OPTIONS)
$(MAKE) -C VUMAT_BBC2008 $(OPTIONS)
endef

.PHONY : clean

all :
	$(make-all)

clean : OPTIONS=clean
clean :
	$(make-all)

