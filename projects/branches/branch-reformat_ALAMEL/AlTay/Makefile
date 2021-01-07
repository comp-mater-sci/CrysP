# $Id$
#
#

include ../build/cmake-common/bootstrap.makefile

install all : cub_all
clean : cub_clean
mrproper : cub_mrproper

.PHONY : cub_all cub_clean cub_mrproper

cub_all :
	$(MAKE) -C cub
cub_clean : 
	$(MAKE) -C cub clean
cub_mrproper : 
	$(MAKE) -C cub mrproper 

