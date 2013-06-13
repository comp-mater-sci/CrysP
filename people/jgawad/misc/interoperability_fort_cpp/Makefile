


all	: cppProject  cppProjectLib  fortranProject

cppProject  cppProjectLib  fortranProject :
	$(MAKE) -C $@ all 		

.PHONY	: cppProjectLib cppProject fortranProject

cppProject : cppProjectLib
		
fortranProject : cppProjectLib


clean	:
	$(MAKE) -C cppProject $@ 
	$(MAKE) -C cppProjectLib $@  
	$(MAKE) -C fortranProject $@
