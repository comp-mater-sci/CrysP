
CXX=icpc
AR=xiar

.PHONY : clean

CPPLIBDIR=../cppProjectLib

all	: cppProject

cppProject	: cppProject.cpp
	$(CXX) -I$(CPPLIBDIR) -o $@ $^ -L$(CPPLIBDIR) -lcppProject 

clean	:
	-rm *.o cppProject
