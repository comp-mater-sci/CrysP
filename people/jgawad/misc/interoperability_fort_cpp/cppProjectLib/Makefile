
CXX=icpc
AR=xiar

.PHONY : clean

all	: libcppProject.a

libcppProject.a	: cppLibrary.o 
	$(AR) crs $@ $^ 	

gmetexFStringTest.o: gmetexFStringTest.cpp gmetexFStringTest.h gmetexFString.hpp 
	$(CXX) -c $<
	
cppLibrary.o	:  cppLibrary.cpp  cppLibrary.h
	$(CXX) -c $<

clean	:
	-rm *.o libcppProject.a
