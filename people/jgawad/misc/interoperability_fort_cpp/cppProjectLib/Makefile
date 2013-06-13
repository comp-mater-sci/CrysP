
CXX=icpc
AR=xiar

.PHONY : clean

all	: libcppProject.a

libcppProject.a	: cppLibrary.o 
	$(AR) crs $@ $^ 	

cppLibrary.o	:  cppLibrary.cpp  cppLibrary.h
	$(CXX) -c $<

clean	:
	-rm *.o libcppProject.a
