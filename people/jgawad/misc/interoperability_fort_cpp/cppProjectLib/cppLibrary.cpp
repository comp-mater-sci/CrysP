
#include <iostream>
#include <string>
#include <string.h>

#include "cppLibrary.h"

using namespace std;


void cppFx_simple()
{
	std::cout << "cppFx_simple" << std::endl;
}


int cppFx_ArgsIntPtrInt(size_t n, int arr[])
{
	std::cout << "cppFx_ArgsIntPtrInt" << std::endl;
	for(size_t i = 0; i < n; i++)
		std::cout << arr[i] << ' ';
	std::cout << std::endl;
	return n;
}


int cppFx_Fstring( const char * str, int len, int len_trim )
{
	std::cout << "cppFx_Fstring" << std::endl;
	std::cout <<'\'';
	for(int i = 0; i < len; i++)
		std::cout << str[i];
	std::cout <<"'\n";
	return 0;
}

int cppFx_CType1(CType1 & obj)
{
	std::cout <<"cppFx_CType1"  << std::endl;
	std::cout << "sizeof(obj): " << sizeof(obj) << std::endl
			  << "sizeof(obj.n): " << sizeof(obj.n) << " obj.n= " << obj.n << std::endl
			  << "sizeof(obj.array): " << sizeof(obj.array) << " obj.array= ";
	size_t n_elems = sizeof(obj.array) / sizeof(obj.array[0]);
	for (size_t i = 0; i < n_elems; i++) 
		std::cout << obj.array[i] << ' ';
	std::cout << std::endl;
	return 0;
}

int cppFx_CType2(CType2 & obj)
{
	std::cout <<"cppFx_CType2"  << std::endl;
	std::cout << "sizeof(obj): " << sizeof(obj) << std::endl
		<< "sizeof(obj.n): " << sizeof(obj.n) << " obj.n= " << obj.n << std::endl
		<< "sizeof(obj.array): " << sizeof(obj.array) << " obj.array= ";
	for (size_t i = 0; i < obj.n; i++) 
		std::cout << obj.array[i] << ' ';
	std::cout << std::endl;
	return 0;
}


void * cppFx_allocate(size_t n)
{
	double * ptr = new double[n];
	for (size_t i = 0; i < n; ptr[i++]=i);
	return static_cast<void*>(ptr);
}