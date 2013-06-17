
#include <iostream>
#include <string>
#include <cstring>

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

int cppFx_CType1(const CType1 & obj)
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

int cppFx_CType2(const CType2 & obj)
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
	std::cout <<"cppFx_allocate"  << std::endl;
	double * ptr = new double[n];
	for (size_t i = 0; i < n; ptr[i++]=i);
	return static_cast<void*>(ptr);
}

void cppFx_CType4( CType4 & obj)
{
	std::cout <<"cppFx_CType4"  << std::endl;
	std::cout << "sizeof(obj): " << sizeof(obj) << std::endl
			  << "sizeof(obj.n): " << sizeof(obj.n) << " obj.n= " << obj.n << std::endl;

	for (size_t i = 0; i < obj.n; i++)
	{
		for (size_t j = 0; j < 3; j++)
		{
			for (size_t k = 0; k < 3; k++) 
				std::cout << obj.array[i].tens[j][k] << ' ';
			std::cout << '\n';
		}
		std::cout << "------" << std::endl;	

		// Transpose the elements in tens

		for (size_t j = 0; j < 3; j++)
			for (size_t k = j; k < 3; k++) 
				std::swap(obj.array[i].tens[j][k] , obj.array[i].tens[k][j]);
	}

}

CType4 * cppFx_allocateCType4(size_t n)
{
		std::cout <<"cppFx_allocateCType4"  << std::endl
			      << "n: " << n << std::endl;
		CType4 * ptr = new CType4;
		ptr->n = n;
		ptr->array = new CType3[n];
		// Initialize the structure
		for (size_t i = 0; i < n; i++)
		{
			for (size_t j = 0, l = 0; j < 3; j++)
				for (size_t k = 0; k < 3; k++)
					ptr->array[i].tens[j][k] = i*10 + ++l;
		}
		return ptr;
}


size_t cppFx_CType5(const CType5 & cobj)
{
	std::cout <<"cppFx_CType5"  << std::endl
			  << "cobj.n: " << cobj.n << std::endl;
	for (size_t i = 0; i < cobj.n; i++)
		std::cout << cobj.array[i] << ' ';
	std::cout << std::endl;
	return cobj.n;
}