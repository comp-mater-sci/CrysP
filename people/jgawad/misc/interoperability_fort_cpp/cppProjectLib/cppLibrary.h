#ifndef __CPP_LIBRARY_2C171D9B_82D8_42E9_9878_DC6A7247054A
#define __CPP_LIBRARY_2C171D9B_82D8_42E9_9878_DC6A7247054A

#pragma once


	extern "C" {

		void cppFx_simple();

		int cppFx_ArgsIntPtrInt(size_t n, int arr[]);

		int cppFx_Fstring(const char * str, int len, int len_trim);


		struct CType1 
		{
			size_t	n;
			double	array[10];
		};

		int cppFx_CType1(const CType1 & obj);


		struct CType2
		{
			size_t			n;
			double *		array;
		};

		int cppFx_CType2(const CType2 & obj);

		void * cppFx_allocate(size_t n);


		struct CType3
		{
			double	tens[3][3];
		};

		struct CType4
		{
			size_t   n;
			CType3 * array;
		};

		void cppFx_CType4(CType4 &);


		CType4 * cppFx_allocateCType4(size_t n);

	}



#endif