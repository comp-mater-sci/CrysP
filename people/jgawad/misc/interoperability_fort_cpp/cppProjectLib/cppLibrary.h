#ifndef __CPP_LIBRARY_GAHGH_TA5HW4HVV_WTH2H2J5Y
#define __CPP_LIBRARY_GAHGH_TA5HW4HVV_WTH2H2J5Y

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

		int cppFx_CType1(CType1 & obj);


		struct CType2
		{
			size_t			n;
			double *		array;
		};

		int cppFx_CType2(CType2 & obj);

		void * cppFx_allocate(size_t n);
	}



#endif