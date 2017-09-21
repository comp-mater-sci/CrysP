#ifndef slisdefs_15DBE750_3BFB_4B5D_8CE5_2763DF5C7C60
#define slisdefs_15DBE750_3BFB_4B5D_8CE5_2763DF5C7C60
#pragma once

#ifdef _WIN32

	#ifdef SLIS_DLL
	// Define API modifier on Windows platform
		#ifdef SLIS_EXPORTS
			#define SLIS_API __declspec(dllexport)
		#else
			#define SLIS_API __declspec(dllimport)
		#endif 

	#else
		// Empty API modifier
		#define SLIS_API
	#endif

#endif // _WIN32

#endif // !slisdefs_15DBE750_3BFB_4B5D_8CE5_2763DF5C7C60
