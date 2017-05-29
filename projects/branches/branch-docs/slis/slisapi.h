#ifndef slisapi_91863F51_FAD9_4E0B_9CD9_54A23F6A6E3C
#define slisapi_91863F51_FAD9_4E0B_9CD9_54A23F6A6E3C
#include <stdbool.h>

extern "C"
{
	int initSlis(const char * envvar);

	bool isLicenseValid(const char * uuid, bool print_output);
}


#endif // slisapi_91863F51_FAD9_4E0B_9CD9_54A23F6A6E3C

