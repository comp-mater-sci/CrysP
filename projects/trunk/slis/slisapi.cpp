#include "stdafx.h"
#include "slisapi.h"
#include "slis_v1.hpp"

using namespace slis::slis_v1;

Slis & theSlis()
{
	static Slis the_slis;
	return the_slis;
}



extern "C"
{

	int initSlis(const char * envvar)
	{
		try
		{
			return theSlis().load_license(envvar);
		}
		catch (...)
		{
			return -1;
		}
		return -1;
	}


	bool isLicenseValid(const char * uuid, bool print_output)
	{
		try
		{
			return theSlis().check_license(uuid, print_output);
		}
		catch (...)
		{
			return false;
		}
		return false;
	}

}