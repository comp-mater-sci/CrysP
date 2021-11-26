#include "stdafx.h"
#include "digest.hpp"
#include <sstream>

namespace slis{

	namespace digest{

		std::string hexdigest(const digest_type digest)
		{
			// based on the code found here: https://gist.github.com/jhasse/990731
			char hash[20];
			for (size_t i = 0; i < 5; ++i)
			{
				const char* tmp = reinterpret_cast<const char*>(digest);
				hash[i * 4] = tmp[i * 4 + 3];
				hash[i * 4 + 1] = tmp[i * 4 + 2];
				hash[i * 4 + 2] = tmp[i * 4 + 1];
				hash[i * 4 + 3] = tmp[i * 4];
			}
			std::ostringstream buf;
			buf << std::hex;
			for (size_t i = 0; i < sizeof(hash); ++i)
			{
				buf << ((hash[i] & 0x000000F0) >> 4)
					<< (hash[i] & 0x0000000F);
			}
			return buf.str();
		}
	}
}