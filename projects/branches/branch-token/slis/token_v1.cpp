#include "stdafx.h"
#include "token_v1.hpp"

#include <sstream>

// needed for the serialized STL containers
#include <boost/serialization/array.hpp>
#include <boost/serialization/vector.hpp>

// needed for uuid serialization
#include <boost/uuid/uuid_serialize.hpp>
#include <boost/uuid/uuid_io.hpp>

#include "signature_serialization.h"

using namespace slis;




namespace slis {
	namespace tokens {

		// low level
		bool readToken(Token & token, const char * token_path)
		{
			std::ifstream inp(token_path);
			return !inp ? false : token.read(inp);
		}

		bool writeToken(const Token & token, const char * token_path)
		{
			std::ofstream out(token_path);
			return !out ? false : token.write(out);
		}


		slis::signature::byte_array_t Token::signable() const
		{
			using namespace slis::signature;
			// serialize token content
			std::ostringstream ofs;
			{
				boost::archive::text_oarchive oa(ofs);
				oa & content;
			}
			ofs.flush();
			return from_string(ofs.str());
		}



		bool Token::isAuthentic(const slis::signature::Signer & signer) const
		{
			// ask signer if it has signed the content.
			return signer.verify_signature(signable(), signature);
		}

		bool Token::read(std::istream & in)
		{
			try
			{
				boost::archive::text_iarchive arch(in);
				arch & *this;
			}
			catch (boost::archive::archive_exception &)
			{
				return false;
			}
			return true;
			
		}

		bool Token::write(std::ostream & out) const
		{
			try
			{
				boost::archive::text_oarchive arch(out);
				arch & *this;
			}
			catch (boost::archive::archive_exception &)
			{
				return false;
			}
			return true;
		}

	} // namespace tokens
} // namespace slis
