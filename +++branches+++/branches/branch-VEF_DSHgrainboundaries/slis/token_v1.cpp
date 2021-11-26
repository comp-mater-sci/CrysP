#include "stdafx.h"
#include "token_v1.hpp"

#include <fstream>
#include <sstream>
#include <exception>

// needed for the serialized STL containers
#include <boost/serialization/array.hpp>
#include <boost/serialization/vector.hpp>

// needed for uuid serialization
#include <boost/uuid/uuid_serialize.hpp>
#include <boost/uuid/uuid_io.hpp>

#include "signature_serialization.h"
#include "token_secret.hpp"

using namespace slis;




namespace slis {
	namespace tokens {

		// low level
		bool readToken(Token & token, const boost::filesystem::path & token_path)
		{
			std::ifstream inp(token_path.native());
			return !inp ? false : token.read(inp);
		}

		bool writeToken(const Token & token, const boost::filesystem::path & token_path)
		{
			std::ofstream out(token_path.native());
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

		namespace tokenapi_v1 {


			struct InvalidTokenException : std::exception {
				const char* what() const noexcept { return "Invalid token"; }
			};


			Token authenticToken(const boost::filesystem::path & token_path) 
			{
				signature::Signer token_signer(tokens::constants::token_secret);
				// Load the token and check if it is valid
				Token token;
				if (!(readToken(token, token_path) && token.isAuthentic(token_signer))) throw InvalidTokenException {};
				return token;
			}

			signature::Signer fileSigner(const Token & token)
			{
				TokenContent content = token.get();
				// take token id as the salt
				signature::byte_array_t salt;
				copy(content.id.begin(), content.id.end(), std::back_inserter(salt));
				return signature::Signer{ tokens::constants::token_secret, salt };
			}

			int signFile(const boost::filesystem::path & file_path,
						 const boost::filesystem::path & token_path, 
						 const boost::filesystem::path & signature_path) noexcept
			{
				try
				{
					Token token = authenticToken(token_path);
					signature::Signer file_signer = fileSigner(token);
					return (signature::sign_file(file_signer, file_path, signature_path, token.alias()) ?
							signature_status::ok 
							: 
							signature_status::invalid_signature);
				}
				catch (InvalidTokenException &)
				{
					return signature_status::invalid_token;
				}
				catch(...)
				{ /* noexcept */ }
				return signature_status::error;
			}

			bool isSignatureValid(const boost::filesystem::path & file_path,
									const boost::filesystem::path & token_path,
									const boost::filesystem::path & signature_path) noexcept
			{
				try
				{
					Token token = authenticToken(token_path);
					signature::Signer file_signer = fileSigner(token);
					return signature::verify_file_signature(file_signer, file_path, signature_path); 
				}
				catch (InvalidTokenException &)
				{
					return false;
				}
				catch (...)
				{ /* noexcept */
				}
				return false;
			}

			bool isTokenValid(const boost::filesystem::path & token_path) noexcept
			{
				try
				{
					Token token = authenticToken(token_path);
					return true;
				}
				catch (InvalidTokenException &)
				{ }
				catch(...)
				{  /* noexcept */ }
				// will not reach here unless exception is thrown
				return false;
			}


		}

	} // namespace tokens
} // namespace slis


