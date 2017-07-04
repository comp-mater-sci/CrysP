#ifndef signature_896B0FBD_BE55_4865_8B53_C8327A87CC54
#define signature_896B0FBD_BE55_4865_8B53_C8327A87CC54

#include <string>
#include <vector>
#include <boost/filesystem.hpp>
#include <boost/uuid/sha1.hpp>

#include "digest.hpp"

namespace slis {

	namespace signature {

		typedef boost::filesystem::path path_t;

		typedef std::vector<unsigned char> byte_array_t;

		// signature type stems from the upstream sha1 digest type.
		struct signature_t {
			signature_t();
			signature_t(digest::digest_type);

			std::array<unsigned int, 5> digest;
		};

		const byte_array_t default_salt({ 's', 'l', 'i', 's' });


		inline byte_array_t from_string(const std::string & str)
		{
			byte_array_t out;
			std::copy(str.begin(), str.end(), std::back_inserter(out));
			return out;
		}

		class Signer
		{
			byte_array_t key;

		public:

			Signer(const byte_array_t & secret_key, const byte_array_t & salt = default_salt)
			{
				// compose the key out of secret and salt
				std::copy(secret_key.begin(), secret_key.end(), std::back_inserter(key));
				std::copy(salt.begin(), salt.end(), std::back_inserter(key));
				key.shrink_to_fit();
			}

			signature_t get_signature(const byte_array_t & value) const;

			bool verify_signature(const byte_array_t & value, const signature_t & sig) const;

		};	

		const auto sigfile_extension = std::string(".slissig");

		//! Calculate signature of file at file_path using signer
		signature_t file_signature(const Signer & signer, const path_t & file_path);

		//! Verify if the signature file contains a valid signature of 
		bool verify_file_signature(const Signer & signer,
									const path_t & file_path,
									const path_t & sigfile_path = path_t{});

		bool sign_file(const Signer & signer,
						const path_t & path,
						const std::string & comment = std::string(),
						const std::string & ext = sigfile_extension);

	}
}

#endif // signature_896B0FBD_BE55_4865_8B53_C8327A87CC54