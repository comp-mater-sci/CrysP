
#include "stdafx.h"

#include "signature.hpp"

#include <algorithm>
#include <memory>
#include <fstream>

namespace slis{
	namespace signature{


		signature_t::signature_t()
		{
			// would be ' : digest{ {0, 0, 0, 0, 0} }; with VC++ > 2013
			digest = { { 0, 0, 0, 0, 0 } };
		}

		signature_t::signature_t(digest::digest_type digest_raw)
		{
			std::copy(digest_raw, digest_raw + 5, digest.begin());
		}

		signature_t Signer::get_signature(const byte_array_t & value) const
		{
			boost::uuids::detail::sha1 s;

			if (!value.empty())
				s.process_bytes(std::addressof(*value.begin()), value.size());
			if (!key.empty())
				s.process_bytes(std::addressof(*key.begin()), key.size());

			
			// Upstream library representation
			digest::digest_obj_type digest = { 0, 0, 0, 0, 0 };
			s.get_digest(digest);

			return signature_t(digest);
		}

		bool Signer::verify_signature(const byte_array_t & value, const signature_t & signature) const
		{
			auto proper_signature = get_signature(value);
			return proper_signature.digest == signature.digest;
		}



		signature_t file_signature(const Signer & signer, const path_t & file_path)
		{
			using namespace std;
			ifstream inp(file_path.string(), ios_base::in & ios_base::binary);
			// Read as a sequence of bytes
			istream_iterator<byte_array_t::value_type> inp_it(inp), term_it;
			byte_array_t buffer;
			copy(inp_it, term_it, back_inserter(buffer));
			return signer.get_signature(buffer);
		}


		bool sign_file(const Signer & signer, const path_t & file_path, 
						const std::string & comment,
						const std::string & ext)
		{
			using namespace std;
			using namespace boost;

			if (filesystem::exists(file_path)){
				signature_t signature = file_signature(signer, file_path);

				path_t output_path(file_path);
				output_path += ext;

				ofstream out(output_path.string());
				
				copy(signature.digest.begin(), signature.digest.end(), 
					ostream_iterator<decltype(signature.digest)::value_type>(out, " "));
				if (! comment.empty())
					out << "# " << comment;
				if (out.good()) return true;
			}
			return false;
		}
		
	}
}