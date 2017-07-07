
#include "stdafx.h"

#include "signature.hpp"

#include <algorithm>
#include <memory>
#include <fstream>
#include <sstream>

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
			// read as a sequence of bytes
			ifstream inp{ file_path.native(), ios_base::in & ios_base::binary };
			auto tmp_sstream = ostringstream{};
			tmp_sstream << inp.rdbuf();
			auto s = tmp_sstream.str();
			byte_array_t buffer{ s.begin(), s.end() };
			/* A slower, but pretty canonical and idiomatic version would be:
			ifstream inp{file_path.native(), ios_base::in & ios_base::binary};
			byte_array_t buffer(std::istreambuf_iterator<char>{inp}, std::istreambuf_iterator<char>{});
			*/
			inp.close();
			return signer.get_signature(buffer);
		}

		// ? Check if the sigfile was signed by the signer.
		// ? Check if the content of sigfile corresponds to the content of the file (valid signature).
		// ? Check if the computed signature and the sigfile signature are identical.
		bool verify_file_signature(const Signer & signer, 
									const path_t & file_path, 
									const path_t & sigfile_path)
		{
			using namespace std;
			using namespace boost;
			if (! filesystem::exists(file_path)) return false;

			// deduce the signature path if it is empty: build it out ot the file path and the default extension
			path_t signature_file_path{ sigfile_path };
			if (signature_file_path.empty())
			{
				signature_file_path = file_path;
				signature_file_path += sigfile_extension;
			}
			if (!filesystem::exists(signature_file_path)) return false;
			signature::signature_t computed_signature, sigfile_signature;
			// process the signature file
			ifstream inp{ signature_file_path.native() };
			if (inp) {
				typedef decltype(sigfile_signature.digest)::value_type item_type;
				std::istream_iterator<item_type> eos_it, inp_it(inp); 
				std::copy_n(inp_it, sigfile_signature.digest.size(), sigfile_signature.digest.begin());
				if (! inp.good() || inp.fail()) return false;
			}
			computed_signature = file_signature(signer, file_path);
			return computed_signature.digest == sigfile_signature.digest;
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
				if (out) {
					copy(signature.digest.begin(), signature.digest.end(),
						ostream_iterator<decltype(signature.digest)::value_type>(out, " "));
					if (!comment.empty())
						out << "# " << comment;
				}
				if (out.good()) return true;
			}
			return false;
		}
		
	}
}