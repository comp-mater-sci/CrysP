#ifndef token_8E548C6E_FE23_4C59_9EB4_1C72FC03A957
#define token_8E548C6E_FE23_4C59_9EB4_1C72FC03A957

#include <string>
// #include <ctime>

#include <boost/filesystem/path.hpp>
// UUID
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/nil_generator.hpp>
#include <boost/uuid/uuid_serialize.hpp>
// Serialization
#include <boost/archive/text_oarchive.hpp>
#include <boost/archive/text_iarchive.hpp>

#include "signature.hpp"

namespace slis {

	namespace tokens {
		
		typedef boost::uuids::uuid id_type;
		typedef std::string alias_type;
		typedef std::string owner_name_type;
		typedef std::string date_type;
		typedef std::vector<char> payload_type;

		class TokenContent 
		{
		public:
			
			TokenContent()
			{
				id = boost::uuids::nil_uuid();
			}
				

			TokenContent(alias_type alias, owner_name_type owner_name,
						date_type expiry_date, payload_type payload)
						: id(boost::uuids::random_generator()()),
						alias(alias),
						owner_name(owner_name),
					//	creation_time(std::ctime(nullptr),
						expiry_date(expiry_date),
						payload(payload)
			{}
			
			
			id_type				id;
			alias_type			alias;
			owner_name_type		owner_name;
			//date_type			creation_time;
			date_type			expiry_date;
			payload_type		payload;



			// Boost serialization
			template<class Archive>
			void serialize(Archive & ar, const unsigned int version)
			{
				ar & id;
				ar & alias;
				ar & owner_name;
				ar & expiry_date;
				ar & payload;
			}

		};

		class Token
		{

		public:
			// Create a default empty token
			Token() = default;

			// Create an unsigned token
			Token(
				const alias_type & alias,
				const owner_name_type & owner_name,
				const date_type & expiry_date,
				const payload_type & payload)
				: content{ alias, owner_name, expiry_date, payload }
			{
				/*TODO*/
			}


			// Generate and sign a token.
			Token(const slis::signature::Signer & signer,
				  const alias_type & alias,
				  const owner_name_type & owner_name,
				  const date_type & expiry_date, 
				  const payload_type & payload = payload_type())
				  : Token(alias, owner_name, expiry_date, payload)
			{
				signature = signer.get_signature(signable());
			}


			slis::signature::byte_array_t signable() const;

			template<class Archive>
			void serialize(Archive & ar, const unsigned int version)
			{
				ar & signature;
				ar & content;
			}

			// Verify if the token was signed by the signer.
			bool isAuthentic(const slis::signature::Signer & signer) const;
		
			// Read the token
			bool read(std::istream &);

			// Write out the token
			bool write(std::ostream &) const;

			std::string alias() const { return content.alias; }
			id_type id() const { return content.id; }
			date_type expiry_date() const { return content.expiry_date; }

			TokenContent get() const { return content; }

		private:
			slis::signature::signature_t signature;

			TokenContent content;

		};

		
		// Low level functions
		bool readToken(Token & token, const boost::filesystem::path & token_path);


		bool writeToken(const Token & token, const boost::filesystem::path & token_path);

		namespace tokenapi_v1 {

			enum signature_status {
				ok = 0,
				invalid_token = -1,
				invalid_signature = -2,
				io_error = -3,
				error = -10
			};

			// Implementation of tokenapi functions

			int signFile(const boost::filesystem::path & file_path, //<! path to the file
						 const boost::filesystem::path & token_path, //<! path to the token file
						 const boost::filesystem::path & signature_path //<! path to the signature file
						) noexcept;

			/*! Verify if the file is signed with the token.
			*/
			bool isSignatureValid(const boost::filesystem::path & file_path, //<! path to the file
									const boost::filesystem::path & token_path, //<! path to the token file
									const boost::filesystem::path & signature_path//<! path to the signature file
								) noexcept;

			/*! Verify authenticity of the token in token_path*/
			bool isTokenValid(const boost::filesystem::path & token_path) noexcept;

		}

	}




};

#endif
