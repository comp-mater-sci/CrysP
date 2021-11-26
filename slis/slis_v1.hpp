#ifndef slis_v1_07116DD3_0D5E_4CCC_8ABC_EBF0C6EE2F77
#define slis_v1_07116DD3_0D5E_4CCC_8ABC_EBF0C6EE2F77
#include <algorithm>
#include <vector>
#include <list>
#include <string>
#include <map>
#include <boost/uuid/uuid.hpp>
#include <boost/date_time/gregorian/gregorian.hpp>

namespace slis {

	namespace slis_v1{

		const size_t short_text_lenght = 32;
		const size_t long_text_length = 128;
		const size_t date_as_text_length = 8+1; // e.g. "20150130" (incl. '\0'
		const size_t date_as_text_last = date_as_text_length - 1; //index of last character

		enum license_types{
			demonstration,
			evaluation,
			research,
			commercial
		};

		enum license_status{
			missing = -1,
			valid,
			expired
		};


		class SlisContainer{

			struct slis_license {
				char licensee_name[long_text_length];
				license_types license_type;
			};

			struct slis_record {
				boost::uuids::uuid feature_uuid;
				char name[short_text_lenght];
				char valid_from[date_as_text_length];
				char valid_to[date_as_text_length];
			};

			slis_license license_overview;
			//std::vector<slis_record> feature_licenses;
			std::map<boost::uuids::uuid, slis_record> feature_licenses;
			
			int getDigest(unsigned int(&digest)[5]) const;


		public:
			SlisContainer();

			SlisContainer(const std::string & licensee, license_types license_type);
			
			void addLicense(boost::uuids::uuid uuid, const std::string & name, 
							const boost::gregorian::date & from, const boost::gregorian::date & to);
			
			int save(const std::string & path) const;
			
			int load(const std::string & path);

			license_status checkLicense(const boost::uuids::uuid & uuid) const;

			void printLicenseSummary(std::ostream & out, const boost::uuids::uuid & uuid) const;

		};

		const std::string license_fname = "license.slis";


		class Slis{

			bool license_loaded;

			SlisContainer licenses;

		public:
			Slis();

			int load_license(const std::string & envvar);
			bool check_license(const std::string & uuid, bool print_output);
		};

	}

}


#endif // !slis_v1_07116DD3_0D5E_4CCC_8ABC_EBF0C6EE2F77
