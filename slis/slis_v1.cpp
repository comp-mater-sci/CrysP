#include "stdafx.h"
#include "slis_v1.hpp"
#include <cstdlib>
#include <cstring>
#include <fstream>
#include <boost/filesystem/path.hpp>
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/detail/sha1.hpp>
// can be #include <boost/uuid/sha1.hpp> dependant on boost version.


using namespace slis::slis_v1;

// The salt was generated in Python:
// >>> ', '.join([hex(random.randint(0, 255)) for i in xrange(32)])
unsigned char salt[32] = {
	0x12, 0x29, 0xd7, 0x82, 0x74, 0x55, 0x32, 0x2c,
	0xfe, 0xd, 0x4, 0xd0, 0x9e, 0x13, 0x9e, 0xfa,
	0x4c, 0x41, 0xe, 0x2d, 0xe4, 0x1b, 0xb2, 0x56,
	0x94, 0xf1, 0xed, 0x7f, 0x17, 0x44, 0x3d, 0x39
};


SlisContainer::SlisContainer()
{
	std::fill(license_overview.licensee_name,
		license_overview.licensee_name + sizeof(license_overview.licensee_name),
		char(0));
}


SlisContainer::SlisContainer(const std::string & licensee, license_types license_type)
{
	// make sure there is space for \0: leave one char left.
	size_t nchars = std::min(licensee.size(), sizeof(license_overview.licensee_name) - 1);
	std::copy_n(licensee.cbegin(),
		nchars,
		license_overview.licensee_name);
	// make sure padding consists of zeros
	std::fill(license_overview.licensee_name + nchars,
		license_overview.licensee_name + sizeof(license_overview.licensee_name),
		char(0));
	license_overview.license_type = license_type;
}

void SlisContainer::addLicense(boost::uuids::uuid uuid, const std::string & name,
							   const boost::gregorian::date & from, const boost::gregorian::date & to)
{
	slis_record feature_license;
	feature_license.feature_uuid = uuid;
	// TODO: dry this code: next 3 lines can be refactored 
	// into a template where size_t N is the parameter.
	size_t nchars = std::min(name.size(), sizeof(feature_license.name));
	std::copy_n(name.cbegin(), nchars, feature_license.name);
	// make sure padding consist of zeroes.
	std::fill(feature_license.name + nchars, feature_license.name + sizeof(feature_license.name), char(0));
	//
	boost::gregorian::to_iso_string(from).copy(feature_license.valid_from, sizeof(feature_license.valid_from));
	feature_license.valid_from[date_as_text_last] = '\0';
	boost::gregorian::to_iso_string(to).copy(feature_license.valid_to, sizeof(feature_license.valid_to));
	feature_license.valid_to[date_as_text_last] = '\0';
	feature_licenses[uuid]= feature_license;
}

int SlisContainer::getDigest(unsigned int (& digest)[5]) const
{
	boost::uuids::detail::sha1 s;
	s.process_bytes(&license_overview, sizeof(license_overview));
	
	for (auto iblk = feature_licenses.begin(); iblk != feature_licenses.end(); iblk++)
	{
		// std::cout << sizeof(iblk->second) << std::endl;
		s.process_bytes((void*)&(iblk->second), sizeof(iblk->second));
	}
	// put the salt to get the "signed" digest
	s.process_bytes((void*)salt, sizeof(salt));
	s.get_digest(digest);
	//cout << hexdigest(digest) << endl;
	return 0;
}

int SlisContainer::save(const std::string & path) const
{

	const size_t hdr_size = 4, ftr_size = 5;
	const char * hdr = "SLIS"; // 4
	const char * ftr = "ESLIS"; // 5
	int version = 1;
	size_t n_elems;
	unsigned int digest[5] = { 0, 0, 0, 0, 0 };
	
	getDigest(digest);

	std::ofstream out;
	out.exceptions(std::ios::failbit | std::ios::badbit);
	try 
	{
		// we use binary mode.
		out.open(path, std::ios::out | std::ios::binary);
		// header
		out.write(hdr, hdr_size);
		out.write((char*)&version, sizeof(version));
		// meta
		n_elems = feature_licenses.size();
		out.write((char*)&license_overview, sizeof(license_overview));
		out.write((char*)&n_elems, sizeof(n_elems));
		// details
		for (auto iblk = feature_licenses.begin(); iblk != feature_licenses.end(); iblk++)
		{
			// std::cout << sizeof(iblk->second) << " " << sizeof(slis_record) << std::endl;
			out.write((char*)&(iblk->second), sizeof(iblk->second));
		}
		out.write((char*)digest, sizeof(digest));
		// footer
		out.write(ftr, ftr_size);
	}
	catch (std::ofstream::failure e) 
	{
		std::cerr << "Cannot write " << path << std::endl;
		return -1;
	}
	catch(...) 
	{
		return -1;
	}
	return 0;
}

int SlisContainer::load(const std::string & path)
{
	// Expected header/footer data
	const size_t hdr_size = 4 , ftr_size = 5;
	const char * hdr = "SLIS"; // 4
	const char * ftr = "ESLIS"; // 5
	const int version = 1;
	const size_t max_elems = 1024;

	char r_hdr[hdr_size];
	char r_ftr[ftr_size];
	int r_version = 1;
	size_t r_n_elems = 0;
	const size_t digest_size = 5;
	unsigned int digest[digest_size] = { 0, 0, 0, 0, 0 },
				 r_digest[digest_size] = { 0, 0, 0, 0, 0 };

	std::ifstream in;
	in.exceptions(std::ios::failbit | std::ios::badbit | std::ios::eofbit);
	try {
		// we use binary mode.
		in.open(path, std::ios::in | std::ios::binary);
		// read + check the header
		in.read(r_hdr, hdr_size);
		if (!std::equal(hdr, hdr + hdr_size, r_hdr)) throw std::domain_error("Wrong header");
		in.read((char*)&r_version, sizeof(r_version));
		if (version != r_version) throw std::domain_error("Wrong SLIS format version");
		// meta
		in.read((char*)&license_overview, sizeof(license_overview));
		in.read((char*)&r_n_elems, sizeof(r_n_elems));
		if (r_n_elems > max_elems) throw std::domain_error("Wrong number of features");
		// details
		for (size_t i = 0; i < r_n_elems; i++)
		{
			slis_record feature;
			in.read((char*)&feature, sizeof(feature));
			//feature_licenses.push_back(feature);
			feature_licenses[feature.feature_uuid] = feature;
		}
		in.read((char*)r_digest, sizeof(r_digest));
		// read + check the footer
		in.read(r_ftr, ftr_size);
		if (!std::equal(ftr, ftr + hdr_size, r_ftr)) throw std::domain_error("Wrong footer");
	}
	catch (std::ofstream::failure e) 
	{
		std::cerr << "Cannot read " << path << std::endl;
		return -1;
	}
	catch (std::domain_error e)
	{
		std::cerr << "Unexpected format of the license file " << path 
				  << ": " << e.what() << std::endl;
		return -1;
	}
	catch (...)
	{
		return -1;
	}
	// compare the digests
	getDigest(digest);
	return (std::equal(digest, digest + digest_size, r_digest) ? 0 : -1);
}


license_status SlisContainer::checkLicense(const boost::uuids::uuid & uuid) const
{
	license_status result = license_status::missing;
	try
	{
		slis_record feature = feature_licenses.at(uuid);
		// check the license:
		using namespace boost::gregorian;
		
		date today = day_clock::local_day(),
			from(from_undelimited_string(feature.valid_from)),
			to(from_undelimited_string(feature.valid_to));

		if ((from <= today) && (to >= today))
			result = license_status::valid;
		else
			result = license_status::expired;

	}
	catch (std::out_of_range)
	{
		return license_status::missing;
	}
	return result;
}


void SlisContainer::printLicenseSummary(std::ostream & out, const boost::uuids::uuid & uuid) const
{
	try
	{
		switch(checkLicense(uuid))
		{
		case(valid) :
			out << "This software is licensed to: " << license_overview.licensee_name << std::endl
				<< "License type: ";
			switch (license_overview.license_type)
			{
			case(demonstration) :
				out << "DEMONSTRATION";
				break;
			case(evaluation):
				out << "EVALUATION";
				break;
			case(research):
				out << "RESEARCH PURPOSES ONLY";
				break;
			case(commercial):
				out << "COMMERCIAL";
				break;
			default:
				out << "(unknown)";
				break;
			}
			out << std::endl;
			break;
		case(missing) :
			out << "No valid license for this software product." << std::endl;
			break;
		case(expired) :
			try
			{
				using namespace boost::gregorian;
				slis_record feature = feature_licenses.at(uuid);
				out << "The license has expired on " 
					<< to_iso_extended_string(from_undelimited_string(feature.valid_to)) << std::endl;
			}
			catch (std::out_of_range)
			{
				out << "The license has expired." << std::endl;
			}
			break;
		}
	}
	catch (...)
	{
		out << "An error has occured while processing the license." << std::endl;
	}
	
}

Slis::Slis()
	:license_loaded(false)
{
	/* Empty */
}

int Slis::load_license(const std::string & envvar)
{
	char * buffer = std::getenv(envvar.c_str());
	if (!(buffer && std::strlen(buffer) > 0)) return -1;
	// Process the content
	boost::filesystem::path file_path(buffer);
	file_path /= license_fname;
	// try to get the license
	int result = licenses.load(file_path.string());
	license_loaded = (result == 0);
	std::cout << "file_path.string(): " << file_path.string() << "  " << license_loaded << std::endl;
	return result;

}


bool Slis::check_license(const std::string & uuid, bool print_output)
{
	if (!license_loaded) return false;
	try
	{
		using namespace boost;
		uuids::string_generator gen;
		uuids::uuid feature_uuid = gen(uuid);

		if (print_output) {
			licenses.printLicenseSummary(std::cout, feature_uuid);
			std::cout << std::flush;
		}
		return (licenses.checkLicense(feature_uuid) == license_status::valid ? true : false);
	}
	catch (...)
	{
		return false;
	}
	return false;
}
