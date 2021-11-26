// slisadmin.cpp : Defines the entry point for the console application.
//

#include "stdafx.h"
#include <iostream>
#include <string>
#include <map>
#include <boost/date_time/gregorian/gregorian.hpp>
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>

// #include <boost/filesystem.hpp>
#include "slis_v1.hpp"
#include "token_v1.hpp"
#include "token_secret.hpp"

int main(int argc, char * argv[])
{
	// Dummy program. We generate just one license file.
	using namespace slis::slis_v1;
	using namespace boost;
	using namespace boost::gregorian;
	using namespace std;
	// using namespace boost::filesystem;

#ifndef HMS_LICENSES


	int errcode;
	uuids::string_generator gen;
#ifdef LICENSE_WARWICK
	string licname("University of Warwick");
	SlisContainer lic(licname, license_types::research);

	uuids::uuid alamdmc_feature_uuid = gen("ff921f1e-fa42-11e5-97dc-ecf4bb152acb");
	date from(2016, 2, 22), to(2018, 2, 22);
#endif

#ifdef LICENSE_ALERIS_EVALUATION
	string licname("Aleris Aluminum Duffel bvba");
	SlisContainer lic(licname, license_types::evaluation);
	uuids::uuid alamdmc_feature_uuid = gen("ff921f1e-fa42-11e5-97dc-ecf4bb152acb");
	// date from(2016, 6, 15), to(2016, 7, 20); // original license 
	// date from(2016, 6, 15), to(2016, 8, 31);  // extended license;
	date from(2016, 10, 5), to(2016, 10, 15);  // 2nd extended license;
#endif

#ifdef LICENSE_ALERIS_COMMERCIAL
	string licname("Aleris Aluminum Duffel bvba");
	SlisContainer lic(licname, license_types::commercial);
	uuids::uuid alamdmc_feature_uuid = gen("ff921f1e-fa42-11e5-97dc-ecf4bb152acb");
	// Note: the license agreement says: from 2017/03/01 for 6 months.
	// The license is provided on 2017/03/8
	date from(2017, 03, 01), to(2017, 9, 8); 
#endif

#ifdef LICENSE_ALERIS_COMMERCIAL_EXT
	string licname("Aleris Aluminum Duffel bvba");
	SlisContainer lic(licname, license_types::commercial);
	uuids::uuid alamdmc_feature_uuid = gen("ff921f1e-fa42-11e5-97dc-ecf4bb152acb");
	// Note: the license agreement says: from 2017/03/01 for 6 months.
	// The license is provided on 2017/03/8
	// Note 2: following C. Bollmann's request, the license is extended till
	// the end of September 2017.
	date from(2017, 03, 01), to(2017, 9, 30);
#endif

#ifdef LICENSE_ALERIS_COMMERCIAL_EXT2
	// License for the preparations of the Numisheet benchmark.
	// Issued prior to signing the license agreement.
	string licname("Aleris Aluminum Duffel bvba");
	SlisContainer lic(licname, license_types::commercial);
	uuids::uuid alamdmc_feature_uuid = gen("ff921f1e-fa42-11e5-97dc-ecf4bb152acb");
	date from(2018, 2, 1), to(2018, 3, 1);
#endif

#ifdef LICENSE_ALERIS_COMMERCIAL_EXT3
	string licname("Aleris Aluminum Duffel bvba");
	SlisContainer lic(licname, license_types::commercial);
	uuids::uuid alamdmc_feature_uuid = gen("ff921f1e-fa42-11e5-97dc-ecf4bb152acb");
	date from(2018, 2, 1), to(2019, 2, 1);
#endif


#ifdef LICENSE_OCAS_EVALUATION
	string licname("Onderzoekscentrum voor Aanwending van Staal NV");
	SlisContainer lic(licname, license_types::evaluation);
	uuids::uuid alamdmc_feature_uuid = gen("ff921f1e-fa42-11e5-97dc-ecf4bb152acb");
	date from(2016, 11, 7), to(2016, 12, 8);
#endif	

#ifdef LICENSE_TUDELFT
	string licname("Technische Universiteit Delft");
	SlisContainer lic(licname, license_types::research);
	uuids::uuid alamdmc_feature_uuid = gen("ff921f1e-fa42-11e5-97dc-ecf4bb152acb");
	date from(2016, 9, 1), to(2018, 9, 30);
#endif

#ifdef LICENSE_TUAT
	string licname("Tokyo University of Agriculture and Technology");
	SlisContainer lic(licname, license_types::research);
	uuids::uuid alamdmc_feature_uuid = gen("ff921f1e-fa42-11e5-97dc-ecf4bb152acb");
	date from(2016, 11, 8), to(2018, 11, 8);
#endif

#ifdef LICENSE_UNAM
	string licname("Universidad Nacional Autonoma de Mexico");
	SlisContainer lic(licname, license_types::research);
	uuids::uuid alamdmc_feature_uuid = gen("ff921f1e-fa42-11e5-97dc-ecf4bb152acb");
	date from(2017, 1, 20), to(2019, 2, 1);
#endif

#ifdef LICENSE_KULEUVEN_INTERNAL
	string licname("KU Leuven (internal research)");
	SlisContainer lic(licname, license_types::research);
	uuids::uuid alamdmc_feature_uuid = gen("ff921f1e-fa42-11e5-97dc-ecf4bb152acb");
	date from(2016, 7, 14), to(2016, 9, 20);
#endif

#ifdef LICENSE_MITSUBISHI_EVALUATION
	string licname("Mitsubishi Materials Corporation");
	SlisContainer lic(licname, license_types::evaluation);
	uuids::uuid alamdmc_feature_uuid = gen("ff921f1e-fa42-11e5-97dc-ecf4bb152acb");
	date from(2017, 9, 1), to(2017, 9, 25);
#endif

#ifdef LICENSE_MITSUBISHI_COMMERCIAL
	string licname("Mitsubishi Materials Corporation");
	SlisContainer lic(licname, license_types::commercial);
	uuids::uuid alamdmc_feature_uuid = gen("ff921f1e-fa42-11e5-97dc-ecf4bb152acb");
	date from(2017, 9, 1), to(2099, 12, 31);
	date token_expiry(2019, 9, 1);
	size_t ntokens = 20;
#endif

	lic.addLicense(alamdmc_feature_uuid, "alamDMC", from, to);
	string licensefile_path("license.slis");
	lic.printLicenseSummary(cout, alamdmc_feature_uuid);
	errcode = lic.save(licensefile_path);
	cout << "save returned " << errcode << endl;
	//
	cout << "Checking the license: ";
	SlisContainer r_lic;
	errcode = r_lic.load(licensefile_path);
	cout << "load returned " << errcode << endl;

	r_lic.printLicenseSummary(cout, alamdmc_feature_uuid);

#ifdef GENERATE_TOKENS
	{
		using namespace slis::tokens;
		using namespace slis::signature;
		using namespace boost::uuids;
		Signer signer(constants::token_secret);

		string report_fname = "tokens.csv";
		ofstream out{ report_fname };

		out << "fname" << ","
			<< "alias" << ","
			<< "id" << ","
			<< "expiry_date" << ","
			<< "status" << endl;

		for (size_t i = 1; i <= ntokens; i++) {
			string alias = "token_" + to_string(i);
			auto output_fname = alias + ".slistkn";
			// Note: to_iso_extended_string produces YYYY-MM-DD, to_iso_string produces YYYYMMDD
			Token token{ signer, alias, licname,to_iso_extended_string(token_expiry)};

			// write out and validate
			auto status = writeToken(token, output_fname);
			auto authentic = tokenapi_v1::isTokenValid(output_fname);

			out << output_fname << ","
				<< token.alias() << "," 
				<< token.id() << ","
				<< token.expiry_date() << ","
				<< (status && authentic ? "OK" : "FAILED")<< endl;

		}
	}

#endif // GENERATE_TOKENS


#else // HMS_LICENSES is defined

	/*

	HMS licenses

	*/

	int errcode;
	uuids::string_generator gen;

	// Map: feature_name -> feature_uuid
	map<string, uuids::uuid> feature_map = { 
		{"alamDMC" , gen("ff921f1e-fa42-11e5-97dc-ecf4bb152acb") },
		{"vumat_ps", gen("03c3bba0-b3ff-426b-bab0-c19d888d21b3") },
		{"vumat_3d", gen("61f7d842-0c1e-4d8d-a816-0dc275fe386a") },
		{ "fng",     gen("98c7a26d-ffa9-4803-859a-a16e942002a2") }
	};


#ifdef LICENSE_MPIE
	string licname("Max-Planck-Institut fur Eisenforschung GmbH");
	SlisContainer lic(licname, license_types::research);
	date from(2018, 3, 22), to(2019, 3, 22);
#endif

#ifdef LICENSE_KUL
	string licname("KU Leuven - essentially eternal license");
	SlisContainer lic(licname, license_types::research);
	date from(2018, 3, 22), to(2099, 12, 31);
#endif

	for (auto item : feature_map)
		lic.addLicense(item.second, item.first, from, to);

	string licensefile_path("license.slis");
	errcode = lic.save(licensefile_path);
	std::cout << "save returned " << errcode << endl;
	//
	std::cout << "Checking the license: ";
	SlisContainer r_lic;
	errcode = r_lic.load(licensefile_path);
	std::cout << "load returned " << errcode << endl;

	for (auto item : feature_map)
	{
		std::cout << item.first << std::endl;
		r_lic.printLicenseSummary(std::cout, item.second);
	}

#endif // HMS_LICENSES


	return 0;
}

