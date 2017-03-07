// slisadmin.cpp : Defines the entry point for the console application.
//

#include "stdafx.h"
#include <iostream>
#include <string>
#include <boost/date_time/gregorian/gregorian.hpp>
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/filesystem.hpp>
#include "slis_v1.hpp"

int _tmain(int argc, _TCHAR* argv[])
{
	// Dummy program. We generate just one license file.
	using namespace slis::slis_v1;
	using namespace boost;
	using namespace boost::gregorian;
	using namespace std;
	using namespace boost::filesystem;

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


	return 0;
}

