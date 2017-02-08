// slisadmin.cpp : Defines the entry point for the console application.
//

#include "stdafx.h"
#include <iostream>
#include <string>
#include <boost/date_time/gregorian/gregorian.hpp>
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid.hpp>
#include "slis_v1.hpp"

int _tmain(int argc, _TCHAR* argv[])
{
	// Dummy program. We generate just one license file.
	using namespace slis::slis_v1;
	using namespace boost;
	using namespace boost::gregorian;
	using namespace std;

	int errcode;
	string licname("University of Warwick");
	SlisContainer lic(licname, license_types::research);
	uuids::string_generator gen;
	uuids::uuid alamdmc_feature_uuid = gen("ff921f1e-fa42-11e5-97dc-ecf4bb152acb");

	date from(2016, 2, 22), to(2018, 2, 22);

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

