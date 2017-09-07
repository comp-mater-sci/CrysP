// slistest.cpp : Defines the entry point for the console application.
//

#include "stdafx.h"
#include <string>
#include <iostream>
#include <sstream>
#include <map>
#include <boost/date_time/gregorian/gregorian.hpp>
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/sha1.hpp>
#include "slis_v1.hpp"
#include "slisapi.h"

using namespace std;


void date_test()
{
	using namespace boost;

	using namespace boost::gregorian;
	date start_date(2015, 01, 01);
	date end_date(2017, Feb, 1);
	date today = day_clock::local_day();

	if (end_date >= today)
	{
		cout << "OK" << endl;
	} //date comparison operators 

	std::string ds1("2002/1/25");
	date dds1(from_simple_string(ds1));
	cout << dds1 << endl;

	std::string ds2("2004-1-25");
	date dds2(from_simple_string(ds2));
	cout << dds2 << endl;

	cout << to_iso_string(dds2) << endl;

	// from date_time examples:
	// The following date is in ISO 8601 extended format (CCYY-MM-DD)
	std::string ud("20011009"); //2001-Oct-09
	date d1(from_undelimited_string(ud));
	std::cout << to_iso_extended_string(d1) << std::endl;
}


std::string hexdigest(const unsigned int digest[5])
{
	// based on the code found here: https://gist.github.com/jhasse/990731
	char hash[20];
	for (size_t i = 0; i < 5; ++i)
	{
		const char* tmp = reinterpret_cast<const char*>(digest);
		hash[i * 4] = tmp[i * 4 + 3];
		hash[i * 4 + 1] = tmp[i * 4 + 2];
		hash[i * 4 + 2] = tmp[i * 4 + 1];
		hash[i * 4 + 3] = tmp[i * 4];
	}
	ostringstream buf;
	buf << std::hex;
	for (size_t i = 0; i < sizeof(hash); ++i)
	{
		buf << ((hash[i] & 0x000000F0) >> 4)
			<< (hash[i] & 0x0000000F);
	}
	return buf.str();
}


void test_sha1()
{
	boost::uuids::detail::sha1 s;
	std::string a = "Some sample string";
	s.process_bytes(a.c_str(), a.size());

	unsigned int digest[5];

	s.get_digest(digest);
	cout << hexdigest(digest) << endl;

	// test multi-buffer

	boost::uuids::detail::sha1 sx;
	std::string s1 = "Some sample",
				s2 = " string";
	sx.process_bytes(s1.c_str(), s1.size());
	sx.process_bytes(s2.c_str(), s2.size());

	unsigned int digestx[5];

	sx.get_digest(digestx);
	cout << hexdigest(digestx) << endl;


}

void test_salt()
{
	// Python: ', '.join([hex(random.randint(0, 255)) for i in xrange(32)])
	unsigned char salt[32] = {
		0x12, 0x29, 0xd7, 0x82, 0x74, 0x55, 0x32, 0x2c,
		0xfe, 0xd,  0x4,  0xd0, 0x9e, 0x13, 0x9e, 0xfa,
		0x4c, 0x41, 0xe,  0x2d, 0xe4, 0x1b, 0xb2, 0x56,
		0x94, 0xf1, 0xed, 0x7f, 0x17, 0x44, 0x3d, 0x39
	};

}



void slis_test()
{
	using namespace slis::slis_v1;
	using namespace boost;
	int errcode;
	string licname("Univ of Somewhere");
	SlisContainer lic(licname, license_types::research);

	// make data for the license file
	uuids::string_generator gen;

	uuids::uuid feature_uuid = gen("{01234567-89ab-cdef-0123-456789abcdef}"),
				feature_uuid3 = gen("{a1c14f2e-f9dc-11e5-b5ad-ecf4bb152acb}");
	gregorian::date from(2015, 1, 1), to(2016, 5, 1);
	lic.addLicense(feature_uuid, "feature1", from, to);
	string licensefile_path("tests\\license.slis");
	errcode = lic.save(licensefile_path);
	cout << "save returned " << errcode << endl;
	SlisContainer r_lic;
	errcode = r_lic.load(licensefile_path);
	cout << "load returned " << errcode << endl;

	r_lic.printLicenseSummary(cout, feature_uuid);

}

void test_uuid()
{
	using namespace boost;
	uuids::string_generator gen;
	uuids::uuid feature_uuid1 = gen("{01234567-89ab-cdef-0123-456789abcdef}"),
				feature_uuid2 = gen("01234567-89ab-cdef-0123-456789abcdef"),
				feature_uuid3 = gen("{a1c14f2e-f9dc-11e5-b5ad-ecf4bb152acb}");

	cout << "f1 == f2 ?" << (feature_uuid1 == feature_uuid2 ? "yes" : "no") << endl;
	cout << "f1 == f3 ?" << (feature_uuid1 == feature_uuid3 ? "yes" : "no") << endl;

}


void test_map()
{
	struct X 
	{
		boost::uuids::uuid uuid;
		int x;
		char y;
	};
	std::map<boost::uuids::uuid, X> mapping;

	for (auto iter = mapping.begin(); iter != mapping.end(); iter++)
	{ }
}


void test_api()
{
	std::string env_var("PRODUCT_ROOT");
	int result;

	result = initSlis(env_var.c_str());
	cout << "initSlis: " << result << endl;

	bool is_license_valid;
	is_license_valid = isLicenseValid("01234567-89ab-cdef-0123-456789abcdef", true);
	cout << "isLicenseValid: " << (is_license_valid ? "true" : "false") << endl;
}

int _tmain(int argc, _TCHAR* argv[])
{
	test_api();

	test_uuid();
	test_sha1();
	date_test();
	slis_test();

	test_salt();

	return 0;
}

