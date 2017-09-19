// slistest.cpp : Defines the entry point for the console application.
//

#include "stdafx.h"
#include <string>
#include <iostream>
#include <sstream>
#include <map>
#include <iterator>
#include <boost/date_time/gregorian/gregorian.hpp>
#include <boost/uuid/string_generator.hpp>

#include <boost/uuid/uuid.hpp>
#include <boost/uuid/sha1.hpp>

#include <boost/archive/text_oarchive.hpp>
#include <boost/archive/text_iarchive.hpp>

#include <boost/archive/xml_oarchive.hpp>


// needed for the serialized STL containers
#include <boost/serialization/array.hpp>
#include <boost/serialization/vector.hpp>

// needed for uuid serialization
#include <boost/uuid/uuid_serialize.hpp>
#include <boost/uuid/uuid_io.hpp>

#ifndef SLIS_DLL
// slis components
#include "slis_v1.hpp"

#include "digest.hpp"
#include "signature.hpp"
#include "token_v1.hpp"
#include "signature_serialization.h"
#endif // !SLIS_DLL

#include "slisapi.h"
#include "tokenapi.h"

#include "token_secret.hpp" // to be removed

using namespace std;


/*
	Testset for the internals 
*/

#ifndef SLIS_DLL


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





void test_sha1()
{

	using namespace slis::digest;

	boost::uuids::detail::sha1 s;
	std::string a = "Some sample string";
	s.process_bytes(a.c_str(), a.size());

	digest_obj_type digest;

	s.get_digest(digest);
	cout << slis::digest::hexdigest(digest) << endl;

	// test multi-buffer

	boost::uuids::detail::sha1 sx;
	std::string s1 = "Some sample",
				s2 = " string";
	sx.process_bytes(s1.c_str(), s1.size());
	sx.process_bytes(s2.c_str(), s2.size());

	digest_obj_type digestx;

	sx.get_digest(digestx);
	cout << slis::digest::hexdigest(digestx) << endl;


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

#endif // !SLIS_DLL

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

#ifndef SLIS_DLL


// Secret for testing & development purposes
// ', '.join([hex(random.randint(0, 255)) for i in xrange(32)])
const  slis::signature::byte_array_t devel_secret(
{
	0x80, 0x9a, 0x74, 0x76, 0x18, 0x42, 0xc2, 0x90,
	0x1b, 0xc4, 0x12, 0x65, 0x21, 0xef, 0x23, 0xb8,
	0xe0, 0x13, 0x34, 0x45, 0xc3, 0x38, 0x8, 0x9e,
	0xd6, 0x3, 0xc1, 0x2a, 0x5e, 0x45, 0xf4, 0x2f
});


const  slis::signature::byte_array_t devel_secret_tampered(
{
	// |<-- just this one
	0x81, 0x9a, 0x74, 0x76, 0x18, 0x42, 0xc2, 0x90,
	0x1b, 0xc4, 0x12, 0x65, 0x21, 0xef, 0x23, 0xb8,
	0xe0, 0x13, 0x34, 0x45, 0xc3, 0x38, 0x8, 0x9e,
	0xd6, 0x3, 0xc1, 0x2a, 0x5e, 0x45, 0xf4, 0x2f
});


void test_signer()
{
	using namespace slis::signature;
	using namespace slis;



	std::string str1("some sample string to be signed");
	byte_array_t var1, var1_tampered;

	

	var1 = from_string(str1);
	var1_tampered = from_string(str1);
	// tamper the data
	var1_tampered[1] = 'a';


	Signer signer(devel_secret);
	auto sig1 = signer.get_signature(var1);

	//cout << digest::hexdigest(sig1) << endl;

	cout<< "signature valid on original data: "<< signer.verify_signature(var1, sig1) << endl
		<< "signature valid on tampered data: "<< signer.verify_signature(var1_tampered, sig1) << endl;


	auto file_sig = file_signature(signer, "test_file.txt");

	//cout << "test_file.txt: " << digest::hexdigest(file_sig) << endl;

	auto file_sig_none = file_signature(signer, "test_file_nonexisting.txt");

	//cout << "test_file_nonexisting.txt: " << digest::hexdigest(file_sig_none) << endl;

	auto signing_success = sign_file(signer, "test_file.txt", "testing");
	auto verifying_success = verify_file_signature(signer, "test_file.txt");
}


void test_boost_archive()
{

	using namespace boost;
	uuids::string_generator gen;
	std::ostringstream ofs;

	boost::archive::text_oarchive oa(ofs);

	std::array<unsigned int, 5> x{ { 1, 2, 3, 4, 5 } };

	uuids::uuid token_uuid1 = gen("{01234567-89ab-cdef-0123-456789abcdef}");

	oa & x;
	oa & token_uuid1;
	ofs.flush();

	cout << ofs.str() << endl;

}




void test_token_archive()
{
	using namespace boost;
	
	std::ostringstream ofs;
	boost::archive::text_oarchive oa(ofs);
	//boost::archive::xml_oarchive oa(ofs);
	slis::tokens::Token a_token;
	

	oa & a_token;

	ofs.flush();

	cout << ofs.str() << endl;
}


void test_token_making()
{
	using namespace slis::signature;
	using namespace slis::tokens;

	Signer signer(devel_secret);
	Signer forger(devel_secret_tampered);

	{
		std::string token_path("token1.slistkn");
		Token token{};
		auto status = writeToken(token,token_path.c_str());
		cout << (status ? "OK" : "FAILED") << endl;

	}
	{
		Token token{ signer, "token2", "development", "20170930" };
		auto status = writeToken(token, "token2.slistkn");
		auto authentic = token.isAuthentic(signer);
		cout << "status: " << status << " authentic:" << authentic << endl;
		cout << (status && authentic ? "OK" : "FAILED") << endl;
	}
	{
		// read token and check authenticity
		Token token;
		auto status = readToken(token, "token2.slistkn");
		auto authentic = token.isAuthentic(signer);
		cout << "status: " << status << " authentic:" << authentic << endl;
		cout << (status && authentic ? "OK" : "FAILED") << endl;
	}

	{
		// read token and check authenticity
		Token token;
		auto status = readToken(token, "token2.slistkn");
		auto authentic = token.isAuthentic(forger);
		cout << "status: " << status << " authentic:" << authentic << endl;
		cout << (status && !authentic ? "OK" : "FAILED") << endl;
	}
	
	// read non-exisitng token
	{
		Token token;
		auto status = readToken(token, "non_existing_token.slistkn");
		auto authentic = token.isAuthentic(signer);
		cout << "status: " << status << " authentic:" << authentic << endl;
		cout << (!(status || authentic) ? "OK" : "FAILED") << endl;
	}
	{
		// read a malformed token
		Token token;
		auto status = readToken(token, "malformed_token.slistkn");
		auto authentic = token.isAuthentic(signer);
		cout << "status: " << status << " authentic:" << authentic << endl;
		cout << (!(status || authentic) ? "OK" : "FAILED") << endl;
	}

}


void make_authentic_token()
{
	using namespace slis::signature;
	using namespace slis::tokens;

	// Create a token that can be used by token API
	Signer signer(constants::token_secret);
	Token token{ signer, "authentic_token", "development", "20170930"};
	auto status = writeToken(token, "authentic_token.slistkn");
	auto authentic = token.isAuthentic(signer);


}
#endif // !SLIS_DLL

void test_token_api() 
{
	// signing with non-authentic/wrong token
	{
		int exitcode = signFile("datafile.txt", "test_token.slistkn","");
		cout << "Signing file with non-authentic token: " << exitcode <<  " "
			 << (exitcode != 0? "OK" : "FAILED") << endl;
	}

	// signing with authentic token
	{
		int exitcode = signFile("datafile.txt", "authentic_token.slistkn", "");
		cout << "Signing file: " << exitcode << " "
			<< (exitcode == 0 ? "OK" : "FAILED") << endl;
	}

	// signing with authentic token, explicit signature name
	{
		int exitcode = signFile("datafile.txt", "authentic_token.slistkn", "datafile_arbitrary.txt.slissig");
		cout << "Signing file: " << exitcode << " "
			<< (exitcode == 0 ? "OK" : "FAILED") << endl;
	}


	// Checking signature with wrong token
	{
		cout << "isSignatureValid with wrong token:" 
			<< (isSignatureValid("datafile.txt", "test_token.slistkn", "") ? "FAILED" : "OK") << endl;
	}

	// Checking signature with proper token
	{
		cout << "isSignatureValid with wrong token:"
			<< (isSignatureValid("datafile.txt", "authentic_token.slistkn", "") ? "OK" : "FAILED") << endl;
	}

	// Checking malformed signature with proper token
	{
		cout << "isSignatureValid with malformed signature:"
			<< (isSignatureValid("datafile.txt", "authentic_token.slistkn", "datafile.txt.slissig.malformed") ? "FAILED" : "OK") << endl;
	}

	// Checking non-existing signature with proper token
	{
		cout << "isSignatureValid with non-existing signature:"
			<< (isSignatureValid("datafile.txt", "authentic_token.slistkn", "datafile.txt.nonexisting.slissig") ? "FAILED" : "OK") << endl;
	}


}



void sign_files()
{
	int exitcode = signFile("sid1687f.smt", "authentic_token.slistkn", "");

	exitcode = signFile("sid1687f_udsa_0_000.CUR", "authentic_token.slistkn", "");
}

int _tmain(int argc, _TCHAR* argv[])
{
	test_token_api();

#ifndef SLIS_DLL
	// make_authentic_token();
	sign_files();
	test_token_making();
	test_boost_archive();
	test_token_archive();
#endif
	test_api();

#ifndef SLIS_DLL
	test_uuid();
	test_sha1();
	date_test();
	slis_test();
	test_salt();
	test_signer();
#endif
	return 0;
}

