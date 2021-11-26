#include "stdafx.h"

#include <boost/filesystem.hpp>

#include "tokenapi.h"
#include "token_v1.hpp"


using namespace slis;
using namespace boost::filesystem;

/*! Compute signature of the file with token file and store the result
in signature file.
*/
int signFile(const char * file_path, //<! path to the file
			const char * token_path, //<! path to the token file
			const char * signature_path //<! path to the signature file
			)
{
	return tokens::tokenapi_v1::signFile(path(file_path), path(token_path), path(signature_path));
}

/*! Verify if the file is signed with the token.
*/
bool isSignatureValid(const char * file_path, //<! path to the file
	const char * token_path, //<! path to the token file
	const char * signature_path //<! path to the signature file
	)
{
	return tokens::tokenapi_v1::isSignatureValid(path(file_path), path(token_path), path(signature_path));
}

/*! Verify authenticity of the token in token_path*/
bool isTokenValid(const char * token_path)
{
	return tokens::tokenapi_v1::isTokenValid(path(token_path));
}

