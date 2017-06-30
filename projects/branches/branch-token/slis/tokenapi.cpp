#include "stdafx.h"

#include "tokenapi.h"
#include "signature.hpp"
#include "token_v1.hpp"
#include "token_secret.hpp"


using namespace slis;

/*! Compute signature of the file with token file and store the result
in signature file

\return 0 on success, -1 on token problem, -2 on I/O problem.
*/
int signFile(const char * file_path, //<! path to the file
	const char * token_path, //<! path to the token file
	const char * signature_path //<! path to the signature file
	)
{

	return -1;
}

/*! Verify if the file is signed with the token.
*/
bool isSignatureValid(const char * file_path, //<! path to the file
	const char * signature_path, //<! path to the signature file
	const char * token_path //<! path to the token file
	)
{
	return false;
}

/*! Verify authenticity of the token in token_path*/
bool isTokenValid(const char * token_path)
{
	signature::Signer signer(tokens::constants::token_secret);
	tokens::Token token;

	

	return false;
}

