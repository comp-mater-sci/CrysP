#ifndef tokenapi_8DD031A2_BAB5_45E1_92F2_62FF2BEA9B32
#define tokenapi_8DD031A2_BAB5_45E1_92F2_62FF2BEA9B32
#include <stdbool.h>

/*! token C API to be used by language bindings.*/
extern "C"
{

	/*! Compute signature of the file with token file and store the result
		in signature file

		\return 0 on success, 
		\return -1 on token problem
		\return -2 on signature problem,
		\return -3 on I/O problem
		\return -10 on a general error.
	*/
	int signFile(const char * file_path, //<! path to the file
				 const char * token_path, //<! path to the token file
				 const char * signature_path //<! path to the signature file
				);

	/*! Verify if the file is signed with the token.

	\return true if the signature is valid.
	*/
	bool isSignatureValid(const char * file_path, //<! path to the file
						  const char * token_path, //<! path to the token file
						  const char * signature_path //<! path to the signature file
		);

	/*! Verify authenticity of the token in token_path
	
	\return true is token is valid.
	*/
	bool isTokenValid(const char * token_path);



}


#endif // tokenapi_8DD031A2_BAB5_45E1_92F2_62FF2BEA9B32