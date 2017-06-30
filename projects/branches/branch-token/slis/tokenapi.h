#ifndef tokenapi_8DD031A2_BAB5_45E1_92F2_62FF2BEA9B32
#define tokenapi_8DD031A2_BAB5_45E1_92F2_62FF2BEA9B32
#include <stdbool.h>

/*! C API to be used by language bindings.*/
extern "C"
{
	enum signature_status {
		ok = 0,
		invalid_token = -1,
		invalid_signature = -2,
		io_error = -10
	};

	/*! Compute signature of the file with token file and store the result
		in signature file

		\return 0 on success, -1 on token problem, -2 on I/O problem.
	*/
	int signFile(const char * file_path, //<! path to the file
				 const char * token_path, //<! path to the token file
				 const char * signature_path //<! path to the signature file
				);

	/*! Verify if the file is signed with the token.
	*/
	bool isSignatureValid(const char * file_path, //<! path to the file
						  const char * signature_path, //<! path to the signature file
						  const char * token_path //<! path to the token file
						 ); 

	/*! Verify authenticity of the token in token_path*/
	bool isTokenValid(const char * token_path);



}


#endif // tokenapi_8DD031A2_BAB5_45E1_92F2_62FF2BEA9B32