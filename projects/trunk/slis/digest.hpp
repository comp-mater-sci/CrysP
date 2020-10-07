#ifndef digest_EB90DE58_9062_4FCE_83B7_989412BF2F17
#define digest_EB90DE58_9062_4FCE_83B7_989412BF2F17

#include <string>
#include <array>
#include <type_traits>

#include <boost/uuid/detail/sha1.hpp>
// can be #include <boost/uuid/sha1.hpp> for older boost version.

namespace slis {
	namespace digest {

		// See definition in as defined in <boost/uuid/sha1.hpp>:
		// typedef unsigned int(&digest_type)[5];
		// Yes, with explicit constant '5'.
		typedef boost::uuids::detail::sha1::digest_type digest_type;

		// as defined in boost::uuids::detail::sha1, but without reference.
		typedef unsigned int(digest_obj_type)[5]; 

		
		/*Return string with hexadecimad representation of the digest*/
		std::string hexdigest(const digest_type digest);

	}

}


#endif // digest_EB90DE58_9062_4FCE_83B7_989412BF2F17