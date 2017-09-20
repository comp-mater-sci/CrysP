#ifndef signature_serialization_2FC5C637_6B4F_4C16_B464_4B8881F1D84D
#define signature_serialization_2FC5C637_6B4F_4C16_B464_4B8881F1D84D

#include <boost/serialization/array.hpp>
#include "signature.hpp"

namespace boost {
	namespace serialization {

		// To be updated on any change to signature_t.
		template<class Archive>
		void serialize(Archive & ar, slis::signature::signature_t & t, const unsigned int version)
		{
			ar & t.digest;
		}

	} // namespace serialization
} // namespace boost


#endif
