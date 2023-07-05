#pragma once

#include <cstdint>

namespace newf4 {

// Index of a GB element
using Index = uint32_t;

// Index of a monomial in a monomial hash table.
// 0 is a sentinel, meaning that something that usually would
// point to a monomial is currently undefined.  Valid values are > 0.
using MonomialIndex = uint32_t;

// Data type of the underlying monomial store.
// A monomial is a sequence of MonomialInts
// Also used to store degree of a monomial.
using MonomialInt = uint32_t;

// Number indicating the free module component
using ComponentIndex = uint32_t;

// Value of hashing a monomial (which may or may not include the component)
using HashInt = uint64_t;

// Used for divisibility masks in MonomialLookupTable
using MonomialMask = uint64_t;

}// end namespace f4