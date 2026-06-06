// Copyright 2016  Michael E. Stillman

#ifndef _res_monomial_types_hpp_
#define _res_monomial_types_hpp_

#include <cstdint>

#include "ExponentList.hpp"
#include "ExponentVector.hpp"

enum class MonomialOrderingType { Lex, GRevLex, Weights };

typedef int32_t myword;
typedef myword component_index;

// Legacy specialization for ExponentVector
using res_ntuple_monomials = ExponentVector<myword, false>;

typedef res_ntuple_monomials::Exponent res_ntuple_word;
typedef res_ntuple_word *res_ntuple_monomial;
typedef const res_ntuple_word *res_const_ntuple_monomial;

// Legacy specialization for ExponentList
using res_varpower_monomials = ExponentList<myword, false>;
using index_res_varpower_monomial = ExponentListIterator<myword, false>;

typedef res_varpower_monomials::Exponent res_varpower_word;
typedef res_varpower_word *res_varpower_monomial;
typedef const res_varpower_word *res_const_varpower_monomial;


typedef myword res_monomial_word;
typedef res_monomial_word* res_packed_monomial;
typedef const res_monomial_word* res_const_packed_monomial;

// The following is possibly out of date information
// format: [hash,component,e1,...,envars],
// where [e1,...,envars] is packed.
// OR: [hash,component,weight,e1,...,envars]
// and weight is NOT packed.
// packing info, hash values, weights are all
// defined in: PackedMonomials
// with weight vector values:
// [hashvalue comp w1 w2 ... wr e1 e2 ... en]
// or is it:
// [hashvalue comp e1 e2 ... en -wr ... -w1]

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
