// Copyright 2016  Michael E. Stillman

#ifndef _res_monomial_types_hpp_
#define _res_monomial_types_hpp_

#include <cstdint>

enum class MonomialOrderingType { Lex, GRevLex, Weights };

typedef int32_t myword;
typedef myword component_index;

typedef myword res_ntuple_word;
typedef res_ntuple_word* res_ntuple_monomial;
typedef const res_ntuple_word* res_const_ntuple_monomial;

typedef myword res_monomial_word;
typedef res_monomial_word* res_packed_monomial;
typedef const res_monomial_word* res_const_packed_monomial;

typedef myword res_varpower_word;
typedef res_varpower_word* res_varpower_monomial;
typedef const res_varpower_word* res_const_varpower_monomial;
// format: [length, v1, e1, ..., vr, er]
// and v1 > v2 > ... > vr >= 0, and all
// exponents ei > 0.
// and length is 2r+1.
// Operations are defined in VarpowerMonomials

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
