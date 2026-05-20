// Copyright 2016  Michael E. Stillman

#ifndef _res_monomial_types_hpp_
#define _res_monomial_types_hpp_

/**
 * @file schreyer-resolution/res-monomial-types.hpp
 * @brief Typed-monomial vocabulary shared by `ResMonoid`, `ResPolyRing`, `SchreyerFrame`, and `F4Res`.
 *
 * Declares the typedefs the F4 resolution layer uses in place
 * of raw `int*`. `myword = int32_t` is the per-slot integer;
 * `res_ntuple_monomials = ExponentVector<myword, false>` is
 * the dense exponent-vector specialisation and
 * `res_varpower_monomials = ExponentList<myword, false>` the
 * sparse `(variable, exponent)` list specialisation. From each
 * the `_word` / `_monomial` / `_const_*` typedefs spell out
 * the value / mutable-pointer / const-pointer flavours so
 * callers pass the right `const`-ness without restating the
 * template. `res_monomial_word` / `res_packed_monomial` /
 * `res_const_packed_monomial` name the packed monomial layout
 * used by `ResMonoidDense` and `ResMonoidSparse`, and the
 * `MonomialOrderingType` enum tags the supported orderings.
 *
 * The trailing comment block records the (possibly stale)
 * packed-monomial encoding `[hash, component, weights...,
 * exponents...]` --- the F4-side `f4-types.hpp` cousin uses
 * the same shape.
 *
 * @see ExponentVector.hpp
 * @see ExponentList.hpp
 * @see res-moninfo.hpp
 * @see res-poly-ring.hpp
 * @see f4/f4-types.hpp
 */

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
