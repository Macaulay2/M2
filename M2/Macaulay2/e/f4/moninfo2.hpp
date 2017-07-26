// Copyright 2005  Michael E. Stillman

#ifndef _moninfo_h_
#define _moninfo_h_

#include <cstdio>

#include "config.h"
#include <stdio.h>
#if HAVE_STDINT_H
#include <stdint.h>
#elif HAVE_INTTYPES_H
#include <inttypes.h>
#else
#error integer type definitions not available
#endif

#include "../../d/M2types.h"

#include "varpower-monomial.hpp"
#include "ntuple-monomial.hpp"

// typedef int64_t monomial_word; // Used for all types of monomials.  Is this
// OK?
typedef long monomial_word;  // Used for all types of monomials.  Is this OK?
typedef monomial_word *packed_monomial;
typedef const monomial_word *const_packed_monomial;
// format: [hash,component,e1,...,envars],
// where [e1,...,envars] is packed.
// OR: [hash,component,weight,e1,...,envars]
// and weight is NOT packed.
// packing info, hash values, weights are all
// defined in: PackedMonomials

// Format of monomials:
// [hashval p0 p1 ... pr]
// Multiplication: as long as the answer is in range,
//   it is just addition in each slot
// Comparison: Either:
//   either lex in order p0 --> p1 --> ...
//   or grevlex in the order pr --> p(r-1) --> ...
// Component:
//   either at beginning or end, or neither:
//     p0, or pr or not present
//
// Design constraints:
//   0. we assume 64 bit words for now
//   a. additive hashvalue, which takes an entire 64 bit word
//   b. the variable exponents are packed: either 8 bits, 16 bits, etc.
//   c. weight vector values take the same space as a variable
//      these come first (lex) or last (grevlex).  In the later case
//      the value of the weight vector is negated so that larger values
//      come first
//      The total degree is included as a weight vector value?
//   d. component order can be first/last, up/down. In half the cases, the
//   negative
//      of the actual value is encoded.
// Examples:
// 1. grevlex on 7 vars with an elimination order (first 3 vars)
//    [hashval -component {p0,p1,p2,p3,p4,p5,p6,wt(1,1,1)} {-degree,unused:7}]
// 2.
class MonomialInfoPacked : public our_new_delete
{
  int nbytes;  // per monomial

  int last_var_loc;
  monomial_word last_var_mask;

  int nvars;
  int nslots;
  monomial_word *hashfcn;

  monomial_word mask;
  // monomial format: [hashvalue, component, pack1, pack2, ..., packr]
  // other possible:
  //    [hashvalue, len, component, v1, v2, ..., vr] each vi is potentially
  //    packed too.
  //    [hashvalue, component, wt1, ..., wtr, pack1, ..., packr]
 public:
  typedef packed_monomial monomial;
  typedef const_packed_monomial const_monomial;
  typedef monomial value;

  // To be redone //
  MonomialInfoPacked(int nvars, int nbits_per_exp);
  // Default monomial order is reverse lexicographic

  virtual ~MonomialInfoPacked();

  // Reworked //
  int n_vars() const { return nvars; }
  int max_monomial_size() const { return nslots; }
  int monomial_size(const_packed_monomial m) const { return nslots; }
  long hash_value(const_packed_monomial m) const { return *m; }
  // This hash value is an ADDITIVE hash (trick due to A. Steel)

  void copy(const_packed_monomial src, packed_monomial target) const
  {
    memcpy(target, src, nbytes);
  }

  bool is_equal(const_packed_monomial m, const_packed_monomial n) const
  {
    for (int j = nslots; j > 0; --j)
      if (*m++ != *n++) return false;
    return true;
  }

  void unchecked_mult(const_packed_monomial m,
                      const_packed_monomial n,
                      packed_monomial result) const
  {
    for (int j = nslots; j > 0; --j) *result++ = *m++ + *n++;
  }

  void unchecked_divide(const_packed_monomial m,
                        const_packed_monomial n,
                        packed_monomial result) const
  {
    for (int j = nslots; j > 0; --j) *result++ = *m++ - *n++;
  }

  int compare_grevlex(const_packed_monomial m, const_packed_monomial n) const
  {
    const_packed_monomial m1 = m + nslots;
    const_packed_monomial n1 = n + nslots;
    for (int i = nslots - 1; i > 0; i--)
      {
        int cmp = *--m1 - *--n1;
        if (cmp < 0) return -1;
        if (cmp > 0) return 1;
      }
    return 0;
  }

  bool one(long comp, packed_monomial result) const
  {
    // Pack the vector (0,...,0,comp) with nvars zeroes.
    // Hash value = 0. ??? Should the hash-function take component into account
    // ???
    memset(result, 0, nbytes);
    set_component(comp, result);
    return true;
  }

  monomial_word get_exponent(const_packed_monomial m, int i) const { return }
  void show() const;

  long last_exponent(const_packed_monomial m) const
  {
    return get_exponent(m, nvars - 1);
  }

  void set_component(long component, packed_monomial m) const
  {
    m[1] = component;
  }

  long get_component(const_packed_monomial m) const { return m[1]; }
  bool from_exponent_vector(const_ntuple_monomial e,
                            long comp,
                            packed_monomial result) const
  {
    // Pack the vector e[0]..e[nvars-1],comp.  Create the hash value at the same
    // time.
    result[0] = 0;
    result[1] = comp;
    for (int i = 0; i < nvars; i++)
      {
        result[2 + i] = e[i];
        if (e[i] > 0) result[0] += hashfcn[i] * e[i];
      }
    return true;
  }

  bool to_exponent_vector(const_packed_monomial m,
                          ntuple_monomial result,
                          long &result_comp) const
  {
    // Unpack the monomial m.
    result_comp = m[1];
    m += 2;
    for (int i = 0; i < nvars; i++) *result++ = *m++;
    return true;
  }

  void to_varpower_monomial(const_packed_monomial m,
                            varpower_monomial result) const
  {
    // 'result' must have enough space allocated
    varpower_word *t = result + 1;
    const_packed_monomial m1 = m + 2 + nvars;
    int len = 0;
    for (int i = nvars - 1; i >= 0; i--)
      {
        if (*--m1 > 0)
          {
            *t++ = i;
            *t++ = *m1;
            len++;
          }
      }
    *result = len;
  }

  void from_varpower_monomial(const_varpower_monomial m,
                              long comp,
                              packed_monomial result) const
  {
    // 'result' must have enough space allocated
    result[0] = 0;
    result[1] = comp;
    for (int i = 0; i < nvars; i++)
      {
        result[2 + i] = 0;
      }
    for (index_varpower_monomial j = m; j.valid(); ++j)
      {
        int v = j.var();
        int e = j.exponent();
        result[2 + v] = e;
        if (e == 1)
          result[0] += hashfcn[v];
        else
          result[0] += e * hashfcn[v];
      }
  }

  bool check_monomial(const_packed_monomial m) const
  {
    // Determine if m represents a well-formed monomial.
    m++;
    for (int j = nslots - 1; j > 0; --j)
      if (mask & (*m++)) return false;
    return true;
  }

  bool mult(const_packed_monomial m,
            const_packed_monomial n,
            packed_monomial result) const
  {
    unchecked_mult(m, n, result);
    return check_monomial(result);
  }

  monomial_word monomial_weight(const_packed_monomial m,
                                const M2_arrayint wts) const;

  void show(const_packed_monomial m) const;

  bool unnecessary(const_packed_monomial m,
                   const_packed_monomial p1,
                   const_packed_monomial p2,
                   const_packed_monomial lcm) const
  // Returns true if the corresponding pair could be removed
  // This is essentially the Buchberger-Moeller criterion
  // Assumptions: lcm(p1,p2) = lcm.
  // Here is the criterion:
  //   (a) if component(lcm) != component(m) return false
  //   (b) if m does not divide lcm, return false
  //   (c) need that (A) lcm(p1,m) != lcm and that (B) lcm(p2,m) != lcm
  //       (in these two cases, we will have already removed one of the other
  //        two pairs: (p1,m), (p2,m).  Note that in any case,
  //        if (b) holds, then lcm(p1,m) divides lcm, same with lcm(p2,m).
  //        if A and B then return true
  {
    bool A = false;
    bool B = false;
    m += 2;
    p1 += 2;
    p2 += 2;
    lcm += 2;
    for (int i = 0; i < nvars; i++)
      {
        if (m[i] > lcm[i]) return false;
        if (m[i] == lcm[i]) continue;
        if (!A && p1[i] < lcm[i])
          {
            A = true;
            continue;
          }
        if (!B && p2[i] < lcm[i])
          {
            B = true;
          }
      }
    return (A && B);
  }

  void quotient_as_vp(const_packed_monomial a,
                      const_packed_monomial b,
                      varpower_monomial result,
                      int &deg,
                      bool &are_disjoint) const
  {
    // sets result, deg, are_disjoint
    deg = 0;
    are_disjoint = true;
    a += 2;
    b += 2;
    int len = 0;
    varpower_word *r = result + 1;
    for (int i = nvars - 1; i >= 0; --i)
      {
        if (a[i] != 0 && b[i] != 0) are_disjoint = false;
        long c = a[i] - b[i];
        if (c > 0)
          {
            *r++ = i;
            *r++ = c;
            deg += c;
            len++;
          }
      }
    result[0] = len;
  }
};
#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
