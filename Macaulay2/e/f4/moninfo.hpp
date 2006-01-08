// Copyright 2005  Michael E. Stillman

#ifndef _moninfo_h_
#define _moninfo_h_

#include <cstdio>
#include "../../d/M2types.h"

#include "varpower_monomial.hpp"
#include "ntuple_monomial.hpp"

typedef int64 monomial_word; // Used for all types of monomials.  Is this OK?
typedef monomial_word * packed_monomial; 
typedef const monomial_word * const_packed_monomial; 
  // format: [hash,component,e1,...,envars],
  // where [e1,...,envars] is packed.
  // OR: [hash,component,weight,e1,...,envars]
  // and weight is NOT packed.
  // packing info, hash values, weights are all
  // defined in: PackedMonomials

class MonomialInfo
{
  int nvars;
  int nslots;
  monomial_word *hashfcn;
  monomial_word mask;
  // monomial format: [hashvalue, component, pack1, pack2, ..., packr]
  // other possible:
  //    [hashvalue, len, component, v1, v2, ..., vr] each vi is potentially packed too.
  //    [hashvalue, component, wt1, ..., wtr, pack1, ..., packr]
public:
  typedef packed_monomial monomial;
  typedef const_packed_monomial const_monomial;
  typedef monomial value;

  MonomialInfo(int nvars);
  // Default monomial order is reverse lexicographic

  virtual ~MonomialInfo();

  int n_vars() const { return nvars; }
  
  int max_monomial_size() const { return nslots; }

  int monomial_size(const_packed_monomial m) const { return nslots; }

  void show() const;

  long hash_value(const_packed_monomial m) const { return *m; }
  // This hash value is an ADDITIVE hash (trick due to A. Steel)

  void copy(const_packed_monomial src, packed_monomial target) const {
    for (int i=0; i<nslots; i++)
      *target++ = *src++;
  }

  long last_exponent(const_packed_monomial m) const {
    return m[nslots-1];
  }

  void set_component(long component, packed_monomial m) const { m[1] = component; }
  
  long get_component(const_packed_monomial m) const { return m[1]; }

  bool from_exponent_vector(const_ntuple_monomial e, long comp, packed_monomial result) const {
    // Pack the vector e[0]..e[nvars-1],comp.  Create the hash value at the same time.
    result[0] = 0;
    result[1] = comp;
    for (int i=0; i<nvars; i++)
      {
	result[2+i] = e[i];
	if (e[i] > 0)
	  result[0] += hashfcn[i] * e[i];
      }
    return true;
  }

  bool to_exponent_vector(const_packed_monomial m, ntuple_monomial result, long &result_comp) const {
    // Unpack the monomial m.
    result_comp = m[1];
    m += 2;
    for (int i=0; i<nvars; i++)
      *result++ = *m++;
    return true;
  }

  void to_varpower_monomial(const_packed_monomial m, varpower_monomial result) {
    // 'result' must have enough space allocated for this
    varpower_word *t = result+1;
    for (int i=nvars+1; i>1; i--)
      if (m[i] > 0) 
	{
	  *t++ = i;
	  *t++ = m[i];
	}
    *result = t-result+1;
  }

  bool is_equal(const_packed_monomial m, const_packed_monomial n) const {
    for (int j=nslots; j>0; --j)
      if (*m++ != *n++) return false;
    return true;
  }

  bool check_monomial(const_packed_monomial m) const {
    // Determine if m represents a well-formed monomial.
    m++;
    for (int j=nslots-1; j>0; --j)
      if (mask & (*m++)) return false;
    return true;
  }
  
  void unchecked_mult(const_packed_monomial m, const_packed_monomial n, packed_monomial result) const {
    for (int j=nslots; j>0; --j) *result++ = *m++ + *n++;
  }

  void unchecked_divide(const_packed_monomial m, const_packed_monomial n, packed_monomial result) const {
    for (int j=nslots; j>0; --j) *result++ = *m++ - *n++;
  }

  bool mult(const_packed_monomial m, const_packed_monomial n, packed_monomial result) const {
    unchecked_mult(m,n,result);
    return check_monomial(result);
  }

  void quotient(const_packed_monomial m, const_packed_monomial n, packed_monomial result) const;
  // This one requires tricks or unpacking the monomial
  // However, maybe this isn't done so much, so it doesn't matter...

  monomial_word monomial_weight(const_packed_monomial m, const M2_arrayint wts) const {
    const_packed_monomial m1 = m;
    int top = wts->len;
    int *n = wts->array;
    monomial_word sum = 0;
    for (int j=top; j>0; --j) sum += *m1++ + *n++;
    return sum;
  }

  void show(const_packed_monomial m) const;

  int compare_grevlex(const_packed_monomial m, const_packed_monomial n) const {
    const_packed_monomial m1 = m+2;
    const_packed_monomial n1 = n+2;
    for (int i=nslots-2; i>=0; i--) {
      int cmp = *m1++ - *n1++;
      if (cmp < 0) return -1;
      if (cmp > 0) return 1;
    }
    monomial_word cmp = m[1]-n[1];
    if (cmp < 0) return 1;
    if (cmp > 0) return -1;
    return 0;
  }
};
#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
