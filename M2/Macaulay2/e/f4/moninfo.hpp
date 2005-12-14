// Copyright 2005  Michael E. Stillman

#ifndef _moninfo_h_
#define _moninfo_h_

#include <cstdio>
#include "../../d/M2types.h"

//#define newarray(A,B) new A[B]
//#define deletearray(A) delete[]A

class MonomialInfo
{
  int nvars;
  int nslots;
  long *hashfcn;
  long mask;
  // monomial format: [hashvalue, component, pack1, pack2, ..., packr]
  // other possible:
  //    [hashvalue, len, component, v1, v2, ..., vr] each vi is potentially packed too.
  //    [hashvalue, component, wt1, ..., wtr, pack1, ..., packr]
public:
  typedef long *monomial;
  typedef monomial value;

  MonomialInfo(int nvars);
  // Default monomial order is reverse lexicographic

  virtual ~MonomialInfo();

  int n_vars() const { return nvars; }
  
  int max_monomial_size() const { return nslots; }

  int monomial_size(monomial m) const { return nslots; }

  void show() const;

  long hash_value(monomial m) { return *m; }
  // This hash value is an ADDITIVE hash (trick due to A. Steel)

  bool from_exponent_vector(long *e, long comp, monomial result) const {
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

  bool to_exponent_vector(monomial m, long *result, long &result_comp) const {
    // Unpack the monomial m.
    result_comp = m[1];
    m += 2;
    for (int i=0; i<nvars; i++)
      *result++ = *m++;
    return true;
  }

  bool is_equal(monomial m, monomial n) const {
    for (int j=nslots; j>0; --j)
      if (*m++ != *n++) return false;
    return true;
  }

  bool check_monomial(monomial m) const {
    // Determine if m represents a well-formed monomial.
    m++;
    for (int j=nslots-1; j>0; --j)
      if (mask & (*m++)) return false;
    return true;
  }
  
  void unchecked_mult(monomial m, monomial n, monomial result) const {
    for (int j=nslots; j>0; --j) *result++ = *m++ + *n++;
  }

  void unchecked_divide(monomial m, monomial n, monomial result) const {
    for (int j=nslots; j>0; --j) *result++ = *m++ - *n++;
  }

  bool mult(monomial m, monomial n, monomial result) const {
    unchecked_mult(m,n,result);
    return check_monomial(result);
  }

  void quotient(monomial m, monomial n, monomial result) const;
  // This one requires tricks or unpacking the monomial
  // However, maybe this isn't done so much, so it doesn't matter...

  long monomial_weight(const monomial m, const M2_arrayint wts) const {
    const long *m1 = m;
    int top = wts->len;
    int *n = wts->array;
    long sum = 0;
    for (int j=top; j>0; --j) sum += *m1++ + *n++;
    return sum;
  }

  void show(monomial m) const;

  int compare_grevlex(monomial m, monomial n) const {
    long *m1 = m+2;
    long *n1 = n+2;
    for (int i=nslots-2; i>=0; i--) {
      int cmp = *m1++ - *n1++;
      if (cmp < 0) return -1;
      if (cmp > 0) return 1;
    }
    int cmp = m[1]-n[1];
    if (cmp < 0) return 1;
    if (cmp > 0) return -1;
    return 0;
  }
};
#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
