// Copyright 2016-2017  Michael E. Stillman

#ifndef _res_moninfo_sparse_hpp_
#define _res_moninfo_sparse_hpp_

#include "skew.hpp"                   // for SkewMultiplication
#include "schreyer-resolution/res-monomial-types.hpp"     // for res_monomial_word, res_const_pa...
#include "schreyer-resolution/res-varpower-monomial.hpp"  // for index_res_varpower_monomial

#include <iostream>                   // for ostream
#include <memory>                     // for unique_ptr
#include <vector>                     // for vector

// Format for monomials here:
// a. length (in bytes) (int32)
// b. hash value (int32)
// c. component (int32)
// d. v1 v2 ... vd
// where d = length-2
// v1 >= v2 >= ... >= vd >= 0 are indices of variables.
// or, maybe also have degree before this, and other weight values...
// SO, general form:
// [length, hashval, component, w1, ..., wr, v, ..., vd]
class ResMonoidSparse
{
  int mNumVars;
  int nslots;
  std::unique_ptr<res_monomial_word[]>
      hashfcn;  // array 0..mNumVars-1 of hash values for each variable
  res_monomial_word mask;
  std::vector<int> mVarDegrees;  // array 0..mNumVars-1 of primary (heft)
                                 // degrees for each variable.

  int mFirstVar;  // = 2, if no weight vector, otherwise 2 + mNumWeights

  int mFirstWeight;  // index of the first weight value slot
  int mNumWeights;   // number of weight function values

  // flattened array 0..mNumWeights of array 0..mNumVars-1 of weights
  std::vector<int> mWeightVectors;

  mutable unsigned long ncalls_hash_value;
  mutable unsigned long ncalls_compare;
  mutable unsigned long ncalls_compare_grevlex;
  mutable unsigned long ncalls_mult;
  mutable unsigned long ncalls_get_component;
  mutable unsigned long ncalls_from_exponent_vector;
  mutable unsigned long ncalls_to_exponent_vector;
  mutable unsigned long ncalls_to_varpower;
  mutable unsigned long ncalls_from_varpower;
  mutable unsigned long ncalls_is_equal;
  mutable unsigned long ncalls_is_equal_true;
  mutable unsigned long ncalls_divide;
  mutable unsigned long ncalls_weight;
  mutable unsigned long ncalls_unneccesary;
  mutable unsigned long ncalls_quotient_as_vp;

 public:
  typedef res_packed_monomial monomial;
  typedef res_const_packed_monomial const_monomial;
  typedef monomial value;

  ResMonoidSparse(int mNumVars,
                  const std::vector<int>& var_degrees,
                  const std::vector<int>& weightvecs,
                  const MonomialOrderingType moType);

  ~ResMonoidSparse();

  int n_vars() const { return mNumVars; }
  int max_monomial_size() const { return nslots; }
  int monomial_size(res_const_packed_monomial m) const { return *m; }
  void show() const;

  res_monomial_word hash_value(res_const_packed_monomial m) const
  {
    ncalls_hash_value++;
    return m[1];
  }
  // This hash value is an ADDITIVE hash (trick due to A. Steel)

  void copy(res_const_packed_monomial src, res_packed_monomial target) const
  {
    for (int i = *src; i > 0; --i) *target++ = *src++;
  }

  void set_component(component_index component, res_packed_monomial m) const
  {
    m[2] = component;
  }

  component_index get_component(res_const_packed_monomial m) const
  {
    ncalls_get_component++;
    return m[2];
  }

  void setWeightAndHash(res_packed_monomial result) const
  {
    // assumes the following is set already:
    //  result[0] -- length
    //  result[2] -- component
    //  result[mFirstVar..len-1] -- monomial part
    // sets:
    //  result[1] -- hash code
    //  result[FirstWeight..mFirstVar-1] -- weight vector values
    const int* wt = mWeightVectors.data();
    for (int j = 0; j < mNumWeights; j++, wt += mNumVars)
      {
        res_monomial_word val = 0;
        for (int* v = result + mFirstVar; v != result + *result; ++v)
          val += wt[*v];
        result[mFirstWeight + j] = val;
      }
    res_monomial_word val = 0;
    for (int* v = result + mFirstVar; v != result + *result; ++v)
      val += hashfcn[*v];
    result[1] = val;
  }
  bool from_exponent_vector(res_const_ntuple_monomial e,
                            component_index comp,
                            res_packed_monomial result) const
  {
    // Pack the vector e[0]..e[mNumVars-1],comp.  Create the hash value at the
    // same time.
    ncalls_from_exponent_vector++;
    result[0] = 0;     // length
    result[1] = 0;     // hash value, not set here (other than this...)
    result[2] = comp;  // component

    int next_var_slot = mFirstVar;

    for (int i = mNumVars - 1; i >= 0; --i)
      {
        if (e[i] > 0)
          {
            for (int j = 0; j < e[i]; j++)
              {
                result[1] += hashfcn[i];
                result[next_var_slot] = i;
                next_var_slot++;
              }
          }
      }
    result[0] = next_var_slot;  // this is the length of this monomial
    setWeightAndHash(result);
    return true;
  }

  int skew_vars(const SkewMultiplication* skew,
                res_const_packed_monomial m,
                int* skewvars) const
  {
    return skew->skew_vars(m + 2 + mNumWeights, skewvars);
  }

  int skew_mult_sign(const SkewMultiplication* skew,
                     res_const_packed_monomial m,
                     res_const_packed_monomial n) const
  {
    return skew->mult_sign(m + 2 + mNumWeights, n + 2 + mNumWeights);
  }

  bool one(component_index comp, res_packed_monomial result) const
  {
    // The monomial "1"
    result[0] = mFirstVar;  // length
    result[1] = 0;          // hash value
    result[2] = comp;       // component
    for (int i = 3; i < mFirstVar; i++) result[i] = 0;
    return true;
  }

  bool to_exponent_vector(res_const_packed_monomial m,
                          res_ntuple_monomial result,
                          component_index& result_comp) const
  {
    // Unpack the monomial m.
    ncalls_to_exponent_vector++;
    result_comp = m[2];
    for (int i = 0; i < mNumVars; i++) result[i] = 0;
    auto top = *m;
    for (int i = mFirstVar; i < top; i++) result[m[i]]++;
    return true;
  }

  void to_varpower_monomial(res_const_packed_monomial m,
                            res_varpower_monomial result) const
  {
    // 'result' must have enough space allocated
    ncalls_to_varpower++;
    int len = 0;
    res_varpower_word* r = result + 1;

    if (mFirstVar != *m)  // ie, m != 1.
      {
        int currentvar = m[mFirstVar];
        int deg = 1;
        const int* mend = m + *m;
        for (auto p = m + mFirstVar + 1; p != mend; ++p)
          {
            if (*p == currentvar)
              deg++;
            else
              {
                *r++ = currentvar;
                *r++ = deg;
                len++;
                currentvar = *p;
                deg = 1;
              }
          }
      }
    result[0] = len;
  }

  void from_varpower_monomial(res_const_varpower_monomial m,
                              component_index comp,
                              res_packed_monomial result) const
  {
    // 'result' must have enough space allocated
    ncalls_from_varpower++;
    int len = 0;
    int* r = result + mFirstVar;
    for (index_res_varpower_monomial j = m; j.valid(); ++j)
      {
        res_varpower_word v = j.var();
        res_varpower_word e = j.exponent();
        for (int i = 0; i < e; i++) *r++ = v;
        len += e;
      }
    result[0] = len;
    result[2] = comp;
    setWeightAndHash(result);
  }

  bool is_equal(res_const_packed_monomial m, res_const_packed_monomial n) const
  {
    ncalls_is_equal++;
    if (*m != *n) return false;
    for (int j = *m; j > 0; --j)
      if (*m++ != *n++) return false;
    ncalls_is_equal_true++;
    return true;
  }

  bool monomial_part_is_equal(res_const_packed_monomial m,
                              res_const_packed_monomial n) const
  {
    // Don't consider component
    ncalls_is_equal++;
    if (*m != *n) return false;
    if (m[1] != n[1]) return false;
    int len = *m;
    for (int j = len; j > 0; --j)
      if (*m++ != *n++) return false;
    ncalls_is_equal_true++;
    return true;
  }

#if 0  
  bool check_monomial(res_const_packed_monomial m) const {
    // Determine if m represents a well-formed monomial.
    // basically, this means that weight values are non-negative, and variables are in descending order, all >= 0.
    // REWRITE
    
    m++;
    for (int j=nslots-1; j>0; --j)
      if (mask & (*m++)) return false;
    return true;
  }
#endif

  void unchecked_mult(res_const_packed_monomial a,
                      res_const_packed_monomial b,
                      res_packed_monomial result) const
  {
    // a: mFirstVar + degA
    // b: mFirstVar + degB
    // result size is mFirstVar + degA + degB = size(a) + size(b) - mFirstVar
    ncalls_mult++;
    result[0] = a[0] + b[0] - mFirstVar;
    for (int i = 1; i < mFirstVar; ++i) result[i] = a[i] + b[i];
    // Now we merge the rest
    const int* v1 = a + mFirstVar;
    const int* v2 = b + mFirstVar;
    const int* end1 = a + *a;
    const int* end2 = b + *b;
    int* res = result + mFirstVar;
    if (v1 == end1)
      {
        while (v2 != end2) *res++ = *v2++;
        return;
      }
    if (v2 == end2)
      {
        while (v1 != end1) *res++ = *v1++;
        return;
      }
    for (;;)
      {
        if (*v1 >= *v2)
          {
            *res++ = *v1;
            v1++;
            if (v1 == end1)
              {
                while (v2 != end2) *res++ = *v2++;
                return;
              }
          }
        else
          {
            *res++ = *v2;
            v2++;
            if (v2 == end2)
              {
                while (v1 != end1) *res++ = *v1++;
                return;
              }
          }
      }
  }

  bool divide(res_const_packed_monomial m,
              res_const_packed_monomial n,
              res_packed_monomial result) const
  {
    // computes m/n, or tries to.  If n divides m, then return true and set
    // 'result' to m/n
    // otherwise return false.  In the latter case, it is possible that result
    // will have been written into partially,
    // but the result is not a well-formed monomial.
    ncalls_divide++;
    if (*m < *n) return false;
    const int* vm = m + mFirstVar;
    const int* vn = n + mFirstVar;
    const int* vend = n + *n;
    int* vresult = result + mFirstVar;
    for (; vn != vend; ++vn)
      {
        if (*vn > *vm) return false;
        if (*vn == *vm) ++vm;
        *vresult++ = *vm++;
      }
    for (int i = 1; i < mFirstVar; ++i) result[i] = m[i] - n[i];
    result[0] = m[0] - n[0] + mFirstVar;
    return true;
  }

  bool mult(res_const_packed_monomial m,
            res_const_packed_monomial n,
            res_packed_monomial result) const
  {
    unchecked_mult(m, n, result);
    return true;
    //    return check_monomial(result);
  }

  void show(res_const_packed_monomial m) const;

  void showAlpha(res_const_packed_monomial m) const;

  int compare_grevlex(res_const_packed_monomial m,
                      res_const_packed_monomial n) const
  {
    ncalls_compare_grevlex++;
    for (int i = 3; i < mFirstVar; i++)
      {
        int cmp = m[i] - n[i];
        if (cmp > 0) return 1;
        if (cmp < 0) return -1;
      }
    const int* m1 = m + mFirstVar;
    const int* n1 = n + mFirstVar;
    const int* mend = m + *m;
    const int* nend = n + *n;
    while (*m1 == *n1)
      {
        m1++;
        n1++;
        if (m1 == mend)
          {
            if (n1 == nend)
              break;
            else
              {
                return -1;
              }
          }
        else
          {
            if (n1 == nend) return 1;
            ;
          }
      }
    res_monomial_word cmp = m[2] - n[2];
    if (cmp < 0) return 1;
    if (cmp > 0) return -1;
    return 0;
  }

  int compare_schreyer(res_const_packed_monomial m,
                       res_const_packed_monomial n,
                       res_const_packed_monomial m0,
                       res_const_packed_monomial n0,
                       component_index tie1,
                       component_index tie2) const
  {
    // REWRITE
    ncalls_compare++;
#if 0
    printf("compare_schreyer: ");
    printf("  m=");
    showAlpha(m);
    printf("  n=");    
    showAlpha(n);
    printf("  m0=");    
    showAlpha(m0);
    printf("  n0=");    
    showAlpha(n0);
    printf("  tiebreakers: %ld %ld\n", tie1, tie2);
#endif
    int* m1 = new int[*m + *m0];  // need less than this
    int* n1 = new int[*n + *n0];  // need less than this
    mult(m, m0, m1);
    mult(n, n0, n1);
    int cmp = compare_grevlex(m1, n1);
    if (cmp == 0)
      {
        if (tie1 > tie2)
          cmp = -1;
        else if (tie1 < tie2)
          cmp = 1;
        else
          cmp = 0;
      }
    delete[] m1;
    delete[] n1;
    return cmp;
  }

  void variable_as_vp(int v, res_varpower_monomial result) const
  {
    // REWRITE?
    result[0] = 1;
    result[1] = v;
    result[2] = 1;
  }

  int degree_of_vp(res_const_varpower_monomial a) const
  {
    return static_cast<int>(res_varpower_monomials::weight(a, mVarDegrees));
  }

  void quotient_as_vp(res_const_packed_monomial m,
                      res_const_packed_monomial n,
                      res_varpower_monomial result) const
  // sets result to be m:n, as a varpower.
  // 'result' should have enough space to hold deg(m) integers.  How do we know
  // here??
  {
    ncalls_quotient_as_vp++;

    const int* vm = m + mFirstVar;
    const int* vn = n + mFirstVar;
    const int* vend = n + *n;
    int* vresult = result + mFirstVar;
    for (; vn != vend; ++vn)
      {
        if (*vn > *vm) continue;
        if (*vn == *vm) ++vm;
        *vresult++ = *vm++;
      }
    // now make these into a varpower monomial.
    int len = 0;
    res_varpower_word* r = result + 1;

    int currentvar = result[mFirstVar];
    int deg = 1;
    for (auto p = result + mFirstVar + 1; p != vresult; ++p)
      {
        if (*p == currentvar)
          deg++;
        else
          {
            *r++ = currentvar;
            *r++ = deg;
            len++;
            currentvar = *p;
            deg = 1;
          }
      }
    result[0] = len;
  }

  void dump(std::ostream& o, res_const_packed_monomial mon);
};
#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
