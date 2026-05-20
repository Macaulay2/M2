// Copyright 2010 Michael E. Stillman

#ifndef _skew_hpp_
#define _skew_hpp_

class SkewMultiplication
{
 public:
  int _n_vars;
  int _n_skew;
  int *_skew_list;  // An array 0.._n_skew-1, listing the indices of the skew
                    // comm variables.
  bool *_skew_exp;  // 0.._n_vars-1

  unsigned long skew_byte_size;

  SkewMultiplication();
  SkewMultiplication(int nvars, int nskew, int *skew_list);
  ~SkewMultiplication() {}
  int n_skew_vars() const { return _n_skew; }
  bool is_skew_var(int i) const { return _skew_exp[i]; }
  int skew_variable(int i) const { return _skew_list[i]; }
  // number of variables occurring in 'exp' which are skew variables
  int skew_degree(const int *exp) const;

  int skew_vars(const int *exp, int *result) const;
  int skew_vars(const long *exp, int *result) const;
  // The number s of skew variables in 'exp' is returned, and their
  // indices are placed in result[0], ..., result[s-1].
  // The space that 'result' points to MUST hold at least 'nskew' ints.

  int mult_sign(const int *exp1, const int *exp2) const;
  int mult_sign(const long *exp1, const long *exp2) const;

  int diff(const int *exp1, const int *exp2, int *result) const;
  int divide(const int *exp1, const int *exp2, int *result) const;
  bool exp_is_zero(const int *exp) const;
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
