// Copyright 2004 Michael E. Stillman

#ifndef __coeffZZ_hpp_
#define __coeffZZ_hpp_

class CoeffZZ
{
 public:
  typedef int coeff_type;

  CoeffZZ() 
    {
    }

  void init(coeff_type &a)
    {
      a = 0; 
    }

  void add(coeff_type &result, coeff_type a, coeff_type b)
  {
    result = a+b;
  }

  void mult(coeff_type &result, coeff_type a, coeff_type b)
  {
    result = a * b;
  }

  bool is_zero(coeff_type a)
  {
    return a == 0;
  }

  bool is_equal(coeff_type a, coeff_type b)
  {
    return a == b;
  }

  bool is_one(coeff_type a)
  {
    return a == 1;
  }

  void copy(coeff_type &result, coeff_type b)
  {
    result = b;
  }

};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
