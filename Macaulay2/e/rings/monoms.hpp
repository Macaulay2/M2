class Monomials
// Template type
{
public:
  typedef int *monomial;

  // nvars
  // monomial_size
  // to/from exponents
  // compare <-- this one is a bit tricky
  // multi-degree
  // nslots, compare n
  // is_one
  // elem_text_out

  Monomials(int nvars) {}

  void init(monomial &result)
  {
    // Allocate a monomial, or, have this point to 0.
  }

  void set(monomial &result, monomial a)
  {
  }

  void mult(monomial &result, monomial a, monomial b)
  {
  }

  bool is_equal(monomial a, monomial b)
  {
    return true;
  }

  void copy(monomial &result, monomial b)
  {
  }

  int compare(monomial a, int acomp, monomial b, int bcomp)
  {
    return acomp-bcomp;
  }

  // divide, is_divisible
  
};

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
