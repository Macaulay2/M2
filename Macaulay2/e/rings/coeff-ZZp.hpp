class CoeffZZp
{
  int p;
 public:
  typedef int coeff_type;
  // elements of this type are represented in the range 0..p-1

  CoeffZZp(int p0) : p(p0)
    {
    }
    
  void init(coeff_type &a)
  {
    a = 0;
  }

  void from_int(coeff_type &a, int n)
  {
    a = n % p;
    if (a < 0) a += p;
  }

  void copy(coeff_type &result, coeff_type b)
  {
    result = b;
  }

  void negate_to(coeff_type &a)
  {
    a = p-a;
  }

  void add(coeff_type &result, coeff_type a, coeff_type b)
  {
    result = a + b;
    if (result >= p) result -= p;
  }

  void mult(coeff_type &result, coeff_type a, coeff_type b)
  {
    result = a * b;
    if (result >= p) result = result % p;
  }

  bool is_equal(coeff_type a, coeff_type b)
  {
    return a == b;
  }

  bool is_zero(coeff_type a)
  {
    return a == 0;
  }

  bool is_one(coeff_type a)
  {
    return a == 1;
  }

  void invert(coeff_type &result, coeff_type a)
  {
  }

  void syzygy(coeff_type &result_u, coeff_type &result_v,
	      coeff_type a, coeff_type b)
  // Sets u,v s.t. u*a+v*b=0.  If b==1,then u=1,v=-a.
  {
  }

  // The following are not really needed for GB computations?

  void divide(coeff_type &result, coeff_type a, coeff_type b)
  {
  }

  void power(coeff_type &result, coeff_type a, int n)
  {
  }

  // elem_text_out ??
};

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
