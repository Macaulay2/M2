// this is the public interface to the packed monomial routines
// see pi-readme.txt
// Copyright, Daniel R. Grayson, 2007

#ifdef __GNUC__
#define expect_false(x) (__builtin_expect(x,0))
#define expect_true(x)  (__builtin_expect(x,1))
#else
#define expect_false(x) (x)
#define expect_true(x)  (x)
#endif

int pm_sign = 1;

template <typename EXP, typename BIN>
struct pm {
protected:
  const BIN * const identity;
  const BIN * const overflow_bits;
  void signal_overflow(char *msg);
public:
  const int numexps;
  const int numbins;
  virtual void encode(BIN *dest, EXP *);
  virtual void decode(EXP *dest, BIN *);
  int compare(BIN *x, BIN *y) {
    int i;
    for (i=0; i<numbins; i++) {
      if expect_true (x[i] == y[i]) continue;
      return x[i] > y[i] ? 1 : -1;
    }
  }
  void mult_unsafe(BIN *dest, BIN *x, BIN *y) { 
    int i = numbins;
    BIN carries = 0;
    while (i-- > 0) dest[i] = x[i] + y[i] - identity[i];
  }
  void mult(BIN *dest, BIN *x, BIN *y);
  virtual void div(BIN *dest, BIN *, BIN *);
  void div_unsafe(BIN *dest, BIN *, BIN *);
  virtual bool divides(BIN *, BIN *);
  virtual void lcm(BIN *dest, BIN *fac1, BIN *fac2, BIN *, BIN *);
  virtual void gcd(BIN *dest, BIN *fac1, BIN *fac2, BIN *, BIN *);
  virtual EXP  degree(BIN *m, EXP *wt);
  virtual EXP  degree_unsafe(BIN *m, EXP *wt);
};

// Local Variables:
// compile-command: "g++ -Wall -c -x c++ pm.hpp -o /dev/null"
// End:
