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
public:
  const int numexps;
  const int numbins;
  virtual void encode(BIN *dest, EXP *);
  virtual void decode(EXP *dest, BIN *);
  int compare(BIN *x, BIN *y) {
    int j = 0, n = numbins;
    while (1) {
      if expect_false (x[j] > y[j]) return  1; 
      else if expect_false (x[j] < y[j]) return -1;
      j++;
      n--;
      if expect_false (n == 0) return 0;
    }
  }
  void mult(BIN *dest, BIN *, BIN *);
  void mult_unsafe(BIN *dest, BIN *x, BIN *y) { 
    int j = numbins;
    while (--j >= 0) dest[j] = x[j] + y[j] - identity[j];
  }
  virtual void div(BIN *dest, BIN *, BIN *);
  void div_unsafe(BIN *dest, BIN *x, BIN *y) {
    int j = numbins;
    while (--j >= 0) dest[j] = x[j] - y[j] + identity[j];
  }
  virtual bool divides(BIN *, BIN *);
  virtual void lcm(BIN *dest, BIN *fac1, BIN *fac2, BIN *, BIN *);
  virtual void gcd(BIN *dest, BIN *fac1, BIN *fac2, BIN *, BIN *);
  virtual EXP  degree(BIN *m, EXP *wt);
  virtual EXP  degree_unsafe(BIN *m, EXP *wt);
  virtual EXP  degree_bound(EXP *wt);
};

// Local Variables:
// compile-command: "g++ -Wall -c -x c++ pm.hpp -o /dev/null"
// End:
