// this is the public interface to the packed monomial routines
// see pi-readme.txt
// Copyright, Daniel R. Grayson, 2007

int pm_sign = 1;

template <typename EXP, typename BIN>
struct pm {
  int numexps;
  int numbins;

  virtual void encode(BIN *dest, EXP *);
  virtual void decode(EXP *dest, BIN *);
  virtual int compare(BIN *, BIN *);
  virtual void mult(BIN *dest, BIN *, BIN *);
  virtual void unsafe_mult(BIN *dest, BIN *, BIN *);
  virtual void div(BIN *dest, BIN *, BIN *);
  virtual void unsafe_div(BIN *dest, BIN *, BIN *);
  virtual bool divides(BIN *, BIN *);
  virtual void lcm(BIN *dest, BIN *fac1, BIN *fac2, BIN *, BIN *);
  virtual void gcd(BIN *dest, BIN *fac1, BIN *fac2, BIN *, BIN *);

};

// Local Variables:
// compile-command: "g++ -Wall -c -x c++ pm.hpp -o /dev/null"
// End:
