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


template <typename BIN> struct pmonomial { BIN * const contents; };
template <typename BIN> struct vmonomial { BIN * const contents; };
template <typename EXP, typename BIN>
struct pm {
private:
  pm() {}
public:
  virtual ~pm() {}
  const int numexps;
  const int numbins;
  const pmonomial<BIN> identity;
  pm(int numexps0,int numbins0,pmonomial<BIN> identity0) : numexps(numexps0), numbins(numbins0), identity(identity0) {}
  virtual EXP exponent(int varnum);
  virtual void encode(pmonomial<BIN> dest, const EXP[]);
  virtual void encode(vmonomial<BIN> dest, const EXP[], EXP component);
  virtual void decode(EXP dest[], const pmonomial<BIN>);
  virtual void decode(EXP dest[], EXP &component, const vmonomial<BIN>);
  virtual int compare_to_identity(pmonomial<BIN> x);
  virtual int compare_to_identity_partial(pmonomial<BIN> x, int numparts);
  int compare(pmonomial<BIN> x, pmonomial<BIN> y) {
    int j = 0, n = numbins;
    while (1) {
      if expect_false (x[j] > y[j]) return  1; 
      else if expect_false (x[j] < y[j]) return -1;
      j++;
      n--;
      if expect_false (n == 0) return 0;
    }
  }
  virtual int compare_partial(pmonomial<BIN> x, pmonomial<BIN> y, int numparts);
  virtual int Schreyer_compare(vmonomial<BIN> x, vmonomial<BIN> y, pmonomial<BIN> *sch, int *tiebreaker);
  virtual void mult(pmonomial<BIN> dest, const pmonomial<BIN> , const pmonomial<BIN> );
  virtual void mult_unsafe(pmonomial<BIN> dest, const pmonomial<BIN> x, const pmonomial<BIN> y) { 
    int j = numbins;
    while (--j >= 0) dest.contents[j] = x.contents[j] + y.contents[j] - identity.contents[j];
  }
  virtual void div(pmonomial<BIN> dest, const pmonomial<BIN> , const pmonomial<BIN> );
  virtual void div_unsafe(pmonomial<BIN> dest, const pmonomial<BIN> x, const pmonomial<BIN> y) {
    int j = numbins;
    while (--j >= 0) dest.contents[j] = x.contents[j] - y.contents[j] + identity.contents[j];
  }
  virtual bool divides(const pmonomial<BIN> , const pmonomial<BIN> );
  virtual void lcm(pmonomial<BIN> dest, pmonomial<BIN> fac1, pmonomial<BIN> fac2, const pmonomial<BIN> , const pmonomial<BIN> );
  virtual void gcd(pmonomial<BIN> dest, pmonomial<BIN> fac1, pmonomial<BIN> fac2, const pmonomial<BIN> , const pmonomial<BIN> );
  virtual EXP  degree(const pmonomial<BIN> m, const EXP *wt);
  virtual EXP  degree_unsafe(const pmonomial<BIN> m, const EXP *wt);
  virtual EXP  degree_bound(const EXP *wt);
};

template <typename EXP, typename BIN> 
struct pm_makers {
  pm<EXP,BIN> *grevlex(int numvars, int bitsperfield);
  pm<EXP,BIN> *lrevlex(int numvars, int bitsperfield);
  pm<EXP,BIN> *wrevlex(int numvars, int bitsperfield, const EXP *wts);
  pm<EXP,BIN> *revlex(int numvars, int bitsperfield);
  pm<EXP,BIN> *lex(int numvars, int bitsperfield);
  pm<EXP,BIN> *grouplex(int numvars, int bitsperfield);
  pm<EXP,BIN> *grouprevlex(int numvars, int bitsperfield);
  pm<EXP,BIN> *extalg(int numvars);
  pm<EXP,BIN> *addweights(int numwts, int bitsperfield, const EXP *wts, pm<EXP,BIN> *);
  pm<EXP,BIN> *product(int nummonoids, pm<EXP,BIN> **);
  pm<EXP,BIN> *position_up();
  pm<EXP,BIN> *position_down();
};


// Local Variables:
// compile-command: "cd $M2BUILDDIR/Macaulay2/e && make pm-test.o"
// End:
