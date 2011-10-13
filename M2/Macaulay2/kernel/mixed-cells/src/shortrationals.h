  /** short rational numbers ************************************
   */
namespace mixedCells
{
  class ShortRat{
    int c,d;
    void reduce()
    {
      if (c==0) {
	d = 1;
      } else {
	int g = gcd(d,c);
	d /= g;
	c /= g;
      }
    }
  public:
    ShortRat(int a, int b)
    {
      assert (b!=0);
      c=a; d=b;
      reduce();
    }
    ShortRat(int a)
    {
      c=a;
      d=1;
    }
    ShortRat()
    {
      c=0;
      d=1;
    }
    int toInteger() const
    {
      return c/d;
    }
    bool isInteger() const
    {
      return gcd(d,c)==d;
    }
    MY_INLINE static bool isField() 
    {
      return true;
    }
    friend bool isZero(ShortRat const &a)
    {
      return (a.c==0);
    }
    friend bool isZero2(ShortRat const &a)
    {
      return (a.c==0);
    }
    friend bool isOne(ShortRat const &a)
    {
      return (a.c==a.d);
    }
    friend ShortRat operator/(ShortRat const &a, ShortRat const &b)
    {
      assert (!isZero(b));
      return ShortRat(a.c*b.d, a.d*b.c);
    }
    friend ShortRat operator-(ShortRat const &a)
    {
      return ShortRat(-a.c,a.d);
    }
    friend ShortRat operator-(ShortRat const &a, ShortRat const &b)
    {
      return ShortRat(a.c*b.d-b.c*a.d, a.d*b.d);
    }
    friend ShortRat operator+(ShortRat const &a, ShortRat const &b)
    {
      return ShortRat(a.c*b.d+b.c*a.d, a.d*b.d);
    }
    friend class DoubleGen;
    friend class DoubleGen operator*(ShortRat const &a, DoubleGen const &b);
    friend int volumeToInt(ShortRat const &a)
    {
      assert(a.isInteger());
      return ABS(a.toInteger());
    }
    friend ShortRat operator*(ShortRat const &s, ShortRat const &t)
    {
      return ShortRat(s.c*t.c, s.d*t.d);
    }
    void operator+=(ShortRat const &a)
    {
      c = c*a.d+d*a.c;
      d *= a.d; 
      reduce();
    }
    void operator-=(ShortRat const &a)
    {
      c = c*a.d-d*a.c;
      d *= a.d; 
      reduce();
    }
    void operator/=(ShortRat const &a)
    {
      assert(!isZero(a));
      c *= a.d;
      d *= a.c;
      reduce();
    }
    void operator*=(ShortRat const &a)
    {
      c *= a.c;
      d *= a.d;
      reduce();
    }
    friend ShortRat gcd(ShortRat const &s, ShortRat const &t)
    {
      return ShortRat(1);
    }
    friend double toDoubleForPrinting(ShortRat const &s)//change this to produce string
    {
      return ((double)s.c)/s.d;
    }
    friend std::ostream& operator<<(std::ostream& s, const ShortRat &a)
    {
      s<<toDoubleForPrinting(a);
      return s;
    }
    friend bool isNegative(ShortRat const &a)
    {
      return a.c<0&&a.d>0 || a.c>0&&a.d<0;
    }
    friend bool isPositive(ShortRat const &a)
    {
      return a.c<0&&a.d<0 || a.c>0&&a.d>0;
    }
    friend bool isEpsilonLessThan(ShortRat const &a, ShortRat const &b)
    {
      return isNegative(a-b);
    }
    friend bool operator<(ShortRat const &a, ShortRat const &b)
    {
      return isNegative(a-b);
    }
  };
}
