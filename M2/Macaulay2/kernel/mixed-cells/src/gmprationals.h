  /** Arbitrary precision rational numbers using gmp.
   */
#include "gmp.h"

#include <iostream>

namespace mixedCells
{
  class GmpRational{
  mpq_t value;
  public:
    GmpRational(int a, int b)
    {
      //std::cerr<<"constructingAB"<<this<<" "<<a<<" "<<b<<"\n";
      assert (b!=0);
      mpq_init(value);
      mpq_set_si(value,a,b);
    }
    GmpRational(int a)
    {
      //std::cerr<<"constructingA"<<this<<" "<<a<<"\n";
      mpq_init(value);
      mpq_set_si(value,a,1);
    }
    GmpRational(const GmpRational &a)
    {
      //std::cerr<<"copyconstructing"<<this<<&a<<"\n";
      mpq_init(value);
      mpq_set(value,a.value);
    }
    GmpRational& operator=(const GmpRational& a)
      {
	//std::cerr<<"assigning"<<this<<&a<<"\n";
	if (this != &a) {
	  mpq_clear(value);
	  mpq_init(value);
	  mpq_set(value,a.value);
	}
	return *this;
      }
    GmpRational()
    {
      //std::cerr<<"constructing"<<this<<"\n";
      mpq_init(value);
      mpq_set_si(value,0,1);
    }
    ~GmpRational()
    {
      //std::cerr<<"destructing"<<this<<"\n";
      mpq_clear(value);
    }
    int toInteger() const
    {
      assert(isInteger());
      return mpz_get_si(mpq_numref(value));
    }
    bool isInteger() const
    {
      //std::cerr<<"A1"<<std::endl;
      return mpz_cmp_si(mpq_denref(value),1)==0;
    }
    MY_INLINE static bool isField() 
    {
      return true;
    }
    friend bool isZero(GmpRational const &a)
    {
      //std::cerr<<"A1"<<std::endl;
      return mpq_sgn(a.value)==0;
    }
    friend bool isZero2(GmpRational const &a)
    {
      //std::cerr<<"A1"<<std::endl;
      return mpq_sgn(a.value)==0;
    }
    friend bool isOne(GmpRational const &a)
    {
      //std::cerr<<"A1"<<std::endl;
      return mpq_cmp_si(a.value,1,1)==0;
    }
    friend GmpRational operator/(GmpRational const &a, GmpRational const &b)
    {
      //std::cerr<<"A1"<<std::endl;
      GmpRational ret;
      assert (!isZero(b));
      mpq_div(ret.value,a.value,b.value);
      return ret;
    }
    friend GmpRational operator-(GmpRational const &a)
    {
      //std::cerr<<"A1"<<std::endl;
      GmpRational ret;
      mpq_neg(ret.value,a.value);
      return ret;
    }
    friend GmpRational operator-(GmpRational const &a, GmpRational const &b)
    {
      //std::cerr<<"A1"<<std::endl;
      GmpRational ret;
      mpq_sub(ret.value,a.value,b.value);
      return ret;
    }
    friend GmpRational operator+(GmpRational const &a, GmpRational const &b)
    {
      //std::cerr<<"A1"<<std::endl;
      GmpRational ret;
      mpq_add(ret.value,a.value,b.value);
      return ret;
    }
    //    friend class DoubleGen;
    //friend class DoubleGen operator*(GmpRational const &a, DoubleGen const &b);
    friend int volumeToInt(GmpRational const &a)
    {
      //std::cerr<<"A1"<<std::endl;
      assert(a.isInteger());
      return ABS(a.toInteger());
    }
    friend GmpRational operator*(GmpRational const &s, GmpRational const &t)
    {
      //std::cerr<<"A1"<<std::endl;
      GmpRational ret;
      mpq_mul(ret.value,s.value,t.value);
      return ret;
    }
    void operator+=(GmpRational const &a)
    {
      //std::cerr<<"A1"<<std::endl;
      mpq_add(value,value,a.value);
    }
    void operator-=(GmpRational const &a)
    {
      //std::cerr<<"A1"<<std::endl;
      mpq_sub(value,value,a.value);
    }
    void operator/=(GmpRational const &a)
    {
      //std::cerr<<"A1"<<std::endl;
      assert(!isZero(a));
      mpq_div(value,value,a.value);
    }
    void operator*=(GmpRational const &a)
    {
      //std::cerr<<"A1"<<std::endl;
      mpq_mul(value,value,a.value);
    }
    friend GmpRational gcd(GmpRational const &s, GmpRational const &t)
    {
      //std::cerr<<"A1"<<std::endl;
      return GmpRational(1);
    }
    friend double toDoubleForPrinting(GmpRational const &s)//change this to produce string
    {
      //std::cerr<<"A1"<<std::endl;
      return mpq_get_d(s.value);
    }
    friend std::ostream& operator<<(std::ostream& s, const GmpRational &a)
    {
      //std::cerr<<"A1"<<std::endl;
      s<<toDoubleForPrinting(a);
      return s;
    }
    friend bool isNegative(GmpRational const &a)
    {
      //std::cerr<<"A1"<<std::endl;
      return mpq_sgn(a.value)<0;
    }
    friend bool isPositive(GmpRational const &a)
    {
      //std::cerr<<"A1"<<std::endl;
      return mpq_sgn(a.value)>0;
    }
    friend bool isEpsilonLessThan(GmpRational const &a, GmpRational const &b)
    {
      //std::cerr<<"A1"<<std::endl;
      return isNegative(a-b);
    }
    friend bool operator<(GmpRational const &a, GmpRational const &b)
    {
      //std::cerr<<"A1"<<std::endl;
      return isNegative(a-b);
    }
    
  };
}
