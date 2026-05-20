/* This code written by Franziska Hinkelmann is in the public domain */

#include <set>
#include <vector>
#include <iostream>
#include <list>
#include <map>
#include <string>

//////// CAREFUL ////////
// addition is not const! It changes this
//////// CAREFUL ////////

typedef unsigned long brMonomial;

struct lex
{
  bool operator()(const brMonomial &lhs, const brMonomial &rhs) const
  {
    return lhs > rhs;
  }
};

typedef std::list<brMonomial> monomials;
typedef std::set<brMonomial, lex> monomials_set;

class BRP
{
  friend std::ostream &operator<<(std::ostream &out, const BRP &self)
  {
    for (monomials::const_iterator i = self.m.begin(); i != self.m.end(); ++i)
      {
        out << *i;
        out << " ";
      }
    return out;
  }

 public:
  monomials m;  // this is the ordered list of monomials represented as integers

  static bool isDivisibleBy(const brMonomial &a, const brMonomial &b)
  {
    // check if a is divisible by b
    brMonomial lcm = a | b;
    return lcm == a;
  }

  static bool isRelativelyPrime(const brMonomial &a, const brMonomial &b)
  {
    // check if a and b are relatively prime
    return (((a | b) ^ a) == b);
  }

  BRP(){};
  BRP(const monomials &other) { m = other; }
  BRP(const monomials_set &other);
  BRP(const brMonomial &val) { m.push_back(val); }
  bool isZero() const;
  bool operator==(const brMonomial &val) const;
  bool operator!=(const brMonomial &val) const { return !((*this) == val); }
  bool operator==(const BRP &other) const { return m == other.m; }
  bool operator!=(const BRP &other) const { return !((*this) == other); }
  BRP &operator+(const BRP &other);
  void addition(const BRP &other, monomials::iterator pos);

  BRP operator*(const BRP &other) const;
  BRP operator*(const brMonomial &other) const;

  unsigned int size() const { return static_cast<unsigned int>(m.size()); }
  bool isLeadingReducibleBy(const BRP &other) const;
  bool isLeadingReducibleBy(const brMonomial &other) const;

  BRP remainder(const BRP &x)
      const;  // this = ax + b, return remainder b of division by x

  brMonomial LT() const
  {
    //    if ( (*this) == 0 ) {
    //      cerr << "ERROR, calling LT on 0" << endl;
    //      throw "calling LT on 0 polynomial";
    //    }
    return *(m.begin());
  }  // Leading Term

  bool reduceTail(const BRP &g);
  // reduce tail of f with leading term of g
  // return true if a change happened, otherwise false
  // f is being changed to its reduction
};

typedef std::map<int, BRP> IntermediateBasisMap;
typedef std::map<int, BRP> IntermediateBasis;
// typedef std::vector<BRP*> IntermediateBasis;

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
