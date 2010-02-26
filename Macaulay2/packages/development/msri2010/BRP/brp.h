#include <set>
#include <iostream>
#include <list>
#include <map>
#include <string>

//////// CAREFUL ////////
// multiplication and addition are not const! they don't work as expected but
//change one of the arguments!
//////// CAREFUL ////////

using namespace std;

inline bool funccomp(const int &a, const int &b) {
  return b < a;
}

class Bits
{
  public:
  static int numberOfBits(int a);
  static bool reverseLex(int a, int b);
};

inline bool funccompGRL(const int &lhs, const int &rhs) {
  if (Bits::numberOfBits(lhs) > Bits::numberOfBits(rhs) ) {
    return true;
  } else if (Bits::numberOfBits(lhs) < Bits::numberOfBits(rhs) ) {
    return false;
  } else {
    return Bits::reverseLex(lhs,rhs);
  }
}

struct lex {
  bool operator() (const int& lhs, const int& rhs) const {
    return lhs>rhs;
  }
};

struct gRevLex {
  bool operator() (const int& lhs, const int& rhs) const {
    if (Bits::numberOfBits(lhs) > Bits::numberOfBits(rhs) ) {
      return true;
    } else if (Bits::numberOfBits(lhs) < Bits::numberOfBits(rhs) ) {
      return false;
    } else {
      return Bits::reverseLex(lhs,rhs);
    }
  }
};

typedef list<int> monomials;
typedef set<int,lex> monomials_set;

class BRP
{

  friend ostream& operator<< (ostream &out, const BRP &self) {
    for(monomials::const_iterator i = self.m.begin(); i != self.m.end(); ++i) {
      out << *i;
      out << " ";
    }
    return out;
  }

  public:
  
  monomials m; // this is the ordered list of monomials represented as integers

  static bool isDivisibleBy(const int &a, const int &b) {
    // check if a is divisible by b
    int lcm = a | b;
    return lcm == a;
  }

  static bool isRelativelyPrime( const int &a, const int &b) {
    // check if a and b are relatively prime
    return ( ((a | b) ^ a) == b);
  }

  BRP() {};
  BRP(const monomials &other) { m = other; }
  BRP(const monomials_set &other);
  BRP(const int &val) { m.push_back(val); }

  bool operator==(const int &val) const;
  bool operator!=(const int &val) const { return !( (*this) == val); }

  bool operator==(const BRP &other) const { return m == other.m; }
  bool operator!=(const BRP &other) const { return !(  (*this) == other); }

  BRP& operator+(const BRP &other);

  BRP operator*(const BRP &other) const;
  BRP operator*(const int &other) const;

  bool isLeadingReducibleBy(const BRP &other) const;
  bool isLeadingReducibleBy(const int &other) const;
  
  BRP remainder(const BRP &x) const;

  int LT() const { return *(m.begin()); }

  bool reduceLowerTerms(const BRP &g);

};


