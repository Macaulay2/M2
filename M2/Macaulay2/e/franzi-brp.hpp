/* This code written by Franziska Hinkelmann is in the public domain */

/**
 * @file franzi-brp.hpp
 * @brief `brMonomial` --- bit-packed Boolean-ring monomials for the Hinkelmann GB engine.
 *
 * Declares `brMonomial = unsigned long`, the bit-set encoding of a
 * monomial in `F_2[x_1, ..., x_n] / (x_i^2 - x_i)`: bit `i` is set
 * iff variable `x_i` appears. Multiplication of monomials is
 * bitwise OR (idempotent exponents) and divisibility of `a` by `b`
 * is `(a | b) == a`; the coprimality test in `isRelativelyPrime`
 * is the bit-twiddling equivalent of `(a & b) == 0`. The
 * polynomial type `BRP` stores its terms as a sorted
 * `std::list<brMonomial>` and adds in F_2 by walking the two
 * lists and dropping any monomial that appears in both. The
 * `brMonomial` encoding caps the engine at 64 variables ---
 * `rawGbBoolean` errors out for larger inputs.
 *
 * The companion files in the `franzi-*` family (`franzi-brp.cpp`,
 * `franzi-brp-test.cpp`, `franzi-gb.cpp`, `franzi-interface.cpp`)
 * carry the polynomial-level operations, a standalone test
 * driver, the Buchberger-style GB algorithm, and the
 * `extern "C"` entry point `rawGbBoolean` --- which sits outside
 * the standard `comp-gb.cpp` GB dispatcher and is reached via its
 * own M2-level top-level call. The `// addition is not const`
 * banner in this header is deliberate: `+=` mutates the left
 * operand for speed, a pitfall for the engine's `const`-careful
 * code elsewhere. The sibling `bibasis/` directory offers an
 * alternative (variable-unbounded, involutive-basis) Boolean GB
 * engine.
 *
 * @see comp-gb.hpp
 */

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

/**
 * @brief Boolean (`F_2`-coefficient) polynomial stored as an ordered list of
 * square-free monomials.
 *
 * @details Coefficient ring is implicit (always 1 in `F_2`); the polynomial
 * is just the symmetric-difference sum of the listed monomials.
 * Underlying storage is a `monomials` (`std::list<brMonomial>`)
 * kept sorted under the lex comparator declared above, so the
 * leading term is `m.front()` and addition is a linear merge.
 * Used by the Franzi GB code path for boolean polynomial rings.
 */
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
