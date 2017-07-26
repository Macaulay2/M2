/* This code written by Franziska Hinkelmann is in the public domain */

#include "franzi-brp.hpp"

BRP::BRP(const monomials_set &other)
{
  for (monomials_set::iterator it = other.begin(); it != other.end(); ++it)
    {
      m.push_back(*it);
    }
}

bool BRP::isZero() const { return m.empty(); }
bool BRP::operator==(const brMonomial &val) const
{
  if (val == 0)
    {
      return m.empty();
    }
  else if (val == 1)
    {
      return m.size() == 1 && *(m.begin()) == 0;
    }
  return false;
}

void BRP::addition(const BRP &other, monomials::iterator pos)
{
  // merge m with other.m, while removing doubles
  monomials::const_iterator it = other.m.begin();
  monomials::iterator pos_end = m.end();
  monomials::const_iterator other_end = other.m.end();
  while (it != other_end && pos != pos_end)
    {
      // if ( funccompGRL(*it, *pos) ) {
      if (*it > *pos)
        {
          m.insert(pos, *it);
          ++it;
        }
      else if (*it == *pos)
        {
          pos = m.erase(pos);
          ++it;
        }
      else
        {
          ++pos;
        }
    }
  if (pos == pos_end)
    {
      m.insert(pos, it, other_end);
    }
}

BRP &BRP::operator+(const BRP &other)
{  // careful here!
  addition(other, m.begin());
  return *this;
  // a+b changes a, and return a reference to a ( which is now equal to a+b)
}

BRP BRP::operator*(const BRP &other) const
{
  // other _must_ be a monomial
  if (other == 0)
    {
      std::cout << "Multiplication by 0" << std::endl;
      return BRP();
    }
  else
    {
      brMonomial mono = *(other.m.begin());
      return (*this) * mono;
    }
}

inline bool funccomp(const brMonomial &a, const brMonomial &b) { return b < a; }
BRP BRP::operator*(const brMonomial &other) const
{
  BRP tmp;
  monomials::const_iterator end = m.end();
  brMonomial last = -1;  // set this to -1 so it never is the same as mono, if
                         // we set it to 0, we have problems when multiplying
                         // 1*1
  for (monomials::const_iterator it = m.begin(); it != end; it++)
    {
      brMonomial mono = other | *it;
      if (last == mono)
        {
          tmp.m.pop_back();
          last = -1;
        }
      else
        {
          tmp.m.push_back(mono);
          last = mono;
        }
    }
  tmp.m.sort(funccomp);
  monomials::iterator it = tmp.m.begin();
  monomials::iterator lastIt = tmp.m.begin();
  monomials::iterator tmpEnd = tmp.m.end();
  it++;
  while (it != tmpEnd)
    {
      if (*lastIt == *it)
        {
          lastIt = tmp.m.erase(lastIt, ++it);
          if (lastIt != end)
            {
              ++it;
            }
        }
      else
        {
          ++it;
          ++lastIt;
        }
    }
  return tmp;
}

bool BRP::isLeadingReducibleBy(const BRP &other) const
{
  //  if ( (*this) == 0 ) cout << "This is 0 " << endl;
  //  if ( other == 0 ) cout << "Other is 0 " << endl;
  return isDivisibleBy(LT(), other.LT());
}

bool BRP::isLeadingReducibleBy(const brMonomial &other) const
{
  return isDivisibleBy(LT(), other);
}

// write f as f = ax+b, return b
BRP BRP::remainder(const BRP &x) const
{
  monomials tmp;
  monomials::const_iterator end = m.end();
  brMonomial xx = x.LT();
  for (monomials::const_iterator it = m.begin(); it != end; it++)
    {
      brMonomial mono = *it;
      if (!isDivisibleBy(mono, xx))
        {
          tmp.push_back(mono);  // don't remove doubles, there shouldn't be any
        }
    }
  return BRP(tmp);
}

// reduce all terms of f with leading term of g until no term of f can be
// cancelled
// return true if a change happened, otherwise false
// f is being changed to its reduction
//
bool BRP::reduceTail(const BRP &g)
{
  // cout << "reduceTail: this = " << (*this) << ", by g = " << g << endl;
  //  if ( (*this) == 0 ) {
  //    cerr << "Called reduceTail on 0 brp" << endl;
  //    return false;
  //  }
  brMonomial lt = g.LT();
  bool ret = false;
  monomials::iterator it = m.begin();
  ++it;  // really only reduce tail
  monomials::iterator end = m.end();
  for (; it != end;)
    {
      brMonomial mono = *it;
      if (isDivisibleBy(mono, lt))
        {
          // cout << mono << " is divisible by " << lt << endl;
          addition(g * (mono ^ lt), it--);
          // cout << *this << endl;
          ++it;
          ret = true;
        }
      if (lt > mono)
        {  // stop iterating because smaller are never divisible by larger
          return ret;
        }
      ++it;
    }
  // cout << "at the end of reduceTail, this = " << (*this) << endl;
  return ret;
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
