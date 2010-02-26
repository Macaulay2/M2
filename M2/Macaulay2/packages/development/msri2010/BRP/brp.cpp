#include "brp.h"

BRP::BRP(const monomials_set &other) {
  for (set<int>::iterator it = other.begin(); it != other.end(); ++it) {
    m.push_back(*it);
  }
}
bool BRP::operator==(const int &val) const{
  if(val == 0) {
    return m.empty();
  } else if(val == 1) {
    return m.size() == 1 && *(m.begin()) == 0;
  }
  return false;
}


BRP& BRP::operator+(const BRP &other) { // careful here!
  // a+b changes a, and return a reference to a ( which is now equal to a+b)
  
  // merge m with other.m, while removing doubles
  monomials::iterator pos = m.begin();
  monomials::const_iterator it = other.m.begin(); 
  monomials::iterator pos_end = m.end();
  monomials::const_iterator other_end = other.m.end();
  while (it != other_end && pos != pos_end) {
    //if ( funccompGRL(*it, *pos) ) {
    if ( *it > *pos ) {
      m.insert(pos, *it);
      ++it;
    } else if ( *it == *pos ) {
      pos = m.erase(pos);
      ++it;
    } else {
      ++pos;
    }
  }
  if ( pos == pos_end ) {
    m.insert(pos, it, other_end );
  }
  return *this;
}

BRP BRP::operator*(const BRP &other) const {
  // other _must_ be a monomial
  if(other == 0) {
    cout << "Multiplication by 0" << endl;
    return BRP();
  } else {
    int mono = *(other.m.begin());
    return (*this) * mono;
  }
}

BRP BRP::operator*(const int &other) const {
  BRP tmp;
  monomials::const_iterator end = m.end();
  int last = -1;
  for( monomials::const_iterator it = m.begin(); it != end; it++ ) {
    int mono = other|*it;
    if (last == mono) {
      tmp.m.pop_back();
      last = -1;
    } else {
      tmp.m.push_back(mono);
      last = mono;
    }
  }
  //tmp.m.sort(funccompGRL);
  tmp.m.sort(funccomp);
  monomials::iterator it = tmp.m.begin();
  monomials::iterator lastIt = tmp.m.begin();
  monomials::iterator tmpEnd = tmp.m.end();
  it++;
  while( it != tmpEnd ) {
    if ( *lastIt == *it ) {
      lastIt = tmp.m.erase(lastIt, ++it);
      if ( lastIt != end ) {
        ++it;
      }

    } else {
      ++it;
      ++lastIt;
    }
  }
  return tmp;
}

bool BRP::isLeadingReducibleBy(const BRP &other) const {
  return isDivisibleBy(LT(), other.LT());
}

bool BRP::isLeadingReducibleBy(const int &other) const {
  return isDivisibleBy(LT(), other);
}

// write f as f = ax+b, return a
BRP BRP::remainder(const BRP &x) const {
  monomials tmp;
  monomials::const_iterator end = m.end();
  for(monomials::const_iterator it = m.begin(); it != end; it++) {
    int mono = *it;
    if ( !isDivisibleBy(mono, x.LT() ) ) {
      tmp.push_back(mono); // don't remove doubles, there shouldn't be any
    }
  }
  return BRP(tmp);
}

// reduce all term of f with leading term of g
// return true if a change happened, otherwise false
// f is being changes to its reduction
bool BRP::reduceLowerTerms(const BRP &g) {
  int LT = g.LT();
  bool ret = false;
  monomials::iterator it = m.begin();
  monomials::iterator end = m.end();
  for( ; it != end; ) {
    int mono = *it;
    if (isDivisibleBy(mono,LT) ) {
      (*this) + g*(mono^LT);
      it = m.begin();
      ret = true;
    }
    if (LT>mono) { // stop iterating because smaller are never divisible by larger
      return ret;
    }
    ++it;
  }
  return ret;
}
  

int Bits::numberOfBits(int v) {
  int c;
  for (c = 0; v; v >>= 1)
  {
    c += v & 1;
  }
  return c;
}

// for a and b with same degree
// true if a >= b
bool Bits::reverseLex(int a, int b) {
  for (; a; a>>=1, b>>=1) {
    int lastBitA = a & 1;
    int lastBitB = b & 1;
    if (lastBitA - lastBitB < 0 ) {
      return true;
    } else if (lastBitA - lastBitB > 0) {
      return false;
    } 
  }
  return true;
}


