#include "brp.h"

BRP::BRP() {
  //cout << "empty init" << endl;
}

BRP::BRP(const list<int> &alist) {
  //cout << "called init" << endl;
  mylist = mod2(alist);
}

BRP::BRP(int val) {
  mylist.push_back(val);
}

bool BRP::operator!=(int val) const {
  return !( (*this) == val);
}
bool BRP::operator==(int val) const{
  if(val == 0) {
    return mylist.empty();
  } else if(val == 1) {
    return mylist.size() == 1 && mylist.front() == 0;
  }
  return false;
}

bool BRP::operator==(const BRP &other) const {
  return mylist == other.mylist;
}

bool BRP::operator!=(const BRP &other) const{
  return !(  (*this) == other);
}

BRP BRP::operator+(BRP other) {
  //list<int> tmp2 = other.mylist;
  mylist.merge(other.mylist);
  return BRP(mylist); 
}

BRP BRP::operator*(const BRP &other) const {
  // other _must_ be a monomial
  if(other == 0) {
    cout << "Multiplication by 0" << endl;
    return BRP();
  } else if(other.mylist.size() != 1) {
    throw "Not monomial";
  } else {
    int monomial = other.mylist.front();
    list<int> tmp;
    for(list<int>::const_iterator i=mylist.begin(); i != mylist.end(); ++i) {
      tmp.push_back(*i | monomial);
    }
    return BRP(tmp);
  }
}

BRP BRP::operator*(int other) const {
  list<int> tmp;
  for(list<int>::const_iterator i=mylist.begin(); i != mylist.end(); ++i) {
    tmp.push_back(*i | other);
  }
  return BRP(tmp);
}

int BRP::operator/(const BRP &other) const {
  // divide a by b
  int a = mylist.front();
  int b = other.mylist.front();
  return (a^b);
}

int BRP::operator/(int other) const {
  // divide a by b
  int a = mylist.front();
  return (a^other);
}

bool BRP::isDivisibleBy(const BRP &other) const {
  // both _must_ be a monomial
//  if(other.mylist.size() != 1 || mylist.size() != 1) {
//    throw "Not monomial";
//  }
  // check if a is divisible by b
  int a = mylist.front();
  int b = other.mylist.front();
  int lcm = a | b;
  return lcm == a;
}

bool BRP::isLeadingReducibleBy(const BRP &other) const {
  BRP a = BRP(LT());
  BRP b = BRP(other.LT());
  return a.isDivisibleBy(b);
}

bool BRP::leadingIsRelativelyPrime(const BRP &other) const {
  // check if a is relatively prime with regards to b
  int a = LT();
  int b = other.LT();
  return ( ((a | b) ^ a) == b);
}

// write f as f = ax+b, return a
BRP BRP::remainder(const BRP &x) const {
  list<int> tmp;
  for(list<int>::const_iterator i=mylist.begin(); i != mylist.end(); ++i) {
    BRP m = BRP(*i);
    if ( !m.isDivisibleBy(x) ) {
      tmp.push_back( m.mylist.front() );
    }
  }
  return BRP(tmp);
}

int BRP::LT() const {
  return mylist.front();
}
