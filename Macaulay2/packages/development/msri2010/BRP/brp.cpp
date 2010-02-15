#include "brp.h"

BRP::BRP() {
  //cout << "empty init" << endl;
}

BRP::BRP(list<int> alist) {
  //cout << "called init" << endl;
  mylist = mod2(alist);
}

BRP::BRP(int val) {
  mylist.push_back(val);
}

bool BRP::operator!=(int val) {
  return (! this == val);
}
bool BRP::operator==(int val) {
  if(val == 0) {
    return mylist.empty();
  } else if(val == 1) {
    return mylist.size() == 1 && mylist.front() == 0;
  }
  return false;
}

bool BRP::operator==(BRP other) {
  return mylist == other.mylist;
}

BRP BRP::operator+(BRP other) {
  list<int> tmp = other.mylist;
  list<int> tmp2 = mylist;
  tmp.merge(tmp2);
  return BRP(tmp); 
}

BRP BRP::operator*(BRP other) {
  // other _must_ be a monomial
  if(other == 0) {
    cout << "Multiplication by 0" << endl;
    return BRP();
  } else if(other.mylist.size() != 1) {
    throw "Not monomial";
  } else {
    int monomial = other.mylist.front();
    list<int> tmp;
    for(i=mylist.begin(); i != mylist.end(); ++i) {
      tmp.push_back(*i | monomial);
    }
    return BRP(tmp);
  }
}

BRP BRP::operator/(BRP other) {
  if(!isDivisibleBy(other)) {
    throw "not divisible";
  }
  // divide a by b
  int a = mylist.front();
  int b = other.mylist.front();
  list<int> tmp;
  tmp.push_back(a ^ b);
  return BRP(tmp);
  
}

bool BRP::isDivisibleBy(BRP other) {
  // both _must_ be a monomial
  if(other.mylist.size() != 1 || mylist.size() != 1) {
    throw "Not monomial";
  }
  // check if a is divisible by b
  int a = mylist.front();
  int b = other.mylist.front();
  int lcm = a | b;
  return lcm == a;
}

bool BRP::isLeadingReducibleBy(BRP other) {
  BRP a = BRP(LT());
  BRP b = BRP(other.LT());
  return a.isDivisibleBy(b);
}

bool BRP::leadingIsRelativelyPrime(BRP other) {
  // check if a is relatively prime with regards to b
  int a = LT();
  int b = other.LT();
  return ((a | b) ^ a) == b;
}

// write f as f = ax+b
BRP BRP::divisiblePart(BRP x){
  list<int> tmp;
  for(i=mylist.begin(); i != mylist.end(); ++i) {
    BRP m = BRP(*i);
    if ( m.isDivisibleBy(x) ) {
      BRP t = m / x;
      tmp.push_back( t.mylist.front() );
    }
  }

  return BRP(tmp);
}

int BRP::LT() {
  return mylist.front();
}
