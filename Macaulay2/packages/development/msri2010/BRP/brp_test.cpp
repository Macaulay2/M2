#include "brp.h"

void testEquality0() {
  BRP bar = BRP();
  if (bar != 0 ) {
    cout << "Testing equality = 0: " << (bar == 0 ? "is 0" : "not 0") << endl;
  }
  if (bar == 1 ) {
    cout << "Testing equality = 0: " << (bar == 0 ? "is 0" : "not 0") << endl;
  }
  if ( (bar == 0) == false ) {
    cout << "Testing equality = 0: " << (bar == 0 ? "is 0" : "not 0") << endl;
  }
}

void testEquality1() {
  list<int> foo;
  foo.push_back(0);
  BRP bar = BRP(foo);
  if (bar != 1) {
    cout << "Testing equality = 1: " << (bar == 1? "is 1" : "not 1") << endl;
  }
  if (bar == 0) {
    cout << "Testing equality = 0: " << (bar == 0? "is 1" : "not 1") << endl;
  }
  if ( (bar == 1) == false ) {
    cout << "Testing equality = 1: " << (bar == 1? "is 1" : "not 1") << endl;
  }
}

void testEquality() {
  list<int> foo;
  foo.push_back(0);
  foo.push_back(3);
  foo.push_back(8);
  BRP bar = BRP(foo);
  if (bar != bar) {
    cout << "Testing same BRP for equality failed" << endl;
  }
  
  list<int> foo1;
  foo1.push_back(0);
  foo1.push_back(3);
  foo1.push_back(8);
  BRP bar1 = BRP(foo1);
  if (bar1 != bar) {
    cout << "Testing equal BRPs for equality failed" << endl;
  }
  
  list<int> foo2;
  foo2.push_back(1);
  foo2.push_back(3);
  foo2.push_back(8);
  BRP bar2 = BRP(foo2);
  if ( (bar2 != bar) == false) {
    cout << "Testing different BRPs for equality failed" << endl;
  }
  if (bar2 == bar) {
    cout << "Testing different BRPs for equality failed" << endl;
  }
  
}

void testAddition() {
  list<int> foo;
  foo.push_back(1);
  foo.push_back(2);
  foo.push_back(3);
  list<int> foo2;
  foo2.push_back(3);
  foo2.push_back(4);
  foo2.push_back(5);
  BRP bar = BRP(foo);
  BRP bar2 = BRP(foo2);

  BRP bar3 = bar + bar2;

  list<int> foo3;
  foo3.push_back(2);
  foo3.push_back(4);
  foo3.push_back(1);
  foo3.push_back(5);

  BRP bar4 = BRP(foo3);
 
  if (bar3 != bar4 ) {
    cout << "Testing addition: " << (bar3 == bar4 ? "is good" : "is bad") << endl;
  }
}

void testMultiplication() {
  list<int> foo;
  foo.push_back(14);
  foo.push_back(1);
  list<int> foo2;
  foo2.push_back(8);

  BRP bar = BRP(foo);
  BRP bar2 = BRP(foo2);

  BRP barMultiply = bar * bar2;

  list<int> foo3;
  foo3.push_back(14);
  foo3.push_back(9);

  BRP bar3 = BRP(foo3);
  if (barMultiply != bar3) {
    cout << "Testing multiplication: " << (barMultiply == bar3 ? "is good" : "is bad") << endl;
  }

}

void testDivide() {
  list<int> foo;
  foo.push_back(14);
  list<int> foo2;
  foo2.push_back(1);
  list<int> foo3;
  foo3.push_back(8);

  BRP bar = BRP(foo);
  BRP bar2 = BRP(foo2);
  BRP bar3 = BRP(foo3);

  BRP barDivide = bar / bar3;

  list<int> foo4;
  foo4.push_back(6);

  BRP bar4 = BRP(foo4);

  if ( bar.isDivisibleBy(bar2)) {
    cout << "Testing isDivisibleBy: " << (bar.isDivisibleBy(bar2) ? "is bad" : "is good") << endl;
  }
  if (barDivide != bar4 ) {
    cout << "Testing divide: " << (barDivide == bar4 ? "is good" : "is bad") << endl;
  }
}

void testLT() {
  list<int> foo;
  foo.push_back(3);
  foo.push_back(8);
  foo.push_back(5);
  foo.push_back(5);
  
  BRP bar = BRP(foo);
  
  if ( bar.LT() != 8 ) {
    cout << "Testing LT(): " << (bar.LT() == 8 ? "is good" : "is bad") << endl;
  }

  list<int> foo1;
  foo1.push_back(0); // foo1 == 1
  BRP bar1 = BRP(foo1);
  if ( bar1.LT() != 0 ) {
    cout << "Testing LT(): " << (bar.LT() == 8 ? "is good" : "is bad") << endl;
  }
  if ( BRP(bar1.LT()) != bar1 ) {
    cout << "Testing LT(): " << (bar.LT() == 8 ? "is good" : "is bad") << endl;
  }
}
 
void clearAll(list<int> l) {
  l.clear();
}

void testLeadingIsRelativelyPrime() {
  list<int> foo;
  foo.push_back(3);
  foo.push_back(8);
  foo.push_back(5);
  foo.push_back(5);
 
  clearAll(foo);
//  foo.clear();
  BRP bar = BRP(foo);
  
  list<int> foo1;
  foo1.push_back(8);
  BRP barA = BRP(foo1);
  foo1.push_back(3);
  foo1.push_back(5);
  foo1.push_back(5);
  
  BRP bar1 = BRP(foo1);
  
  list<int> fooB;
  fooB.push_back(1);
  BRP barB = BRP(fooB);
  
  if ( ! barB.leadingIsRelativelyPrime(barA) ) {
    cout << "should be relatively prime" << endl;
  }
  if ( !barB.leadingIsRelativelyPrime(bar1) ) {
    cout << "w and x are rel prime" << endl;
  }
  if ( barA.leadingIsRelativelyPrime(bar1) ) {
    cout << "error checking same BRPs for relatively primeness" << endl;
  }
  if ( bar.leadingIsRelativelyPrime(bar1) ) {
    cout << "error checking same BRPs for relatively primeness" << endl;
  }
  if ( bar.leadingIsRelativelyPrime(bar) ) {
    cout << "error checking same BRPs for relatively primeness" << endl;
  }

  BRP x = BRP(8);
  BRP w = BRP(1);
  BRP zw = BRP(3);
  BRP barC = zw + BRP(w); // zw + w
  if ( ! x.leadingIsRelativelyPrime(w) ) {
    cout << "x and w are rel prime" << endl;
  }
  if ( !x.leadingIsRelativelyPrime(zw) ) {
    cout << "x and zw are rel prime" << endl;
  }
  if ( w.leadingIsRelativelyPrime(barC) ) {
    cout << "w and zw+w  should not be rel prime" << endl;
  }
  if ( !x.leadingIsRelativelyPrime(barC) ) {
    cout << "x and zw+z should be rel rel prime" << endl;
  }
  if ( ! x.leadingIsRelativelyPrime(barC) ) {
    cout << "x and w are rel prime" << endl;
  }
}

void testIsLeadingReducibleBy() {
  BRP f = BRP(2) * BRP(8) + BRP(7);
  if ( !f.isLeadingReducibleBy( BRP(8) ) ) {
    cout << "error" << endl;
  }
  if ( !f.isLeadingReducibleBy( BRP(2) ) ) {
    cout << "error" << endl;
  }
  if ( f.isLeadingReducibleBy( BRP(1) ) ) {
    cout << "error" << endl;
  }
  
}
  
void testRemainder() { //f = ax+g 
  BRP f = BRP(2) * BRP(8) + BRP(7);
  if ( (BRP(7) )  != f.remainder(BRP(8) ) ) { 
    cout << "error" << endl;
  }
  if ( (BRP(2) )  == f.remainder(BRP(8) ) ) { 
    cout << "error" << endl;
  }

  BRP a = BRP(8) * BRP(2);
  if ( a != f.remainder( BRP(7) ) ){
    cout << "error" << endl;
  }
}

int main() {
  cout << "Hello Testing Friends!" << endl;

  testEquality0();
  testEquality1();
  testEquality();
  testAddition();
  testMultiplication();
  testDivide();
  testLT();
  testLeadingIsRelativelyPrime();
  testIsLeadingReducibleBy();
  testRemainder();
}

