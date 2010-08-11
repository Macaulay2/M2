#include "franzi-brp.hpp"
#include <vector>
// g++ franzi-brp.cpp franzi-brp-test.cpp -o franzi-brp-test

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
  monomials  foo;
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
  monomials foo;
  foo.push_back(8);
  foo.push_back(3);
  foo.push_back(0);
  BRP bar = BRP(foo);
  if (bar != bar) {
    cout << "Testing same BRP for equality failed" << endl;
  }
  
  monomials foo1;
  foo1.push_back(8);
  foo1.push_back(3);
  foo1.push_back(0);
  BRP bar1 = BRP(foo1);
  if (bar1 != bar) {
    cout << "Testing equal BRPs for equality failed" << endl;
  }
  
  monomials foo2;
  foo2.push_back(8);
  foo2.push_back(3);
  foo2.push_back(1);
  BRP bar2 = BRP(foo2);
  if ( (bar2 != bar) == false) {
    cout << "Testing different BRPs for equality failed" << endl;
  }
  if (bar2 == bar) {
    cout << "Testing different BRPs for equality failed" << endl;
  }
  
}

void testAddition() {
  monomials foo;
  foo.push_back(3);
  foo.push_back(2);
  foo.push_back(1);
  monomials foo2;
  foo2.push_back(5);
  foo2.push_back(4);
  foo2.push_back(3);
  BRP bar = BRP(foo);
  //cout << bar << endl;
  BRP bar2 = BRP(foo2);
  //  cout << bar2 << endl;

  BRP bar3 = bar + bar2;
  //  cout << bar3 << endl;

  monomials foo3;
  foo3.push_back(5);
  foo3.push_back(4);
  foo3.push_back(2);
  foo3.push_back(1);

  BRP bar4 = BRP(foo3);
 
  if (bar3 != bar4 ) {
    cout << "Testing addition: " << (bar3 == bar4 ? "is good" : "is bad") << endl;
  }
  BRP a = BRP(16) + BRP(15) + BRP(5) + BRP(2);
  BRP b = BRP(13) + BRP(12) + BRP(6) + BRP(4) + BRP(2) + BRP(1);
  BRP c = a+b;
  BRP correct = BRP(16) + BRP(15) + BRP(13) + BRP(12) + BRP(6) + BRP(5) + BRP(4) + BRP(1);

  if ( c != correct ) {
    cout << "Addition failed" << endl;
  }
}
  
void testAddition2() {
  BRP a = BRP(35) + BRP(16) + BRP(15) + BRP(5) + BRP(2);
  BRP b = BRP(38) + BRP(13) + BRP(12) + BRP(6) + BRP(4) + BRP(2) + BRP(1);
  BRP c = a+b;
  BRP correct = BRP(38) + BRP(35) + BRP(16) + BRP(15) + BRP(13) + BRP(12) + BRP(6) + BRP(5) + BRP(4) + BRP(1);

  if ( c != correct ) {
    cout << "Addition failed" << endl;
  }
}
  
void testAddition3() {
  BRP a = BRP(35) + BRP(16) + BRP(15) + BRP(5) + BRP(2) + BRP(0);
  BRP b = BRP(38) + BRP(13) + BRP(12) + BRP(6) + BRP(4) + BRP(2) + BRP(1);
  BRP c = a+b;
  BRP correct = BRP(38) + BRP(35) + BRP(16) + BRP(15) + BRP(13) + BRP(12) + BRP(6) + BRP(5) + BRP(4) + BRP(1) + BRP(0);

  if ( c != correct ) {
    cout << "Addition failed" << endl;
  }
}

void testAddition4() {
  BRP a = BRP(35) + BRP(16) + BRP(15) + BRP(5) + BRP(2) + BRP(0);
  BRP b = BRP(35) + BRP(13) + BRP(12) + BRP(6) + BRP(4) + BRP(2) + BRP(1);
  BRP c = a+b;
  BRP correct = BRP(16) + BRP(15) + BRP(13) + BRP(12) + BRP(6) + BRP(5) + BRP(4) + BRP(1) + BRP(0);

  if ( c != correct ) {
    cout << "Addition failed" << endl;
  }
}

void testMultiplication() {
  monomials foo;
  foo.push_back(14);
  foo.push_back(1);
  BRP bar = BRP(foo);
  //cout << bar << endl;

  monomials foo2;
  foo2.push_back(8);
  BRP bar2 = BRP(foo2);
  //cout << bar2 << endl;

  BRP barMultiply = bar * bar2;
  //cout << barMultiply << endl;

  monomials foo3;
  foo3.push_back(14);
  foo3.push_back(9);

  BRP bar3 = BRP(foo3);
  if (barMultiply != bar3) {
    cout << "Testing multiplication: " << (barMultiply == bar3 ? "is good" : "is bad") << endl;
  }
  
  BRP a = BRP(13) + BRP(12) + BRP(6) + BRP(4) + BRP(2) + BRP(1);
  BRP b = BRP(13);
  BRP c = a*b;
  // 13 + 13 + 15 + 13 + 15 + 13
  BRP correct = BRP();

  if (c != correct ) { 
    cout << "Multiplication not correct. Is " << c << endl;
  }
  
  a =  BRP(13) + BRP(12) + BRP(6) + BRP(4) + BRP(2) + BRP(1);
  int d = 13;
  c = a*d;
  // 13 + 13 + 15 + 13 + 15 + 13
  correct = BRP();

  if (c != correct ) { 
    cout << "Multiplication not correct. Is " << c << endl;
  }
  
  a = BRP (16) + BRP(13) + BRP(12) + BRP(6) + BRP(4) + BRP(2) + BRP(1);
  b = BRP(13);
  c = a*b;
  // 29 + 13 + 13 + 15 + 13 + 15 + 13
  correct = BRP(29);
  if (c != correct ) { 
    cout << "Multiplication not correct. Is " << c << endl;
  }

  a = BRP (16) + BRP(13) + BRP(12) + BRP(6) + BRP(4) + BRP(2) + BRP(1);
  d = 13;
  c = a*d;
  // 29 + 13 + 13 + 15 + 13 + 15 + 13
  correct = BRP(29);
  if (c != correct ) { 
    cout << "Multiplication not correct. Is " << c << endl;
  }

  a = BRP (16) + BRP(13) + BRP(12) + BRP(6) + BRP(4); 
  d = 220;
  c = a*d;
  // 220 + 221 + 220 + 222 + 220 
  correct = BRP(222) + BRP(221) + BRP(220);
  if (c != correct ) { 
    cout << "Multiplication not correct. Is " << c << endl;
  }

}

void testDivide() {

  if ( BRP::isDivisibleBy(14, 1)) {
    cout << "Testing isDivisibleBy is bad" << endl;
  }
  if ( !BRP::isDivisibleBy(1, 0)) {
    cout << "Testing isDivisibleBy is bad" << endl;
  }
  int barDivide = 14 ^ 8;
  if (barDivide != 6) {
    cout << "Testing divide: " << (barDivide == 6 ? "is good" : "is bad") << endl;
  }
}

void testLT() {
  monomials foo;
  // w, yz
  foo.push_back(8);
  foo.push_back(3);
  BRP bar = BRP(foo);
  
  if ( bar.LT() != 8 ) {
    cout << "Testing LT(): " << (bar.LT() == 8 ? "is good" : "is bad") << endl;
  }

  monomials foo1;
  foo1.push_back(0); // foo1 == 1
  BRP bar1 = BRP(foo1);
  if ( bar1.LT() != 0 ) {
    cout << "Testing LT(): " << (bar.LT() == 8 ? "is good" : "is bad") << endl;
  }
  if ( BRP(bar1.LT()) != bar1 ) {
    cout << "Testing LT(): " << (bar.LT() == 8 ? "is good" : "is bad") << endl;
  }

  //  xy + wz
  BRP inGRL = BRP(9) + BRP(6);
  if( inGRL.LT() == 9 ) {
    cout << inGRL.LT() << endl;
    cout << "ERROR for leading term, we're in grevlex order" << endl;
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

// the number of bits in an integer is the same as the total degree of a monomial
void testNumberOfBits() {
  if ( Bits::numberOfBits(0) != 0 ) {
    cout << "Error, 1 should have 0 bits" << endl;
  }
  if ( Bits::numberOfBits(1) != 1 ) {
    cout << "Error, z should have 1 bits" << endl;
  }
  if ( Bits::numberOfBits(7) != 3 ) {
    cout << "Error, xyz should have 3 bits" << endl;
  }
  if ( Bits::numberOfBits(6) != 2 ) {
    cout << "Error, xy should have 2 bits" << endl;
  }
};

// assume a,b have the same degree
// true if a >= b
void testReverseLex() {
  // yz, xy
  // yz < xy
  if ( Bits::reverseLex(3, 6) ) {
    cout << "Error, yz in revLex < xy" << endl;
  }

  // yz, xz
  // yz < xz
  if ( Bits::reverseLex(3, 5) ) {
    cout << "Error, yz in revLex < xz" << endl;
  }

  // yz, yz
  // yz >= yz
  if ( Bits::reverseLex(3, 3) ) {
    cout << "Error, yz in revLex = yz" << endl;
  }

  // xy, yz
  // xy > yz
  if ( !Bits::reverseLex(6, 3) ) {
    cout << "Error, xy in revLex > yz" << endl;
  }

  // xy, xz
  // xy > xz
  if ( !Bits::reverseLex(6, 5) ) {
    cout << "Error, xy in revLex > xz" << endl;
  }

  if ( Bits::reverseLex(6, 6) ) {
    cout << "Error, xy in revLex = xy" << endl;
  }

  // w,x,y,z
  // xy, wy
  // xy < wy
  if ( Bits::reverseLex(6, 10) ) {
    cout << "Error, xy in revLex < wy" << endl;
  }

  // xy, wz
  // xy > wz 
  // This is different in Lex
  if ( !Bits::reverseLex(6, 9) ) {
    cout << "Error, xy in revLex > wz" << endl;
  }

};

void testGRevLexSorting() {
  set<int,gRevLex> s;
  // x*y*z + x*y + x*z + y*z + x + y
  s.insert( 6 ); // x*y
  s.insert( 7 ); // x*y*z
  s.insert( 4 ); // x
  s.insert( 5 ); // x*z
  s.insert( 2 ); // y
  s.insert( 3 ); // y*z
 
  int myints [] = {7,6,5,3,4,2};
  vector<int> correct (myints, myints + sizeof(myints) / sizeof(int) );
  int i = 0;
  for (set<int,gRevLex>::iterator it = s.begin(); it != s.end(); ++it, i++ ) {
    if ( correct[i] != *it ) {
      cout << "Error in gRevLex sorting" << endl;
    }
  }  
}

void testIsRelativelyPrime() {
  BRP f = BRP(2) * BRP(8) + BRP(7);
  if ( BRP::isRelativelyPrime( f.LT(), BRP(8).LT() ) ) {
    cout << "error" << endl;
  }
  if ( BRP::isRelativelyPrime( BRP(8).LT(), f.LT() ) ) {
    cout << "error" << endl;
  }
  if ( BRP::isRelativelyPrime( BRP(2).LT(), f.LT() ) ) {
    cout << "error" << endl;
  }
  if ( !BRP::isRelativelyPrime( f.LT(), BRP(1).LT() ) ) {
    cout << "error" << endl;
  }
  if ( !BRP::isRelativelyPrime( BRP(1).LT(), f.LT() ) ) {
    cout << "error" << endl;
  }
  if ( !BRP::isRelativelyPrime( BRP(1).LT(), BRP(0).LT() ) ) { // x, 1
    cout << "error" << endl;
  }
  if ( !BRP::isRelativelyPrime( f.LT(), BRP(0).LT() ) ) { // f, 1
    cout << "error" << endl;
  }
}

int main() {
  cout << "Hello Testing Friends!" << endl;

  testEquality0();
  testEquality1();
  testEquality();
  testAddition();
  testAddition2();
  testAddition3();
  testAddition4();
  testMultiplication();
  testDivide();
  testLT();
  testIsLeadingReducibleBy();
  testRemainder();
  testIsRelativelyPrime();
  testReverseLex();
  testNumberOfBits();
  testGRevLexSorting();

  cout << "All tests passed" << endl;
}

