#include "brp.h"

void testEquality0() {
  BRP bar = BRP();
  cout << "Testing equality = 0: " << (bar == 0 ? "is 0" : "not 0") << endl;
}

void testEquality1() {
  list<int> foo;
  foo.push_back(0);
  BRP bar = BRP(foo);
  cout << "Testing equality = 1: " << (bar == 1? "is 1" : "not 1") << endl;
}

void testAddition() {
  list<int> foo;
  foo.push_back(1);
  foo.push_back(2);
  foo.push_back(3);
  list<int> foo2;
  foo2.push_back(4);
  foo2.push_back(5);
  foo2.push_back(3);
  BRP bar = BRP(foo);
  BRP bar2 = BRP(foo2);

  BRP bar3 = bar + bar2;

  list<int> foo3;
  foo3.push_back(1);
  foo3.push_back(2);
  foo3.push_back(4);
  foo3.push_back(5);

  BRP bar4 = BRP(foo3);

  cout << "Testing addition: " << (bar3 == bar4 ? "is good" : "is bad")
  << endl;
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
  //cout << barMultiply << endl;

  list<int> foo3;
  foo3.push_back(14);
  foo3.push_back(9);

  BRP bar3 = BRP(foo3);
  cout << "Testing multiplication: " << (barMultiply == bar3 ? "is good" : "is bad") << endl;

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

  cout << "Testing isDivisibleBy: " << (bar.isDivisibleBy(bar2) ? "is bad" : "is good") << endl;
  cout << "Testing divide: " << (barDivide == bar4 ? "is good" : "is bad") << endl;
}

void testLT() {
  list<int> foo;
  foo.push_back(3);
  foo.push_back(8);
  foo.push_back(5);
  foo.push_back(5);
  
  BRP bar = BRP(foo);
  
  cout << "Testing LT(): " << (bar.LT() == 8 ? "is good" : "is bad") << endl;
}

int main() {
  cout << "Hello World!" << endl;

  testEquality0();
  testEquality1();
  testAddition();
  testMultiplication();
  testDivide();
  testLT();
}

