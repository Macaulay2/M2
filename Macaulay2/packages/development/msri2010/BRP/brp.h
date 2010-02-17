#include <list>
#include <iostream>
#include <vector>
#include <map>
#include <string>

//////// CAREFUL ////////
// multiplication and addition are not const! they don't work as expected but
//change one of the arguments!
//////// CAREFUL ////////

using namespace std;

class BRP
{

  friend ostream& operator<< (ostream &out, BRP &self) {
    for(list<int>::iterator i=self.mylist.begin(); i != self.mylist.end(); ++i) {
      out << *i;
      out << " ";
    }
    return out;
  }

  static void mod2(list<int> &alist) {
    map<int,int> tally;
    for (list<int>::const_iterator i=alist.begin(); i != alist.end(); ++i) {
      ++tally[*i];
    }
    
    list<int> tmp;
    for(map<int,int>::iterator iter = tally.begin(); iter != tally.end(); ++iter) {
      if(iter->second % 2 == 1) {
        tmp.push_back(iter->first);
      }
    }
    tmp.sort();
    tmp.reverse();
    alist = tmp;
  }

  public:
  
  list<int> mylist;

  static bool isDivisibleBy(int a, int b) {
    // check if a is divisible by b
    int lcm = a | b;
    return lcm == a;
  }

  static bool isRelativelyPrime(int a, int b) {
    // check if a and b are relatively prime
    return ( ((a | b) ^ a) == b);
  }

  BRP();

  BRP(const list<int> &alist);

  BRP(int val);

  bool operator==(int val) const;
  bool operator!=(int val) const;

  bool operator==(const BRP &other) const;
  bool operator!=(const BRP &other) const;

  BRP operator+(BRP other);

  BRP operator*(const BRP &other) const;
  BRP operator*(int other) const;

  int operator/(const BRP &other) const;
  int operator/(int other) const;

  bool leadingIsRelativelyPrime(const BRP &other) const;

  bool isLeadingReducibleBy(const BRP &other) const;
  bool isLeadingReducibleBy(int other) const;
  
  BRP remainder(const BRP &x) const;

  int LT() const;

  void removeLeading();
};


