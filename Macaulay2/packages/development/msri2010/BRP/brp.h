#include <list>
#include <iostream>
#include <vector>
#include <map>
#include <string>

using namespace std;

class BRP
{
  list<int> mylist;

  friend ostream& operator<< (ostream &out, BRP &self) {
    for(list<int>::iterator i=self.mylist.begin(); i != self.mylist.end(); ++i) {
      out << *i;
      out << " ";
    }
    return out;
  }

  list<int> mod2(const list<int> &alist) {
    map<int,int> tally;
    list<int> tmp;
    for (list<int>::const_iterator i=alist.begin(); i != alist.end(); ++i) {
      ++tally[*i];
    }
    for(map<int,int>::iterator iter = tally.begin(); iter != tally.end();
    ++iter) {
      if(iter->second % 2 == 1) {
        tmp.push_back(iter->first);
      }
    }
    tmp.sort();
    tmp.reverse();
    return tmp;
  }

  public:

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

  bool isDivisibleBy(const BRP &other) const;

  bool leadingIsRelativelyPrime(const BRP &other) const;

  bool isLeadingReducibleBy(const BRP &other) const;
  
  BRP remainder(const BRP &x) const;

  int LT() const;
};


