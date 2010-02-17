#include <list>
#include <iostream>
#include <vector>
#include <map>
#include <string>

using namespace std;

class BRP
{
  list<int> mylist;
  list<int>::iterator i;

  friend ostream& operator<< (ostream &out, BRP &self) {
    for(list<int>::iterator i=self.mylist.begin(); i != self.mylist.end(); ++i) {
      out << *i;
      out << " ";
    }
    return out;
  }

  list<int> mod2(list<int> alist) {
    map<int,int> tally;
    list<int> tmp;
    for(i=alist.begin(); i != alist.end(); ++i) {
      ++tally[*i];
//      cout << *i << ": " << tally[*i] << endl;
    }
//    cout << tally.size() << endl;
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

  BRP(list<int> alist);

  BRP(int val);

  bool operator==(int val);
  bool operator!=(int val);

  bool operator==(BRP other);
  bool operator!=(BRP other);

  BRP operator+(BRP other);

  BRP operator*(BRP other);

  BRP operator/(BRP other);

  bool isDivisibleBy(BRP other);

  bool leadingIsRelativelyPrime(BRP other);

  bool isLeadingReducibleBy(BRP other);
  
  BRP divisiblePart(BRP x);
  BRP remainder(BRP x);

  int LT();
};


