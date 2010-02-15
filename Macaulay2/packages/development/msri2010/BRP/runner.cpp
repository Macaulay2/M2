#include "brp.h"
#include <set>
#include <math.h>

class Pair {
  friend bool operator< (const Pair &pair1, const Pair &pair2) {
    if(pair1.j < pair2.j) {
      return true;
    } else if(pair1.j == pair2.j) {
      return pair1.i < pair2.i;
    } else {
      return false;
    }
  }

  public:
  int i;
  int j;

  Pair(int a, int b) {
    if(a < b) {
      i = a;
      j = b;
    } else if(b < a) {
      i = b;
      j = a;
    } else {
      throw "Invalid numbers.";
    }
  }
}; 

class FunctionPair {
  public: 
  BRP f;
  BRP g;
  bool good;

  FunctionPair(BRP a, BRP b) {
    f = a;
    g = b;
  }

  // i < j
  FunctionPair(Pair pair, map<int,BRP> F )  {
    int i = pair.i;
    int j = pair.j;

    if(F.find(j) == F.end() || (i >= 0 && F.find(i) == F.end())) {
      good = false;
    } else { 
      good = true;
      if(i < 0) { // working with field polynomial, generate x_(-i)
        g = BRP(pow(2,(-i - 1)));
      } else {
        g = F[i];
      }
      f = F[j];
    }
  }
};

/*
    F = map of functions
    n = number of variables in the ring
*/
set<Pair> makeList(map<int,BRP> F, int n) {
  set<Pair> B;
  set<Pair>::iterator position = B.begin();
  for(map<int,BRP>::iterator iter = F.begin(); iter != F.end(); ++iter) {
    int j = iter->first;
    for(int i=-n; i<0; i++) {
      Pair pair = Pair(i, j);
      position = B.insert(position, pair);
    }
    for(int i=0; i<j; i++) {
      Pair pair = Pair(i, j);
      position = B.insert(position, pair);
    }
  }
  return B;
}

set<Pair> makeNewPairs(int newIndex, map<int,BRP> F, int n) {
  set<Pair> B;
  set<Pair>::iterator position = B.begin();
  for(int i=-n; i<0; i++) {
    Pair pair = Pair(i, newIndex);
    position = B.insert(position, pair);
  }
  for(map<int,BRP>::iterator iter = F.begin(); iter != F.end(); ++iter) {
    int j = iter->first;
    Pair pair = Pair(newIndex, j);
    position = B.insert(position, pair);
  }
  return B;
}

bool inList(int i, int j, set<Pair> B) {
  Pair p = Pair(i,j);
  return B.find(p) != B.end();
}

bool isGoodPair(Pair pair, map<int,BRP> F, set<Pair> B) {
  FunctionPair fp = FunctionPair(pair, F);
  if (!fp.good) {
    return false;
  }
  
  if(fp.g.leadingIsRelativelyPrime(fp.f)) {
    return false;
  }

  BRP lcmpair = BRP(fp.g.LT() | fp.f.LT());
  
  int i = pair.i;
  int j = pair.j;
  for(map<int, BRP>::iterator it = F.begin(); it != F.end(); ++it) {
    if(!(it->first != i && it->first != j &&
    lcmpair.isDivisibleBy(BRP(it->second.LT())) && 
    !inList(i,it->first, B) && !inList(j, it->first, B))) {
      return false;
    }
  }
  
  return true;
}


BRP sPolynomial(Pair pair, map<int,BRP> F, int n) {
  FunctionPair fp = FunctionPair(pair, F);
  if (!fp.good) {
    return BRP();
  }

  if (pair.i < 0 ) { 
    // g = ax + b
    BRP a = (fp.f).divisiblePart(fp.g);
    return a * fp.g + a;
  } 
  BRP lcm = fp.f.LT() | fp.g.LT();
  return fp.f*( lcm/ BRP(fp.f.LT()) ) + fp.g*(lcm/ BRP(fp.g.LT()) );
}

// Reduce the leading term of a polynomial one step using a polynomial
// only call this when isReducible(f,g)
BRP reduceOnce(BRP f,BRP g) {
  return f + g *( f.LT() / g.LT() );
}

BRP reduceOnce(BRP f, map<int,BRP> F) {
  for(map<int, BRP>::iterator it = F.begin(); it != F.end(); ++it) {
    BRP g = it->second;
    if (f.isLeadingReducibleBy( g)) {
      return reduceOnce(f, g);
    }
  }
  return f;
}

BRP reduce(BRP f, map<int,BRP> F) {
  while (f!=0) {
    BRP g = reduceOnce(f, F);
    if ( f == g || f == 0 ) {
      return f;
    }
    f = g;
  }
  return f; // f = 0 !
}
  
// complete algorithm to compute a Groebner basis F  
map<int,BRP> gb( map<int,BRP> F, int n) {
  int nextIndex = F.size(); 
  set<Pair> B = makeList(F, n);
  while (!B.empty()) {
    Pair pair = *(B.begin());
    B.erase(B.begin());
    if (isGoodPair(pair,F,B)) {
      BRP S = sPolynomial(pair,F,n);
      S = reduce(S,F);
      if (S != 0 ) {
        set<Pair> newList = makeNewPairs(nextIndex, F, n);
        F[nextIndex] = S;
        nextIndex++;
        B.insert(newList.begin(), newList.end());
      }
    }
  }
  return F;
}

int main() {
  map<int,BRP> F;

  F[0] = BRP(7);
  F[1] = BRP(8);
  F[2] = BRP(2);

  set<Pair> B = makeList(F, 4);

  for(set<Pair>::iterator Biter = B.begin(); Biter != B.end(); ++Biter) {
    cout << "(" << Biter->i << "," << Biter->j << ")" << endl;
  }

  cout << (inList(1,2,B) == true) << endl;
  cout << (inList(2,1,B) == true) << endl;
  cout << (inList(5,7,B) != true) << endl;
}
