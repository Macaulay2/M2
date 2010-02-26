#include "brp.h"
#include "long.h"
#include "longTest1.h"
#include "longTest2.h"
#include "longTest3.h"
#include "longTest4.h"
#include "longTest5.h"
//#include "longGRevLex.h"
#include "long1.h"
#include "long5.h"
#include "long6.h"
#include "long7.h"
#include "long8.h"
#include <sys/time.h>
#include <set>
#include <time.h>

typedef map<int,set<int> > memoryMap;
typedef map<int,BRP> intermediateBasis;

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
      throw "Invalid numbers in Pair.";
    }
  }
}; 

typedef set<Pair> Pairs;

class FunctionPair {
  BRP fieldpolynomial;
  public: 
  const BRP *f;
  const BRP *g;
  bool good;

  // i < j
  FunctionPair(const Pair &pair, const intermediateBasis &F, int n )  {
    int i = pair.i;
    int j = pair.j;
    
    intermediateBasis::const_iterator end = F.end();
    if(F.find(j) == end || (i >= 0 && F.find(i) == end)) {
      good = false;
    } else { 
      good = true;
      if(i < 0) { // working with field polynomial, generate x_(-i)
        //g = BRP( 1 << n-(-i) );
        fieldpolynomial = BRP( 1 << n-(-i) );
        g = &fieldpolynomial;
      } else {
        g = &F.find(i) ->second;
      }
      f = &F.find(j) ->second;
    }
  }
};

/*
    F = map of functions
    n = number of variables in the ring
*/
Pairs makeList(const intermediateBasis &F, int n) {
  Pairs B;
  Pairs::iterator position = B.begin();
  intermediateBasis::const_iterator end = F.end();
  for(intermediateBasis::const_iterator iter = F.begin(); iter != end; ++iter) {
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

Pairs makeNewPairs(int newIndex, const intermediateBasis &F, int n) {
  Pairs B;
  Pairs::iterator position = B.begin();
  for(int i=-n; i<0; i++) {
    Pair pair = Pair(i, newIndex);
    position = B.insert(position, pair);
  }
  intermediateBasis::const_iterator end = F.end();
  for(intermediateBasis::const_iterator iter = F.begin(); iter != end; ++iter) {
    int j = iter->first;
    Pair pair = Pair(newIndex, j);
    position = B.insert(position, pair);
  }
  return B;
}

bool inList(int i, int j, const Pairs &B) {
  Pair p = Pair(i,j);
  return B.find(p) != B.end();
}

bool isGoodPair(const Pair &pair, const intermediateBasis &F, const Pairs &B, int n) {
  FunctionPair fp = FunctionPair(pair, F, n);
  if (!fp.good) {
    return false;
  }
  
  int i = pair.i;
  int j = pair.j;

  int g = fp.g->LT();
  int f = fp.f->LT();
  if( BRP::isRelativelyPrime(g,f) ) {
    return false;
  }

  int lcm = g | f;
  intermediateBasis::const_iterator end = F.end();
  for(intermediateBasis::const_iterator it = F.begin(); it != end; ++it) {
    int k = it->first;
    const BRP *K = &(it->second);

    if(( k != i && k != j && BRP::isDivisibleBy(lcm, K->LT() ) && !inList(i,k,B) && !inList(j,k,B))) {
      return false;
    }
  }
  
  return true;
}


BRP sPolynomial(const Pair &pair, const intermediateBasis &F, int n) {
  FunctionPair fp = FunctionPair(pair, F, n);
  if (!fp.good) {
    return BRP();
  }

  if (pair.i < 0 ) { 
    // g = ax + b
    BRP a = (fp.f)->remainder(*fp.g);
    return a * *fp.g + a;
  } 
  int f = fp.f->LT();
  int g = fp.g->LT();
  int lcm = f | g;
  return *fp.f * (lcm ^ f ) + *fp.g * ( lcm ^ g );
}

// Reduce the leading term of a polynomial one step using the leading term of
// another a polynomial
// only call this when isLeadingReducibleBy(f,g)
void reduceLTOnce(BRP &f, const BRP &g) {
  int a = f.LT() ^ g.LT();
  f + g * a;
}

// Reduce the leading term of f one step with the first polynomial g_i in the
// intermediate basis that satisfies isLeadingReducibleBy(f,g_i)
bool reduceLTOnce(BRP &f, const intermediateBasis &F) {
  intermediateBasis::const_iterator end = F.end();
  for(intermediateBasis::const_iterator it = F.begin(); it != end; ++it) {
    const BRP *g = &(it->second);
    if (f.isLeadingReducibleBy(g->LT())) {
      reduceLTOnce(f, *g);
      return true;
    }
  }
  return false;
}

// reduce all terms of f with leading terms of all polynomials in F
// if f is in F, then f is reduced to 0
void reduceLowerTerms(BRP &f, const intermediateBasis &F) {
  intermediateBasis::const_iterator end = F.end();
  for(intermediateBasis::const_iterator it = F.begin(); it != end && f != 0; ++it) {
    f.reduceLowerTerms(it->second);
    if (f == 0 ) {
      cout << "f is zero" << endl;
      break;
    }
    // don't start over with the loop, because the everytime we check all
    // terms of f, so if no term of f was reducible by g, then so is no term
    // of the new f
  }
}

// reduce all terms in f by the leading terms of all polynomials in F
// first reduce the leading term completely, then the lower terms
void reduce(BRP &f, const intermediateBasis &F) {
  while (f != 0) {
    if ( !reduceLTOnce(f,F) || f == 0 ){
      reduceLowerTerms(f, F); 
      break;
    }
  }
}

// some effort could be saved on average if we arranged the 
// f i so that their leading terms are listed in increasing order with respect to the cho- 
// sen monomial ordering
void rearrangeBasis(intermediateBasis &F, int nextIndex) {
  for( intermediateBasis::iterator j = F.begin(); j != F.end(); ++j ) {
    if (j->first != nextIndex ) {
      for( intermediateBasis::iterator i = F.begin(); i->first < j->first; ++i) {
        //if ( funccompGRL(i->second.LT(), j->second.LT() )) {
        if ( i->second.LT() > j->second.LT() ) {
          BRP tmp = i->second;
          i->second = j->second;
          j->second = tmp;
        }
      }
    }
  }
}


//initialize memoryMap assuming that the first m elements have already been fully
//reduced
memoryMap initializeMemoryMap(const int &n, const intermediateBasis &F) {
  set<int> dontReduce;
  memoryMap m;
  intermediateBasis::const_iterator end = F.end();
  for(intermediateBasis::const_iterator it = F.begin(); it != end; ++it) {
    m[it->first].insert(n);
  }
  m.erase(n);
  m[n];
  return m;
}


// make a reduced Groebner basis
// assume the first #F entries have already been reduced with each other
void reduce(intermediateBasis &F, memoryMap &notReducible) {
  bool changesHappened = true;
  intermediateBasis::iterator end = F.end();
  while (changesHappened) {
    changesHappened = false;
    intermediateBasis::iterator it1 = F.begin(); 
    bool iteratorIncreased = false;
    while (it1 != end ) {
      BRP *f = &(it1->second);
      iteratorIncreased = false;
      for(intermediateBasis::iterator it2 = F.begin(); it2 != end; ++it2) {
        if ( it1 != it2) {
          memoryMap::iterator dontReduce = notReducible.find(it1->first);
          if ( dontReduce == notReducible.end() || dontReduce->second.find(it2->first) != dontReduce->second.end()) { // there is no map OR it2 is in the list
            if( f->reduceLowerTerms(it2->second) ) {
              if ( (*f) != 0 ) {
                // it1 changed, so we remove it from the map
                notReducible.erase(it1->first);
                // and add it to all reduceSets
                for( memoryMap::iterator m = notReducible.begin(); m != notReducible.end(); ++m) {
                  m->second.insert(it1->first);
                }
                ++it1;
              }
              else {
                F.erase(it1++);
              }
              iteratorIncreased = true;
              changesHappened = true;
              break; // use the next f and compare it to all others
            }
          }
        }
      } 
      if ( (!iteratorIncreased) && it1 != end ) { 
        // it1->first is not reducible by any fi, so we reset it to an empty list
        notReducible.erase(it1->first);
        notReducible[it1->first];
        ++it1;
      }
    }
  }
}

// complete algorithm to compute a Groebner basis F  
void gb( intermediateBasis &F, int n) {
  memoryMap notReducible;
  cout << "." << endl;
  reduce(F, notReducible);
  rearrangeBasis(F, -1);
  int counter = 0;
  int nextIndex = F.size(); 
  Pairs B = makeList(F, n);
  cout << "." << endl;
  while (!B.empty()) {
    Pair pair = *(B.begin());
    B.erase(B.begin());
    if (isGoodPair(pair,F,B,n)) {
      BRP S = sPolynomial(pair,F,n);
      reduce(S,F);
      if (S != 0 ) {
        //cout << ".";
        Pairs newList = makeNewPairs(nextIndex, F, n);
        F[nextIndex] = S;
        B.insert(newList.begin(), newList.end());
        notReducible = initializeMemoryMap(nextIndex,F); // temporarily not reducible by lt(),
        //second list is polynomails that have changed since checking
        reduce(F, notReducible);
        rearrangeBasis(F, nextIndex);
        B = makeList(F, n);
        //cout << F.size() << " ";
        //cout << S.LT() << endl;
        nextIndex++;
      }
    }
  }
}

void testSPolynomial() {
  intermediateBasis G;
  G[0] = BRP(992);
  G[1] = BRP(384) + BRP(256) + BRP(128) + BRP(96) + BRP(64);
  G[2] = BRP(16) + BRP(5) + BRP(2);
  int n = 10;

  Pair p = Pair(-6,2);
  BRP S = sPolynomial(p,G,n);
  BRP correctS = BRP(21) + BRP(18) + BRP(5) + BRP(2);
  if ( S != correctS ) { 
    cout << "error when computing S polynomial" << endl;
    cout << S << endl;
  }
  p = Pair(2,1);
  S = sPolynomial(p,G,n);
  correctS = BRP(389) + BRP(386) + BRP(272) + BRP(144) + BRP(112) + BRP(80);
  if ( S != correctS ) { 
    cout << "error when computing S polynomial" << endl;
    cout << S << endl;
  }
}  

void testInList() {
  intermediateBasis F;
  F[0] = BRP(7);
  F[1] = BRP(8);
  F[2] = BRP(2);
  Pairs B = makeList(F, 4);
  if ( ! inList(1,2,B) || ! inList(2,1,B) ||  ! inList(-4,1,B) || inList(5,7,B) ) { 
    cout << "error in inList" << endl;
  }
}
  
void testChecker( const intermediateBasis &G, intermediateBasis &correct) {
  bool found = false;
  for (map<int,BRP>::const_iterator it1 = G.begin(); it1 != G.end(); ++it1) {
    found = false;
    for (map<int,BRP>::iterator it2 = correct.begin(); it2 != correct.end(); ++it2) {
      if ( it1->second == it2->second ) {
        found = true;
        correct.erase(it2);
        break;
      }
    }
    if (!found) {  
      cout << "error in basis," << it1->second << " not in it" << endl;
      return;
    }
  }
  if (!correct.empty()) {
    cout << "error in basis, too many elements" << endl;
  } else {
    cout << "test passed" << endl;
  }
}

void testPartialChecker( const intermediateBasis &G, intermediateBasis &correct) { //for when the full basis is too long to compile
  bool found = false;
  for (map<int,BRP>::const_iterator it1 = G.begin(); it1 != G.end(); ++it1) {
    found = false;
    for (map<int,BRP>::iterator it2 = correct.begin(); it2 != correct.end(); ++it2) {
      if ( it1->second == it2->second ) {
        found = true;
        correct.erase(it2);
        break;
      }
    }
  }
  if (!correct.empty()) {
    cout << "error in basis, too many elements" << endl;
  } else {
    cout << "test passed" << endl;
  }
}


void timer(intermediateBasis &G, int n) {
  clock_t start, end;
  double cpu_time_used;
  start = clock();
  gb(G,n);
  end = clock();
  cpu_time_used = ((double) (end - start)) / CLOCKS_PER_SEC;

  cout << "It took " << cpu_time_used << " seconds to run a complex Groebner Basis example." << endl;
}

void testShortBasis() {
  intermediateBasis G;
  G[0] = BRP(992);
  G[1] = BRP(384) + BRP(256) + BRP(128) + BRP(96) + BRP(64);
  G[2] = BRP(16) + BRP(5) + BRP(2);
  int n = 10;
  timer(G,n);
  intermediateBasis correctG;
  correctG[0] = BRP( 16) + BRP(5) + BRP(2);
  correctG[1] = BRP(384) + BRP(256) + BRP(128) + BRP(96) + BRP(64);
  correctG[2] = BRP(192) + BRP(128);
  correctG[3] = BRP(320) + BRP(256);
  correctG[4] = BRP(160);
  correctG[5] = BRP(288);
  testChecker(G, correctG);
}


void testLongBasis() {
  map<int,BRP> G;
  G[0] = BRP(1015808);
  G[1] = BRP(393216) + BRP(262144) + BRP(131072) + BRP(98304) + BRP(65536) ;
  G[2] = BRP(16384) + BRP(5120) + BRP(2048) ;
  G[3] = BRP(16384) + BRP(8192);
  G[4] = BRP(524288) + BRP(65536) ;
  G[5] = BRP(196608) + BRP(2048) + BRP(1024) ;
  G[6] = BRP(4) + BRP(2) + BRP(1);
  G[7] = BRP(192) + BRP(48);
  G[8] = BRP(524288) + BRP(1);
  G[9] = BRP(262146) + BRP(2048) + BRP(208) + BRP(8);
  G[10] = BRP(262146) + BRP(4096) + BRP(16) + BRP(8);
  G[11] = BRP(262146) + BRP(2048) + BRP(200);
  G[12] = BRP(262146) + BRP(2048) + BRP(1027) + BRP(200) + BRP(2);
  G[13] = BRP(262656) + BRP(2) + BRP(1);
  G[14] = BRP(262656) + BRP(3072) + BRP(384) + BRP(64) + BRP(12);
  G[15] = BRP(786432) + BRP(2048) + BRP(256) + BRP(136);
  G[16] = BRP(262656) + BRP(65728) + BRP(2048);
  G[17] = BRP(262144) + BRP(67584) + BRP(448) + BRP(8);
  G[18] = BRP(524800) + BRP(147776);
  G[19] = BRP(131072) + BRP(2048) + BRP(448) + BRP(12) + BRP(8);
  int n = 20;

    
  map<int,BRP>c;
  c[0] = BRP(1);
  c[1] = BRP(2);
  c[2] = BRP(4);
  c[3] = BRP(8);
  c[4] = BRP(48);
  c[5] = BRP(64);
  c[6] = BRP(256);
  c[7] = BRP(1024);
  c[8] = BRP(2048);
  c[9] = BRP(4096) + BRP(16);
  c[10] = BRP(8192);
  c[11] = BRP(16384);
  c[12] = BRP(65536);
  c[13] = BRP(131072);
  c[14] = BRP(262144);
  c[15] = BRP(524288);

  timer(G, n);
  testChecker(G, c);
}

void test1Example() {
  cout << "-Example 1-" << endl;
  map<int,BRP> G = testLongTest1Example();
  map<int,BRP> correct = testLongTest1ExampleCorrect();
  timer(G,16);
  if (G.size() != 80) {
    cout << "error in basis for example 1" << endl;
  }
  testPartialChecker(G,correct);
}

void test2Example() {
  cout << "-Example 2-" << endl;
  map<int,BRP> G = testLongTest2Example();
  map<int,BRP> correct = testLongTest2ExampleCorrect();
  timer(G,20);
  if (G.size() != 51) {
    cout << "error in basis for example 2" << endl;
  }
  testChecker(G,correct);
}

void test3Example() {
  cout << "-Example 3-" << endl;
  map<int,BRP> G = testLongTest3Example();
  map<int,BRP> correct = testLongTest3ExampleCorrect();
  timer(G,20);
  if (G.size() != 90) {
    cout << "error in basis for example 3" << endl;
  }
  testChecker(G,correct);
}

void test4Example() {
  cout << "-Example 4-" << endl;
  map<int,BRP> G = testLongTest4Example();
  map<int,BRP> correct = testLongTest4ExampleCorrect();
  timer(G,20);
  if (G.size() != 29) {
    cout << "error in basis for example 4" << endl;
  }
  testChecker(G,correct);
}

void test5Example() {
  cout << "-Example 5-" << endl;
  map<int,BRP> G = testLongTest5Example();
  //map<int,BRP> correct = testLongTest5ExampleCorrect();
  timer(G,20);
  //if (G.size() != 29) {
    //cout << "error in basis for example 5" << endl;
  //}
  //testChecker(G,correct);
}

//void testGRevLex() {
//  cout << "-Example graded Reverse Lex-" << endl;
//  map<int,BRP> G = testLongGRevLexExample();
//  map<int,BRP> correct = testLongGRevLexExampleCorrect();
//  timer(G,20);
//  testChecker(G,correct);
//}

int main() {
  testSPolynomial();
  testInList();
  testShortBasis(); 
  testLongBasis(); 
  test1Example();
  test2Example();
  test3Example();
  test4Example();
  test5Example();
  //testGRevLex();

  cout << "done testing" << endl;

}
