#include "franzi-brp.hpp"
#include <sys/time.h>
#include <set>
#include <time.h>

//#define DEBBBB false;

// pair of indeces 
// negative index -i for field polynomial x_i^2+x_i
class Pair {
  // order with respect to lcm in lex ordering
  friend bool operator< (const Pair &pair1, const Pair &pair2) {
    if (pair1.lcm < pair2.lcm) {
      return true;
    } else if (pair1.lcm > pair2.lcm) {
      return false;
    } else {
      return true;
    }
  }

  public:
  int i;
  int j;
  bool good;
  brMonomial lcm; // lcm of LTs of fi and fj

  Pair(int a, int b, const IntermediateBasis &F) {
    if(a < b) {
      i = a;
      j = b;
    } else if(b < a) {
      i = b;
      j = a;
    } else {
      throw "Invalid numbers in Pair";
    }
    IntermediateBasis::const_iterator end = F.end();
    if(F.find(j) == end || (i >= 0 && F.find(i) == end)) { // fi or fj are not in F anymore
      lcm = 0; // constant 1
      good = false;
    } else { 
      if(i < 0) { // working with field polynomial 
        lcm = F.find(j) ->second.LT();
        good = true; //BRP::isDivisibleBy(lcm, BRP( 1 << n-(-i) ) );
      } else {
        unsigned int a = F.find(i) ->second.LT();
        unsigned int b = F.find(j) ->second.LT();
        lcm = a | b;
        //lcm = F.find(i) ->second.LT() | F.find(j) ->second.LT();
        good = !BRP::isRelativelyPrime( a,b );
      }
    }
  }

}; 

// set of index pairs
// always ordered
typedef set<Pair> Pairs;

// functions f and g, corresponding to index pair j and i, respectively
class FunctionPair {
  BRP fieldpolynomial;
  public: 
  const BRP *f;
  const BRP *g;
  bool good;

  // i < j
  FunctionPair(const Pair &pair, const IntermediateBasis &F, int n )  {
    int i = pair.i;
    int j = pair.j;
    
    IntermediateBasis::const_iterator end = F.end();
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

// generate list of index pairs for given intermediate basis
// first insert all pairs with FPs, then insert pairs of all other polynomials
// the list of indeces was ordered by increasingly
Pairs makeList(const IntermediateBasis &F, int n) {
  Pairs B;
  Pairs::iterator position = B.begin();
  IntermediateBasis::const_iterator end = F.end();
  for(IntermediateBasis::const_iterator iter = F.begin(); iter != end; ++iter) {
    int j = iter->first;
    for(int i=-n; i<0; i++) {
      Pair pair = Pair(i, j, F);
      position = B.insert(position, pair);
    }
    for(int i=0; i<j; i++) {
      Pair pair = Pair(i, j, F);
      if (pair.good) {
        position = B.insert(position, pair);
      }
    }
  }
  return B;
}

// generate list of index pairs for a new index and an intermediate basis
Pairs makeNewPairs(int newIndex, const IntermediateBasis &F, int n) {
  Pairs B;
  Pairs::iterator position = B.begin();
  for(int i=-n; i<0; i++) {
    Pair pair = Pair(i, newIndex, F);
    position = B.insert(position, pair);
  }
  IntermediateBasis::const_iterator end = F.end();
  for(IntermediateBasis::const_iterator iter = F.begin(); iter != end; ++iter) {
    int j = iter->first;
    Pair pair = Pair(newIndex, j, F);
    if (pair.good) {
      position = B.insert(position, pair);
    }
  }
  return B;
}

// return true if pair (i,j) is in the list of indeces
bool inList(int i, int j, const Pairs &B, const IntermediateBasis &F) {
  Pair p = Pair(i, j, F);
  return B.find(p) != B.end();
}

// return true if both functions with indeces of pair are in the intermediate basis and their S polynomial should be computed
bool isGoodPair(const Pair &pair, const IntermediateBasis &F, const Pairs &B, int n) {
  FunctionPair fp = FunctionPair(pair, F, n);
  if (!fp.good) {
    return false;
  }
  
  
  // both polynomials are monomials, so their S polynomial reduces to 0
  if ( fp.g->size() == 1 && fp.f->size() == 1 ) {
    //cout << "m ";
    return false;
  }
  
  brMonomial g = fp.g->LT();
  brMonomial f = fp.f->LT();
  if( BRP::isRelativelyPrime(g,f) ) {
    //cout << "r ";
    return false;
  }

  int i = pair.i;
  int j = pair.j;

  //brMonomial lcm = pair.lcm;
  brMonomial lcm = g | f;
  IntermediateBasis::const_iterator end = F.end();
  for(IntermediateBasis::const_iterator it = F.begin(); it != end; ++it) {
    int k = it->first;
    const BRP *K = &(it->second);

    if(( k != i && k != j && BRP::isDivisibleBy(lcm, K->LT() ) && !inList(i,k,B,F) && !inList(j,k,B,F))) {
      //cout << "l ";
      return false;
    }
  }
  
  //cout << "good pair ";
  return true;
}

// compute S polynomial for a pair
BRP sPolynomial(const Pair &pair, const IntermediateBasis &F, int n) {
  FunctionPair fp = FunctionPair(pair, F, n);
  if (!fp.good) {
    return BRP();
  }

  if (pair.i < 0 ) { 
    // fp.g = x_i
    // f = ax + b
    BRP b = (fp.f)->remainder(*fp.g);
    return b * *fp.g + b;
  } 
  brMonomial f = fp.f->LT();
  brMonomial g = fp.g->LT();
  brMonomial lcm = f | g;
  return *fp.f * (lcm ^ f ) + *fp.g * ( lcm ^ g );
}

// modifies f, cancels the lead term once, f must be non zero
void cancelLeadTerm(BRP &f, const BRP &g) {
  brMonomial a = f.LT() ^ g.LT();
  f + g*a; 
}

// find the first basis element that reduces the leading term of f, if none found return end()  
// return end() if f == 0
IntermediateBasis::const_iterator findDivisor( const BRP &f, const IntermediateBasis &F, const IntermediateBasis::const_iterator itF) {
  IntermediateBasis::const_iterator end = F.end();
  for(IntermediateBasis::const_iterator it = F.begin(); it != end && ! f.isZero(); ++it) {
    if ( itF != it ) {
      if (f.isLeadingReducibleBy(it->second) ) {
        return it;
      }
    }
  }
  return end;
}

// Reduce the leading term of f one step with the first polynomial g_i in the
// intermediate basis that satisfies isLeadingReducibleBy(f,g_i)
bool reduceLt(BRP &f, const IntermediateBasis &F, const IntermediateBasis::const_iterator itF) {
  bool ret = false; // true if anything was reduced
  IntermediateBasis::const_iterator it;
  IntermediateBasis::const_iterator end = F.end();
  while( !f.isZero() && (it = findDivisor(f, F, itF)) != end ) {
    ret = true;
    cancelLeadTerm(f,it->second);
  }
  return ret;
}

// reduce tail of f with leading terms of all polynomials in F
bool reduceTail(BRP &f, const IntermediateBasis &F, const IntermediateBasis::const_iterator itF) {
  IntermediateBasis::const_iterator end = F.end();
  bool ret = false;
  for(IntermediateBasis::const_iterator it = F.begin(); it != end && !f.isZero(); ++it) {
    if ( itF != it ) {
      if (f.reduceTail(it->second) ) {
        it = F.begin();
        ret = true;
      }
    }
  }
  return ret;
}

// reduce all terms in f by the leading terms of all polynomials in F
// first reduce the leading term completely, then the lower terms
bool reduce(BRP &f, const IntermediateBasis &F, const IntermediateBasis::const_iterator itF) {
  bool ret = false;
  ret = reduceLt(f, F, itF);
  if (!f.isZero() ) {
    if ( reduceTail(f, F, itF) ) { 
      ret = true;
    }
  }
  return ret;
}

void reduce(BRP &f, const IntermediateBasis &F) {
  reduce(f,F,F.end());
}

// some effort could be saved on average if we arranged the 
// f i so that their leading terms are listed in increasing order with respect to the cho- 
// sen monomial ordering
void rearrangeBasis(IntermediateBasis &F, int nextIndex) {
  for( IntermediateBasis::iterator j = F.begin(); j != F.end(); ++j ) {
    if (j->first != nextIndex ) {
      for( IntermediateBasis::iterator i = F.begin(); i->first < j->first; ++i) {
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


void stats(const IntermediateBasis &F) {
    unsigned int min = 1<<30; 
    unsigned int max = 0; 
    double avg = 0;
    for(IntermediateBasis::const_iterator it = F.begin(); it != F.end(); ++it ) {
      unsigned int s = it->second.size();
        if ( s < min ) {
          min = s;
        }
        if ( s > max) {
          max = s;
        }
        avg += s;
    }
    avg /= F.size();
    cout << min << " " << max << " " << avg << endl;
}

// Bi = { j: fj is possibly reducible by LT(fi) }
// "possibly reducible" means we do not already know that fj is not reducible by LT(fi)
//
// f = F[i]
// g = F[j]
//
//reduce f with leading term of g, possible outcome: 
//  1)   f -> 0
//  2)   f -> f (no change)
//  3)   f -> f' with new leadTerm (and maybe changes in tail)
//  4)   f -> f' same leadTerm, changes in tail
// the buckets have to be updated as follows: 
//  1) Bi = {}
//     remove i from all Bj's
//  2) remove i from Bj 
//  3) Bj += i for all i != j
//     Bi = {1..n \i}
//  4) Bj += i for all i != j

typedef set<int> Bucket; // list of indeces
typedef vector<Bucket> ReduceBuckets;

// initialize buckets assuming that the elements in F have already been fully
// interreduced, the last element in F has been reduced, but the other 
// elements in F are possibly reducible by the last element
void initializeBuckets(ReduceBuckets &B, const IntermediateBasis &F) {
  B.clear();
  int n = F.rbegin()->first;
  B.resize(n +1); // index n means up to n+1 functions
  IntermediateBasis::const_iterator last = F.end();
  for(IntermediateBasis::const_iterator it = F.begin(); it != last; ++it) {
    if ( n != it->first) {
      B[n].insert(it->first);
    }
  }
}

// fi has new tail, all LT(fj)'s possibly reduce fi
// add i to all Bj's
void newTailUpdateBuckets(const int i, ReduceBuckets &B, const IntermediateBasis &F) {
  IntermediateBasis::const_iterator end = F.end();
  for(IntermediateBasis::const_iterator it = F.begin(); it != end; ++it) {
    if (it->first != i ) {
      B[it->first].insert(i);
    }
  }
}

// fi has new leadTerm, LT(fi) possibly reduces all fj's
void newLeadUpdateBuckets(const int i, ReduceBuckets &B, const IntermediateBasis &F) {
  IntermediateBasis::const_iterator end = F.end();
  for(IntermediateBasis::const_iterator it = F.begin(); it != end; ++it) {
    if (it->first != i ) {
      B[i].insert(it->first);
    }
  }
}

// F[i] reduced to 0 case 1
// remove Bi and remove i from all Bj
void reduceToZeroUpdateBucket(int i, ReduceBuckets &B) {
  B[i].clear();
  for( int j = 0; j < B.size(); j++) {
    B[j].erase(i);
  }
}

// just print all elements in B
void printBucket(const ReduceBuckets &B) {
  for (int i = 0; i < B.size(); i++ ) {
    if (! B[i].empty()) {
      cout << "B" << i << ": ";
      for (Bucket::const_iterator it = B[i].begin(); it != B[i].end(); ++it) {
        cout << *it << ", ";
      }
      cout << endl;
    }
  }
}

// interreduction
// assume the first n-1 elements of F have been fully interreduced, fn is reduced, 
// but f1..f(n-1) are possibly reducible by LT(fn)
// nextIndex is the index of the last element in F
void interreductionWithBuckets(IntermediateBasis &F) {
  int initialBasisSize = F.size();

#ifdef DEBBBB  
  map<int,int> LtChanged;
  map<int,int> TailChanged;
#endif

  ReduceBuckets B;
  initializeBuckets(B, F);
  bool changesHappened = true;
  IntermediateBasis::iterator end = F.end();
  unsigned long numChanged = 0;
  while (changesHappened) {
    changesHappened = false;
    //printBucket(B);
    for (int i = 0; i < B.size(); i++) {
      if ( !B[i].empty() ) {
        for (Bucket::iterator j = B[i].begin(); j != B[i].end(); ) {
          //cout << "B[" << i << "], " << *j << "\t\t";
//          if (i == *j) { // don't reduce fi with fi
//            throw "this shouldn't happen";
//            B[i].erase(j++);
//          } else {
            //try to reduce fj by fi
            if ( F[*j].isLeadingReducibleBy(F[i]) ) {
              cancelLeadTerm(F[*j], F[i]);
#ifdef DEBBBB  
              LtChanged[*j] += 1;
#endif              

              while ( !F[*j].isZero()  && F[*j].isLeadingReducibleBy(F[i]) ) { 
                cancelLeadTerm(F[*j], F[i]);
#ifdef DEBBBB  
              LtChanged[*j] += 1;
#endif              
              }
              // canceled leading term
              numChanged++;
              if (!F[*j].isZero()) { 
                if (F[*j].reduceTail( F[i] ) ) {
#ifdef DEBBBB  
                  TailChanged[*j] += 1;
#endif              
                }
//                if (F[*j] == 0 ) { 
//                  throw "reduceTail generated a 0 polynomial";
//                }

                // change buckets case 3
                newLeadUpdateBuckets(*j,B,F);
                newTailUpdateBuckets(*j,B,F);
     //           cout << "Updated Buckets case 3, B[" << i << "], " << *j << endl;

                changesHappened = true;
                ++j;
              } else {
                // change buckets case 1
                //cout << "Updated Buckets case 1, B[" << i << "], " << *j << endl;
                F.erase(*j);
                reduceToZeroUpdateBucket(*j++,B);
              }
            } else if ( F.find(*j) == F.end() ) { 
              cout << *j << " is in B[" << i << "], but not in F" << endl;
            } else if (F[*j].reduceTail(F[i]) ) { // only changed tail
#ifdef DEBBBB  
                  TailChanged[*j] += 1;
#endif              
              // change buckets case 4
              newTailUpdateBuckets(*j,B,F);
              //cout << "Updated Buckets case 4, B[" << i << "], " << *j << endl;
              numChanged++;
              changesHappened = true;
              ++j;
            } else {
               // change buckets case 2
               //cout << " case 2 " << endl;
               // fj not reducible by fi
               // remove j from Bi
               // cout << "B[" << i << "], " << *j << endl;
               B[i].erase(j++); // case 2
            }
//          }
        }
      }
    }
  }
}

// interreduction
void interreduction(IntermediateBasis &F) {
  bool changesHappened = true;
  IntermediateBasis::iterator end = F.end();
  unsigned long numChanged = 0;
  while (changesHappened) {
    changesHappened = false;
    for(IntermediateBasis::iterator it = F.begin(); it != end; ) {
      if (reduce(it->second, F, it)) { 
        // we changed it
        numChanged++;
        if ( it->second.isZero() ) { //reduced an element to 0, remove it from F
          F.erase(it++);
        } else {
          ++it;
        }
        changesHappened = true;
      } else {
        ++it;
      }
    }
  }
  //cout << "Number of reductions: " << numChanged << ", F.size(): " << F.size() << ". Stats: ";
  //stats(F);
}

// A good (normal? Sugar?) selection strategy should be implemented here
Pair bestPair(Pairs &B) {
  return *(B.begin() );
}

// complete algorithm to compute a Groebner basis F  
void gb( IntermediateBasis &F, int n) {
  int interreductionMod = 0;
  int nextIndex = F.size(); 
  rearrangeBasis(F, -1);
  interreduction(F);
  Pairs B = makeList(F, n);
  unsigned int countAddPoly = 0;
  unsigned int numSPoly= 0;
  while (!B.empty()) {
    Pair pair = bestPair(B);
    B.erase(B.begin());
    if (isGoodPair(pair,F,B,n)) {
      numSPoly++;
      BRP S = sPolynomial(pair,F,n);
      reduce(S,F);
      if ( ! S.isZero() ) {
        cout << "Number of pairs currently in list: " << (int) B.size() << endl;
        countAddPoly++;
        Pairs newList = makeNewPairs(nextIndex, F, n);
        F[nextIndex] = S;
        B.insert(newList.begin(), newList.end());
        nextIndex++;
      }
    }
  }
  interreduction(F);
  cout << "we computed " << numSPoly << " S Polynomials and added " << countAddPoly << " of them to the intermediate basis." << endl;
}

