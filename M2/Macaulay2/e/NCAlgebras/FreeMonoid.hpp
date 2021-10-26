#ifndef _free_monoid_hpp_
#define _free_monoid_hpp_

#include "Polynomial.hpp"      // for Monom, IntVector
#include "newdelete.hpp"       // for VECTOR, our_new_delete
#include "polyring.hpp"        // for PolynomialRing
#include "style.hpp"           // for GT
#include "NCAlgebras/Word.hpp" // for Word
#include "MemoryBlock.hpp"     // for MemoryBlock
#include <iosfwd>              // for string, ostream
#include <vector>              // for vector

class Monoid;  // lines 15-15
//class Word;  // lines 16-16
class buffer;  // lines 17-17

// TODO for weights in orders
//  1. make sure it is input correctly from front end.
//  2. make sure that monomials have weight function values put in.
//  3. make sure compare uses that info
//  4.
// format
// [total length] wt0 wt1 ... w(tr-1) w0 w1  ... ws

class FreeMonoidLogger
{
public:
  static size_t mCompares;

  static void reset()
  {
    mCompares = 0;
  }

  static void logCompare()
  {
    mCompares++;
  }
};

std::ostream& operator<<(std::ostream& o, FreeMonoidLogger a);

class FreeMonoid : public our_new_delete
{
  // types of monomials: (MES: just note to ourselves: remove it eventually).
  //  1. packed varpower (region of memory filled with int's)
  //  2. pointer into a region of int's.
  //  3. (FreeMonoid, pointer into a region of int's)
  //  4. exponent vector (only used for commutative case)
  //  5. Monomial format.
private:
  const std::vector<std::string> mVariableNames;
  const PolynomialRing* mDegreeRing;
  const std::vector<int> mDegrees;       // length numVars()*(length of a single degree vector)
  const std::vector<int> mWeightVectors; // length numVars()*(length of a single weight vector)
  const std::vector<int> mHeftVector;    // length is size of degree vector
  std::vector<int> mHeftDegrees;   // length numVars().  Should be const (after construction)
  const int mNumWeights;
  VECTOR(const int*) mDegreeOfVar;
     // length numVars(), each is a pointer to an allocated degree vector.
     // Should be const (after construction)
public:
  FreeMonoid(
             const std::vector<std::string>& variableNames,
             const PolynomialRing* degreeRing,
             const std::vector<int>& degrees,
             const std::vector<int>& wtvecs,
             const std::vector<int>& heftVector
             );
  
  // Informational
  const std::vector<std::string>& variableNames() const { return mVariableNames; }
  const std::vector<int>& flattenedDegrees() const { return mDegrees; }

  unsigned int numVars() const { return static_cast<unsigned int>(mVariableNames.size()); }
  unsigned int numWeights() const { return mNumWeights; }

  const PolynomialRing* degreeRing() const { return mDegreeRing; }
  const Monoid& degreeMonoid() const { return * mDegreeRing->getMonoid(); }
  
  // Monomial operations
  using MonomialInserter = IntVector;

  void one(MonomialInserter& m) const;

  bool is_one(const Monom& m) const;

  void copy(const Monom& m, MonomialInserter& result) const;
  
  void mult(const Monom& m1, const Monom& m2, MonomialInserter& result) const;
  void mult3(const Monom& m1, const Monom& m2, const Monom& m3, MonomialInserter& result) const;

  int compare(const Monom& m1, const Monom& m2) const;
  int compare(const Word& w1, const Word& m2) const;

  bool isEqual(const Monom& m1, const Monom& m2) const;

  // index_of_variable: returns 0..numgens-1, if monomial is that, otherwise returns -1.  
  int index_of_variable(const Monom& m) const; 

  void var(int v, MonomialInserter& result) const;

  // Determine the multidegree of the monomial m. Result is placed into
  // already_allocated_degree_vector which should have been allocated with
  // e.g. degreeMonoid().make_one()
  void multi_degree(const Monom& m, int* already_allocated_degree_vector) const;
  
  // display (to a buffer, and to a ostream)
  void elem_text_out(buffer& o, const Monom& m1) const;

  // transfer to Monomial, from Monomial

  // getMonomial:
  // Input is of the form: [len deg v1 v2 ... vn]
  //                        where len = n + 2 and deg is the sum of the degree of vi
  // The output is of the form, and stored in result.
  // [2n+1 v1 e1 v2 e2 ... vn en], where each ei > 0, (in 'varpower' format)
  void getMonomial(Monom monom, std::vector<int>& result) const;
  void getMonomialReversed(Monom monom, std::vector<int>& result) const;

  // fromMonomial:
  // Input is of the form: [2n+1 v1 e1 v2 e2 ... vn en] (in 'varpower' format)
  // The output is of the form, and stored in result.
  // [len deg v1 v2 v3 ... vn], where each ei > 0, (in 'varpower' format)
  // where len = n+2 and deg = sum of the degrees of the vi 
  void fromMonomial(const int* monom, MonomialInserter& result) const;

  // these functions create a Word from the (prefix/suffix of) a Monom and visa versa
  void wordFromMonom(Word& result, const Monom& m) const;
  void wordPrefixFromMonom(Word& result, const Monom& m, int endIndex) const;
  void wordSuffixFromMonom(Word& result, const Monom& m, int beginIndex) const;
  void monomInsertFromWord(MonomialInserter& result, const Word& w) const;

  // some functions to create monoms from words and monoms, placing result in
  // a MemoryBlock object.  This is primarily for NCF4.
  Monom wordProductAsMonom(const Word& left,
                           const Word& right,
                           MemoryBlock& memBlock) const;
  Monom wordProductAsMonom(const Word& left,
                           const Word& mid,
                           const Word& right,
                           MemoryBlock & memBlock) const;
  Monom wordProductAsMonom(const Word& left,
                           const Monom& mid,
                           const Word& right,
                           MemoryBlock & memBlock) const;

  Word wordProductAsWord(const Word& left,
                         const Word& right,
                         MemoryBlock& memBlock) const;
  Word wordProductAsWord(const Word& left,
                         const Word& mid,
                         const Word& right,
                         MemoryBlock& memBlock) const;

  
  int wordHeft(Word& word) const { return wordWeight(word, mHeftDegrees, 0); }
  int wordHeft(Word& word, int start_index) const { return wordWeight(word, mHeftDegrees, start_index); }

  // monomial support functions
  void support(const Monom& m, std::vector<int>& result) const;

private:
  int wordLength(const Monom&m) const { return m[0] - mNumWeights - 1; }

  void setWeights(Monom&m ) const; // assumes length and word are already in place.

  int weightOfVar(int v, int wt) const { return mWeightVectors[v+wt*numVars()]; }
  int heftOfVar(int v) const { return mHeftDegrees[v]; }

  int wordWeight(Word& word, const std::vector<int>& weight, int start_index) const;
};

class MonomEq
{
public:
  MonomEq() : mMonoid(nullptr) {}
  MonomEq(const FreeMonoid& M) : mMonoid(&M) {}
  MonomEq(const MonomEq& M) : mMonoid(M.mMonoid) {}
  MonomEq(MonomEq& M) : mMonoid(M.mMonoid) {}
  ~MonomEq() {}

  bool operator() (const Monom a, const Monom b) const
  {
    int retval = mMonoid->compare(a, b);
    return (retval == GT);
  }

private:
  const FreeMonoid* mMonoid;
};

class MonomHash {
public:
  int operator()(const Monom &V) const {
      int hash = V[0];
      for(auto &i : V) {
          hash ^= i + 0x9e3779b9 + (hash << 6) + (hash >> 2);
      }
      return hash;
  }
  int operator()(const Word &V) const {
      int hash = V.begin()[0];
      for(auto &i : V) {
          hash ^= i + 0x9e3779b9 + (hash << 6) + (hash >> 2);
      }
      return hash;
  }
};

class MonomHashEqual {
public:
  MonomHashEqual() : mMonoid(nullptr) {}
  MonomHashEqual(const FreeMonoid& M) : mMonoid(&M) {}
  MonomHashEqual(const MonomHashEqual& M) : mMonoid(M.mMonoid) {}
  MonomHashEqual(MonomHashEqual& M) : mMonoid(M.mMonoid) {}
  ~MonomHashEqual() {}

  bool operator() (const Monom a, const Monom b) const
  {
    //int retval = mMonoid->compare(a, b);
    //return (retval == EQ);
    return mMonoid->isEqual(a,b);
  }
  // should make an operator() that works on Words too
  bool operator() (const Word a, const Word b) const
  {
    return std::equal(a.begin(),a.end(),b.begin(),b.end());
  }


private:
  const FreeMonoid* mMonoid;
};

// this works whether T = std::vector<Monom> or std::vector<Word>
template<class T>
struct MonomSort {
  const FreeMonoid* mMonoid;
  const T* mMonomContainer;

  MonomSort(const FreeMonoid* monoid, const T* container) :
    mMonoid(monoid),
    mMonomContainer(container) {}

  bool operator() (int a, int b) const
  {
    // this function determines whether the monomial in position
    // a of the container is less than than the monomial in position b
    // of the container
    int retval = mMonoid->compare((*mMonomContainer)[a], (*mMonomContainer)[b]);
    return (retval == GT);
  }
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
