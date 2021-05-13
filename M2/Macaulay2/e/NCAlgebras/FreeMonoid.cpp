#include "NCAlgebras/FreeMonoid.hpp"

#include "MemoryBlock.hpp"             // for MemoryBlock
#include "NCAlgebras/Word.hpp"         // for Word
#include "buffer.hpp"                  // for buffer
#include "monoid.hpp"                  // for Monoid

#include <cassert>                     // for assert
#include <algorithm>                   // for copy
#include <ostream>                     // for string, operator<<, ostream
#include <utility>                     // for pair

#include <tbb/tbb.h>                   // for locks

size_t FreeMonoidLogger::mCompares = 0;

std::ostream& operator<<(std::ostream& o, FreeMonoidLogger a)
{
  o << "# compares: " << a.mCompares;
  return o;
}

FreeMonoid::FreeMonoid(
          const std::vector<std::string>& variableNames,
          const PolynomialRing* degreeRing,
          const std::vector<int>& degrees,
          const std::vector<int>& wtvecs,
          const std::vector<int>& heftVector)
  : mVariableNames(variableNames),
    mDegreeRing(degreeRing),
    mDegrees(degrees),
    mWeightVectors(wtvecs),
    mHeftVector(heftVector),
    mNumWeights(wtvecs.size() / variableNames.size())
{
  auto ndegrees = degreeMonoid().n_vars();
  [[maybe_unused]] auto nvars = variableNames.size();
  assert(nvars * ndegrees == mDegrees.size());

  for (const int* i = mDegrees.data(); i != mDegrees.data() + mDegrees.size(); i += ndegrees)
    {
      int* deg = degreeMonoid().make_one();
      degreeMonoid().from_expvector(i, deg);
      mDegreeOfVar.push_back(deg);
    }
  for (auto deg : mDegreeOfVar)
    mHeftDegrees.push_back(degreeMonoid().degree_weights(deg, mHeftVector));
}

void FreeMonoid::one(MonomialInserter& m) const
{
  m.push_back(mNumWeights+1);
  for (int i=0; i<mNumWeights; ++i) m.push_back(0);
}

void FreeMonoid::var(int v, MonomialInserter& m) const
{
  m.push_back(mNumWeights+2);
  for (int i=0; i<mNumWeights; ++i)
    m.push_back(weightOfVar(v,i));
  m.push_back(v);
}

bool FreeMonoid::is_one(const Monom& m) const
{
  return m[0] == mNumWeights+1;
}

int FreeMonoid::index_of_variable(const Monom& m) const
{
  if (m[0] != mNumWeights+2) return -1;
  return m[mNumWeights+1];
}

void FreeMonoid::copy(const Monom& m, MonomialInserter& result) const
{
  result.insert(result.end(),m.begin(),m.end());
}

void FreeMonoid::mult(const Monom& m1, const Monom& m2, MonomialInserter& result) const
{
  int sz = m1[0] + wordLength(m2);
  result.push_back(sz);
  for (int i=1; i<=mNumWeights; ++i)
    result.push_back(m1[i] + m2[i]);
  result.insert(result.end(),m1.begin()+mNumWeights+1,m1.end());
  result.insert(result.end(),m2.begin()+mNumWeights+1,m2.end());
}

void FreeMonoid::mult3(const Monom& m1, const Monom& m2, const Monom& m3, MonomialInserter& result) const
{
  int sz = m1[0] + wordLength(m2) + wordLength(m3);
  result.push_back(sz);
  for (int i=1; i<=mNumWeights; ++i)
    result.push_back(m1[i] + m2[i] + m3[i]);
  result.insert(result.end(),m1.begin()+mNumWeights+1,m1.end());
  result.insert(result.end(),m2.begin()+mNumWeights+1,m2.end());
  result.insert(result.end(),m3.begin()+mNumWeights+1,m3.end());
}

int FreeMonoid::compare(const Monom& m1, const Monom& m2) const
{
  // order of events:
  // compare weights first
  // then compare word length
  // then compare with lex

  // is this thread-safe?
  //FreeMonoidLogger::logCompare();
  
  // compare weights first
  for (int j = 1; j <= mNumWeights; ++j)
    {      
      if (m1[j] > m2[j]) return GT;
      if (m1[j] < m2[j]) return LT;
    }

  // at this point, the weights are the same.
  // the total length is just mNumWeights + 1 + wordLength, so just
  // compare the total length (i.e. m1[0] and m2[0]
  if (m1.size() > m2.size()) return GT;
  if (m1.size() < m2.size()) return LT;

  // at this stage, they have the same weights and word length, so use lex order
  for (int j = mNumWeights+1; j < m1.size(); ++j)
    {
      if (m1[j] > m2[j]) return LT;
      if (m1[j] < m2[j]) return GT;
    }
  // if we are here, the monomials are the same.
  return EQ;
}

int FreeMonoid::compare(const Word& w1, const Word& w2) const
{
  int weight1;
  int weight2;

  // compute and compare weights
  for (int i = 0; i < mNumWeights; ++i)
  {
    weight1 = 0;
    weight2 = 0;
    for (int j = 0; j < w1.size(); ++j)
      weight1 += weightOfVar(w1[j],i);
    for (int j = 0; j < w2.size(); ++j)
      weight2 += weightOfVar(w2[j],i);
    if (weight1 > weight2) return GT;
    if (weight1 < weight2) return LT;
  }

  if (w1.size() > w2.size()) return GT;
  if (w1.size() < w2.size()) return LT;
  // at this stage, they have the same weights and word length, so use lex order
  for (int i = 0; i < w1.size(); ++i)
    {
      if (w1[i] > w2[i]) return LT;
      if (w1[i] < w2[i]) return GT;
    }
  // if we are here, the monomials corresponding to the words are the same.
  return EQ;
}

bool FreeMonoid::isEqual(const Monom& m1, const Monom&m2) const
{
  //if (wordLength(m1) != wordLength(m2)) return false;
  return std::equal(m1.begin(),m1.end(),m2.begin(),m2.end());
}

void FreeMonoid::multi_degree(const Monom& m, int* already_allocated_degree_vector) const
{
  int* result = already_allocated_degree_vector; // just to use a smaller name...
  degreeMonoid().one(result); // reset value

  auto word_length = wordLength(m);
  auto word_ptr = m + mNumWeights+1;
  for (auto j = 0; j < word_length; j++)
    {
      degreeMonoid().mult(result, mDegreeOfVar[word_ptr[j]], result);
    }
}
    
void FreeMonoid::elem_text_out(buffer& o, const Monom& m) const
{
  auto word_length = wordLength(m);
  auto word_ptr = m + mNumWeights + 1;
  for (auto j = 0; j < word_length; j++)
    {
      // for now, just output the string.
      int curvar = word_ptr[j];
      int curvarPower = 0;
      o << mVariableNames[curvar];
      while ((j < word_length) && (word_ptr[j] == curvar))
        {
          j++;
          curvarPower++;
        }
      if (curvarPower > 1) o << "^" << curvarPower;
      // back j up one since we went too far looking ahead.
      j--;
    }
}

// This function should reverse the order of the varpower terms.
// as the front end reverses the order of terms in a monomial.
void FreeMonoid::getMonomial(Monom m, std::vector<int>& result) const
// Input is of the form: [len wt1 .. wtm v1 v2 ... vn]
//                        where len = m + n + 1, wt are the weights, and vs are the variables in the word
// The output is of the following form, and appended to result.
// [2n+1 v1 e1 v2 e2 ... vn en], where each ei > 0, (in 'varpower' format)
// and the order is that of m.  that is: a*b is encoded as [5, 0 1, 1 1] (commas are only for clarity)
{
  auto start = result.size();
  result.push_back(0);

  auto word_length = wordLength(m);
  auto word_ptr = m + mNumWeights + 1;
  for (auto j = 0; j < word_length; j++)
    {
      int curvar = word_ptr[j];
      int curvarPower = 0;
      result.push_back(curvar);
      while ((j < word_length) && (word_ptr[j] == curvar))
        {
          j++;
          curvarPower++;
        }
      result.push_back(curvarPower);
      // back j up one since we went too far looking ahead.
      --j;
    }
  result[start] = static_cast<int>(result.size() - start);
}

void FreeMonoid::getMonomialReversed(Monom m, std::vector<int>& result) const
// Input is of the form: [len wt1 .. wtm v1 v2 ... vn]
//                        where len = m + n + 1, wt are the weights, and vs are the variables in the word
// The output is of the following form, and appended to result.
// [2n+1 v1 e1 v2 e2 ... vn en], where each ei > 0, (in 'varpower' format)
// and the order is the OPPOSITE of m.  that is: a*b is encoded as [5, 1 1, 0 1] (commas are only for clarity)
{
  auto start = result.size();
  result.push_back(0);
  auto word_length = wordLength(m);
  auto word_ptr = m + mNumWeights + 1;
  for (auto j = word_length-1; j >= 0; --j)
    {
      int curvar = word_ptr[j];
      int curvarPower = 0;
      result.push_back(curvar);
      while ((j >= 0) && (word_ptr[j] == curvar))
        {
          --j;
          curvarPower++;
        }
      result.push_back(curvarPower);
      // back j up one since we went too far looking ahead.
      j++;
    }
  result[start] = static_cast<int>(result.size() - start);
}

// This function should reverse the order of the varpower terms
void FreeMonoid::fromMonomial(const int* monom, MonomialInserter& result) const
  // Input is of the form: [2n+1 v1 e1 v2 e2 ... vn en] (in 'varpower' format)
  // The output is of the following form, and stored in result.
  // [len wt1 wt2 ... wtm v1 v2 v3 ... vn]
  // where len = m+n+1 and wt1 .. wtm are the weights and v1 .. vn is the word 
{
  int inputMonomLength = *monom;
  int startMon = static_cast<int>(result.size());  
  // make space for the length and the weights
  for (int i=0; i<mNumWeights+1; ++i)
    result.push_back(0);
  for (int j = inputMonomLength-2; j >= 1; j -= 2)
    {
      auto v = monom[j];
      for (int k = 0; k < monom[j+1]; k++)
        {
          result.push_back(v);
        }
    }
  result[startMon] = static_cast<int>(result.size() - startMon);
  Monom tmpMon(result.data()+startMon);
  setWeights(tmpMon);
}

// these functions create a Word from the (prefix/suffix of) a Monom
void FreeMonoid::wordFromMonom(Word& result, const Monom& m) const
{
  // just call the prefix command on the word length of the monom
  result.init(m.begin() + mNumWeights + 1, m.end());
  //wordPrefixFromMonom(result,m,wordLength(m));
}

void FreeMonoid::wordPrefixFromMonom(Word& result, const Monom& m, int endIndex) const 
{
  result.init(m.begin() + mNumWeights + 1, m.begin() + mNumWeights + 1 + endIndex);
}

void FreeMonoid::wordSuffixFromMonom(Word& result, const Monom& m, int beginIndex) const
{
  result.init(m.begin() + mNumWeights + 1 + beginIndex, m.end());
}

void FreeMonoid::monomInsertFromWord(MonomialInserter& result, const Word& word) const
{
  result.push_back(word.size() + mNumWeights + 1);
  for (int j = 0; j < mNumWeights; ++j)
    result.push_back(0);
  for (auto a : word) result.push_back(a);
  Monom tmpMonom(&*(result.end()-1-mNumWeights-word.size()));
  setWeights(tmpMonom);
}

void FreeMonoid::setWeights(Monom& m) const
{
  // since Monoms are wrappers to const ints, it seems we need
  // this line so we can set the weights, but I'm not 100% sure
  auto monom = const_cast<int*>(m.begin());

  int word_len = wordLength(m);
  for (int j = 1; j <= mNumWeights; ++j)
      monom[j] = 0;
  // eventually replace both for loops with:
  // word = Word(m)
  //for (int k = 0; k < mNumWeights; ++k)
  //  monom[k+1] = wordWeight(word,iterator to beginning of kth weight)
  for (int j = 0; j < word_len; ++j)
    {
      for (int k = 0; k < mNumWeights; ++k)
        {
          monom[k+1] += weightOfVar(m[mNumWeights+1+j],k);
        }
    }
}

int FreeMonoid::wordWeight(Word& word, const std::vector<int>& weight, int start_index) const
{
  assert(start_index < word.size());
  int result = 0;
  for (int j = start_index; j < word.size(); ++j)
    result += weight[word.begin()[j]];
  return result;
}

  // some functions to create monoms from words and monoms
Monom FreeMonoid::wordProductAsMonom(const Word& left, const Word& right, MemoryBlock& memBlock) const
{
  int monomOffset = numWeights() + 1;
  int sz = left.size() + right.size() + numWeights() + 1;
  auto rg = memBlock.allocateArray<int>(sz);
  rg.first[0] = sz;
  std::copy(left.begin(), left.end(), rg.first + monomOffset);
  std::copy(right.begin(), right.end(), rg.first + monomOffset + left.size());
  Monom newmon = Monom(rg.first);
  setWeights(newmon);
  return newmon;
}

Word FreeMonoid::wordProductAsWord(const Word& left, const Word& right, MemoryBlock& memBlock) const
{
  int sz = left.size() + right.size();
  auto rg = memBlock.allocateArray<int>(sz);
  std::copy(left.begin(), left.end(), rg.first);
  std::copy(right.begin(), right.end(), rg.first + left.size());
  Word newword(rg.first, rg.second);
  return newword;
}

Monom FreeMonoid::wordProductAsMonom(const Word& left, const Word& mid, const Word& right, MemoryBlock & memBlock) const
{
  int monomOffset = numWeights() + 1;
  int sz = left.size() + mid.size() + right.size() + monomOffset;
  auto rg = memBlock.allocateArray<int>(sz);
  rg.first[0] = sz;
  std::copy(left.begin(), left.end(), rg.first + monomOffset);
  std::copy(mid.begin(), mid.end(), rg.first + left.size() + monomOffset);
  std::copy(right.begin(), right.end(), rg.first + left.size() + mid.size() + monomOffset);
  Monom newmon = Monom(rg.first);
  setWeights(newmon);
  return newmon;
}

Word FreeMonoid::wordProductAsWord(const Word& left,
                                   const Word& mid,
                                   const Word& right,
                                   MemoryBlock& memBlock) const
{
  int sz = left.size() + mid.size() + right.size();
  auto rg = memBlock.allocateArray<int>(sz);
  std::copy(left.begin(), left.end(), rg.first);
  std::copy(mid.begin(), mid.end(), rg.first + left.size());
  std::copy(right.begin(), right.end(), rg.first + left.size() + mid.size());
  Word newword(rg.first, rg.second);
  return newword;
}

Monom FreeMonoid::wordProductAsMonom(const Word& left,
                                     const Monom& mid,
                                     const Word& right,
                                     MemoryBlock & memBlock) const
{
  int monomOffset = numWeights() + 1;
  int sz = left.size() + mid.size() + right.size();
  auto rg = memBlock.allocateArray<int>(sz);
  rg.first[0] = sz;
  std::copy(left.begin(), left.end(), rg.first + monomOffset);
  std::copy(mid.begin()+monomOffset, mid.end(), rg.first + left.size() + monomOffset);
  std::copy(right.begin(), right.end(), rg.first + left.size() + mid.size());
  Monom newmon = Monom(rg.first);
  setWeights(newmon);
  return newmon;
}

// we are just placing the answer in result.  it is up to the caller
// to clean it up.
void FreeMonoid::support(const Monom& m, std::vector<int> &result) const
{
  int numVarsFound = 0;
  int monomOffset = numWeights() + 1;
  result.clear();
  std::vector<int> varsFound;
  for (auto i = 0; i < numVars(); i++)
    varsFound.push_back(0);
  for (auto i = m.begin() + monomOffset; i < m.end(); i++)
  {
    if (varsFound[*i] == 0) numVarsFound++;
    varsFound[*i]++;
    if (numVarsFound == numVars()) break;
  }
  for (auto i = 0; i < numVars(); i++)
    if (varsFound[i] > 0) result.push_back(i);
} 

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
