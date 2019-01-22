#include "NCGroebner.hpp"

//////////////////////
// KnuthMorrisPratt //
//////////////////////
class KnuthMorrisPratt
{
private:
  std::vector<int> mTable;
  std::vector<int> mReducerStart; // indices into mTable, one for each reducer.
public:
  KnuthMorrisPratt(const PolynomialAlgebra* A,
                   const NCGroebner::ConstPolyList& reducers) {}

  // operation:
  // inputs: two monomials (words): m, n.
  // output: list of indices where m is a subword of n.  Returns the index in n of the start of the match.
  // e.g. m = aba, n = abaabbaababa
  //  output: 0,7,9.

  // table to be constructed (one list of ints for each reducer 'm').
  // the length of this list is length of the word 'm'.
  // Corman, Leiserson, Rivest: Chapter 34.4 Knuth-Morris-Pratt.
  // m = ababaca, n = ababaabcbab
};


void NCGroebner::compute(int maxdeg)
{
}

const NCGroebner::ConstPolyList* NCGroebner::currentValue()
{
  return &mInput;
}

auto NCGroebner::twoSidedReduction(const PolynomialAlgebra* A,
                                   const Poly* reducee,
                                   const ConstPolyList& reducers,
                                   KnuthMorrisPratt& table
                                   ) -> Poly*
{
  return const_cast<Poly*>(reducee);
  // TO BE WRITTEN
  
}

auto NCGroebner::twoSidedReduction(const PolynomialAlgebra* A,
                                   const ConstPolyList& reducees,
                                   const ConstPolyList& reducers) -> ConstPolyList
{
  KnuthMorrisPratt table(A,reducers);
  ConstPolyList result;
  for (auto i = reducees.cbegin(); i != reducees.cend(); ++i)
    result.push_back(twoSidedReduction(A, *i, reducers, table));
  return result;
}


// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e  "
// indent-tabs-mode: nil
// End:
