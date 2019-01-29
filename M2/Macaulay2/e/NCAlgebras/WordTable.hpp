#ifndef _word_table_hpp_
#define _word_table_hpp_

#include <vector>

class ConstMonomial
{
public:
  int* begin;
  int* end;
};

class WordTable
{
  // abstract table class.
public:
  WordTable() {}

  ~WordTable() {}

  size_t monomialCount() const { return mMonomials.size(); }

  size_t insert(ConstMonomial* w);

  // lookup routines

  // which monomial, index into word, length of monomial.
  // subwords returns a vector of all subword matches:
  //   each entry is a triple:
  //     a. index of the monomial in the table
  //     b. index into that monomial
  size_t subwords(ConstMonomial* word,
                  std::vector<std::pair<int,int>>& output);

private:
  std::vector<ConstMonomial*> mMonomials;
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:

