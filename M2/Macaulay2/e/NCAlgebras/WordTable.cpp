#include "WordTable.hpp"

size_t WordTable::insert(ConstMonomial w)
{
  mMonomials.push_back(w);
  return mMonomials.size();
}

size_t WordTable::subwords(ConstMonomial word,
                           std::vector<std::pair<int,int>>& output)
{
  auto wordlen = word.size();
  output.clear();
  for (auto i = 0; i < mMonomials.size(); ++i)
    {
      for (auto j = 0; j <= wordlen - mMonomials[i].size(); ++j)
        {
          bool match = true;
          for (auto k = 0; k < mMonomials[i].size(); ++k)
            if (mMonomials[i].begin()[k] != word.begin()[j+k])
              {
                match = false;
                break;
              }
          if (match) output.push_back(std::make_pair(i,j));
        }
    }
  return output.size();
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
