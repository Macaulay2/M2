#include <memory>
#include <gtest/gtest.h>

#include "NCAlgebras/WordTable.hpp"

TEST(WordTable, create)
{
  WordTable W;

  std::vector<int> monom1 {0, 1, 0};  //aba.
  std::string monom2 {"aba"}; // variables are a,b,...

  EXPECT_TRUE(monom1.size() == 3);
  EXPECT_TRUE(monom2.size() == 3);
}        

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/unit-tests runNCGroebnerTest  "
// indent-tabs-mode: nil
// End:
