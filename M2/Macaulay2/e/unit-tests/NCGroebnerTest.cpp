#include <memory>
#include <gtest/gtest.h>

#include "NCAlgebras/WordTable.hpp"

std::vector<int> monom1 {2, 0, 1};  // cab
std::vector<int> monom2 {2, 2};  // cc
std::vector<int> monom3 {1, 0, 1, 0};  // baba
std::vector<int> word {2, 0, 1, 2, 2, 1, 0, 1, 0};  // cabccbaba

TEST(WordTable, create)
{
  WordTable W;

  EXPECT_EQ(monom1.size(), 3);
  EXPECT_EQ(monom2.size(), 2);

  W.insert(ConstMonomial(monom1));
  W.insert(ConstMonomial(monom2));
  W.insert(ConstMonomial(monom3));

  EXPECT_EQ(W.monomialCount(), 3);
}        

TEST(WordTable, insert)
{
  WordTable W;

  EXPECT_EQ(monom1.size(), 3);
  EXPECT_EQ(monom2.size(), 2);

  W.insert(ConstMonomial(monom1));
  W.insert(ConstMonomial(monom2));
  W.insert(ConstMonomial(monom3));

  std::vector<std::pair<int,int>> matches;
  W.subwords(ConstMonomial(word), matches);

  EXPECT_EQ(matches.size(), 3);
  EXPECT_EQ(matches[0], std::make_pair(0, 0));
  EXPECT_EQ(matches[1], std::make_pair(1, 3));
  EXPECT_EQ(matches[2], std::make_pair(2, 5));
}        

TEST(WordTable, subwords)
{
  std::vector<int> monom1 {1, 0, 1, 2};  // babc
  std::vector<int> monom2 {1, 0, 2, 2};  // bacc
  std::vector<int> monom3 {1, 0, 1, 0};  // baba
  std::vector<int> monom4 {1, 0};  // ba
  std::vector<int> word {1, 0, 1, 0, 2, 2, 1, 0, 1, 2};

  WordTable W;

  EXPECT_EQ(monom1.size(), 4);
  EXPECT_EQ(monom2.size(), 4);
  EXPECT_EQ(monom3.size(), 4);
  EXPECT_EQ(monom4.size(), 2);
  EXPECT_EQ(word.size(), 10);
  
  W.insert(ConstMonomial(monom1));
  W.insert(ConstMonomial(monom2));
  W.insert(ConstMonomial(monom3));
  W.insert(ConstMonomial(monom4));

  std::vector<std::pair<int,int>> matches;
  W.subwords(ConstMonomial(word), matches);

  EXPECT_EQ(matches.size(), 6);
  EXPECT_EQ(matches[0], std::make_pair(0, 6));
  EXPECT_EQ(matches[1], std::make_pair(1, 2));
  EXPECT_EQ(matches[2], std::make_pair(2, 0));
  EXPECT_EQ(matches[3], std::make_pair(3, 0));
  EXPECT_EQ(matches[4], std::make_pair(3, 2));
  EXPECT_EQ(matches[5], std::make_pair(3, 6));
}        

TEST(WordTable, skylanin)
{
  // X,Y Z are the 3 variables
#if 0  
  mons = {{Z, X}, {Z, Y}, {Z, Z}, {Y, Y, X}, {Y, Y, Z},
            {Y, X, Y, Y}, {Y, Y, Y, Y}, {Y, X, Y, X, X},
            {Y, X, Y, X, Y}, {Y, X, Y, X, Z},
            {Y, X, X, Y, X, X}, {Y, X, X, Y, X, Z},
          {Y, X, X, Y, Y, Y}}
#endif
          
  std::vector<int> m1 {2,0};  // ZX
  std::vector<int> m2 {2,1};  // ZY
  std::vector<int> m3 {2,2};  // ZZ
  std::vector<int> m4 {1,1,0};  // YYX
  std::vector<int> m5 {1,1,2};  // YYZ
  std::vector<int> m6 {1,0,1,1};  // YXYY
  std::vector<int> m7 {1,1,1,1};  // YYYY
  std::vector<int> m8 {1,0,1,0,0};  // YXYXX
  std::vector<int> m9 {1,0,1,0,1};  // YXYXY
  std::vector<int> m10 {1,0,1,0,2};  // YXYXZ
  std::vector<int> m11 {1,0,0,1,0,0};  // YXXYXX
  std::vector<int> m12 {1,0,0,1,0,2};  // YXXYXZ
  std::vector<int> m13 {1,0,0,1,1,1};  // YXXYYY

  WordTable W;
}        


// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/unit-tests runNCGroebnerTest  "
// indent-tabs-mode: nil
// End:
