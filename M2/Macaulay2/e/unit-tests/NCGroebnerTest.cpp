#include <memory>
#include <gtest/gtest.h>

#include "engine.h"
#include "poly.hpp"
#include "aring-glue.hpp"
#include "NCAlgebras/FreeAlgebra.hpp"
#include "NCAlgebras/WordTable.hpp"
#include <iostream>

std::vector<int> monom1 {2, 0, 1};  // cab
std::vector<int> monom2 {2, 2};  // cc
std::vector<int> monom3 {1, 0, 1, 0};  // baba
std::vector<int> word {2, 0, 1, 2, 2, 1, 0, 1, 0};  // cabccbaba

extern const QQ * globalQQ;

TEST(MonomialOrdering, create)
{
  auto mo1 = MonomialOrderings::Lex(5);
  auto mo2 = MonomialOrderings::GroupLex(4);
  auto mo3 = MonomialOrderings::join({mo1, mo2});
  std::string answer3 { "MonomialOrder => {\n    Lex => 5,\n    GroupLex => 4\n    }" };
  EXPECT_EQ(answer3, MonomialOrderings::toString(mo3));
  EXPECT_EQ(9, rawNumberOfVariables(mo3));
  EXPECT_TRUE(moIsLex(mo1));

  auto mo4 = MonomialOrderings::GRevLex({3,2,5,7});
  EXPECT_TRUE(moIsGRevLex(mo4));
  auto mo5 = MonomialOrderings::GRevLex2({1,1,1,1});
  EXPECT_TRUE(moIsGRevLex(mo5));
  auto mo6 {
    MonomialOrderings::join(
    {
     MonomialOrderings::GRevLex(3),
     MonomialOrderings::GRevLex2(4),
     MonomialOrderings::GRevLex4(5),
     MonomialOrderings::GroupLex(3)
    })};
  std::cout << MonomialOrderings::toString(mo6) << std::endl;
}

const Monoid* degreeMonoid(const std::vector<std::string>& names)
{
  MonomialOrdering* mo = MonomialOrderings::join
    ({
      MonomialOrderings::Weights( {-1} ),
      MonomialOrderings::GroupLex(1),
      MonomialOrderings::PositionUp()
    });

  auto result = Monoid::create(mo,
                        names,
                        IM2_Ring_trivial_polyring()->cast_to_PolynomialRing(),
                        {},
                        {});
  if (result == nullptr or error())
    {
      std::cout << "Error: " << error_message() << std::endl;
      EXPECT_TRUE(false);
    }
  return result;
}

const PolynomialRing* degreeRing(const std::vector<std::string>& names)
{
  auto degM = degreeMonoid(names);
  return PolyRing::create(globalZZ, degM);
}
const PolynomialRing* degreeRing(int ndegrees)
{
  if (ndegrees == 1)
    return degreeRing({"T"});
}

TEST(FreeAlgebra, create)
{
  FreeAlgebra* A = FreeAlgebra::create(globalQQ,
                                       { "x", "y", "z" },
                                       degreeRing(1),
                                       {1,2,3});
  EXPECT_TRUE(A != nullptr);
}

TEST(FreeAlgebra, polyarithmetic)
{
  FreeAlgebra* A = FreeAlgebra::create(globalQQ,
                                       { "x", "y", "z" },
                                       degreeRing(1),
                                       {1,2,3});
  FreeAlgebraElement x(A), y(A), z(A), f(A), g(A), h(A);
  A->var(*x, 0);
  A->var(*y, 1);
  A->var(*z, 2);
  f = x + y;
  g = y + z;
  EXPECT_TRUE(x + y == y + x);
  EXPECT_FALSE(f == g);
  EXPECT_TRUE(x * (y + z) == x * y + x * z);
  EXPECT_TRUE((f * g) * f == f * (g * f));
  EXPECT_TRUE((f^2) == (f * f));

  A->from_word(*h, {1,2,1,0,1});
  EXPECT_TRUE(h == y * z * y * x * y);

  A->setZero(*f);
  A->setZero(*g);
  A->from_long(*f,1);
  A->from_word(*g,{});
  // from_rational test? How to create an mpq_ptr?
  EXPECT_TRUE(f == g);
  EXPECT_TRUE(A->is_unit(*f));
  EXPECT_TRUE((h^0) == f);
}

TEST(FreeAlgebra, comparisons)
{
  FreeAlgebra* A = FreeAlgebra::create(globalQQ,
                                       { "x", "y", "z" },
                                       degreeRing(1),
                                       {1,2,3});
  FreeAlgebraElement x(A), y(A), z(A), f(A), g(A), h(A);
  A->var(*x, 0);
  A->var(*y, 1);
  A->var(*z, 2);
  EXPECT_TRUE(A->compare_elems(*x,*y) == GT);
  EXPECT_TRUE(A->compare_elems(*y,*x) == LT);
  EXPECT_TRUE(A->compare_elems(*x,*x) == EQ);
}

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

std::ostream& operator<<(std::ostream& o, const std::vector<Triple>& val)
{
  int count = 0;
  for (auto a : val)
    {
      o << "[" << std::get<0>(a) << ","
        << std::get<1>(a) << ","
        << std::get<2>(a) << "]";
      o << " ";
      count++;
      if (count % 10 == 0) o << std::endl;
    }
  o << std::endl;
  return o;
}

std::ostream& operator<<(std::ostream& o, const std::vector<std::pair<int,int>>& val)
{
  int count = 0;
  for (auto a : val)
    {
      o << "[" << std::get<0>(a) << ","
        << std::get<1>(a) << "]";
      o << " ";
      count++;
      if (count % 10 == 0) o << std::endl;
    }
  o << std::endl;
  return o;
}

#if 0
template<class T>
std::ostream& operator<<(std::ostream& o, const std::vector<T>& val)
{
  int count = 0;
  for (auto a : val)
    {
      o << "[";
      int sz = std::tuple_size<T>::value;
      for (auto i=0; i<sz; ++i)
        {
          o << std::get<i>(a) << ",";
        }
        o << "]";
      o << " ";
      count++;
      if (count % 10 == 0) o << std::endl;
    }
  o << std::endl;
  return o;
}
#endif

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
          
  std::vector<int> m0 {2,0};  // ZX
  std::vector<int> m1 {2,1};  // ZY
  std::vector<int> m2 {2,2};  // ZZ
  std::vector<int> m3 {1,1,0};  // YYX
  std::vector<int> m4 {1,1,2};  // YYZ
  std::vector<int> m5 {1,0,1,1};  // YXYY
  std::vector<int> m6 {1,1,1,1};  // YYYY
  std::vector<int> m7 {1,0,1,0,0};  // YXYXX
  std::vector<int> m8 {1,0,1,0,1};  // YXYXY
  std::vector<int> m9 {1,0,1,0,2};  // YXYXZ
  std::vector<int> m10 {1,0,0,1,0,0};  // YXXYXX
  std::vector<int> m11 {1,0,0,1,0,2};  // YXXYXZ
  std::vector<int> m12 {1,0,0,1,1,1};  // YXXYYY

  std::vector<Triple> overlaps;
  std::vector<std::pair<int,int>> matches;
  
  WordTable W;
  W.insert(m0, overlaps);
  EXPECT_EQ(0, overlaps.size());

  W.insert(m1, overlaps);
  EXPECT_EQ(0, overlaps.size());

  W.insert(m2, overlaps);
  std::cout << overlaps;
  std::vector<Triple> ans {
      std::make_tuple(2,1,0),
      std::make_tuple(2,1,1),
      std::make_tuple(2,1,2)
      };
  EXPECT_EQ(overlaps, ans);
  overlaps.clear();
  W.leftOverlaps(overlaps);
  EXPECT_EQ(0, overlaps.size());

  W.insert(m3);
  W.insert(m4);
  W.insert(m5);
  W.insert(m6);
  W.insert(m7);
  W.insert(m8);
  W.insert(m9);
  W.insert(m10);
  W.insert(m11);
  W.insert(m12);

  matches.clear();
  W.superwords(std::vector<int> {1, 1}, matches);
  std::vector<std::pair<int,int>> ans2
    {
     std::make_tuple(3,0),
     std::make_tuple(4,0),
     std::make_tuple(5,2),
     std::make_tuple(6,0),
     std::make_tuple(6,1),
     std::make_tuple(6,2),
     std::make_tuple(12,3),
     std::make_tuple(12,4)
    };
  EXPECT_EQ(ans2, matches);
  std::cout << matches;  

  matches.clear();
  W.subwords(std::vector<int> {2,2,0,1,1,0,1,0,1,1}, matches); // ZZXYYXYXYY
  std::cout << matches;
  //        Which is: { (0, 1), (2, 0), (3, 3), (5, 6), (8, 4) }
  std::vector<std::pair<int,int>> ans3
    {
     {0,1},
     {2,0},
     {3,3},
     {5,6},
     {8,4}
    };
  EXPECT_EQ(ans3, matches);

}        


// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/unit-tests runNCGroebnerTest  "
// indent-tabs-mode: nil
// End:
