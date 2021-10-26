// Copyright 2017 Michael E. Stillman

#include <cstdio>
#include <string>
#include <iostream>
#include <memory>
#include <gtest/gtest.h>

#include "schreyer-resolution/res-moninfo.hpp"

TEST(ResMonoidDense, create)
{
  ResMonoidDense M1(4,
                    std::vector<int>{1, 1, 1, 1},
                    std::vector<int>{},
                    MonomialOrderingType::GRevLex);

  ResMonoidDense M2(4,
                    std::vector<int>{1, 2, 3, 4},
                    std::vector<int>{1, 1, 1, 1, 1, 1, 0, 0},
                    MonomialOrderingType::Weights);

  ResMonoidDense M3(4,
                    std::vector<int>{1, 1, 1, 1},
                    std::vector<int>{},
                    MonomialOrderingType::Lex);

  EXPECT_EQ(4, M1.n_vars());
  std::vector<res_monomial_word> monomspace(100);
  EXPECT_EQ(100, monomspace.size());
}

TEST(ResMonoidDense, encodeDecode)
{
  ResMonoidDense M(4,
                   std::vector<int>{1, 1, 1, 1},
                   std::vector<int>{},
                   MonomialOrderingType::GRevLex);

  // Loop through a number of exponent vectors, encode, then decode.

  for (int i = 0; i < M.n_vars(); i++)
    for (int j = i; j < M.n_vars(); j++)
      {
        int exp[]{0, 0, 0, 0};
        exp[i]++;
        exp[j]++;
        int mon[]{0, 0, 0, 0, 0, 0, 0, 0};
        // first encode
        M.from_exponent_vector(exp, 3, mon);
        // now decode
        component_index comp;
        int exp2[]{0, 0, 0, 0};
        M.to_exponent_vector(mon, exp2, comp);
        EXPECT_EQ(3, comp);
        for (int k = 0; k < M.n_vars(); k++) EXPECT_EQ(exp[k], exp2[k]);
        std::cout << "i=" << i << " j=" << j << " mon = ";
        M.dump(std::cout, mon);
        std::cout << std::endl;
      }
}

TEST(ResMonoidSparse, encodeDecode)
{
  ResMonoidSparse M(4,
                    std::vector<int>{1, 1, 1, 1},
                    std::vector<int>{},
                    MonomialOrderingType::GRevLex);

  // Loop through a number of exponent vectors, encode, then decode.

  for (int i = 0; i < M.n_vars(); i++)
    for (int j = i; j < M.n_vars(); j++)
      {
        int exp[]{0, 0, 0, 0};
        int mon[]{0, 0, 0, 0, 0, 0, 0, 0};
        component_index comp;
        int exp2[]{0, 0, 0, 0};
        int mon2[]{0, 0, 0, 0, 0, 0, 0, 0};

        exp[i]++;
        exp[j]++;
        // first encode
        M.from_exponent_vector(exp, 3, mon);
        // now decode
        M.to_exponent_vector(mon, exp2, comp);
        EXPECT_EQ(3, comp);
        for (int k = 0; k < M.n_vars(); k++) EXPECT_EQ(exp[k], exp2[k]);
        // now re-encode
        M.from_exponent_vector(exp2, 3, mon2);
        EXPECT_TRUE(M.is_equal(mon, mon2));
        // now display
        std::cout << "i=" << i << " j=" << j << " mon = ";
        M.dump(std::cout, mon);
        std::cout << std::endl;
      }
}

TEST(ResMonoidDense, mult)
{
  ResMonoidDense M(4,
                   std::vector<int>{1, 1, 1, 1},
                   std::vector<int>{},
                   MonomialOrderingType::GRevLex);

  int exp1[]{0, 0, 0, 0};
  int exp2[]{0, 1, 0, 3};
  int exp3[]{0, 0, 0, 0};
  int mon1[]{0, 0, 0, 0, 0, 0};
  int mon2[]{0, 0, 0, 0, 0, 0, 0, 0, 0};
  int mon3[]{0, 0, 0, 0, 0, 0, 0, 0, 0};
  int mon[]{0, 0, 0, 0, 0, 0, 0, 0, 0};
  int comp;
  M.from_exponent_vector(exp1, 0, mon1);
  M.from_exponent_vector(exp2, 0, mon2);
  M.mult(mon1, mon2, mon);
  M.to_exponent_vector(mon, exp3, comp);
  EXPECT_EQ(6, M.monomial_size(mon1));
  EXPECT_EQ(6, M.monomial_size(mon2));
  EXPECT_EQ(6, M.monomial_size(mon));

  for (int i = 0; i < M.n_vars(); i++)
    for (int j = i; j < M.n_vars(); j++)
      for (int k = 0; k < M.n_vars(); k++)
        {
          int exp1[]{0, 0, 0, 0};
          exp1[i]++;
          exp1[j]++;
          int exp2[]{0, 0, 0, 0};
          exp2[k]++;
          M.from_exponent_vector(exp1, 0, mon1);
          M.from_exponent_vector(exp2, 0, mon2);
          M.mult(mon1, mon2, mon);
          M.mult(mon1, mon2, mon3);
          EXPECT_TRUE(M.is_equal(mon, mon3));
          M.to_exponent_vector(mon, exp3, comp);
          int exp3a[]{0, 0, 0, 0};
          exp3a[i]++;
          exp3a[j]++;
          exp3a[k]++;
          for (int ell = 0; ell < M.n_vars(); ell++)
            EXPECT_EQ(exp3[ell], exp3a[ell]);
          EXPECT_EQ(M.monomial_size(mon), M.monomial_size(mon1));
          EXPECT_EQ(M.monomial_size(mon), M.monomial_size(mon2));
        }
}

TEST(ResMonoidSparse, mult)
{
  ResMonoidSparse M(4,
                    std::vector<int>{1, 1, 1, 1},
                    std::vector<int>{},
                    MonomialOrderingType::GRevLex);

  int exp1[]{0, 0, 0, 0};
  int exp2[]{0, 1, 0, 3};
  int exp3[]{0, 0, 0, 0};
  int mon1[]{0, 0, 0, 0, 0, 0};
  int mon2[]{0, 0, 0, 0, 0, 0, 0, 0, 0};
  int mon3[]{0, 0, 0, 0, 0, 0, 0, 0, 0};
  int mon[]{0, 0, 0, 0, 0, 0, 0, 0, 0};
  int comp;
  M.from_exponent_vector(exp1, 0, mon1);
  M.from_exponent_vector(exp2, 0, mon2);
  M.mult(mon1, mon2, mon);
  M.to_exponent_vector(mon, exp3, comp);
  EXPECT_EQ(3, M.monomial_size(mon1));
  EXPECT_EQ(7, M.monomial_size(mon2));
  EXPECT_EQ(7, M.monomial_size(mon));

  for (int i = 0; i < M.n_vars(); i++)
    for (int j = i; j < M.n_vars(); j++)
      for (int k = 0; k < M.n_vars(); k++)
        {
          int exp1[]{0, 0, 0, 0};
          exp1[i]++;
          exp1[j]++;
          int exp2[]{0, 0, 0, 0};
          exp2[k]++;
          M.from_exponent_vector(exp1, 0, mon1);
          M.from_exponent_vector(exp2, 0, mon2);
          M.mult(mon1, mon2, mon);
          M.mult(mon1, mon2, mon3);
          EXPECT_TRUE(M.is_equal(mon, mon3));
          M.to_exponent_vector(mon, exp3, comp);
          int exp3a[]{0, 0, 0, 0};
          exp3a[i]++;
          exp3a[j]++;
          exp3a[k]++;
          for (int ell = 0; ell < M.n_vars(); ell++)
            EXPECT_EQ(exp3[ell], exp3a[ell]);
          EXPECT_EQ(M.monomial_size(mon),
                    M.monomial_size(mon1) + M.monomial_size(mon2) - 3);
        }
}

TEST(ResMonoidDense, encode5) {}
TEST(ResMonoidDense, encode6) {}
TEST(ResMonoidDense, concatenateResMonoidDense) {}
TEST(ResMonoidDense, outOfRange) {}
TEST(ResMonoidDense, encodeBoundary) {}
// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/unit-tests check  "
// indent-tabs-mode: nil
// End:
