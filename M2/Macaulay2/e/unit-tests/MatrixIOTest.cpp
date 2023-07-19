#include <iostream>
#include <memory>
#include <gtest/gtest.h>

#include "util-polyring-creation.hpp"
#include "matrix.hpp"
#include "BasicPolyListParser.hpp"

// These are more benchmark examples, and the files to be read are quite large
// So we can't run these by default.

// TODO: the first one can be added in

TEST(MatrixIO, readMsolve)
{
  std::string filename { "/Users/mike/src/git-from-others/msolve/MES-examples/eg2-gb.ms" };
  std::string contents = R"(#Reduced Groebner basis for input in characteristic 1235952427
#for variable order x, y, z
#w.r.t. grevlex monomial ordering
#consisting of 4 elements:
[1*x^1+2*y^1+2*z^1+1235952426,
1*y^1*z^1+494380972*z^2+370785728*y^1+247190485*z^1,
1*y^2+988761941*z^2+741571456*y^1+494380971*z^1,
1*z^3+924021576*z^2+700373042*y^1+653289140*z^1]:
)";

  auto result = parseMsolveFromString(contents);
  EXPECT_TRUE(result.size() == 4);

  const PolynomialRing* R = simplePolynomialRing(1235952427, {"x", "y", "z"});
  const Matrix* M = toMatrix(R->make_FreeModule(1), result);

  EXPECT_TRUE(M->n_rows() == 1);
  EXPECT_TRUE(M->n_cols() == 4);
}

#if 1
TEST(MatrixIO, readMsolveBig1)
{
  std::string filename { "/Users/mike/src/git-from-others/msolve/MES-examples/6pts-a-gb.ms" };
  auto result = parseMsolveFile(filename);
  EXPECT_TRUE(result.size() == 1019);

  // TODO: parseMsolveFile should also return: modulus, varnames, monorder.
  std::vector<std::string> varnames {"t12", "t13", "t14", "t15", "t16",
    "t23", "t24", "t25", "t26", "t34", "t35", "t36", "t45", "t46", "t56", "z1", "z2"};
  const PolynomialRing* R = simplePolynomialRing(65537, varnames);
  const Matrix* M = toMatrix(R->make_FreeModule(1), result);

  EXPECT_TRUE(M->n_rows() == 1);
  EXPECT_TRUE(M->n_cols() == 1019);
}

TEST(MatrixIO, readMsolveBig2)
{
  std::string filename { "/Users/mike/src/git-from-others/msolve/MES-examples/6pts-b-gb.ms" };
  auto result = parseMsolveFile(filename);
  EXPECT_TRUE(result.size() == 1391);

  std::vector<std::string> varnames {"t12", "t13", "t14", "t15", "t16",
    "t23", "t24", "t25", "t26", "t34", "t35", "t36", "t45", "t46", "t56", "z1", "z2"};
  const PolynomialRing* R = simplePolynomialRing(65537, varnames);
  const Matrix* M = toMatrix(R->make_FreeModule(1), result);

  EXPECT_TRUE(M->n_rows() == 1);
  EXPECT_TRUE(M->n_cols() == 1391);
}

#endif

TEST(MatrixIO, readPolys)
{
  std::string contents = R"(1*x^1+2*y^1+2*z^1+1235952426
1*y^1*z^1+494380972*z^2+370785728*y^1+247190485*z^1
1*y^2+988761941*z^2+741571456*y^1+494380971*z^1
1*z^3+924021576*z^2+700373042*y^1+653289140*z^1
)";

  std::vector<std::string> varnames = {"x", "y", "z"};
  auto result = parseBasicPolyListFromString(contents, varnames);
  EXPECT_TRUE(result.size() == 4);

  const PolynomialRing* R = simplePolynomialRing(1235952427, varnames);
  const Matrix* M = toMatrix(R->make_FreeModule(1), result);

  EXPECT_TRUE(M->n_rows() == 1);
  EXPECT_TRUE(M->n_cols() == 4);

  buffer o;
  M->text_out(o);
  std::cout << o.str() << std::endl;
}
