#include <iostream>
#include <memory>
#include <gtest/gtest.h>

#include "util-polyring-creation.hpp"
#include "matrix.hpp"
#include "BasicPolyListParser.hpp"
#include "gb-f4/PolynomialList.hpp"
#include "VectorArithmetic.hpp"
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
  auto B = parseMsolveFile(filename);
  EXPECT_TRUE(B.size() == 1019);

  // TODO: parseMsolveFile should also return: modulus, varnames, monorder.
  std::vector<std::string> varnames {"t12", "t13", "t14", "t15", "t16",
    "t23", "t24", "t25", "t26", "t34", "t35", "t36", "t45", "t46", "t56", "z1", "z2"};
  const PolynomialRing* R = simplePolynomialRing(65537, varnames);

  const Ring *K = R->getCoefficients();
  auto VA = new VectorArithmetic(K);
  newf4::MonomialHashTable monHashTable;
  newf4::PolynomialList L(*VA, monHashTable);
  newf4::PolynomialListStreamCollector S(65537, 17, 1, L);
  toStream(B, S);
  std::cout << "Number of monomials: " << monHashTable.size() << std::endl;
  monHashTable.dump();
  
  // const Matrix* M = toMatrix(R->make_FreeModule(1), result);
  // EXPECT_TRUE(M->n_rows() == 1);
  // EXPECT_TRUE(M->n_cols() == 1019);
}

TEST(MatrixIO, readMsolveBig2)
{
  std::string filename { "/Users/mike/src/git-from-others/msolve/MES-examples/6pts-b-gb.ms" };
  auto B = parseMsolveFile(filename);
  EXPECT_TRUE(B.size() == 1391);

  std::vector<std::string> varnames {"t12", "t13", "t14", "t15", "t16",
    "t23", "t24", "t25", "t26", "t34", "t35", "t36", "t45", "t46", "t56", "z1", "z2"};
  const PolynomialRing* R = simplePolynomialRing(65537, varnames);

  const Ring *K = R->getCoefficients();
  auto VA = new VectorArithmetic(K);
  newf4::MonomialHashTable monHashTable;
  newf4::PolynomialList L(*VA, monHashTable);
  newf4::PolynomialListStreamCollector S(65537, 17, 1, L);
  toStream(B, S);
  std::cout << "Number of monomials: " << monHashTable.size() << std::endl;
  monHashTable.dump();
  

  // const Matrix* M = toMatrix(R->make_FreeModule(1), B);
  // EXPECT_TRUE(M->n_rows() == 1);
  // EXPECT_TRUE(M->n_cols() == 1391);
}

TEST(MatrixIO, readMsolveBig3)
{
  std::string filename { "/Users/mike/src/git-from-others/msolve/MES-examples/eg2-gb.ms" };
  auto B = parseMsolveFile(filename);
  EXPECT_TRUE(B.size() == 4761);

  // TODO: parseMsolveFile should also return: modulus, varnames, monorder.
  std::vector<std::string> varnames {
    "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m",
    "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z",
    "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L",
    "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V"
  };
  const PolynomialRing* R = simplePolynomialRing(101, varnames);

  const Ring *K = R->getCoefficients();
  auto VA = new VectorArithmetic(K);
  newf4::MonomialHashTable monHashTable;
  newf4::PolynomialList L(*VA, monHashTable);
  newf4::PolynomialListStreamCollector S(101, 48, 1, L);
  toStream(B, S);
  std::cout << "Number of monomials: " << monHashTable.size() << std::endl;
  monHashTable.dump();
  
  // const Matrix* M = toMatrix(R->make_FreeModule(1), result);
  // EXPECT_TRUE(M->n_rows() == 1);
  // EXPECT_TRUE(M->n_cols() == 1019);
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

#if 0
restart
dot = (e) -> (sum for i from 0 to 19 list ((e#i * vals#i) % 2^64)) % 2^20
  dot = (e) -> ((sum for i from 0 to 19 list ((e#i * vals#i) % 2^64)) >> 25) % 2^20
vals = {12550986463692465404, 3911555212215091238, 15090669942851505316, 16174113364685515424, 18172793978567602378, 4970727551569665824, 15244287395755336378, 3641586221293608170, 5697307520845005385, 17982501052917221133, 4205210476184990958, 3995014217224167515, 10391875845945764299, 17483720614571824287, 1115562083531405255, 7842315096810324507, 673864007402015535, 15878473700446701422, 15632675738063166334, 17700395182034373329}
  R = ZZ/101[t_0..t_19]
  exps = (flatten entries basis(0,6,R))/exponents/first;
  allhashes = for e in exps list dot e;
(#allhashes, #unique allhashes)
allhashes
max values tally allhashes
#endif
