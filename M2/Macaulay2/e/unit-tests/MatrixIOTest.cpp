#include <iostream>
#include <memory>
#include <gtest/gtest.h>

#include "unit-tests/util-polyring-creation.hpp"
#include "matrices/matrix-con.hpp"
#include "matrices/matrix.hpp"
#include "BasicPolyList.hpp"
#include "BasicPolyListParser.hpp"
#include "ring-elements/ring-element.hpp"
#include "error.h"
#include "gb-f4/PolynomialList.hpp"
#include "gb-f4/GBF4Interface.hpp"
#include "basic-rings/vector-arithmetic.hpp"
// These are more benchmark examples, and the files to be read are quite large
// So we can't run these by default.

#define EXAMPLE_DIR "/Users/mike/src/git-from-others/msolve/MES-examples/"
//#define EXAMPLE_DIR "/Users/moorewf/Dropbox/NCEngine/GB examples/"

// MES: just in process of adding this.
// TODO: add in function f.ToString(varnames); // f is a BasicPoly

TEST(MatrixIO, readPolynomial)
{
  std::string polyStr { "13*x^2*y-x*y-2" };
  std::vector<std::string> varnames = {"x", "y", "z"};
  auto result = parseBasicPoly(polyStr, varnames);
  EXPECT_TRUE(result.termCount() == 3);
  std::cout << "poly: ";
  std::cout << result.toString(varnames);
  std::cout << '\n';
  EXPECT_TRUE(polyStr == result.toString(varnames));

  polyStr = "-x+y^2-13*x*y*z+1";
  result = parseBasicPoly(polyStr, varnames);
  std::cout << "poly: " << result.toString(varnames) << '\n';
  EXPECT_TRUE(result.toString(varnames) == polyStr);
  std::cout << "bytes used: " << result.bytesUsed() << '\n';
  // #bytes: 

  polyStr = "2*x+y^2-13*x*y*z-1";
  result = parseBasicPoly(polyStr, varnames);
  std::cout << "poly: " << result.toString(varnames) << '\n';
  EXPECT_TRUE(result.toString(varnames) == polyStr);
}

TEST(MatrixIO, readPolynomialErrors)
{
  std::vector<std::string> varnames = {"x", "y", "z"};
  EXPECT_THROW(parseBasicPoly("3*w-2", varnames), parsing_error);
  try {
    parseBasicPoly("3*w-2", varnames);
  } catch (parsing_error& e) {
    std::cout << "expected parse error: " << e.what() << std::endl;
  }
  
  EXPECT_THROW(parseBasicPoly("3*-2", varnames), parsing_error);
  try {
    parseBasicPoly("3*-2", varnames);
  } catch (parsing_error& e) {
    std::cout << "expected parse error: " << e.what() << std::endl;
  }
  
  EXPECT_THROW(parseBasicPoly("3*x^*y-2", varnames), parsing_error);
  try {
    parseBasicPoly("3*x^*y-2", varnames);
  } catch (parsing_error& e) {
    std::cout << "expected parse error: " << e.what() << std::endl;
  }
}


TEST(MatrixIO, readMsolve)
{
  std::string filename { EXAMPLE_DIR"eg2-gb.ms" };
  std::string contents = R"(#Reduced Groebner basis data
#---
#field characteristic: 1235952427
#variable order:       x, y, z
#monomial order:       graded reverse lexicographical
#length of basis:      4 elements sorted by increasing leading monomials
#---
[1*x^1+2*y^1+2*z^1+1235952426,
1*y^1*z^1+494380972*z^2+370785728*y^1+247190485*z^1,
1*y^2+988761941*z^2+741571456*y^1+494380971*z^1,
1*z^3+924021576*z^2+700373042*y^1+653289140*z^1]:
)";

  auto result = parseMsolveFromString(contents);
  std::cout << "bytes used for poly list: " << bytesUsed(result) << '\n';
  EXPECT_TRUE(result.size() == 4);

  const PolynomialRing* R = simplePolynomialRing(1235952427, {"x", "y", "z"});
  const Matrix* M = toMatrix(R->make_FreeModule(1), result);

  EXPECT_TRUE(M->n_rows() == 1);
  EXPECT_TRUE(M->n_cols() == 4);
}

TEST(Matrix, entriesFromSparseColumns)
{
  const Ring* R = simplePolynomialRing(101, {"x"});
  const FreeModule* target = R->make_FreeModule(5);
  MatrixConstructor mat(target, 15);

  mat.set_entry(1, 3, R->from_long(11));
  mat.set_entry(3, 2, R->from_long(22));
  mat.set_entry(4, 1, R->from_long(33));
  mat.compute_column_degrees();

  const Matrix* M = mat.to_matrix();
  engine_RawRingElementArrayArray entries = M->entries();

  ASSERT_NE(entries, nullptr);
  EXPECT_EQ(entries->len, 5);

  for (int r = 0; r < 5; r++)
    {
      ASSERT_NE(entries->array[r], nullptr);
      EXPECT_EQ(entries->array[r]->len, 15);

      for (int c = 0; c < 15; c++)
        {
          ASSERT_NE(entries->array[r]->array[c], nullptr);
          EXPECT_EQ(entries->array[r]->array[c]->get_ring(), R);

          ring_elem expected = R->zero();
          if (r == 1 && c == 3)
            expected = R->from_long(11);
          else if (r == 3 && c == 2)
            expected = R->from_long(22);
          else if (r == 4 && c == 1)
            expected = R->from_long(33);

          EXPECT_TRUE(R->is_equal(entries->array[r]->array[c]->get_value(),
                                  expected))
              << "entry (" << r << ", " << c << ")";
        }
    }
}

TEST(Matrix, entry)
{
  const Ring* R = simplePolynomialRing(101, {"x"});
  const FreeModule* target = R->make_FreeModule(3);
  MatrixConstructor mat(target, 4);

  mat.set_entry(1, 2, R->from_long(17));
  mat.compute_column_degrees();

  const Matrix* M = mat.to_matrix();
  const RingElement* nonzero = M->entry(1, 2);
  ASSERT_NE(nonzero, nullptr);
  EXPECT_EQ(nonzero->get_ring(), R);
  EXPECT_TRUE(R->is_equal(nonzero->get_value(), R->from_long(17)));

  const RingElement* zero = M->entry(0, 0);
  ASSERT_NE(zero, nullptr);
  EXPECT_EQ(zero->get_ring(), R);
  EXPECT_TRUE(R->is_zero(zero->get_value()));

  EXPECT_EQ(M->entry(3, 0), nullptr);
  EXPECT_TRUE(error());
  EXPECT_STREQ(error_message(), "matrix row index 3 out of range 0 .. 2");

  EXPECT_EQ(M->entry(0, 4), nullptr);
  EXPECT_TRUE(error());
  EXPECT_STREQ(error_message(), "matrix column index 4 out of range 0 .. 3");
}

TEST(Matrix, concatArray)
{
  const PolynomialRing* R = simplePolynomialRing(101, {"x", "y"});
  const FreeModule* target = R->make_FreeModule(2);

  MatrixConstructor left(target, 2);
  left.set_entry(0, 0, R->from_long(2));
  left.set_entry(1, 1, R->var(0));
  left.compute_column_degrees();
  const Matrix* A = left.to_matrix();

  MatrixConstructor empty(target, 0);
  const Matrix* E = empty.to_matrix();

  MatrixConstructor right(target, 2);
  right.set_entry(1, 0, R->from_long(5));
  right.set_entry(0, 1, R->var(1));
  right.compute_column_degrees();
  const Matrix* B = right.to_matrix();

  const Matrix* const matrices[] = {A, E, B};
  const Matrix* C = Matrix::concat(3, matrices);

  ASSERT_NE(C, nullptr);
  EXPECT_EQ(C->rows(), target);
  EXPECT_EQ(C->n_rows(), 2);
  EXPECT_EQ(C->n_cols(), 4);

  EXPECT_TRUE(R->is_equal(C->elem(0, 0), R->from_long(2)));
  EXPECT_TRUE(R->is_zero(C->elem(1, 0)));
  EXPECT_TRUE(R->is_zero(C->elem(0, 1)));
  EXPECT_TRUE(R->is_equal(C->elem(1, 1), R->var(0)));
  EXPECT_TRUE(R->is_zero(C->elem(0, 2)));
  EXPECT_TRUE(R->is_equal(C->elem(1, 2), R->from_long(5)));
  EXPECT_TRUE(R->is_equal(C->elem(0, 3), R->var(1)));
  EXPECT_TRUE(R->is_zero(C->elem(1, 3)));

  const Monoid* D = R->degree_monoid();
  EXPECT_EQ(D->compare(C->cols()->degree(0), A->cols()->degree(0)), 0);
  EXPECT_EQ(D->compare(C->cols()->degree(1), A->cols()->degree(1)), 0);
  EXPECT_EQ(D->compare(C->cols()->degree(2), B->cols()->degree(0)), 0);
  EXPECT_EQ(D->compare(C->cols()->degree(3), B->cols()->degree(1)), 0);
}

TEST(Matrix, directSum)
{
  const PolynomialRing* R = simplePolynomialRing(101, {"x", "y"});
  const FreeModule* target = R->make_FreeModule(2);

  MatrixConstructor left(target, 2);
  left.set_entry(0, 0, R->from_long(2));
  left.set_entry(1, 1, R->var(0));
  left.compute_column_degrees();
  const Matrix* A = left.to_matrix();

  MatrixConstructor mid(target, 1);
  mid.set_entry(0, 0, R->from_long(7));
  mid.compute_column_degrees();
  const Matrix* M = mid.to_matrix();

  MatrixConstructor right(target, 2);
  right.set_entry(1, 0, R->from_long(5));
  right.set_entry(0, 1, R->var(1));
  right.compute_column_degrees();
  const Matrix* B = right.to_matrix();

  const Matrix* const singleton[] = {A};
  EXPECT_EQ(Matrix::direct_sum(1, singleton), A);

  const Matrix* const matrices[] = {A, M, B};
  const Matrix* C = Matrix::direct_sum(3, matrices);

  ASSERT_NE(C, nullptr);
  EXPECT_EQ(C->n_rows(), 6);
  EXPECT_EQ(C->n_cols(), 5);

  EXPECT_TRUE(R->is_equal(C->elem(0, 0), R->from_long(2))); // from A
  EXPECT_TRUE(R->is_zero(C->elem(1, 0)));
  EXPECT_TRUE(R->is_zero(C->elem(2, 0)));
  EXPECT_TRUE(R->is_zero(C->elem(3, 0)));
  EXPECT_TRUE(R->is_zero(C->elem(4, 0)));
  EXPECT_TRUE(R->is_zero(C->elem(5, 0)));

  EXPECT_TRUE(R->is_zero(C->elem(0, 1)));
  EXPECT_TRUE(R->is_equal(C->elem(1, 1), R->var(0))); // from A
  EXPECT_TRUE(R->is_zero(C->elem(2, 1)));
  EXPECT_TRUE(R->is_zero(C->elem(3, 1)));
  EXPECT_TRUE(R->is_zero(C->elem(4, 1)));
  EXPECT_TRUE(R->is_zero(C->elem(5, 1)));

  EXPECT_TRUE(R->is_zero(C->elem(0, 2)));
  EXPECT_TRUE(R->is_zero(C->elem(1, 2)));
  EXPECT_TRUE(R->is_equal(C->elem(2, 2), R->from_long(7))); // from M
  EXPECT_TRUE(R->is_zero(C->elem(3, 2)));
  EXPECT_TRUE(R->is_zero(C->elem(4, 2)));
  EXPECT_TRUE(R->is_zero(C->elem(5, 2)));

  EXPECT_TRUE(R->is_zero(C->elem(0, 3)));
  EXPECT_TRUE(R->is_zero(C->elem(1, 3)));
  EXPECT_TRUE(R->is_zero(C->elem(2, 3)));
  EXPECT_TRUE(R->is_zero(C->elem(3, 3)));
  EXPECT_TRUE(R->is_zero(C->elem(4, 3)));
  EXPECT_TRUE(R->is_equal(C->elem(5, 3), R->from_long(5))); // from B

  EXPECT_TRUE(R->is_zero(C->elem(0, 4)));
  EXPECT_TRUE(R->is_zero(C->elem(1, 4)));
  EXPECT_TRUE(R->is_zero(C->elem(2, 4)));
  EXPECT_TRUE(R->is_zero(C->elem(3, 4)));
  EXPECT_TRUE(R->is_equal(C->elem(4, 4), R->var(1))); // from B
  EXPECT_TRUE(R->is_zero(C->elem(5, 4)));

  const Monoid* D = R->degree_monoid();
  EXPECT_EQ(D->compare(C->rows()->degree(0), A->rows()->degree(0)), 0);
  EXPECT_EQ(D->compare(C->rows()->degree(1), A->rows()->degree(1)), 0);
  EXPECT_EQ(D->compare(C->rows()->degree(2), M->rows()->degree(0)), 0);
  EXPECT_EQ(D->compare(C->rows()->degree(3), M->rows()->degree(1)), 0);
  EXPECT_EQ(D->compare(C->rows()->degree(4), B->rows()->degree(0)), 0);
  EXPECT_EQ(D->compare(C->rows()->degree(5), B->rows()->degree(1)), 0);
  EXPECT_EQ(D->compare(C->cols()->degree(0), A->cols()->degree(0)), 0);
  EXPECT_EQ(D->compare(C->cols()->degree(1), A->cols()->degree(1)), 0);
  EXPECT_EQ(D->compare(C->cols()->degree(2), M->cols()->degree(0)), 0);
  EXPECT_EQ(D->compare(C->cols()->degree(3), B->cols()->degree(0)), 0);
  EXPECT_EQ(D->compare(C->cols()->degree(4), B->cols()->degree(1)), 0);
}

TEST(Matrix, promote)
{
  const Ring* ZZ = globalZZ;
  const PolynomialRing* P = degreeRing({"x"});

  const FreeModule* zzTarget = ZZ->make_FreeModule(3);
  MatrixConstructor mat(zzTarget, 4);

  mat.set_entry(0, 0, ZZ->from_long(7));
  mat.set_entry(2, 1, ZZ->from_long(-3));
  mat.set_entry(1, 3, ZZ->from_long(11));
  mat.compute_column_degrees();

  const Matrix* M = mat.to_matrix();
  const FreeModule* polynomialTarget = P->make_FreeModule(3);
  const Matrix* promoted = M->promote(polynomialTarget);

  ASSERT_NE(promoted, nullptr);
  EXPECT_EQ(promoted->rows(), polynomialTarget);
  EXPECT_EQ(promoted->get_ring(), P);
  EXPECT_EQ(promoted->n_rows(), 3);
  EXPECT_EQ(promoted->n_cols(), 4);

  EXPECT_TRUE(P->is_equal(promoted->elem(0, 0), P->from_long(7)));
  EXPECT_TRUE(P->is_equal(promoted->elem(2, 1), P->from_long(-3)));
  EXPECT_TRUE(P->is_equal(promoted->elem(1, 3), P->from_long(11)));
  EXPECT_TRUE(P->is_zero(promoted->elem(1, 1)));
}

TEST(Matrix, promoteFailureReportsFirstEntry)
{
  const Ring* ZZ = globalZZ;
  const PolynomialRing* P = degreeRing({"x"});

  const FreeModule* polynomialTarget = P->make_FreeModule(3);
  MatrixConstructor mat(polynomialTarget, 2);

  mat.set_entry(2, 0, P->from_long(7));
  mat.set_entry(0, 1, P->from_long(11));
  mat.compute_column_degrees();

  const Matrix* M = mat.to_matrix();
  const Matrix* promoted = M->promote(ZZ->make_FreeModule(3));

  EXPECT_EQ(promoted, nullptr);
  EXPECT_TRUE(error());
  EXPECT_STREQ(error_message(),
               "first error occurred while promoting matrix entry at row 2, column 0");
}

TEST(Matrix, liftFromPolynomialRingToCoefficientRing)
{
  const Ring* ZZ = globalZZ;
  const PolynomialRing* P = degreeRing({"x"});

  const FreeModule* polynomialTarget = P->make_FreeModule(3);
  MatrixConstructor mat(polynomialTarget, 4);

  mat.set_entry(0, 0, P->from_long(7));
  mat.set_entry(2, 1, P->from_long(-3));
  mat.set_entry(1, 3, P->from_long(11));
  mat.compute_column_degrees();

  const Matrix* M = mat.to_matrix();
  const Matrix* lifted = M->lift(ZZ->make_FreeModule(3));

  ASSERT_NE(lifted, nullptr);
  EXPECT_EQ(lifted->get_ring(), ZZ);
  EXPECT_EQ(lifted->n_rows(), 3);
  EXPECT_EQ(lifted->n_cols(), 4);

  EXPECT_TRUE(ZZ->is_equal(lifted->elem(0, 0), ZZ->from_long(7)));
  EXPECT_TRUE(ZZ->is_equal(lifted->elem(2, 1), ZZ->from_long(-3)));
  EXPECT_TRUE(ZZ->is_equal(lifted->elem(1, 3), ZZ->from_long(11)));
  EXPECT_TRUE(ZZ->is_zero(lifted->elem(1, 1)));
}

TEST(Matrix, liftFailureReturnsNull)
{
  const Ring* ZZ = globalZZ;
  const PolynomialRing* P = degreeRing({"x"});

  const FreeModule* polynomialTarget = P->make_FreeModule(2);
  MatrixConstructor mat(polynomialTarget, 1);
  mat.set_entry(0, 0, P->var(0));
  mat.compute_column_degrees();

  const Matrix* M = mat.to_matrix();
  EXPECT_EQ(M->lift(ZZ->make_FreeModule(2)), nullptr);
  EXPECT_TRUE(error());
  EXPECT_STREQ(error_message(),
               "first error occurred while lifting matrix entry at row 0, column 0");
}

#if 0
TEST(MatrixIO, readMsolveBig1)
{
  std::string filename { EXAMPLE_DIR"6pts-a-gb.ms" };
  auto B = parseMsolveFile(filename);
  EXPECT_TRUE(B.size() == 1019);
  std::cout << "bytes used for poly list: " << bytesUsed(B) << '\n';

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
  std::string filename { EXAMPLE_DIR"6pts-b-gb.ms" };
  auto B = parseMsolveFile(filename);
  EXPECT_TRUE(B.size() == 1391);
  std::cout << "bytes used: " << bytesUsed(B) << '\n';

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
  std::string filename { EXAMPLE_DIR"eg2-gb.ms" };
  auto B = parseMsolveFile(filename);
  EXPECT_TRUE(B.size() == 4761);
  std::cout << "bytes used: " << bytesUsed(B) << '\n';

  // TODO: parseMsolveFile should also return: modulus, varnames, monorder.
  std::vector<std::string> varnames {
    "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m",
    "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z",
    "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L",
    "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V"
  };
  const PolynomialRing* R = simplePolynomialRing(101, varnames);

  // Matrix version
  //const Matrix* M = toMatrix(R->make_FreeModule(1), B);
  //newf4::GBF4Interface gbInterface(R,
  //                                 M,
  //                                 {},
  //                                 newf4::Strategy::Normal);
 
  // BasicPolyList version
  newf4::GBF4Interface gbInterface(R,
  				   R->make_FreeModule(1),
  				   B,
  				   {},
  				   newf4::Strategy::Normal);
  
  gbInterface.computation().dumpBasisMonomials();

  //const Ring *K = R->getCoefficients();
  //auto VA = new VectorArithmetic(K);
  //newf4::MonomialHashTable monHashTable;
  //newf4::PolynomialList L(*VA, monHashTable);
  //newf4::PolynomialListStreamCollector S(101, 48, 1, L);
  //toStream(B, S);
  //std::cout << "Number of monomials: " << monHashTable.size() << std::endl;
  //monHashTable.dump();
  
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
