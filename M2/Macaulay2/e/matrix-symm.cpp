/**
 * @file matrix-symm.cpp
 * @brief `SymmMatrix` --- compute the `p`-th symmetric power of a one-row `Matrix`.
 *
 * Implements `SymmMatrix`, a file-local class whose only public
 * surface is the static factory
 * `SymmMatrix::symmetricPower(m0, p)`. The factory checks
 * `m0->n_rows() == 1` (the engine entry insists on a single
 * row; the `ERROR("expected one row")` branch returns `nullptr`
 * otherwise), constructs a transient `SymmMatrix` instance,
 * and returns the result of its `value()` accessor. The
 * recursive instance method `symm1(f, lastn, pow)` walks
 * multi-index tuples in lex order: each level loops `i` from
 * `lastn` to `ncols-1` and recurses with `(i, pow-1)` --- the
 * key detail being that the next level starts at `i`, not
 * `i + 1`, so variables can repeat. This enumerates all
 * `binomial(n + p - 1, p)` symmetric monomials of degree `p`
 * (the `m_{i_1} ... m_{i_p}` products with
 * `1 <= i_1 <= ... <= i_p <= n`), filling them into a
 * `MatrixConstructor` whose target and source free modules are
 * built via `m0->rows()->symm(p)` and `m0->cols()->symm(p)`
 * with the appropriate degree shift. The top-level
 * `Matrix::symm(int n)` (defined at the bottom) is the actual
 * external entry point and just forwards to
 * `SymmMatrix::symmetricPower`.
 *
 * The multi-set enumeration parallels the subset-encoding
 * machinery in `comb.hpp`'s `Subsets`, but is hand-rolled here
 * rather than going through that helper.
 *
 * @see matrix.hpp
 * @see matrix-con.hpp
 * @see comb.hpp
 */

#include "matrix.hpp"
#include "matrix-con.hpp"

/**
 * @brief Helper that builds the `p`-th symmetric power of a 1-by-`n`
 * matrix.
 *
 * @details Invoked through the static `symmetricPower(m, p)` entry point,
 * which validates that the input has exactly one row and then
 * runs the recursive multi-index walker. Each resulting column
 * corresponds to a multiset of `p` columns of `m` whose entries
 * are multiplied together --- the standard symmetric-power-of-a-
 * matrix construction. Used to implement the engine-side
 * `symmetricPower` matrix operation.
 */
class SymmMatrix
{
 public:
  static Matrix /* or null */ *symmetricPower(const Matrix *m0, int p)
  {
    if (m0->n_rows() != 1)
      {
        ERROR("expected one row");
        return nullptr;
      }

    SymmMatrix s(m0, p);
    return s.value();
  }

 private:
  int symm1_next;
  const Ring *R;
  int ncols;
  const Matrix *m;
  MatrixConstructor result;

  void symm1(vec f,      // product so far generated
             int lastn,  // can use lastn..n_cols()-1 in product
             int pow)    // remaining power to take
  {
    if (pow == 0)
      result.set_column(symm1_next++, f);
    else
      {
        for (int i = lastn; i < ncols; i++)
          {
            ring_elem r = m->elem(0, i);
            vec h = R->copy_vec(f);
            R->mult_vec_to(h, r, false);
            symm1(h, i, pow - 1);
          }
        R->remove_vec(f);
      }
  }

  SymmMatrix(const Matrix *m0, int p)
      : symm1_next(0), R(m0->get_ring()), ncols(m0->n_cols()), m(m0), result()
  {
    const FreeModule *Fp = m0->rows()->symm(p);
    const FreeModule *Gp = m0->cols()->symm(p);

    monomial dp = R->degree_monoid()->make_new(m->degree_shift());
    R->degree_monoid()->power(dp, p, dp);

    result = MatrixConstructor(Fp, Gp, dp);

    if (p >= 0)
      {
        vec f = R->e_sub_i(0);
        symm1(f, 0, p);  // consumes f
      }
  }

  Matrix *value() { return result.to_matrix(); }
};

Matrix /* or null */ *Matrix::symm(int n) const
{
  return SymmMatrix::symmetricPower(this, n);
}

#if 0
//
// namespace M2 {
//   Matrix /* or null */ *M2::symmetricPower(const Matrix *m, int p)
//   {
//     if (m->n_rows() != 1)
//       {
//      ERROR("expected one row");
//      return 0;
//       }
//
//     SymmMatrix s(m,p);
//     return s.value();
//   }
// };
// class SymmMatrix
// {
//   int symm1_next;
//   const Ring *R;
//   int ncols;
//   const Matrix *m;
//   MatrixConstructor result;
//
//
//   void symm1(vec f,         // product so far generated
//           int lastn,        // can use lastn..n_cols()-1 in product
//           int pow);   // remaining power to take
//
//   SymmMatrix(const Matrix *m, int p);
//   void compute();
//   Matrix * value();
// public:
//   friend Matrix /* or null */ *M2::symmetricPower(const Matrix *m, int p);
//   static Matrix /* or null */ * symmetricPower(const Matrix *m, int p);
// };
//
// void SymmMatrix::symm1(vec f,           // product so far generated, consumed here
//                     int lastn,       // can use lastn..n_cols()-1 in product
//                     int pow)   // remaining power to take
// {
//   if (pow == 0)
//     result.set_column(symm1_next++, f);
//   else
//     {
//       for (int i=lastn; i<ncols; i++)
//      {
//        ring_elem r = m->elem(0,i);
//        vec h = R->copy(f);
//        R->mult(h,r,false);
//        symm1(h, i, pow-1);
//      }
//       R->remove(f);
//     }
// }
//
// SymmMatrix::SymmMatrix(const Matrix *m0, int p)
//   : symm1_next(0),
//     R(m0->get_ring()),
//     ncols(m0->n_cols()),
//     m(m0),
//     result()
// {
//   const FreeModule *Fp = m0->rows()->symm(p);
//   const FreeModule *Gp = m0->cols()->symm(p);
//
//   monomial dp = R->degree_monoid()->make_new(m->degree_shift());
//   R->degree_monoid()->power(dp, p, dp);
//
//   result = MatrixConstructor(Fp,Gp,dp);
//
//   if (p >= 0)
//     {
//       vec f = R->e_sub_i(0);
//       symm1(f, 0, p);          // consumes f
//     }
// }
//
// Matrix * SymmMatrix::value()
// {
//   return result.to_matrix();
// }
//
// Matrix /* or null */ * SymmMatrix::symmetricPower(const Matrix *m, int p)
// {
//   if (m->n_rows() != 1)
//     {
//       ERROR("expected one row");
//       return 0;
//     }
//
//   SymmMatrix s(m,p);
//   return s.value();
// }
//
// namespace M2 {
//   Matrix /* or null */ *M2::symmetricPower(const Matrix *m, int p)
//   {
//     return SymmMatrix::symmetricPower(m,p);
//   }
// };
#endif
// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
