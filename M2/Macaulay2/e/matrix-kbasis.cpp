#include <stddef.h>  // for NULL
#include <vector>    // for vector

#include "ExponentVector.hpp"   // for exponents, exponents_t
#include "M2mem.h"              // for freemem
#include "engine-includes.hpp"  // for M2_arrayint, M2_arrayint_struct
#include "error.h"              // for ERROR
#include "freemod.hpp"          // for FreeModule
#include "int-bag.hpp"          // for Bag
#include "interrupted.hpp"      // for system_interrupted
#include "matrix-con.hpp"       // for MatrixConstructor
#include "matrix.hpp"           // for Matrix
#include "monideal.hpp"         // for MonomialIdeal
#include "monoid.hpp"           // for Monoid, ALLOCATE_EXPONENTS, EXPONENT_...
#include "newdelete.hpp"        // for newarray_atomic, newarray_atomic_clear
#include "polyring.hpp"         // for PolynomialRing
#include "ring.hpp"             // for Ring
#include "ringelem.hpp"         // for ring_elem, vec
#include "style.hpp"            // for EQ
#include "util.hpp"             // for M2_arrayint_to_stdvector

class KBasis
{
  // A class for construction of
  //  (a) kbasis of a module, in a given degree
  //  (b) kbasis of a module, which is finite
  //  (c) kbasis of a map
 private:
  const PolynomialRing *P;
  const Monoid *D;
  const Monoid *M;

  MatrixConstructor mat;

  enum { KB_FULL, KB_SINGLE, KB_MULTI } computation_type;

  const Matrix *bottom_matrix;
  std::vector<int> mHeftVector;  // length is D->n_vars(), or less.
  // Dot product with a degree of a variable
  // in 'mVariables' will give a non-negative value.

  int *var_degs;
  // var_wts[i] is the (mHeftVector . deg(mVariables[i]-th variable))
  int *var_wts;
  std::vector<int> mVariables;
  bool do_truncation;
  bool weight_has_zeros;
  int limit;  // if >= 0, then stop after that number.

  const int *lo_degree;  // if non-null, the lowest degree to collect, of length
                         // mHeftVector.size()
  const int *hi_degree;  // if non-null, the highest degree to collect, of
                         // length mHeftVector.size()

  // In the singly graded case: collect every monomial whose weight lies >=
  // weight of
  // lo_degree (kb_target_lo_weight), and <= weight of hi_degree
  // (kb_target_hi_weight).
  // (resp -infty, infty, if lo_degree resp hi_degree is null).  (NO: that's not
  // right (!), because ordering of the weights might
  // be the reverse of the ordering of the degrees, if the heft vector is
  // negative.)
  //
  // in multi-graded case, we can only collect one degree, or the entire module.
  // so: lo_degree and hi_degree must be the same (null, or same degree vector).

  int *kb_exp;        // exponent vector being constructed recursively
  int kb_exp_weight;  // weight of this exponent vector

  int kb_target_lo_weight;  // only valid if lo_degree is not null
  int kb_target_hi_weight;  // only valid if hi_degree is not null

  int *kb_target_multidegree;  // in multigraded case this is not null, and is
                               // the
                               // degree vector which is our target.
  int *kb_exp_multidegree;     // used in recursion, and also to unpack the
                               // multidegree even in the singly graded case

  int kb_comp;

  int *kb_mon;

  MonomialIdeal *kb_monideal;

  bool kb_error;  // set if ERROR has been called, e.g. if a full basis of a non
                  // 0-diml module is asked for

  void insert();
  bool try_insert_full();
  bool try_insert_sg();
  bool try_insert_mg();
  bool backtrack(int &curr);
  bool backtrack_mg(int &curr);
  void basis0_full();
  void basis0_singly_graded();
  void basis0_multi_graded();

  KBasis(const Matrix *bottom,
         const int *lo_degree,
         const int *hi_degree,
         std::vector<int> heftvec,
         std::vector<int> varlist,
         bool do_truncation,
         int limit);

  ~KBasis() {}
  void compute();

  Matrix *value() { return (kb_error ? 0 : mat.to_matrix()); }

 public:
  static Matrix *k_basis(const Matrix *bottom,
                         M2_arrayint lo_degree,
                         M2_arrayint hi_degree,
                         std::vector<int> heftvec,
                         std::vector<int> varlist,
                         bool do_truncation,
                         int limit);
};

KBasis::KBasis(const Matrix *bottom,
               const int *lo_degree0,
               const int *hi_degree0,
               std::vector<int> heftvec,
               std::vector<int> varlist,
               bool do_truncation0,
               int limit0)
    : bottom_matrix(bottom),
      mHeftVector(heftvec),
      mVariables(varlist),
      do_truncation(do_truncation0),
      weight_has_zeros(false),
      limit(limit0),
      lo_degree(lo_degree0),
      hi_degree(hi_degree0),
      kb_error(false)
{
  P = bottom->get_ring()->cast_to_PolynomialRing();
  M = P->getMonoid();
  D = P->get_degree_ring()->getMonoid();

  if (lo_degree == 0 && hi_degree == 0) { computation_type = KB_FULL; }
  else if (mHeftVector.size() == 1) { computation_type = KB_SINGLE; }
  else { computation_type = KB_MULTI; }

  // Compute the (non-negative) weights of each of the variables in
  // 'mVariables'.

  var_wts = newarray_atomic(int, mVariables.size());
  var_degs = newarray_atomic(int, mVariables.size() * mHeftVector.size());
  int *exp =
      newarray_atomic(int, D->n_vars());  // used to hold exponent vectors
  int next = 0;
  for (int i = 0; i < mVariables.size(); i++, next += mHeftVector.size())
    {
      int v = mVariables[i];
      D->to_expvector(M->degree_of_var(v), exp);
      var_wts[i] = exponents::weight(mHeftVector.size(), exp, mHeftVector);
      if (var_wts[i] == 0) weight_has_zeros = true;
      exponents::copy(mHeftVector.size(), exp, var_degs + next);
    }
  freemem(exp);

  // Set the recursion variables
  kb_exp = newarray_atomic_clear(int, P->n_vars());
  kb_exp_weight = 0;

  if (lo_degree != NULL)
    kb_target_lo_weight =
        exponents::weight(mHeftVector.size(), lo_degree, mHeftVector);
  if (hi_degree != NULL)
    kb_target_hi_weight =
        exponents::weight(mHeftVector.size(), hi_degree, mHeftVector);

  if (lo_degree && hi_degree && mHeftVector.size() == 1 && mHeftVector[0] < 0)
    {
      int t = kb_target_lo_weight;
      kb_target_lo_weight = kb_target_hi_weight;
      kb_target_hi_weight = t;
    }

  kb_mon = M->make_one();

  mat = MatrixConstructor(bottom->rows(), 0);
  kb_exp_multidegree = D->make_one();

  if (mHeftVector.size() > 1 && lo_degree != NULL)
    {
      kb_target_multidegree = D->make_one();
      exponents::copy(mHeftVector.size(), lo_degree, kb_target_multidegree);
    }
  else { kb_target_multidegree = 0; }
}

void KBasis::insert()
{
  // We have a new basis element

  M->from_expvector(kb_exp, kb_mon);
  ring_elem r = P->make_flat_term(P->getCoefficients()->one(), kb_mon);
  vec v = P->make_vec(kb_comp, r);
  mat.append(v);
  if (limit > 0) limit--;
}

inline bool KBasis::backtrack(int &curr)
{
  // if we are the end, decrease the last entry to 0
  if (curr == mVariables.size() - 1)
    {
      kb_exp_weight -= var_wts[curr] * kb_exp[mVariables[curr]];
      kb_exp[mVariables[curr]] = 0;
      do {
          curr--;
      } while (curr >= 0 && kb_exp[mVariables[curr]] == 0);
    }
  if (curr < 0) return false;
  kb_exp[mVariables[curr]]--;
  kb_exp_weight -= var_wts[curr];
  curr++;
  return true;
}

inline bool KBasis::try_insert_full()
{
  Bag *b;
  if (kb_monideal->search_expvector(kb_exp, b)) return false;
  insert();
  return true;
}

void KBasis::basis0_full()
{
  // insert the all zeros vector
  if (!try_insert_full()) return;
  if (mVariables.size() == 0) return;

  int curr = 0;
  do {
      int vcurr = mVariables[curr];
      // increase the curr index until we reach a limit
      do {
          if (limit == 0 || system_interrupted()) return;
          kb_exp[vcurr]++;
          kb_exp_weight += var_wts[curr];
      } while (try_insert_full());
  } while (backtrack(curr));
}

inline bool KBasis::try_insert_sg()
{
  Bag *b;
  if (kb_monideal->search_expvector(kb_exp, b)) return false;
  if (hi_degree && kb_exp_weight > kb_target_hi_weight)
    {
      if (do_truncation) insert();
      return false;
    }
  if (!lo_degree || kb_exp_weight >= kb_target_lo_weight) insert();
  return true;
}

void KBasis::basis0_singly_graded()
{
  // insert the all zeros vector
  if (!try_insert_sg()) return;
  if (mVariables.size() == 0) return;
  int curr = 0;
  do {
      int vcurr = mVariables[curr];
      // increase the curr index until we reach a limit
      do {
          if (limit == 0 || system_interrupted()) return;
          kb_exp[vcurr]++;
          kb_exp_weight += var_wts[curr];
      } while (try_insert_sg());
  } while (backtrack(curr));
}

inline bool KBasis::backtrack_mg(int &curr)
{
  int vcurr = -1;
  // if we are the end, decrease the last entry to 0
  if (curr == mVariables.size() - 1)
    {
      vcurr = mVariables[curr];
      kb_exp_weight -= var_wts[curr] * kb_exp[vcurr];
      exponents::multpower(mHeftVector.size(),
                        kb_exp_multidegree,
                        var_degs + (mHeftVector.size() * curr),
                        -kb_exp[vcurr],
                        kb_exp_multidegree);
      kb_exp[vcurr] = 0;
      do {
          curr--;
      } while (curr >= 0 && kb_exp[mVariables[curr]] == 0);
    }
  if (curr < 0) return false;
  vcurr = mVariables[curr];
  kb_exp[vcurr]--;
  kb_exp_weight -= var_wts[curr];
  exponents::divide(mHeftVector.size(),
                 kb_exp_multidegree,
                 var_degs + (mHeftVector.size() * curr),
                 kb_exp_multidegree);
  curr++;
  return true;
}

inline bool KBasis::try_insert_mg()
{
  Bag *b;
  if (kb_monideal->search_expvector(kb_exp, b)) return false;
  if (kb_exp_weight > kb_target_lo_weight)
    {
      if (do_truncation) insert();
      return false;
    }
  if (kb_exp_weight == kb_target_lo_weight)
    {
      if (EQ == exponents::lex_compare(mHeftVector.size(),
                                    kb_target_multidegree,
                                    kb_exp_multidegree))
        insert();
    }
  return true;
}

void KBasis::basis0_multi_graded()
{
  // insert the all zeros vector
  if (!try_insert_mg()) return;
  if (mVariables.size() == 0) return;

  int curr = 0;
  do {
      int vcurr = mVariables[curr];
      // increase the curr index until we reach a limit
      do {
          if (limit == 0 || system_interrupted()) return;
          kb_exp[vcurr]++;
          kb_exp_weight += var_wts[curr];
          exponents::mult(mHeftVector.size(),
                       kb_exp_multidegree,
                       var_degs + (mHeftVector.size() * curr),
                       kb_exp_multidegree);
      } while (try_insert_mg());
  } while (backtrack_mg(curr));
}

static bool all_have_pure_powers(const MonomialIdeal *M,
                                 std::vector<int> varlist)
{
  // returns true iff all the variables in varlist have some pure power in M
  std::vector<int> lcms = M2_arrayint_to_stdvector<int>(M->lcm());
  exponents_t exp = ALLOCATE_EXPONENTS(EXPONENT_BYTE_SIZE(lcms.size()));
  for (int i = 0; i < lcms.size(); i++) exp[i] = 0;
  for (int i = 0; i < varlist.size(); i++)
    {
      Bag *b;
      int v = varlist[i];
      exp[v] = lcms[v];
      if (!M->search_expvector(exp, b)) return false;
      exp[v] = 0;
    }
  return true;
}

void KBasis::compute()
// Only the lead monomials of the two matrices 'this' and 'bottom' are
// considered.  Thus, you must perform the required GB's elsewhere.
// Find a basis for (image this)/(image bottom) in degree d.
// If 'd' is NULL, first check that (image this)/(image bottom) has
// finite dimension, and if so, return a basis.
// If 'd' is not NULL, it is an element of the degree monoid.
{
  if (limit == 0) return;
  std::vector<int> zero_vars;
  if (weight_has_zeros)
    {
      for (int i = 0; i < mVariables.size(); i++)
        {
          if (var_wts[i] == 0) { zero_vars.push_back(i); }
        }
    }

  for (int i = 0; i < bottom_matrix->n_rows(); i++)
    {
      if (system_interrupted()) return;
      kb_comp = i;

      // Make the monomial ideal: this should contain only
      // monomials involving 'mVariables'.
      kb_monideal = bottom_matrix->make_monideal(i, true);
      // the true means: over ZZ, don't consider monomials with non-unit lead
      // coeffs

      if (kb_monideal->is_one()) continue;
      if (hi_degree == NULL)
        {
          // check here that kb_monideal is 0-dimensional
          // (at least for the variables being used):
          if (!all_have_pure_powers(kb_monideal, mVariables))
            {
              kb_error = true;
              ERROR("module given is not finite over the base");
              return;
            }
        }
      else if (zero_vars.size() > 0)
        {
          // if we have any variables with zero degrees, then kb_monideal needs
          // to be 0-dimensional in those variables.
          if (!all_have_pure_powers(kb_monideal, zero_vars))
            {
              kb_error = true;
              ERROR(
                  "module given is not finite over the zero-degree variables");
              return;
            }
        }

      const int *component_degree = bottom_matrix->rows()->degree(i);
      D->to_expvector(component_degree, kb_exp_multidegree);
      kb_exp_weight =
          exponents::weight(mHeftVector.size(), kb_exp_multidegree, mHeftVector);

      // Do the recursion
      switch (computation_type)
        {
          case KB_FULL:
            basis0_full();
            break;
          case KB_SINGLE:
            basis0_singly_graded();
            break;
          case KB_MULTI:
            basis0_multi_graded();
            break;
        }
    }
}

Matrix /* or null */ *KBasis::k_basis(const Matrix *bottom,
                                      M2_arrayint lo_degree,
                                      M2_arrayint hi_degree,
                                      std::vector<int> heftvec,
                                      std::vector<int> varlist,
                                      bool do_truncation,
                                      int limit)
{
  // There are essentially 3 situations:
  // (a) basis(M) -- lo_degree and hi_degree are not given
  //     in this case, only need that for each variable in 'varlist',
  //     some power is an initial term of 'bottom' (for each row of 'bottom').
  //     heft is not used here, or considered.
  // (b) basis(lo,hi,M) -- case when the ring is singly-graded
  //     one of lo and hi must be given. (otherwise we are in case (a) above)
  //     In this case, heft is a list with one element in it.
  //     Assume: heft * deg(x) > 0, for all x in 'varlist'.
  //     In this situation: we use kb_target_lo_heft, kb_target_hi_heft
  // (c) basis(d, d, M) -- ring is multi-graded
  //   ASSUME: deg_d(x) . heft > 0 for all vars 'x' in 'varlist'
  //     where deg_d(x) consists of the first #d components of deg(x)
  //   ASSUME: 1 <= #d <= degreeRank of the ring
  //   use kb_target_multidegree, kb_target_lo_heft
  //     and kb_exp_multidegree (of length #d).
  //   if do_truncation, then any generator with heft > kb_target_lo_heft is
  //   placed in the resulting matrix
  //
  // Further assumptions:
  //  1 <= #heft <= degreeRank P
  //  #heft = #lo_degree = #hi_degree, if these are not 0.
  //
  //
  // Do some checks first, return 0 if not good.
  const PolynomialRing *P = bottom->get_ring()->cast_to_PolynomialRing();
  if (P == 0) return Matrix::identity(bottom->rows());

  const PolynomialRing *D = P->get_degree_ring();
  const int *lo = lo_degree->len > 0 ? lo_degree->array : 0;
  const int *hi = hi_degree->len > 0 ? hi_degree->array : 0;

  if (heftvec.size() > D->n_vars())
    {
      ERROR("expected heft vector of length <= %d", D->n_vars());
      return 0;
    }

  if (lo && heftvec.size() != lo_degree->len)
    {
      ERROR("expected degrees of length %d", heftvec.size());
      return 0;
    }

  if (hi && heftvec.size() != hi_degree->len)
    {
      ERROR("expected degrees of length %d", heftvec.size());
      return 0;
    }

  // If heftvec.size() is > 1, and both lo and hi are non-null,
  // they need to be the same
  if (heftvec.size() > 1 && lo && hi)
    for (int i = 0; i < heftvec.size(); i++)
      if (lo_degree->array[i] != hi_degree->array[i])
        {
          ERROR("expected degree bounds to be equal");
          return 0;
        }

  KBasis KB(bottom, lo, hi, heftvec, varlist, do_truncation, limit);

  // If either a low degree, or high degree is given, then we require a
  // non-negative heft vector:
  if (lo || hi)
    for (int i = 0; i < varlist.size(); i++)
      if (KB.var_wts[i] < 0)
        {
          ERROR(
              "basis: computation requires a heft form non-negative on the "
              "degrees of the variables");
          return 0;
        }
  // This next line will happen if both lo,hi degrees are given, and they are
  // different.  This can only be the singly generated case, and in that case
  // the degrees are in the wrong order, so return with 0 basis.
  if (lo != NULL && hi != NULL && lo[0] > hi[0]) return KB.value();

  KB.compute();
  if (system_interrupted()) return 0;
  return KB.value();
}

const Matrix *Matrix::basis(M2_arrayint lo_degree,
                            M2_arrayint hi_degree,
                            M2_arrayint heftvec,
                            M2_arrayint varlist,
                            bool do_truncation,
                            int limit) const
{
  auto heft = M2_arrayint_to_stdvector<int>(heftvec);
  auto vars = M2_arrayint_to_stdvector<int>(varlist);
  return KBasis::k_basis(
      this, lo_degree, hi_degree, heft, vars, do_truncation, limit);
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e matrix-kbasis.o "
// indent-tabs-mode: nil
// End:
