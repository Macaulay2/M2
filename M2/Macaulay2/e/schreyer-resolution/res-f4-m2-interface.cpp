// Copyright 2016 Michael E. Stillman

#include "schreyer-resolution/res-f4-m2-interface.hpp"
#include "ZZ.hpp"                                         // for RingZZ
#include "aring-RRR.hpp"                                  // for ARingRRR
#include "aring-zz-gmp.hpp"                               // for ARingZZGMP
#include "aring-zzp-ffpack.hpp"                           // for ARingZZpFFPACK
#include "aring-zzp-flint.hpp"                            // for ARingZZpFlint
#include "aring.hpp"                                      // for ring_RR
#include "coeffrings.hpp"                                 // for Coefficient...
#include "comp.hpp"                                       // for Computation
#include "mat-linalg.hpp"                                 // for DMatLinAlg
#include "dmat.hpp"                                       // for DMat
#include "engine-exports.h"                               // for M2_arrayint
#include "error.h"                                        // for ERROR
#include "exceptions.hpp"                                 // for engine_error
#include "freemod.hpp"                                    // for FreeModule
#include "gauss.hpp"                                      // for GaussElimCo...
#include "gbring.hpp"                                     // for gbvector
#include "interface/groebner.h"                           // for rawMinimalB...
#include "mat.hpp"                                        // for MutableMatrix
#include "matrix-con.hpp"                                 // for MatrixConst...
#include "matrix.hpp"                                     // for Matrix
#include "monoid.hpp"                                     // for Monoid
#include "newdelete.hpp"                                  // for newarray
#include "polyring.hpp"                                   // for PolynomialRing
#include "ring.hpp"                                       // for Ring, globalZZ
#include "schreyer-resolution/res-f4-computation.hpp"     // for F4ResComput...
#include "schreyer-resolution/res-gausser-QQ-hybrid.hpp"  // for ResGausserQ...
#include "schreyer-resolution/res-gausser-QQ.hpp"         // for ResGausserQQ
#include "schreyer-resolution/res-gausser-ZZp.hpp"        // for ResGausserZZp
#include "schreyer-resolution/res-gausser.hpp"            // for ResGausser
#include "schreyer-resolution/res-moninfo.hpp"            // for ResMonoid
#include "schreyer-resolution/res-monomial-types.hpp"     // for res_monomia...
#include "schreyer-resolution/res-poly-ring.hpp"          // for ResPolynomialIterator
#include "schreyer-resolution/res-schreyer-frame.hpp"     // for SchreyerFrame
#include "schreyer-resolution/res-schreyer-order.hpp"     // for ResSchreyer...
#include "timing.hpp"                                     // for timer, seconds
#include <gmp.h>                                          // for mpz_clear
#include <cstdlib>                                        // for exit, size_t
#include <chrono>                                         // for common_type...
#include <iostream>                                       // for operator<<
#include <type_traits>                                    // for move
#include <vector>                                         // for vector, vec...

bool ResGausserZZp::isAllowedCoefficientRing(const Ring* K) const
{
  return K->isFinitePrimeField();
}

ring_elem ResGausserZZp::to_ring_elem(const Ring* K,
                                      const CoefficientVector& coeffs,
                                      size_t loc) const
{
  auto& elems = coefficientVector(coeffs);
  return K->from_long(coeff_to_int(elems[loc]));
}

void ResGausserZZp::from_ring_elem(CoefficientVector& result,
                                   ring_elem a,
                                   ring_elem unused) const
{
  auto& elems = coefficientVector(result);
  int a1;
  Kp->set_from_long(
      a1, static_cast<int>(get_ring()->coerceToLongInteger(a).second));
  elems.push_back(a1);
}

////////////////////
// NOTE!! //////////
// Even though the ring is the rationals, gbring 'ring_elem's are in ZZ.
bool ResGausserQQ::isAllowedCoefficientRing(const Ring* K) const
{
  return K->ringID() == M2::ring_RR or
         (K->isFinitePrimeField() and
          K->characteristic() == Kp1.characteristic());
}

ring_elem ResGausserQQ::to_ring_elem(const Ring* K,
                                     const CoefficientVector& coeffs,
                                     size_t loc) const
{
  auto& elems = coefficientVector(coeffs);
  ring_elem result;
  if (K->ringID() == M2::ring_RR)
    K->from_double(elems[loc].mDouble, result);
  else if (K == globalZZ)
    {
      result = K->from_long(elems[loc].mDenominatorSize);
    }
  else
    Kp1.to_ring_elem(result, elems[loc].mMod1);
  return result;
}

void ResGausserQQ::from_ring_elem(CoefficientVector& result,
                                  ring_elem numer,
                                  ring_elem denom) const
{
  const M2::ARingZZGMP* Z = globalZZ->get_ARing();
  auto& elems = coefficientVector(result);
  M2::ARingZZGMP::ElementType numer1;
  M2::ARingZZGMP::ElementType denom1;
  Z->init(numer1);
  Z->from_ring_elem(numer1, numer);
  Z->init(denom1);
  Z->from_ring_elem(denom1, denom);
  bool isunit = Z->is_equal(numer1, denom1);
  mpq_t c;
  mpq_init(c);
  mpq_set_num(c, &numer1);
  mpq_set_den(c, &denom1);
  mpq_canonicalize(c);
  FieldElement b;
  b.mDouble = mpq_get_d(c);
  Kp1.set_from_mpq(b.mMod1, c);
  b.mDenominatorSize = (isunit ? 0 : 1);
  elems.push_back(b);
  mpq_clear(c);
  mpz_clear(&numer1);
  mpz_clear(&denom1);
}

////////////////////////////////////
// QQ Hybrid ring //////////////////
////////////////////////////////////
////////////////////
// NOTE!! //////////
// Even though the ring is the rationals, gbring 'ring_elem's are in ZZ.
bool ResGausserQQHybrid::isAllowedCoefficientRing(const Ring* K) const
{
  return K->ringID() == M2::ring_RR or
         (K->isFinitePrimeField() and
          (K->characteristic() == Kp1.characteristic() or
           K->characteristic() == Kp2.characteristic())) or
         (K->get_precision() == mRRing.get_precision()) or (K == globalZZ);
}

ring_elem ResGausserQQHybrid::to_ring_elem(const Ring* K,
                                           const CoefficientVector& coeffs,
                                           size_t loc) const
{
  auto& elems = coefficientVector(coeffs);
  ring_elem result;
  if (K->ringID() == M2::ring_RR)
    K->from_double(elems[loc].mDouble, result);
  else if (K->ringID() == M2::ring_RRR)
    K->from_BigReal(&(elems[loc].mLongDouble), result);
  else if (K == globalZZ)
    {
      result = K->from_long(elems[loc].mDenominatorSize);
    }
  else if (K->characteristic() == Kp1.characteristic())
    Kp1.to_ring_elem(result, elems[loc].mMod1);
  else if (K->characteristic() == Kp2.characteristic())
    Kp2.to_ring_elem(result, elems[loc].mMod2);
  else
    {
      std::cout << "Internal logic error: should not get to this statement"
                << std::endl;
      exit(1);
    }
  return result;
}

void ResGausserQQHybrid::from_ring_elem(CoefficientVector& result,
                                        ring_elem numer,
                                        ring_elem denom) const
{
  //  std::cout << "creating element..." << std::flush;
  const M2::ARingZZGMP* Z = globalZZ->get_ARing();
  auto& elems = coefficientVector(result);
  M2::ARingZZGMP::ElementType numer1;
  M2::ARingZZGMP::ElementType denom1;
  Z->init(numer1);
  Z->from_ring_elem(numer1, numer);
  Z->init(denom1);
  Z->from_ring_elem(denom1, denom);
  bool isunit = Z->is_equal(numer1, denom1);
  mpq_t c;
  mpq_init(c);
  mpq_set_num(c, &numer1);
  mpq_set_den(c, &denom1);
  mpq_canonicalize(c);

  FieldElement b;
  init_element(b);
  from_mpq_element(b, c, (isunit ? 0 : 1));

  elems.emplace_back(std::move(b));

  mpq_clear(c);
  mpz_clear(&numer1);
  mpz_clear(&denom1);
  //  out(std::cout, result, elems.size()-1);
  //  std::cout << " done" << std::endl;
}

////////////////////////////////////
void ResF4toM2Interface::from_M2_vec(const ResPolyRing& R,
                                     const FreeModule* F,
                                     vec v,
                                     ResPolynomial& result)
{
  const PolynomialRing* origR = F->get_ring()->cast_to_PolynomialRing();
  const Monoid* M = origR->getMonoid();

  ring_elem denom;
  gbvector* f = origR->translate_gbvector_from_vec(F, v, denom);
  GBRing* GR = origR->get_gb_ring();
  int n = GR->gbvector_n_terms(f);

#if 0
  buffer o;
  o << "input: ";
  GR->gbvector_text_out(o,F,f,-1);
  o << newline;
  emit(o.str());
#endif

  int* exp = new int[M->n_vars()];

  CoefficientVector coeffs = R.resGausser().allocateCoefficientVector();
  // all these pointers (or values) are still in the element f.
  //  auto monoms = std::unique_ptr<res_monomial_word[]>(new res_monomial_word[n
  //  * R.monoid().max_monomial_size()]);
  std::vector<res_monomial_word> monoms(n * R.monoid().max_monomial_size());
  n = 0;
  res_monomial_word* nextmonom = monoms.data();
  for (gbvector* t = f; t != 0; t = t->next)
    {
      R.resGausser().from_ring_elem(
          coeffs, t->coeff, f->coeff);  // note: f->coeff is assumed to be 1 for
                                        // finite fields, but for QQ both of
                                        // these are integers

      M->to_expvector(t->monom, exp);
      R.monoid().from_exponent_vector(
          exp,
          t->comp - 1,
          nextmonom);  // gbvector components are shifted up by one
      nextmonom += R.monoid().monomial_size(nextmonom);
      n++;
    }

  ResPolynomialConstructor::setPolyFromArrays(result, n, coeffs, monoms);
  GR->gbvector_remove(f);
  delete[] exp;
}

vec ResF4toM2Interface::to_M2_vec(const ResPolyRing& R,
                                  const ResPolynomial& f,
                                  const FreeModule* F)
{
  const PolynomialRing* origR = F->get_ring()->cast_to_PolynomialRing();
  const Monoid* M = origR->getMonoid();

  int* m1 = M->make_one();

  Nterm** comps = newarray(Nterm*, F->rank());
  Nterm** last = newarray(Nterm*, F->rank());
  for (int i = 0; i < F->rank(); i++)
    {
      comps[i] = 0;
      last[i] = 0;
    }

  int* exp = new int[M->n_vars()];

  const res_monomial_word* w = f.monoms.data();
  for (int i = 0; i < f.len; i++)
    {
      component_index comp;
      R.monoid().to_exponent_vector(w, exp, comp);
      w = w + R.monoid().monomial_size(w);
      M->from_expvector(exp, m1);
      ring_elem a =
          R.resGausser().to_ring_elem(origR->getCoefficientRing(), f.coeffs, i);
      Nterm* g = origR->make_flat_term(a, m1);
      g->next = 0;
      if (last[comp] == 0)
        {
          comps[comp] = g;
          last[comp] = g;
        }
      else
        {
          last[comp]->next = g;
          last[comp] = g;
        }
    }
  vec result = 0;
  for (int i = 0; i < F->rank(); i++)
    {
      if (comps[i] != 0)
        {
          vec v = origR->make_vec(i, comps[i]);
          origR->add_vec_to(result, v);
          comps[i] = 0;
          last[i] = 0;
        }
    }

  delete[] exp;
  return result;
}

FreeModule* ResF4toM2Interface::to_M2_freemodule(const PolynomialRing* R,
                                                 SchreyerFrame& C,
                                                 int lev)
{
  FreeModule* result = new FreeModule(R, 0, true);
  if (lev < 0 or lev > C.maxLevel())
    {
      return result;
    }
  const Monoid* M = R->getMonoid();
  auto& thislevel = C.level(lev);
  const ResSchreyerOrder& S = C.schreyerOrder(lev);
  int* exp = new int[M->n_vars()];
  for (auto i = 0; i < thislevel.size(); ++i)
    {
      int d[1];
      d[0] = thislevel[i].mDegree;
      monomial deg = M->degree_monoid()->make_one();
      M->degree_monoid()->from_expvector(d, deg);
      // Now grab the Schreyer info
      // unpack to exponent vector, then repack into monoid element
      monomial totalmonom = M->make_one();
      component_index comp;
      C.monoid().to_exponent_vector(S.mTotalMonom[i], exp, comp);
      M->from_expvector(exp, totalmonom);
      result->append_schreyer(
          deg, totalmonom, static_cast<int>(S.mTieBreaker[i]));
    }
  delete[] exp;
  return result;
}

FreeModule* ResF4toM2Interface::to_M2_freemodule(const PolynomialRing* R,
                                                 const FreeModule* F,
                                                 SchreyerFrame& C,
                                                 int lev)
// The input F must be the original freemodule of level=0.
// assumption: lev >= 0.
{
  if (lev < 0 or lev > C.maxLevel())
    {
      ERROR("expected level in the range %d..%d",1,C.maxLevel());
      return nullptr;
    }
  FreeModule* result = new FreeModule(R, 0, true);
  const Monoid* M = R->getMonoid();
  auto& thislevel = C.level(lev);
  const ResSchreyerOrder& S = C.schreyerOrder(lev);
  int* exp = new int[M->n_vars()];
  monomial deg1 = M->degree_monoid()->make_one();
  for (auto i = 0; i < thislevel.size(); ++i)
    {
      component_index comp;
      C.monoid().to_exponent_vector(S.mTotalMonom[i], exp, comp);
      monomial deg = M->degree_monoid()->make_new(F->degree(comp)); // resulting degree of this element
      M->degree_of_expvector(exp, deg1);
      M->degree_monoid()->mult(deg, deg1, deg);
      // Now grab the Schreyer info
      // unpack to exponent vector, then repack into monoid element
      monomial totalmonom = M->make_one();
      M->from_expvector(exp, totalmonom);
      result->append_schreyer(
          deg, totalmonom, static_cast<int>(S.mTieBreaker[i]));
    }
  delete[] exp;
  M->degree_monoid()->remove(deg1);
  return result;
}

Matrix* ResF4toM2Interface::to_M2_matrix(SchreyerFrame& C,
                                         int lev,
                                         const FreeModule* tar,
                                         const FreeModule* src)
{
  if (lev < 0 or lev > C.maxLevel())
    {
      MatrixConstructor zero(tar, src);
      return zero.to_matrix();
    }
  auto& thislevel = C.level(lev);
  MatrixConstructor result(tar, src);
  int j = 0;
  for (auto i = thislevel.cbegin(); i != thislevel.cend(); ++i, ++j)
    {
      result.set_column(j, to_M2_vec(C.ring(), i->mSyzygy, tar));
    }
  return result.to_matrix();
}

// NEW
MutableMatrix* ResF4toM2Interface::to_M2_MutableMatrix(SchreyerFrame& C,
                                                       const Ring* R,
                                                       int lev)
{
  // Ring will be R, which should be a polynomial ring with the same monoid as
  // ring of C.
  const PolynomialRing* RP = R->cast_to_PolynomialRing();
  const Monoid* M = RP->getMonoid();
  const Ring* K = RP->getCoefficientRing();

  if (lev <= 0 or lev > C.maxLevel())
    {
      return MutableMatrix::zero_matrix(
          R,
          0,  // TODO: set this correctly?
          0,  // TODO: set this correctly?  i.e. one of these might be in range,
          // so getting the rank correct might be good.
          true);
    }

  auto& thislevel = C.level(lev);
  int ncols = static_cast<int>(thislevel.size());
  int nrows = static_cast<int>(C.level(lev - 1).size());

  // create the mutable matrix
  MutableMatrix* result = MutableMatrix::zero_matrix(R, nrows, ncols, true);

  //  Nterm **comps = newarray(Nterm *, nrows);
  Nterm** comps = newarray(Nterm*, nrows);
  Nterm** last = newarray(Nterm*, nrows);

  int* m1 = M->make_one();
  int* exp = new int[M->n_vars() + 1];

  int j = 0;
  for (auto j1 = thislevel.cbegin(); j1 != thislevel.cend(); ++j1, ++j)
    {
      // Now we create the polynomials for column j
      // into 'comps', 'last'.
      const ResPolynomial& f = (*j1).mSyzygy;
      for (int i = 0; i < nrows; i++)
        {
          comps[i] = nullptr;
          last[i] = nullptr;  // used to easily placce monomials in the correct
                              // bin, at the end of the polynomials.
        }
      const res_monomial_word* w = f.monoms.data();
      for (int i = 0; i < f.len; i++)
        {
          component_index comp;
          C.ring().monoid().to_exponent_vector(w, exp, comp);
          w = w + C.ring().monoid().monomial_size(w);
          M->from_expvector(exp, m1);
          ring_elem a = C.gausser().to_ring_elem(K, f.coeffs, i);
          Nterm* g = RP->make_flat_term(a, m1);
          if (g == nullptr) continue;
          g->next = 0;
          if (last[comp] == 0)
            {
              comps[comp] = g;
              last[comp] = g;
            }
          else
            {
              last[comp]->next = g;
              last[comp] = g;
            }
        }
      // Now we have run through the entire vector, so put it into result
      for (int r = 0; r < nrows; r++) result->set_entry(r, j, comps[r]);
    }

  delete[] exp;
  freemem(comps);
  freemem(last);
  return result;
}

MutableMatrix* ResF4toM2Interface::to_M2_MutableMatrix(SchreyerFrame& C,
                                                       const Ring* K,
                                                       int lev,
                                                       int degree)
{
  // The ring K should be the coefficient ring of the poly ring of C,
  // OR: if the coefficient ring is QQ, then it can be RR, or a finite field.

  // Now we loop through the elements of degree 'degree' at level 'lev'
  auto& thislevel = C.level(lev);
  int n = 0;
  for (auto p = thislevel.begin(); p != thislevel.end(); ++p)
    {
      if (p->mDegree == degree) n++;
    }

  auto& prevlevel = C.level(lev - 1);
  int* newcomps = new int[prevlevel.size()];
  int nextcomp = 0;
  for (int i = 0; i < prevlevel.size(); i++)
    if (prevlevel[i].mDegree == degree)
      newcomps[i] = nextcomp++;
    else
      newcomps[i] = -1;

  // create the mutable matrix
  MutableMatrix* result = MutableMatrix::zero_matrix(K, nextcomp, n, true);
  // Now loop through the elements at thislevel,
  // and for each, loop through the terms of mSyzygy.
  // if the component x satisfies newcomps[x] >= 0, then place
  // this coeff into the mutable matrix.
  int col = 0;

  for (auto p = thislevel.begin(); p != thislevel.end(); ++p)
    {
      if (p->mDegree != degree) continue;
      auto& f = p->mSyzygy;
      auto end = ResPolynomialIterator(C.ring(), f, 1);
      auto i = ResPolynomialIterator(C.ring(), f);
      for (; i != end; ++i)
        {
          long comp = C.monoid().get_component(i.monomial());
          if (newcomps[comp] >= 0)
            {
              ring_elem a = C.ring().resGausser().to_ring_elem(
                  K, f.coeffs, i.coefficient_index());
              result->set_entry(newcomps[comp], col, a);
            }
        }
      ++col;
    }

  delete[] newcomps;
  return result;
}

// One way to organize this:
// Create an iterator, such that: i->components() is a std::vector of sorted indices
// and i->coefficients() is a std::vector of ring_elem's.
// or: take a function as input, that knows how to consume this info.
//

class DegreeZeroMapGenerator
{
public:
  DegreeZeroMapGenerator(SchreyerFrame& C, int slanted_degree, int lev)
    : mSchreyerFrame(C),
      mThisLevel(C.level(lev)),
      mDegree(slanted_degree+lev),
      mLevel(lev),
      mNumRows(0),
      mNumColumns(0)
  {
    if (lev <= 0 or lev > C.maxLevel())
      {
        return;
      }
    int degree = slanted_degree + lev;
    for (auto p = mThisLevel.begin(); p != mThisLevel.end(); ++p)
      {
        if (p->mDegree == degree) mNumColumns++;
      }

    auto& prevlevel = C.level(lev - 1);
    mComponentTranslation.resize(prevlevel.size());
    for (int i = 0; i < prevlevel.size(); i++)
      {
        if (prevlevel[i].mDegree == mDegree)
          mComponentTranslation[i] = mNumRows++;
        else
          mComponentTranslation[i] = -1;
      }
  }

  const Ring* ring() const { return mSchreyerFrame.gausser().get_ring(); }

  int numRows() const { return mNumRows; }

  int numColumns() const { return mNumColumns; }

  long numNonzero() const
  {
    long nnonzeros = 0;
    auto& thislevel = mSchreyerFrame.level(mLevel);
    for (auto p = thislevel.begin(); p != thislevel.end(); ++p)
      {
        if (p->mDegree != mDegree) continue;
        auto& f = p->mSyzygy;
        auto end = ResPolynomialIterator(mSchreyerFrame.ring(), f, 1);
        auto i = ResPolynomialIterator(mSchreyerFrame.ring(), f);

        for (; i != end; ++i)
          {
            auto comp = mSchreyerFrame.monoid().get_component(i.monomial());
            if (mComponentTranslation[comp] >= 0)
              nnonzeros++;
          }
      }
    return nnonzeros;
  }

  class iterator
  {
  public:
    iterator(DegreeZeroMapGenerator& D)
      : mGenerator(D),
        mColumn(-1),
        mNumColumns(D.numColumns()),
        mIter(D.mThisLevel.begin()),
        mEnd(D.mThisLevel.end())
    {
      increment();
    }

    iterator(DegreeZeroMapGenerator& D, int) : mGenerator(D), mColumn(D.numColumns()) {}

    bool operator==(const iterator& sentinel) const
    {
      // Do we need to check that these refer to the same object?
      return mColumn == sentinel.mColumn;
    }

    bool operator!=(const iterator& sentinel) const
    {
      // Do we need to check that these refer to the same object?
      return mColumn != sentinel.mColumn;
    }
    
    iterator& operator++()
    {
      increment();
      return *this;
    }

    int column() const { return mColumn; }
    
    const std::vector<int>& components() const { return mComponents; }

    const std::vector<long>& coefficients() const { return mCoefficients; }

  private:    
    void increment()
    {
      ++mColumn;
      mComponents.clear();
      mCoefficients.clear();
      if (mColumn == mNumColumns) return;
      for (; mIter != mEnd; ++mIter)
        {
          if (mIter->mDegree == mGenerator.mDegree) break;
        }
      auto& f = mIter->mSyzygy;
      auto end = ResPolynomialIterator(mGenerator.mSchreyerFrame.ring(), f, 1);
      auto i = ResPolynomialIterator(mGenerator.mSchreyerFrame.ring(), f);

      for (; i != end; ++i)
        {
          auto comp = mGenerator.mSchreyerFrame.monoid().get_component(i.monomial());
          auto new_comp = mGenerator.mComponentTranslation[comp];
          if (new_comp >= 0)
            {
              mComponents.push_back(new_comp);
              long val =
                mGenerator.mSchreyerFrame.gausser().to_modp_long(f.coeffs, i.coefficient_index());
              mCoefficients.push_back(val);
            }
        }
      ++mIter;
    }
  private:
    using Iter = std::vector<SchreyerFrameTypes::FrameElement>::iterator;
    DegreeZeroMapGenerator& mGenerator;
    int mColumn;
    int mNumColumns;
    Iter mIter;
    Iter mEnd;
    std::vector<int> mComponents;
    std::vector<long> mCoefficients;
  };

  friend class DegreeZeroMapGenerator::iterator;
  
  iterator begin() { return iterator(*this); }
  iterator end() { return iterator(*this, 1); }
  
private:
  SchreyerFrame& mSchreyerFrame;
  std::vector<SchreyerFrameTypes::FrameElement>& mThisLevel;
  int mDegree;
  int mLevel;
  int mNumRows;
  int mNumColumns;
  std::vector<int> mComponentTranslation; // indices of the rows. -1 means not present.
};

template<typename Gen>
Matrix* matrixFromSparseMatrixGenerator(Gen& G)
{
  const Ring* R = G.ring();
  MatrixConstructor M(R->make_FreeModule(G.numRows()), R->make_FreeModule(G.numColumns()), nullptr);
  for (auto i = G.begin(); i != G.end(); ++i)
    {
      for (int j=i.components().size()-1; j>=0; --j)
      {
        M.set_entry(i.components()[j], i.column(), R->from_long(i.coefficients()[j]));
      }
    }
  return M.to_matrix();
}

template<typename RingType, typename Gen>
void setDMatFromSparseMatrixGenerator(Gen& G, DMat<RingType>& M)
{
  M.resize(G.numRows(), G.numColumns());

  for (auto i = G.begin(); i != G.end(); ++i)
    {
      for (int j=0; j<i.components().size(); ++j)
      {
        M.ring().set_from_long(M.entry(i.components()[j], i.column()), i.coefficients()[j]);
      }
    }
}

template<typename RingType, typename Gen>
void setDMatFromSparseMatrixGeneratorTransposed(Gen& G, DMat<RingType>& M)
{
  M.resize(G.numColumns(), G.numRows());

  for (auto i = G.begin(); i != G.end(); ++i)
    {
      for (int j=0; j<i.components().size(); ++j)
      {
        M.ring().set_from_long(M.entry(i.column(), i.components()[j]), i.coefficients()[j]);
      }
    }
}


template<typename Gen>
int SchreyerFrame::rankUsingSparseMatrix(Gen& D)
{
  const Matrix* M = matrixFromSparseMatrixGenerator(D);
  //  std::cout << "--- sparse matrix ----" << std::endl;
  //  dmatrix(M);
  //  std::cout << "----------------------" << std::endl;
  auto timeA = timer();
  GaussElimComputation comp { M, 0, 0 };
  comp.set_stop_conditions(false, nullptr, -1, -1, -1, -1, -1, false, nullptr);
  comp.start_computation();
  //  const Matrix* gbM = comp.get_gb();
  //  std::cout << "--- gb of matrix ----" << std::endl;
  //  dmatrix(gbM);
  //  std::cout << "----------------------" << std::endl;
  
  int rk = comp.get_initial(-1)->n_cols();
  auto timeB = timer();
  double nsecs = seconds(timeB - timeA);

  timeComputeSparseRanks += nsecs;

  if (M2_gbTrace >= 2)
    {
      if (M->n_rows() > 0 and M->n_cols() > 0)
        std::cout << "  sparse rank = " << rk
                  << " time = " << nsecs << " sec"
                  << std::endl;
    }

  return rk;
}

template<typename Gen>
int SchreyerFrame::rankUsingDenseMatrix(Gen& D, bool transposed)
{
  unsigned int charac =
      static_cast<unsigned int>(gausser().get_ring()->characteristic());
  M2::ARingZZpFFPACK R(charac);
  DMat<M2::ARingZZpFFPACK> M(R, 0, 0);
  if (!transposed)
    setDMatFromSparseMatrixGenerator(D, M);
  else
    setDMatFromSparseMatrixGeneratorTransposed(D, M);
  auto a = DMatLinAlg<M2::ARingZZpFFPACK>(M);
  //  std::cout << "---- dense matrix ----" << std::endl;
  //  displayMat(M);
  //  std::cout << "----------------------" << std::endl;
  auto timeA = timer();
  int rk = static_cast<int>(a.rank());
  auto timeB = timer();
  double nsecs = seconds(timeB - timeA);

  timeComputeRanks += nsecs;

  if (M2_gbTrace >= 2)
    {
      if (M.numRows() > 0 and M.numColumns() > 0)
        std::cout << "   dense rank = " << rk
                  << " time = " << nsecs << " sec"
                  << std::endl;
    }

  return rk;
}

template<typename Gen>
int SchreyerFrame::rankUsingDenseMatrixFlint(Gen& D, bool transposed)
{
  unsigned int charac =
      static_cast<unsigned int>(gausser().get_ring()->characteristic());
  M2::ARingZZpFlint R(charac);
  DMat<M2::ARingZZpFlint> M(R, 0, 0);
  if (!transposed)
    setDMatFromSparseMatrixGenerator(D, M);
  else
    setDMatFromSparseMatrixGeneratorTransposed(D, M);
  auto a = DMatLinAlg<M2::ARingZZpFlint>(M);
  //  std::cout << "---- dense matrix ----" << std::endl;
  //  displayMat(M);
  //  std::cout << "----------------------" << std::endl;
  auto timeA = timer();
  int rk = static_cast<int>(a.rank());
  auto timeB = timer();
  double nsecs = seconds(timeB - timeA);

  timeComputeRanks += nsecs;

  if (M2_gbTrace >= 2)
    {
      if (M.numRows() > 0 and M.numColumns() > 0)
        std::cout << "   dense rank = " << rk
                  << " time = " << nsecs << " sec"
                  << std::endl;
    }

  return rk;
}

int SchreyerFrame::rank(int slanted_degree, int lev)
{
  DegreeZeroMapGenerator D(*this, slanted_degree, lev);
  long nnonzero = D.numNonzero();
  long nelements = static_cast<long>(D.numRows()) * static_cast<long>(D.numColumns());
  double nnonzeroD = static_cast<double>(nnonzero);
  double nelementsD = static_cast<double>(nelements);
  double frac_nonzero = (nelements > 0 ? nnonzeroD/nelementsD : 1.0);

  if (M2_gbTrace >= 2 and nelements > 0)
    {
      std::cout << "rank(" << slanted_degree << "," << lev << ") size = "
                << D.numRows() << " x " << D.numColumns()
                << " frac non-zero= " << frac_nonzero << std::endl << std::flush;
    }
  int rkSparse = -1;
  int rkDense = -1;
  if (frac_nonzero <= .02)
    rkSparse = rankUsingSparseMatrix(D);
  if (frac_nonzero >= .01)
    {
      rkDense = rankUsingDenseMatrix(D);
      int rkDense1B = rankUsingDenseMatrix(D, true);
      int rkDense2 = rankUsingDenseMatrixFlint(D);
      int rkDense3 = rankUsingDenseMatrixFlint(D, true);
      if (rkDense != rkDense2 or rkDense != rkDense3 or rkDense != rkDense1B)
        {
          std::cout << "ERROR!! dense ranks(" << slanted_degree << "," << lev << ") = " << rkDense
                    << " and " << rkDense2 << " and " << rkDense3 << " and " << rkDense1B << std::endl;
        }
    }
      
  if (rkSparse >= 0 and rkDense >= 0 and rkSparse != rkDense)
    {
      std::cout << "ERROR!! ranks(" << slanted_degree << "," << lev << ") = " << rkSparse
                << " and " << rkDense << std::endl;
    }

  return (rkSparse >= 0 ? rkSparse : rkDense);
}

M2_arrayint rawMinimalBetti(Computation* C,
                            M2_arrayint slanted_degree_limit,
                            M2_arrayint length_limit)
{
  try
    {
      F4ResComputation* G = dynamic_cast<F4ResComputation*>(C);
      if (G != 0)
        return G->minimal_betti(slanted_degree_limit,
                                length_limit);  // Computes it if needed
      ERROR("expected resolution computed via res(...,FastNonminimal=>true)");
      return nullptr;
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return nullptr;
  }
}

// Local Variables:
//  compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
//  End:
