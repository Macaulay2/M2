// Copyright 2016 Michael E. Stillman

#include "../polyring.hpp"
#include "../freemod.hpp"
#include "../matrix-con.hpp"
#include "../matrix.hpp"
#include "../mat.hpp"
#include "../newdelete.hpp"
#include "res-f4-computation.hpp"
#include "res-f4-m2-interface.hpp"
#include "../gbring.hpp"
#include "../aring-zzp-flint.hpp"
#include "../aring-zzp-ffpack.hpp"
#include "../dmat.hpp"
#include "../mat-linalg.hpp"

#include "../timing.hpp"

#include <vector>
#include <iostream>

#include "res-gausser-ZZp.hpp"
#include "res-gausser-QQ.hpp"

ring_elem ResGausserZZp::to_ring_elem(const CoefficientVector& coeffs, size_t loc) const
{
  auto& elems = coefficientVector(coeffs); 
  ring_elem result;
  result.int_val = get_ring()->from_long(coeff_to_int(elems[loc]));
  return result;
}

void ResGausserZZp::from_ring_elem(CoefficientVector& result, ring_elem a, ring_elem unused) const
{
  auto& elems = coefficientVector(result);
  int a1;
  Kp->set_from_long(a1, static_cast<int>(get_ring()->coerceToLongInteger(a).second));
  elems.push_back(a1);
}

////////////////////
// NOTE!! //////////
// Even though the ring is the rationals, gbring 'ring_elem's are in ZZ.
ring_elem ResGausserQQ::to_ring_elem(const CoefficientVector& coeffs, size_t loc) const
{
  auto& elems = coefficientVector(coeffs); 
  ring_elem result;
  // TODO: NEED TO FIX THIS!!!!!!!
  return result;
}

void ResGausserQQ::from_ring_elem(CoefficientVector& result, ring_elem numer, ring_elem denom) const
{
  const M2::ARingZZGMP *Z = globalZZ->get_ARing();
  auto& elems = coefficientVector(result);
  M2::ARingZZGMP::ElementType numer1;
  M2::ARingZZGMP::ElementType denom1;
  Z->init(numer1);
  Z->from_ring_elem(numer1, numer);
  Z->init(denom1);
  Z->from_ring_elem(denom1, denom);
  bool isunit = Z->is_equal(numer1,denom1);
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

void ResF4toM2Interface::from_M2_vec(const ResPolyRing& R,
                                  const FreeModule *F,
                                  vec v,
                                  poly &result)
{
  const PolynomialRing* origR = F->get_ring()->cast_to_PolynomialRing();
  const Monoid* M = origR->getMonoid();

  ring_elem denom;
  gbvector *f = origR->translate_gbvector_from_vec(F,v, denom);
  bool isQQ = origR->is_QQ();
  GBRing *GR = origR->get_gb_ring();
  int n = GR->gbvector_n_terms(f);

  buffer o;
  o << "input: ";
  GR->gbvector_text_out(o,F,f,-1);
  o << newline;
  emit(o.str());
  int *exp = new int[M->n_vars()+1];
  res_ntuple_word *lexp = new res_ntuple_word[M->n_vars()+1];

  CoefficientVector coeffs = R.resGausser().allocateCoefficientVector();
          // all these pointers (or values) are still in the element f.
  auto monoms = std::unique_ptr<res_monomial_word[]>(new res_monomial_word[n * R.monoid().max_monomial_size()]);
  n = 0;
  res_monomial_word *nextmonom = monoms.get();
  for (gbvector *t = f; t != 0; t=t->next)
    {
      R.resGausser().from_ring_elem(coeffs, t->coeff, f->coeff); // note: f->coeff is assumed to be 1 for finite fields, but for QQ both of these are integers
      
      M->to_expvector(t->monom, exp);
      for (int a =0; a<M->n_vars(); a++)
        lexp[a] = exp[a];
      R.monoid().from_exponent_vector(lexp, t->comp-1, nextmonom); // gbvector components are shifted up by one
      nextmonom += R.monoid().monomial_size(nextmonom);
      n++;
    }

  poly_constructor::setPolyFromArrays(result, n, coeffs, monoms);
  GR->gbvector_remove(f);
  delete [] exp;
  delete [] lexp;
}

vec ResF4toM2Interface::to_M2_vec(const ResPolyRing& R,
                               const poly &f,
                               const FreeModule *F)
{
  const PolynomialRing *origR = F->get_ring()->cast_to_PolynomialRing();
  const Monoid *M = origR->getMonoid();
  
  int *m1 = M->make_one();

  Nterm **comps = newarray(Nterm *, F->rank());
  Nterm **last = newarray(Nterm *, F->rank());
  for (int i=0; i<F->rank(); i++)
    {
      comps[i] = 0;
      last[i] = 0;
    }

  int *exp = new int[M->n_vars()+1];
  res_ntuple_word *lexp = new res_ntuple_word[M->n_vars()+1];

  const res_monomial_word *w = f.monoms.get();
  for (int i=0; i<f.len; i++)
    {
      long comp;
      R.monoid().to_exponent_vector(w, lexp, comp);
      w = w + R.monoid().monomial_size(w);
      for (int a=0; a<M->n_vars(); a++)
        exp[a] = static_cast<int>(lexp[a]);
      M->from_expvector(exp, m1);
      ring_elem a = R.resGausser().to_ring_elem(f.coeffs, i);
      Nterm * g = origR->make_flat_term(a, m1);
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
  for (int i=0; i<F->rank(); i++)
    {
      if (comps[i] != 0)
        {
          vec v = origR->make_vec(i,comps[i]);
          origR->add_vec_to(result,v);
          comps[i] = 0;
          last[i] = 0;
        }
    }

  delete [] exp;
  delete [] lexp;
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
  res_ntuple_word* longexp = new res_ntuple_word[M->n_vars()];
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
      long comp;
      C.monoid().to_exponent_vector(S.mTotalMonom[i], longexp, comp);
      for (int j=0; j<M->n_vars(); ++j)
        exp[j] = static_cast<int>(longexp[j]);
      M->from_expvector(exp, totalmonom);
      result->append_schreyer(deg, totalmonom, static_cast<int>(S.mTieBreaker[i]));
    }
  delete [] longexp;
  delete [] exp;
  return result;
}
Matrix *ResF4toM2Interface::to_M2_matrix(SchreyerFrame& C,
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
      result.set_column(j, to_M2_vec(C.ring(),i->mSyzygy, tar));
    }
  return result.to_matrix();
}

MutableMatrix* ResF4toM2Interface::to_M2_MutableMatrix(
                                                       const Ring* K,
                                                       SchreyerFrame& C,
                                                       int lev,
                                                       int degree)
{
  // Now we loop through the elements of degree 'degree' at level 'lev'
  auto& thislevel = C.level(lev);
  int n = 0;
  for (auto p=thislevel.begin(); p != thislevel.end(); ++p)
    {
      if (p->mDegree == degree) n++;
    }

  auto& prevlevel = C.level(lev-1);
  int* newcomps = new int[prevlevel.size()];
  int nextcomp = 0;
  for (int i=0; i<prevlevel.size(); i++)
    if (prevlevel[i].mDegree == degree)
      newcomps[i] = nextcomp++;
    else
      newcomps[i] = -1;

  // create the mutable matrix
  MutableMatrix* result = MutableMatrix::zero_matrix(K,
                                                     nextcomp,
                                                     n,
                                                     true);
  // Now loop through the elements at thislevel,
  // and for each, loop through the terms of mSyzygy.
  // if the component x satisfies newcomps[x] >= 0, then place
  // this coeff into the mutable matrix.
  int col = 0;
  
  for (auto p=thislevel.begin(); p != thislevel.end(); ++p)
    {
      if (p->mDegree != degree) continue;
      auto& f = p->mSyzygy;
      auto end = poly_iter(C.ring(), f, 1);
      auto i = poly_iter(C.ring(), f);
      for ( ; i != end; ++i)
        {
          long comp = C.monoid().get_component(i.monomial());
          if (newcomps[comp] >= 0)
            {
              ring_elem a = C.ring().resGausser().to_ring_elem(f.coeffs, i.coefficient_index());
              result->set_entry(newcomps[comp], col, a);
            }
        }
      ++col;
    }

  delete [] newcomps;
  return result;
}

template<typename RingType>
double ResF4toM2Interface::setDegreeZeroMap(SchreyerFrame& C,
                                       DMat<RingType>& result,
                                       int slanted_degree,
                                       int lev)
// 'result' should be previously initialized, but will be resized.
// return value: -1 means (slanted_degree, lev) is out of range, and the zero matrix was returned.
//   otherwise: the fraction of non-zero elements is returned.
{
  // As above, get the size of the matrix, and 'newcols'
  // Now we loop through the elements of degree 'slanted_degree + lev' at level 'lev'
  const RingType& R = result.ring();
  if (not (lev > 0 and lev <= C.maxLevel()))
    {
      result.resize(0,0);
      return -1;
    }
  assert(lev > 0 and lev <= C.maxLevel());
  int degree = slanted_degree + lev;
  auto& thislevel = C.level(lev);
  int ncols = 0;
  for (auto p=thislevel.begin(); p != thislevel.end(); ++p)
    {
      if (p->mDegree == degree) ncols++;
    }

  auto& prevlevel = C.level(lev-1);
  int* newcomps = new int[prevlevel.size()];
  int nrows = 0;
  for (int i=0; i<prevlevel.size(); i++)
    if (prevlevel[i].mDegree == degree)
      newcomps[i] = nrows++;
    else
      newcomps[i] = -1;

  result.resize(nrows, ncols);

  int col = 0;
  long nnonzeros = 0;  
  for (auto p=thislevel.begin(); p != thislevel.end(); ++p)
    {
      if (p->mDegree != degree) continue;
      auto& f = p->mSyzygy;
      auto end = poly_iter(C.ring(), f, 1);
      auto i = poly_iter(C.ring(), f);
      for ( ; i != end; ++i)
        {
          long comp = C.monoid().get_component(i.monomial());
          if (newcomps[comp] >= 0)
            {
              R.from_ring_elem(result.entry(newcomps[comp], col), C.gausser().to_ring_elem(f.coeffs, i.coefficient_index()));;
              nnonzeros++;
            }
        }
      ++col;
    }
  double frac_nonzero = (nrows*ncols);
  frac_nonzero = static_cast<double>(nnonzeros) / frac_nonzero;

  delete[] newcomps;

  return frac_nonzero;
}

int SchreyerFrame::rank(int slanted_degree, int lev)
{
#if 1
  unsigned int charac = static_cast<unsigned int>(gausser().get_ring()->characteristic());
  M2::ARingZZpFFPACK R(charac);
  DMat<M2::ARingZZpFFPACK> M(R, 0, 0);
  double frac = ResF4toM2Interface::setDegreeZeroMap(*this, M, slanted_degree, lev);
  auto a = DMatLinAlg<M2::ARingZZpFFPACK>(M);
  auto timeA = timer();
  int rk = static_cast<int>(a.rank());
  auto timeB = timer();
  double nsecs = seconds(timeB-timeA);
#else
  M2::ARingZZpFlint R(gausser().get_ring()->characteristic());
  DMat<M2::ARingZZpFlint> M(R, 0, 0);
  double frac = ResF4toM2Interface::setDegreeZeroMap(*this, M, slanted_degree, lev);
  auto a = DMatLinAlg<M2::ARingZZpFlint>(M); 
  auto timeA = timer();
  int rk = static_cast<int>(a.rank());
  auto timeB = timer();
  double nsecs = seconds(timeB-timeA);
#endif
  
  if (M2_gbTrace >= 2)
    {
      std::cout << "rank ("
                << slanted_degree << ","  << lev << ") = "
                << rk
                << " time=" << nsecs << " sec, size= "
                << M.numRows() << " x " << M.numColumns() << " nonzero " << (100*frac) << " %" << std::endl;
    }

#if 0
  if (M1.numRows() != M2.numRows() ||
      (M1.numColumns() != M2.numColumns()) ||
      rk1 != rk2)
    {
      std::cout << "ERROR!!! degree zero computations do not match" << std::endl;
    }

  if (frac1 != frac2)
    {
      std::cout << "frac1=" << frac1 << " frac2=" << frac2 << std::endl;
    }
      
  if (M2_gbTrace >= 2)
    {
      std::cout << "rank ("
                << slanted_degree << ","  << lev << ") = "
                << rk1
                << " time(" << nsecs1 << ", " << nsecs2 << ") size= "
                << M1.numRows() << " x " << M1.numColumns() << " nonzero% " << frac1 << std::endl;
    }
#endif  
  return rk;
}

M2_arrayint
rawMinimalBetti(Computation *C,
                M2_arrayint slanted_degree_limit,
                M2_arrayint length_limit)
{
  try {
    F4ResComputation *G = dynamic_cast<F4ResComputation*>(C);
    if (G != 0)
      return G->minimal_betti(slanted_degree_limit, length_limit); // Computes it if needed
    ERROR("expected resolution computed via res(...,FastNonminimal=>true)");
    return nullptr;
  }
  catch (exc::engine_error e) {
    ERROR(e.what());
    return nullptr;
  }
}

// Local Variables:
//  compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
//  End:
