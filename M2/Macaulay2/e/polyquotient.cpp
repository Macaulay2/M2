// Copyright 2004 Michael E. Stillman

#include "polyquotient.hpp"

#include "buffer.hpp"
#include "comp-gb.hpp"
#include "error.h"
#include "interface/factory.h"
#include "matrix-con.hpp"
#include "matrix.hpp"
#include "monoid.hpp"
#include "polyring.hpp"
#include "relem.hpp"
#include "ring.hpp"

struct RingMap;

PolyRingQuotient::~PolyRingQuotient() {}
#if 0
// PolyRingQuotient *PolyRingQuotient::create(const PolyRing *R,
//                                         VECTOR(Nterm *) &elems)
//   // Grabs 'elems'.  Each element of 'elems' should be in the ring R.
//   // They should also form a GB.
// {
//   PolyRingQuotient *result = new PolyRingQuotient;
//   result->initialize_ring(R->charac(),
//                        R->get_degree_ring());
//   result->R_ = R;
//
//   result->overZZ_ = R->getCoefficients()->is_ZZ();
//   if (result->overZZ_)
//     result->setQuotientInfo(new QRingInfo_ZZ(R,elems));
//   else
//     result->setQuotientInfo(new QRingInfo_field_basic(R,elems));
//
//   for (int i=0; i<result->n_quotients(); i++)
//     {
//       if (!R->is_homogeneous(result->quotient_element(i)))
//      {
//        result->setIsGraded(false);
//        break;
//      }
//     }
//
//   result->zeroV = result->from_int(0);
//   result->oneV = result->from_int(1);
//   result->minus_oneV = result->from_int(-1);
//
//   return result;
// }
//
//
// PolyRingQuotient *PolyRingQuotient::create(const PolynomialRing *R,
//                                         const Matrix *M)
// {
//   if (M->get_ring() != R)
//     {
//       ERROR("quotient elements not in the expected polynomial ring");
//       return 0;
//     }
//   VECTOR(Nterm *) elems;
//   for (int i=0; i<R->n_quotients(); i++)
//     elems.push_back(R->quotient_element(i));
//   for (int i=0; i<M->n_cols(); i++)
//     elems.push_back(M->elem(0,i));
//
//   return create(R->getAmbientRing(),elems);
// }
//
// PolyRingQuotient *PolyRingQuotient::create(const PolyRing *R,
//                                         const PolynomialRing *B)
//   // R should be an ambient poly ring
//   // B should have: ambient of B is the logical coeff ring of R
//   //   i.e. R = A[x], B = A/I
//   // return A[x]/I.
// {
//   VECTOR(Nterm *) elems;
//
//   for (int i=0; i<B->n_quotients(); i++)
//     {
//       ring_elem f;
//       R->promote(B->getAmbientRing(), B->quotient_element(i), f);
//       elems.push_back(f);
//     }
//   return create(R,elems);
// }
//
// Matrix * PolyRingQuotient::getPresentation() const
// {
//   const PolyRing *R = getAmbientRing();
//
//   MatrixConstructor mat(R->make_FreeModule(1), 0);
//   for (int i=0; i<n_quotients(); i++)
//     mat.append(R->make_vec(0, quotient_element(i)));
//   return mat.to_matrix();
// }
#endif

void PolyRingQuotient::text_out(buffer &o) const
{
  o << "Quotient ring of ";
  numerR_->text_out(o);
  o << newline << "quotient elements" << newline;
  for (int i = 0; i < n_quotients(); i++)
    {
      o << "    ";
      elem_text_out(o, quotient_element(i));
      o << newline;
    }
}

bool PolyRingQuotient::is_unit(ring_elem a) const
{
  ring_elem b = invert(a);
  return !is_zero(b);
}

bool PolyRingQuotient::lift(const Ring *Rg,
                            const ring_elem f,
                            ring_elem &result) const
// f is an element of 'this'.  Rg is the desired ring.
{
  const PolynomialRing *Rg1 = Rg->cast_to_PolynomialRing();
  if (Rg == numerR_ || (Rg1 != 0 && Rg1->getAmbientRing() == numerR_))
    {
      result = f;
      return true;
    }
  return numerR_->PolyRing::lift(Rg, f, result);
}

bool PolyRingQuotient::promote(const Ring *Rf,
                               const ring_elem f,
                               ring_elem &result) const
// f is an element of Rf.  result will be an element in 'this'.
{
  const PolynomialRing *R1 = Rf->cast_to_PolynomialRing();
  if (Rf == numerR_ || (R1 != 0 && numerR_ == R1->getAmbientRing()))
    {
      result = copy(f);
      normal_form(result);
      return true;
    }
  return numerR_->PolyRing::promote(Rf, f, result);
}

ring_elem PolyRingQuotient::power(const ring_elem f, mpz_srcptr n) const
{
  return Ring::power(f, n);
}

ring_elem PolyRingQuotient::power(const ring_elem f, int n) const
{
  return Ring::power(f, n);
}

ring_elem PolyRingQuotient::invert(const ring_elem f) const
{
  if (nvars_ == 1 && n_quotients() == 1 && K_->is_field() && ! K_->is_fraction_field())
    {
      ring_elem g = quotient_element(0);

      RingElement *f1 = RingElement::make_raw(getAmbientRing(), f);
      RingElement *g1 = RingElement::make_raw(getAmbientRing(), g);
      const RingElement *u1;
      const RingElement *v1;
      const RingElement *ret = rawExtendedGCDRingElement(f1, g1, &u1, &v1);
      if (ret == nullptr)
        {
          // one reason this might return nullptr is if the coefficient ring is not
          // ZZ/n, ZZ, or QQ
          // now what do we do?
          // we can't return nullptr
          INTERNAL_ERROR("ring element gcd computation failed");
        }
      if (!getAmbientRing()->is_unit(ret->get_value())) return from_long(0);
      return u1->get_value();
    }
  else if (M_->getNonTermOrderVariables()->len == 0)
    return ann(from_long(1), f);
  else
    {
      // An error message is generated higher up
      return from_long(0);
    }
}

ring_elem PolyRingQuotient::divide(const ring_elem f, const ring_elem g) const
{
  ring_elem rem, d;
  rem = numerR_->remainderAndQuotient(f, g, d);
  if (is_zero(rem)) return d;  // This should be in normal form?
  return ann(f, g);
  //  ring_elem ginv = invert(g);
  //  ring_elem result = mult(f, ginv);
  //  normal_form(result);
  //  return result;
}

GBComputation *PolyRingQuotient::make_gb(const ring_elem g) const
// return the GB of g, keep = 0 or 1.
{
  MatrixConstructor mat(make_FreeModule(1), 1);
  mat.set_entry(0, 0, g);
  Matrix *mg = mat.to_matrix();  // {g}

  M2_arrayint weights = M2_makearrayint(n_vars());
  for (int i = 0; i < n_vars(); i++) weights->array[i] = 1;
  GBComputation *G = GBComputation::choose_gb(mg,
                                              false,  // collect syz
                                              -1,
                                              weights,
                                              false,
                                              -1,
                                              0,
                                              0
                                              /* , max_reduction_count */
                                              );
  G->set_stop_conditions(false,
                         nullptr,
                         -1,
                         -1,  // syzygy limit
                         -1,
                         -1,
                         -1,
                         false,
                         nullptr);

  G->start_computation();
  return G;
}

ring_elem PolyRingQuotient::remainder(const ring_elem f,
                                      const ring_elem g) const
{
  if (K_->get_precision() > 0)
    {
      ERROR(
          "polynomial division not yet implemented for RR or CC coefficients");
      return from_long(0);
    }
  MatrixConstructor matf(make_FreeModule(1), 1);
  matf.set_entry(0, 0, f);
  const Matrix *mf = matf.to_matrix();

  GBComputation *G = make_gb(g);

  const Matrix *mrem = G->matrix_remainder(mf);
  ring_elem result = mrem->elem(0, 0);

  delete mrem;
  delete mf;
  delete G;
  return result;
}

ring_elem PolyRingQuotient::quotient(const ring_elem f, const ring_elem g) const
{
  if (K_->get_precision() > 0)
    {
      ERROR(
          "polynomial division not yet implemented for RR or CC coefficients");
      return from_long(0);
    }
  MatrixConstructor matf(make_FreeModule(1), 1);
  matf.set_entry(0, 0, f);
  Matrix *mf = matf.to_matrix();

  GBComputation *G = make_gb(g);

  const Matrix *mrem, *mquot;
  G->matrix_lift(mf, &mrem, &mquot);
  ring_elem result = mquot->elem(0, 0);

  delete mrem;
  delete mquot;
  delete mf;
  delete G;
  return result;
}

ring_elem PolyRingQuotient::remainderAndQuotient(const ring_elem f,
                                                 const ring_elem g,
                                                 ring_elem &quot) const
{
  if (K_->get_precision() > 0)
    {
      ERROR(
          "polynomial division not yet implemented for RR or CC coefficients");
      quot = from_long(0);
      return from_long(0);
    }
  MatrixConstructor matf(make_FreeModule(1), 1);
  matf.set_entry(0, 0, f);
  Matrix *mf = matf.to_matrix();

  GBComputation *G = make_gb(g);

  const Matrix *mrem, *mquot;
  G->matrix_lift(mf, &mrem, &mquot);
  quot = mquot->elem(0, 0);
  ring_elem result = mrem->elem(0, 0);

  delete mrem;
  delete mquot;
  delete mf;
  delete G;
  return result;
}

ring_elem PolyRingQuotient::ann(const ring_elem a, const ring_elem b) const
// return an element h such that h*a is in (b). (Actually: h*a = b)...
// The lift does the following:
// ma = mquot*b + mrem
{
  MatrixConstructor mata(make_FreeModule(1), 1);
  mata.set_entry(0, 0, a);
  Matrix *ma = mata.to_matrix();  // {a}

  GBComputation *G = make_gb(b);

  const Matrix *mrem, *mquot;
  G->matrix_lift(ma, &mrem, &mquot);
  if (mquot->n_cols() == 0 || !mrem->is_zero())
    {
      // We have just determined that a/b does not exist.
      // So b is not a unit in this ring.
      set_non_unit(b);
      return from_long(0);
    }
  return mquot->elem(0, 0);
}

void PolyRingQuotient::syzygy(const ring_elem a,
                              const ring_elem b,
                              ring_elem &x,
                              ring_elem &y) const
{
  MatrixConstructor mat(make_FreeModule(1), 2);
  mat.set_entry(0, 0, a);
  mat.set_entry(0, 1, b);
  Matrix *m = mat.to_matrix();  // {a,b}
  M2_arrayint weights = M2_makearrayint(n_vars());
  for (int i = 0; i < n_vars(); i++) weights->array[i] = 1;
  GBComputation *G = GBComputation::choose_gb(m,
                                              true,  // collect syz
                                              -1,    // keep all rows
                                              weights,
                                              false,
                                              -1,
                                              0,
                                              0
                                              /* , max_reduction_count */
                                              );
  G->set_stop_conditions(false,
                         nullptr,
                         -1,
                         1,  // syzygy limit
                         -1,
                         -1,
                         -1,
                         false,
                         nullptr);
  G->start_computation();
  const Matrix *s = G->get_syzygies();

  // Now extract the two pieces of info
  x = s->elem(0, 0);
  y = s->elem(1, 0);
  ring_elem c = preferred_associate(x);
  ring_elem x1 = mult(c, x);
  ring_elem y1 = mult(c, y);
  x = x1;
  y = y1;
}

ring_elem PolyRingQuotient::random() const
{
  ring_elem result = numerR_->PolyRing::random();
  if (overZZ_) normal_form(result);
  return result;
}

ring_elem PolyRingQuotient::eval(const RingMap *map,
                                 const ring_elem f,
                                 int first_var) const
{
  return numerR_->PolyRing::eval(map, f, first_var);
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
