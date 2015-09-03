
#include "polyroots.hpp"

#define abs(x)  ( ( (x) < 0) ? -(x) : (x) )
#define max(a, b)  ( ( (a) > (b) ) ? (a) : (b) )

typedef M2::ConcreteRing<M2::ARingRR> RingRR;
typedef M2::ConcreteRing<M2::ARingRRR> RingRRR;
typedef M2::ConcreteRing<M2::ARingCC> RingCC;
typedef M2::ConcreteRing<M2::ARingCCC> RingCCC;

engine_RawRingElementArrayOrNull rawRoots(const RingElement *p, long prec) {
  const Ring *R = p->get_ring();
  const PolynomialRing *P = R->cast_to_PolynomialRing();
  if (P == 0) {
    ERROR("expected a polynomial ring");
    return NULL;
  }
  const int n = P->n_vars();
  if (n != 1) {
    ERROR("expected a univariate polynomial ring");
    return NULL;
  }
  const Ring *K = P->getCoefficients();
  int degree = 0;
  for (Nterm *t = p->get_value(); t != NULL; t = t->next) {
    degree = max(degree, abs(*(t->monom)));
  }
  if (prec == -1) {
    prec = (K->get_precision() == 0 ? 53 : K->get_precision());
  }

  /* Start PARI computations. */
  const pari_sp av = avma;

  GEN q = cgetg(2 + degree + 1, t_POL);
  setsigne(q, 1);
  setvarn(q, 0);
  for (int i = 0; i < degree + 1; ++i) {
    gel(q, 2 + i) = gen_0;
  }

  switch (K->ringID()) {
    case M2::ring_ZZ:
      for (Nterm *t = p->get_value(); t != NULL; t = t->next) {
        gel(q, 2 + abs(*(t->monom))) =
            mpz_get_GEN(reinterpret_cast<const mpz_ptr>(t->coeff.poly_val));
      }
      break;
    case M2::ring_QQ:
      for (Nterm *t = p->get_value(); t != NULL; t = t->next) {
        gel(q, 2 + abs(*(t->monom))) =
            mpq_get_GEN(reinterpret_cast<const mpq_ptr>(t->coeff.poly_val));
      }
      break;
    case M2::ring_RR:
      for (Nterm *t = p->get_value(); t != NULL; t = t->next) {
        gel(q, 2 + abs(*(t->monom))) =
            dbltor(*reinterpret_cast<double *>(t->coeff.poly_val));
      }
      break;
    case M2::ring_CC:
      for (Nterm *t = p->get_value(); t != NULL; t = t->next) {
        GEN z = cgetg(3, t_COMPLEX);
        gel(z, 1) = dbltor(
            reinterpret_cast<complex *>(t->coeff.poly_val)->re);
        gel(z, 2) = dbltor(
            reinterpret_cast<complex *>(t->coeff.poly_val)->im);
        gel(q, 2 + abs(*(t->monom))) = z;
      }
      break;
    case M2::ring_RRR:
      for (Nterm *t = p->get_value(); t != NULL; t = t->next) {
        gel(q, 2 + abs(*(t->monom))) =
            mpfr_get_GEN(reinterpret_cast<const mpfr_ptr>(t->coeff.poly_val));
      }
      break;
    case M2::ring_CCC:
      for (Nterm *t = p->get_value(); t != NULL; t = t->next) {
        gel(q, 2 + abs(*(t->monom))) =
            mpc_get_GEN(reinterpret_cast<const mpc_ptr>(t->coeff.poly_val));
      }
      break;
    default:
      ERROR("expected coefficient ring of the form ZZ, QQ, RR or CC");
      return NULL;
  }

  GEN roots = cleanroots(q, nbits2prec(prec));
  output(roots);
  output(gel(roots, 1));

  engine_RawRingElementArrayOrNull result =
      getmemarraytype(engine_RawRingElementArray, degree);
  result->len = degree;
  ring_elem m2_root;

  if (prec <= 53) {
    const RingRR *RR =
        dynamic_cast<const RingRR *>(IM2_Ring_RRR(prec));
    const RingCC *CC =
        dynamic_cast<const RingCC *>(IM2_Ring_CCC(prec));

    for (int i = 0; i < degree; ++i) {
      const pari_sp av2 = avma;
      GEN pari_root = gel(roots, 1 + i);
      switch (typ(pari_root)) {
      case t_REAL: {
        double real_root = rtodbl(pari_root);
        RR->ring().to_ring_elem(m2_root, real_root);
        result->array[i] = RingElement::make_raw(RR, m2_root);
        break;
      }
      case t_COMPLEX: {
        complex cmplx_root = {rtodbl(greal(pari_root)),
                              rtodbl(gimag(pari_root))};
        CC->ring().to_ring_elem(m2_root, cmplx_root);
        result->array[i] = RingElement::make_raw(CC, m2_root);
        break;
      }
      default:
        break;
      }
      avma = av2;
    }
  } else {
    const RingRRR *RRR =
        dynamic_cast<const RingRRR *>(IM2_Ring_RRR(prec));
    const RingCCC *CCC =
        dynamic_cast<const RingCCC *>(IM2_Ring_CCC(prec));

    for (int i = 0; i < degree; ++i) {
      const pari_sp av2 = avma;
      GEN pari_root = gel(roots, 1 + i);
      switch (typ(pari_root)) {
      case t_REAL: {
        mpfr_ptr real_root = NULL;
        pari_mpfr_init_set_GEN(real_root, pari_root, GMP_RNDN);
        RRR->ring().to_ring_elem(m2_root, *real_root);
        result->array[i] = RingElement::make_raw(RRR, m2_root);
        break;
      }
      case t_COMPLEX: {
        mpc_ptr cmplx_root = NULL;
        pari_mpc_init_set_GEN(cmplx_root, pari_root, GMP_RNDN);
        CCC->ring().to_ring_elem(m2_root, *cmplx_root);
        result->array[i] = RingElement::make_raw(CCC, m2_root);
        break;
      }
      default:
        break;
      }
      avma = av2;
    }
  }

  /* End PARI computations. */
  avma = av;
  return result;
}
