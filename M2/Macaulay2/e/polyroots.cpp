
#include "polyroots.hpp"

engine_RawRingElementArrayOrNull rawRoots(const RingElement *p, long prec,
                                          int unique) {
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

  engine_RawRingElementArrayOrNull result = nullptr;

  /* Start PARI computations. */
  pari_CATCH(e_STACK) {
#ifdef NDEBUG
    /*
     * Every time the stack is changed PARI writes a message to the file pari_errfile
     * which by default is /dev/stderr. To avoid showing this message to the user we
     * redirect to /dev/null before the PARI's stack is modified.
     */
    FILE *tmp, *dev_null = fopen("/dev/null", "w");
    if (dev_null != NULL) {
      tmp = pari_errfile;
      pari_errfile = dev_null;
    }
#endif
  allocatemem(0); // passing 0 will double the current stack size.
#ifdef NDEBUG
    /*
     * We set pari_errfile back to the default value just in case PARI crashes.
     */
    if (dev_null != NULL) {
      pari_errfile = tmp;
      fclose(dev_null);
    }
#endif
  } pari_RETRY {
    const pari_sp av = avma;

    GEN q = cgetg(2 + degree + 1, t_POL);
    setsigne(q, 1);
    setvarn(q, 0);
    for (int i = 0; i < degree + 1; ++i) {
      gel(q, 2 + i) = gen_0;
    }

    switch (K->ringID()) {
    case M2::ring_ZZ:
    ZZ_GMP:
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
      pari_CATCH(e_OVERFLOW) {
        ERROR("coefficient is NaN or Infinity");
        avma = av;
        return NULL;
      }
      pari_TRY {
        for (Nterm *t = p->get_value(); t != NULL; t = t->next) {
          gel(q, 2 + abs(*(t->monom))) =
              dbltor(*reinterpret_cast<double *>(t->coeff.poly_val));
        }
      }
      pari_ENDCATCH
      break;
    case M2::ring_CC:
      pari_CATCH(e_OVERFLOW) {
        ERROR("coefficient is NaN or Infinity");
        avma = av;
        return NULL;
      }
      pari_TRY {
        for (Nterm *t = p->get_value(); t != NULL; t = t->next) {
          GEN z = cgetg(3, t_COMPLEX);
          gel(z, 1) = dbltor(reinterpret_cast<complex *>(t->coeff.poly_val)->re);
          gel(z, 2) = dbltor(reinterpret_cast<complex *>(t->coeff.poly_val)->im);
          gel(q, 2 + abs(*(t->monom))) = z;
        }
      }
      pari_ENDCATCH
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
    case M2::ring_old:
      if (K->is_ZZ()) {
        goto ZZ_GMP;
      }
    default:
      ERROR("expected coefficient ring of the form ZZ, QQ, RR or CC");
      return NULL;
    }

    if (unique) {
      q = RgX_div(q, RgX_gcd_simple(q, RgX_deriv(q)));
    }

    GEN roots = cleanroots(q, nbits2prec(prec));

    const size_t num_roots = lg(roots) - 1;
    result = getmemarraytype(engine_RawRingElementArray, num_roots);
    result->len = static_cast<int>(num_roots);

    ring_elem m2_root;
    if (prec <= 53) {
      const RingCC *CC = dynamic_cast<const RingCC *>(IM2_Ring_CCC(prec));

      for (int i = 0; i < num_roots; ++i) {
        const pari_sp av2 = avma;

        GEN pari_root = gel(roots, 1 + i);
        const complex root = {rtodbl(greal(pari_root)),
                              rtodbl(gimag(pari_root))};
        CC->ring().to_ring_elem(m2_root, root);
        result->array[i] = RingElement::make_raw(CC, m2_root);

        avma = av2;
      }
    } else {
      const RingCCC *CCC = dynamic_cast<const RingCCC *>(IM2_Ring_CCC(prec));

      for (int i = 0; i < num_roots; ++i) {
        const pari_sp av2 = avma;

        mpc_t root;
        auto root1 = reinterpret_cast<mpfc_t*>(&root);
        pari_mpc_init_set_GEN(root, gel(roots, 1 + i), GMP_RNDN);
        //        CCC->ring().to_ring_elem(m2_root, **reinterpret_cast<mpfc_t*>(&root));
        CCC->ring().to_ring_elem(m2_root, **root1);
        result->array[i] = RingElement::make_raw(CCC, m2_root);

        avma = av2;
      }
    }

    /* End PARI computations. */
    avma = av;
  }
  pari_ENDCATCH

  return result;
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e  "
// indent-tabs-mode: nil
// End:
