// copyright Daniel R. Grayson, 1995

#include "exceptions.hpp"
#include <M2/config.h>
#include <assert.h>
#include <iostream>
#include <cstdio>

#define Matrix FactoryMatrix
#include <factory/factory.h>  // from Messollen's libfac
#if !HAVE_FACTORY_PREM
CanonicalForm Prem(const CanonicalForm &F, const CanonicalForm &G);
#endif
#undef INT64
#undef Matrix

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wconversion"
#include <NTL/ZZ.h>
#pragma GCC diagnostic pop

#include "matrix.hpp"
#include "ZZp.hpp"
#include "ZZ.hpp"
#include "frac.hpp"
#include "poly.hpp"

#include "relem.hpp"
//#include "GF.hpp"
#include "text-io.hpp"
#include "buffer.hpp"

#include "tower.hpp"

const bool notInExtension = false;

#define REVERSE_VARIABLES \
  1  // did we have a good reason for reversing the variables before?  probably
     // so the ideal reordering of Messollen would work...

enum factoryCoeffMode {
  modeError = 0,
  modeQQ,
  modeZZ,
  modeZn,
  modeGF,
  modeUnknown
};
static enum factoryCoeffMode coeffMode(const PolynomialRing *P)
{
  const Ring *F = P->getCoefficientRing();
  // if (F->cast_to_QQ()) return modeQQ;
  if (F->is_QQ()) return modeQQ;
  if (F->cast_to_RingZZ()) return modeZZ;
  if (F->isFinitePrimeField()) return modeZn;
  if (F->isGaloisField()) return modeGF;
  ERROR("expected coefficient ring of the form ZZ/n, ZZ, QQ, or GF");
  return modeError;
}

// int debugging
// #ifndef NDEBUG
//     = true
// #endif
//     ;

static void init_seeds()
{
  NTL::SetSeed(NTL::ZZ::zero());  // NTL
  factoryseed(0);  // factory (which uses NTL, as we've compiled it)
}

CanonicalForm algebraicElement_Fac;
const RingElement *algebraicElement_M2;

struct enter_factory
{
  enum factoryCoeffMode mode;
  int oldcharac;
  int newcharac;
  int oldRatlState;
  int newRatlState;
  const Ring *Zn;
  void enter();
  void exit();

  enter_factory() : mode(modeUnknown), Zn(NULL) { enter(); }
  enter_factory(const PolynomialRing *P)
      : mode(coeffMode(P)),
        newcharac(mode == modeZn || mode == modeGF
                      ? static_cast<int>(P->characteristic())
                      : 0),
        Zn(mode == modeZn ? P->getCoefficientRing() : NULL)
  {
    enter();
  }

  ~enter_factory() { exit(); }
};

void enter_factory::enter()
{
  oldcharac = getCharacteristic();
  oldRatlState = isOn(SW_RATIONAL);
  // needed for factory 4.1.1; already turned on by default in factory >= 4.1.2
  On(SW_USE_EZGCD_P);
  switch (mode)
    {
      case modeZZ:
        newRatlState = 0;
        Off(SW_RATIONAL);
        newcharac = 0;
        setCharacteristic(0);
        break;
      case modeGF:
      case modeZn:
        newRatlState = 0;
        Off(SW_RATIONAL);
        setCharacteristic(newcharac);
        break;
      case modeQQ:
        newRatlState = 1;
        On(SW_RATIONAL);
        newcharac = 0;
        setCharacteristic(0);
        break;
      default:
        newRatlState = oldRatlState;
        newcharac = oldcharac;
        break;
    }
  // if (debugging)
  //   {
  //     if (oldRatlState != newRatlState)
  //       printf(newRatlState ? "--setting factory rational mode on\n"
  //                           : "--setting factory rational mode off\n");
  //     if (oldcharac != newcharac)
  //       printf("--changing factory characteristic from %d to %d\n",
  //              oldcharac,
  //              newcharac);
  //   }
}

void enter_factory::exit()
{
  if (oldRatlState)
    On(SW_RATIONAL);
  else
    Off(SW_RATIONAL);
  setCharacteristic(oldcharac);
  // if (debugging)
  //   {
  //     if (oldcharac != newcharac)
  //       printf("--changing factory characteristic back from %d to %d\n",
  //              newcharac,
  //              oldcharac);
  //     if (oldRatlState != newRatlState)
  //       printf(oldRatlState ? "--setting factory rational mode back on\n"
  //                           : "--setting factory rational mode back off\n");
  //   }
}

static __mpz_struct toInteger(CanonicalForm h)
{
  //// we don't have access to int_cf.h and int_int.h from factory, so the
  /// following commented-out code won't compile; but it might have worked.
  //     struct enter_factory foo;
  //     assert(h.inZ());
  //     InternalCF *value = h.getval();
  //     MP_INT y = value->MPI();
  //     mpz_t x;
  //     mpz_init(x);
  //     mpz_set(x,y);
  //     return *x;
  static const unsigned int base = 1 << 16;
  intarray v;
  int sign;
  {
    struct enter_factory foo;
    int RationalMode = isOn(SW_RATIONAL) ? (Off(SW_RATIONAL), 1) : 0;
    if (h < 0)
      {
        sign = -1;
        h = -h;
      }
    else
      sign = 1;
    while (h != 0)
      {
        CanonicalForm k = h % (int)base;
        v.append(static_cast<int>(k.intval()));
        h = h / (int)base;
      }
    if (RationalMode) On(SW_RATIONAL);
  }
  mpz_t x;
  mpz_init(x);
  for (int i = v.length() - 1; i >= 0; i--)
    {
      mpz_mul_ui(x, x, base);  // x = x * base;
      mpz_add_ui(x, x, static_cast<unsigned>(v[i]));
    }
  if (sign == -1) mpz_neg(x, x);  // x = -x;
  return x[0];
}

static const RingElement *convertToM2(const PolynomialRing *R, CanonicalForm h)
{
  // this seems not to handle polynomials with rational coefficients at all!!
  const int n = R->n_vars();
  if (h.inCoeffDomain())
    {
      if (h.inZ())
        {
          mpz_t x = {toInteger(h)};
          ring_elem ret = R->from_int(x);
          mpz_clear(x);
          return RingElement::make_raw(R, ret);
        }
      else if (h.inQ())
        {
          struct enter_factory c;
          __mpq_struct z = {toInteger(h.num()), toInteger(h.den())};
          ring_elem val;
          bool ok = R->from_rational(&z, val);
          if (not ok)
            {
              std::cout << "internal error: unexpected failure to lift "
                           "rational number to ring"
                        << std::endl;
              val = R->from_long(0);
            }
          RingElement *ret = RingElement::make_raw(R, val);
          mpq_clear(&z);
          return ret;
        }
      else if (h.inFF())
        return RingElement::make_raw(R, R->from_long(h.intval()));
      else if (h.inExtension())
        {
          assert(algebraicElement_M2 != NULL);
          ring_elem result = R->from_long(0);
          for (int j = h.taildegree(); j <= h.degree(); j++)
            {
              const RingElement *r = convertToM2(R, h[j]);
              if (error()) return RingElement::make_raw(R, R->one());
              ring_elem r1 = r->get_value();
              ring_elem v = algebraicElement_M2->get_value();
              v = R->power(v, j);
              r1 = R->mult(r1, v);
              R->add_to(result, r1);
            }
          return RingElement::make_raw(R, result);
        }
      else
        {
          ERROR("conversion from factory over unknown type");
          return RingElement::make_raw(R, R->one());
        }
    }
  ring_elem result = R->from_long(0);
  for (int j = h.taildegree(); j <= h.degree(); j++)
    {
      const RingElement *r = convertToM2(R, h[j]);
      if (error()) return RingElement::make_raw(R, R->one());
      ring_elem r1 = r->get_value();
      int var =
#if REVERSE_VARIABLES
          (n - 1) -
#endif
          (h.level() - 1);
      ring_elem v = R->var(var);
      v = R->power(v, j);
      r1 = R->mult(r1, v);
      R->add_to(result, r1);
    }
  return RingElement::make_raw(R, result);
}

static struct enter_factory foo1;
static int base_set = 0;
static CanonicalForm base;

// debugging display routines to be called from gdb
// needs factory to be configured without option --disable-streamio
#if FACTORY_STREAMIO
void showvar(Variable &t) { std::cout << t << std::endl; }
void showcf(CanonicalForm &t) { std::cout << t << std::endl; }
void showcfl(CFList &t) { std::cout << t << std::endl; }
void showcffl(CFFList &t) { std::cout << t << std::endl; }
void showmpint(gmp_ZZ p)
{
  mpz_out_str(stdout, 10, p);
  std::cout << std::endl;
}
void showmpz(mpz_srcptr p)
{
  mpz_out_str(stdout, 10, p);
  std::cout << std::endl;
}
#endif

static struct enter_factory foo2;

static CanonicalForm convertToFactory(mpz_srcptr p)
{
  struct enter_factory foo;
  int size = p->_mp_size;
  int sign = size < 0 ? -1 : 1;
  if (size < 0) size = -size;
  if (!base_set)
    {
      base_set = 1;
      base = 1;
      for (int i = 0; i < mp_bits_per_limb; i++) base *= 2;
    }
  CanonicalForm m = 0;
  for (int i = size - 1; i >= 0; i--)
    {
      mp_limb_t digit = p->_mp_d[i];
      for (int j = mp_bits_per_limb; j > 0;)
        {
          int k = j - 16;
          if (k < 0) k = 0;
          int n = j - k;
          int subbase = 1 << n;
          m = subbase * m + (static_cast<int>(digit >> k) & (subbase - 1));
          j = k;
        }
    }
  m = m * sign;
  return m;
}

static CanonicalForm convertToFactory(const RingElement &g, bool inExtension);

////////////////////////////////////////////////////////////////////////
// Code to convert GF elements to/from factory CanonicalForm elements //
////////////////////////////////////////////////////////////////////////
static Variable set_GF_minimal_poly(const PolynomialRing *P)
{
  assert(P->getCoefficientRing()->isGaloisField());
  const Ring *kk = P->getCoefficientRing();
  assert(kk != 0);
  RingElement F = RingElement(kk, kk->var(0));
  F.promote(P, algebraicElement_M2);  // sets algebraicElement_M2
  Variable a = rootOf(
      convertToFactory(*kk->getMinimalPolynomial(), notInExtension), 'a');
  algebraicElement_Fac = a;
  return a;
}
static void getGFRepresentation(const Ring *kk1,
                                const ring_elem &a,
                                std::vector<long> &result_rep)
{
  assert(kk1->isGaloisField());
  //  const GF* kk = kk1->cast_to_GF();
  //  assert(kk != 0);
  const RingElement *F = kk1->getRepresentation(a);
  //  RingElement F(kk->originalR(), kk->get_rep(a));
  F->getSmallIntegerCoefficients(result_rep);
}
static CanonicalForm convertGFToFactory(const std::vector<long> &repr)
{
  // Uses algebraicElement_Fac as the element
  CanonicalForm f = 0;
  for (int i = 0; i < repr.size(); i++)
    {
      if (repr[i] == 0) continue;
      CanonicalForm m = CanonicalForm(repr[i]);
      m *= power(algebraicElement_Fac, i);
      f += m;
    }
  return f;
}
static CanonicalForm convertGFToFactory(const ring_elem &q,
                                        const PolynomialRing *P)
// use algebraicElement_Fac for converting this galois field element
//  SO: one needs to have called set_GF_minimal_poly first!
{
  std::vector<long> poly;
  getGFRepresentation(P->getCoefficientRing(), q, poly);
  return convertGFToFactory(poly);
}
///////////////////////////////////////////////////////////////////////

#if 0
static CanonicalForm convertToFactory(const ring_elem &q, const GF *k) { // use algebraicElement_Fac for converting this galois field element
  const PolynomialRing *A = k->originalR();
  RingElement *g = RingElement::make_raw(A,k->get_rep(q));
  intarray vp;
  const Monoid *M = A->getMonoid();
  const Ring *Zn = k->originalR()->getCoefficientRing();
  CanonicalForm f = 0;
  for (Nterm *t = g->get_value(); t != NULL; t = t->next) {
    vp.shrink(0);
    M->to_varpower(t->monom,vp);

    std::pair<bool,long> res = Zn->coerceToLongInteger(t->coeff);
    assert(res.first);
    int coef = static_cast<int>(res.second);

    CanonicalForm m = CanonicalForm(coef);
    for (index_varpower l = vp.raw(); l.valid(); ++l)
      m *= power( algebraicElement_Fac, l.exponent() );
    f += m;
  }
  return f;
}
#endif

static CanonicalForm convertToFactory(const RingElement &g, bool inExtension)
{
  const Ring *R = g.get_ring();
  const PolynomialRing *P = R->cast_to_PolynomialRing();
  if (P == 0)
    {
      ERROR("expected a polynomial ring");
      return 0;
    }
  const int n = P->n_vars();
  const Monoid *M = P->getMonoid();
  intarray vp;
  struct enter_factory foo(P);
  if (foo.mode == modeError) return 0;
  CanonicalForm f = 0;
  for (Nterm *t = g.get_value(); t != NULL; t = t->next)
    {
      int coef = 0;
      vp.shrink(0);
      M->to_varpower(t->monom, vp);
      if (foo.mode == modeZn)
        {
          std::pair<bool, long> res = foo.Zn->coerceToLongInteger(t->coeff);
          assert(res.first);
          coef = static_cast<int>(res.second);
        }
      CanonicalForm m =
          (foo.mode == modeZn
               ? CanonicalForm(coef)
               : foo.mode == modeGF
                     ? convertGFToFactory(t->coeff, P)
                     : foo.mode == modeZZ
                           ? convertToFactory(t->coeff.get_mpz())
                           : foo.mode == modeQQ
                                 ? (convertToFactory(
                                        mpq_numref(MPQ_VAL(t->coeff))) /
                                    convertToFactory(
                                        mpq_denref(MPQ_VAL(t->coeff))))
                                 : CanonicalForm(0)  // shouldn't happen
           );
      for (index_varpower l = vp.raw(); l.valid(); ++l)
        {
          int index = 1 +
#if REVERSE_VARIABLES
                      (n - 1) -
#endif
                      l.var();
          m *= power(index == 1 && inExtension ? algebraicElement_Fac
                                               : Variable(index),
                     l.exponent());
        }
      f += m;
    }
  return f;
}

void displayCF(const PolynomialRing *R, const CanonicalForm &h)  // for debugging
{
  buffer o;
  const RingElement *g = convertToM2(R, h);
  o << IM2_RingElement_to_string(g) << "\n";
  emit(o.str());
}

bool factoryGoodRing(const PolynomialRing *P)
{
  struct enter_factory foo(P);
  return foo.mode != modeError;
}

const RingElement /* or null */ *rawGCDRingElement(const RingElement *f,
                                                   const RingElement *g,
                                                   const RingElement *mipo,
                                                   const M2_bool inExtension)
{
  const RingElement *ret = NULL;
  const PolynomialRing *P = f->get_ring()->cast_to_PolynomialRing();
  const PolynomialRing *P2 = g->get_ring()->cast_to_PolynomialRing();
  if (P == 0)
    {
      if (f->get_ring()->cast_to_Tower() != 0) return towerGCD(f, g);
      // else we really do have an error:
      ERROR("expected polynomial ring");
      return 0;
    }
  if (P != P2)
    {
      ERROR("encountered different rings");
      return 0;
    }
  {
    struct enter_factory foo(P);
    if (foo.mode == modeError)
      {
        algebraicElement_M2 = NULL;
        return 0;
      }
    if (foo.mode == modeGF)
      {
        assert(!inExtension);
        set_GF_minimal_poly(P);
      }
    if (inExtension)
      {
        CanonicalForm minp = convertToFactory(*mipo, false);
        algebraicElement_Fac = rootOf(minp, 'a');
      }
    CanonicalForm p = convertToFactory(*f, inExtension);
    CanonicalForm q = convertToFactory(*g, inExtension);
    CanonicalForm h = gcd(p, q);
    if (inExtension)
      {
        assert(foo.mode != modeGF);
        algebraicElement_M2 = RingElement::make_raw(
            P, P->var(P->n_vars() - 1));  // the algebraic generator is always
                                          // the last variable in M2, the first
                                          // one in factory
      }
    ret = convertToM2(P, h);
    if (error())
      {
        algebraicElement_M2 = NULL;
        return NULL;
      }
  }
  ring_elem a = P->getNumeratorRing()->preferred_associate_divisor(
      ret->get_value());  // an element in the coeff ring
  ring_elem b = P->getCoefficients()->invert(a);
  ring_elem r = ret->get_value();
  P->mult_coeff_to(b, r);
  algebraicElement_M2 = NULL;
  return RingElement::make_raw(P, r);
}

const RingElement /* or null */ *rawExtendedGCDRingElement(
    const RingElement *f,
    const RingElement *g,
    const RingElement **A,
    const RingElement **B)
{
  const bool inExtension = false;
  const RingElement *ret;
  const PolynomialRing *P = f->get_ring()->cast_to_PolynomialRing();
  const PolynomialRing *P2 = g->get_ring()->cast_to_PolynomialRing();
  *A = NULL;
  *B = NULL;
  if (P == 0)
    {
      if (f->get_ring()->cast_to_Tower() != 0)
        return towerExtendedGCD(f, g, A, B);
      // else we really do have an error:
      ERROR("expected polynomial ring");
      return 0;
    }
  if (P != P2)
    {
      ERROR("encountered different rings");
      return 0;
    }

  if (f->is_zero())
    {
      *A = RingElement::make_raw(P, P->zero());
      *B = RingElement::make_raw(P, P->one());
      return g;
    }

  if (g->is_zero())
    {
      *A = RingElement::make_raw(P, P->one());
      *B = RingElement::make_raw(P, P->zero());
      return f;
    }

  struct enter_factory foo(P);
  if (foo.mode == modeError) return 0;
  if (foo.mode == modeGF)
    {
      set_GF_minimal_poly(P);
    }
  CanonicalForm p = convertToFactory(*f, inExtension);
  CanonicalForm q = convertToFactory(*g, inExtension);
  CanonicalForm a, b;
  CanonicalForm h = extgcd(p, q, a, b);
  ret = convertToM2(P, h);
  if (error())
    {
      algebraicElement_M2 = NULL;
      return NULL;
    }
  *A = convertToM2(P, a);
  if (error())
    {
      algebraicElement_M2 = NULL;
      return NULL;
    }
  *B = convertToM2(P, b);
  if (error())
    {
      algebraicElement_M2 = NULL;
      return NULL;
    }
  algebraicElement_M2 = NULL;
  return ret;
}

const RingElement /* or null */ *rawPseudoRemainder(const RingElement *f,
                                                    const RingElement *g)
{
  const bool inExtension = false;
  const PolynomialRing *P = f->get_ring()->cast_to_PolynomialRing();
  const PolynomialRing *P2 = g->get_ring()->cast_to_PolynomialRing();
  if (P == 0)
    {
      ERROR("expected polynomial ring");
      return 0;
    }
  if (P != P2)
    {
      ERROR("encountered different rings");
      return 0;
    }

  struct enter_factory foo(P);
  if (foo.mode == modeError) return 0;
  if (foo.mode == modeGF)
    {
      set_GF_minimal_poly(P);
    }
  CanonicalForm p = convertToFactory(*f, inExtension);
  CanonicalForm q = convertToFactory(*g, inExtension);
  CanonicalForm h = Prem(p, q);
  const RingElement *r = convertToM2(P, h);
  if (error())
    {
      algebraicElement_M2 = NULL;
      return NULL;
    }
  algebraicElement_M2 = NULL;
  return r;
}

void rawFactorBase(const RingElement *g,
                   engine_RawRingElementArrayOrNull *result_factors,
                   M2_arrayintOrNull *result_powers,
                   const RingElement *mipo = NULL  // minimal polynomial of
                                                   // generator of field
                                                   // extension, if any;
                                                   // generator is last variable
                   )
{
  bool inExtension = mipo != NULL;
  try
    {
      const PolynomialRing *P = g->get_ring()->cast_to_PolynomialRing();
      *result_factors = 0;
      *result_powers = 0;
      if (P == 0)
        {
          ERROR("expected polynomial ring");
          return;
        }
      struct enter_factory foo(P);
      if (foo.mode == modeError) return;

      CFFList q;
      init_seeds();

      if (foo.mode == modeGF)
        {
          inExtension = true;
          Variable a = set_GF_minimal_poly(P);
          CanonicalForm h = convertToFactory(*g, notInExtension);
          q = factorize(h, a);
        }
      else if (mipo != NULL)
        {
          CanonicalForm mipocf = convertToFactory(*mipo, notInExtension);
          Variable a = rootOf(mipocf, 'a');
          algebraicElement_Fac = a;
          CanonicalForm h = convertToFactory(*g, inExtension);
          // displayCF(P,h);
          q = factorize(h, a);
          algebraicElement_M2 = RingElement::make_raw(
              P, P->var(P->n_vars() - 1));  // the algebraic generator is always
                                            // the last variable in M2, the
                                            // first one in factory
        }
      else
        {
          CanonicalForm h = convertToFactory(*g, inExtension);
          // displayCF(P,h);
          q = factorize(h);
          algebraicElement_M2 = NULL;
        }

      int nfactors = q.length();

      *result_factors = getmemarraytype(engine_RawRingElementArray, nfactors);
      (*result_factors)->len = nfactors;

      *result_powers = M2_makearrayint(nfactors);

      int next = 0;
      for (CFFListIterator i = q; i.hasItem(); i++)
        {
          (*result_factors)->array[next] = convertToM2(P, i.getItem().factor());
          (*result_powers)->array[next++] = i.getItem().exp();
        }
      algebraicElement_M2 = NULL;
      if (error()) *result_factors = NULL, *result_powers = NULL;
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return;
  }
}

void rawFactor(const RingElement *g,
               engine_RawRingElementArrayOrNull *result_factors,
               M2_arrayintOrNull *result_powers)
{
  rawFactorBase(g, result_factors, result_powers);
}

void rawFactor2(const RingElement *g,
                const RingElement *minpoly,
                engine_RawRingElementArrayOrNull *result_factors,
                M2_arrayintOrNull *result_powers)
{
  rawFactorBase(g, result_factors, result_powers, minpoly);
}

M2_arrayintOrNull rawIdealReorder(const Matrix *M)
{
  const bool inExtension = false;
  try
    {
      init_seeds();
      const PolynomialRing *P = M->get_ring()->cast_to_PolynomialRing();
      if (P == 0)
        {
          ERROR("expected polynomial ring");
          return 0;
        }
      const int N = P->n_vars();

      struct enter_factory foo(P);
      if (foo.mode == modeError) return NULL;
      if (foo.mode == modeGF)
        {
          ERROR("not implemented yet");
          return NULL;
        }

      CFList I;
      int i;
      for (i = 0; i < M->n_rows(); i++)
        {
          for (int j = 0; j < M->n_cols(); j++)
            {
              const RingElement *g;
              {
                g = RingElement::make_raw(P, M->elem(i, j));
              }
              I.append(convertToFactory(*g, inExtension));
            }
        }

      List<int> t = neworderint(I);

      int n = t.length();
      intarray u(N);
      ListIterator<int> ii(t);
      for (i = 0; ii.hasItem(); ii++, i++)
        u.append((n - 1) - (ii.getItem() - 1)  // REVERSE!
                 );
      if (n > 0)
        for (i = (n - 1) / 2; i >= 0; i--)
          {  // REVERSE!
            int tmp = u[n - 1 - i];
            u[n - 1 - i] = u[i];
            u[i] = tmp;
          }
      for (i = n; i < N; i++) u.append(i);

      M2_arrayint result = M2_makearrayint(N);
      for (i = 0; i < N; i++) result->array[i] = u[i];
      return result;
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return NULL;
  }
}

engine_RawMatrixArrayOrNull rawCharSeries(const Matrix *M)
{
  const bool inExtension = false;
  try
    {
      const PolynomialRing *P = M->get_ring()->cast_to_PolynomialRing();
      if (P == 0)
        {
          ERROR("expected polynomial ring");
          return 0;
        }

      init_seeds();

      struct enter_factory foo(P);
      if (foo.mode == modeError) return NULL;
      if (foo.mode == modeGF)
        {
          ERROR("not implemented yet");
          return NULL;
        }

      CFList I;
      for (int i = 0; i < M->n_rows(); i++)
        {
          for (int j = 0; j < M->n_cols(); j++)
            {
              const RingElement *g;
              {
                g = RingElement::make_raw(P, M->elem(i, j));
              }
              I.append(convertToFactory(*g, inExtension));
            }
        }

      List<CFList> t = irrCharSeries(I);

      engine_RawMatrixArray result =
          getmemarraytype(engine_RawMatrixArray, t.length());
      result->len = t.length();

      int next = 0;
      for (ListIterator<List<CanonicalForm> > ii = t; ii.hasItem(); ii++)
        {
          CFList u = ii.getItem();
          engine_RawRingElementArray result1 =
              getmemarraytype(engine_RawRingElementArray, u.length());
          result1->len = u.length();
          int next1 = 0;
          for (ListIterator<CanonicalForm> j = u; j.hasItem(); j++)
            {
              result1->array[next1++] = convertToM2(P, j.getItem());
              if (error()) return NULL;
            }
          result->array[next++] =
              IM2_Matrix_make1(M->rows(), u.length(), result1, false);
        }

      return result;
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return NULL;
  }
}

CFList convertToCFList(const Matrix &M, bool inExtension)
{
  CFList I;
  for (int i = 0; i < M.n_rows(); i++)
    {
      for (int j = 0; j < M.n_cols(); j++)
        {
          const RingElement *g;
          {
            g = RingElement::make_raw(M.get_ring(), M.elem(i, j));
          }
          I.append(convertToFactory(*g, inExtension));
        }
    }
  return I;
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e x-factor.o "
// indent-tabs-mode: nil
// End:
