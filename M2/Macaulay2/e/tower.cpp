// Copyright 2010 Michael E. Stillman

#include "tower.hpp"

#include "dpoly.hpp"
#include "ring.hpp"
#include "varpower.hpp"
#include "ringmap.hpp"
#include "polyring.hpp"
#include "monoid.hpp"

Tower::~Tower() {}
bool Tower::initialize(long charac0,
                       M2_ArrayString names0,
                       const VECTOR(ring_elem) & extensions)
{
  initialize_ring(charac0);
  declare_field();

  names = names0;
  nvars = names->len;
  level = nvars - 1;

  // Translate extensions to poly's
  if (extensions.size() == 0)
    {
      D = DRing::create(charac0, nvars, 0);
    }
  else
    {
      const_poly *exts = new const_poly[extensions.size()];
      for (int i = 0; i < extensions.size(); i++)
        exts[i] = TOWER_VAL(extensions[i]);
      D = DRing::create(charac0, nvars, exts);
      delete[] exts;
    }

  zeroV = from_long(0);
  oneV = from_long(1);
  minus_oneV = from_long(-1);

  return true;
}
Tower *Tower::create(int charac, M2_ArrayString names)
{
  Tower *result = new Tower;
  VECTOR(ring_elem) extensions;
  if (!result->initialize(charac, names, extensions)) return 0;
  return result;
}

Tower *Tower::create(const Tower *R, M2_ArrayString new_names)
{
  // TODO: write
  return 0;
}

Tower *Tower::create(const Tower *R, VECTOR(ring_elem) & extensions)
{
  Tower *result = new Tower;
  if (!result->initialize(R->characteristic(), R->names, extensions)) return 0;
  return result;
}

unsigned int Tower::computeHashValue(const ring_elem a) const
{
  // TODO HASH
  return 3212415;
}

void Tower::text_out(buffer &o) const
{
  o << "Tower[ZZ/" << characteristic() << "[";
  for (int i = 0; i < nvars - 1; i++) o << names->array[i] << ",";
  if (nvars > 0) o << names->array[nvars - 1];
  o << "]]";
  D->extensions_text_out(o, names);
}

int Tower::index_of_var(const ring_elem a) const
{
  poly f1 = TOWER_VAL(a);
  return D->index_of_var(f1);
}

M2_arrayint Tower::support(const ring_elem a) const
{
  poly f1 = TOWER_VAL(a);
  std::vector<int> max_degs;
  D->degrees_of_vars(f1, max_degs);
  int nelems = 0;
  for (size_t i = 0; i < max_degs.size(); i++)
    if (max_degs[i] > 0) nelems++;
  M2_arrayint result = M2_makearrayint(nelems);
  int next = 0;
  for (size_t i = 0; i < max_degs.size(); i++)
    if (max_degs[i] > 0) result->array[next++] = static_cast<int>(i);
  return result;
}

ring_elem Tower::from_long(long n) const
{
  poly f;
  D->set_from_long(f, n);
  return TOWER_RINGELEM(f);
}

ring_elem Tower::from_int(mpz_srcptr n) const
{
  poly f;
  D->set_from_int(f, n);
  return TOWER_RINGELEM(f);
}

bool Tower::from_rational(mpq_srcptr q, ring_elem &result) const
{
  poly f;
  if (not D->set_from_mpq(f, q)) return false;
  result = TOWER_RINGELEM(f);
  return true;
}

ring_elem Tower::var(int v) const
{
  poly f;
  D->set_var(f, v);
  return TOWER_RINGELEM(f);
}

bool Tower::is_unit(const ring_elem f) const
{
  // Write this.  Git issue #611.
  return false;
}

bool Tower::is_zero(const ring_elem f) const
{
  poly f1 = TOWER_VAL(f);

  return D->is_zero(f1);
}

bool Tower::is_equal(const ring_elem f, const ring_elem g) const
{
  poly f1 = TOWER_VAL(f);
  poly g1 = TOWER_VAL(g);

  return D->is_equal(f1, g1);
}

int Tower::compare_elems(const ring_elem f, const ring_elem g) const
{
  poly f1 = TOWER_VAL(f);
  poly g1 = TOWER_VAL(g);

  return D->compare(f1, g1);
}

ring_elem Tower::copy(const ring_elem f) const
{
  // Write this.  Git issue #611.
  return f;
}

void Tower::remove(ring_elem &) const
{
  // nothing needed to remove?  Or should we remove it?
}

ring_elem Tower::negate(const ring_elem g) const
{
  poly f1;
  poly g1 = TOWER_VAL(g);
  poly h;
  D->set_zero(h);
  D->set_zero(f1);
  D->subtract(h, f1, g1);
  return TOWER_RINGELEM(h);
}

ring_elem Tower::add(const ring_elem f, const ring_elem g) const
{
  poly f1 = TOWER_VAL(f);
  poly g1 = TOWER_VAL(g);
  poly h;
  D->set_zero(h);
  D->add(h, f1, g1);
  return TOWER_RINGELEM(h);
}

ring_elem Tower::subtract(const ring_elem f, const ring_elem g) const
{
  poly f1 = TOWER_VAL(f);
  poly g1 = TOWER_VAL(g);
  poly h;
  D->set_zero(h);
  D->subtract(h, f1, g1);
  return TOWER_RINGELEM(h);
}

ring_elem Tower::mult(const ring_elem f, const ring_elem g) const
{
  poly f1 = TOWER_VAL(f);
  poly g1 = TOWER_VAL(g);
  poly h;
  D->set_zero(h);
  D->mult(h, f1, g1);
  return TOWER_RINGELEM(h);
}

ring_elem Tower::invert(const ring_elem f) const
{
  poly f1 = TOWER_VAL(f);
  poly h;
  D->set_zero(h);
  if (!D->invert(h, f1)) ERROR("element not invertible");
  return TOWER_RINGELEM(h);
}

ring_elem Tower::divide(const ring_elem f, const ring_elem g) const
{
  poly f1 = TOWER_VAL(f);
  poly g1 = TOWER_VAL(g);
  poly h;
  D->set_zero(h);
  D->divide(h, f1, g1);
  return TOWER_RINGELEM(h);
}

ring_elem Tower::remainder(const ring_elem f, const ring_elem g) const
{
  poly f1 = TOWER_VAL(f);
  poly g1 = TOWER_VAL(g);
  poly h;
  D->set_zero(h);
  D->remainder(h, f1, g1);
  return TOWER_RINGELEM(h);
}

ring_elem Tower::random() const
{
  poly f;
  D->set_zero(f);
  D->set_random(f);
  return TOWER_RINGELEM(f);
}

void Tower::elem_text_out(buffer &o,
                          const ring_elem f,
                          bool p_one,
                          bool p_plus,
                          bool p_parens) const
{
  D->elem_text_out(o, TOWER_VAL(f), p_one, p_plus, p_parens, names);
}

class TowerEvaluator : public DPolyTraverser
{
  const RingMap *map;
  const Ring *K;
  SumCollector *H;

  poly f;
  int first_var;
  const Ring *target;
  intarray vp;
  int nvars;

  virtual bool viewTerm(long coeff, const exponents exp)
  {
    // translate exp to varpwer
    // map->eval_term
    // either:
    //  H->add, or target->add_to
    vp.shrink(0);
    varpower::from_ntuple(nvars, exp, vp);
    ring_elem c = K->from_long(coeff);
    ring_elem a = map->eval_term(K, c, vp.raw(), first_var, nvars);
    H->add(a);
    return true;
  }

 public:
  TowerEvaluator(const Tower *T,
                 const RingMap *map0,
                 const ring_elem f0,
                 int first_var0)
      : DPolyTraverser(T->D),
        map(map0),
        f(TOWER_VAL(f0)),
        first_var(first_var0),
        nvars(T->n_vars())
  {
    target = map->get_ring();
    H = target->make_SumCollector();
    const PolynomialRing *P = target->cast_to_PolynomialRing();
    K = (P == 0 ? target : P->getCoefficients());
  }

  virtual ~TowerEvaluator() { delete H; }
  ring_elem getValue()
  {
    traverse(f);
    return H->getValue();
  }
};

ring_elem Tower::eval(const RingMap *map,
                      const ring_elem f,
                      int first_var) const
{
  TowerEvaluator m(this, map, f, first_var);
  return m.getValue();
}

bool Tower::promote(const Ring *Rf, const ring_elem f, ring_elem &result) const
{
  // Write this.  Git issue #611.
  return false;
}

bool Tower::lift(const Ring *Rg, const ring_elem f, ring_elem &result) const
{
  // Write this.  Git issue #611.
  return false;
}

void Tower::syzygy(const ring_elem a,
                   const ring_elem b,
                   ring_elem &x,
                   ring_elem &y) const
{
  // Write this.  Git issue #611.
}

ring_elem Tower::gcd(const ring_elem f, const ring_elem g) const
{
  poly h;
  poly f1 = TOWER_VAL(f);
  poly g1 = TOWER_VAL(g);
  D->gcd(h, f1, g1);
  return TOWER_RINGELEM(h);
}

ring_elem Tower::gcd_extended(const ring_elem f,
                              const ring_elem g,
                              ring_elem &u,
                              ring_elem &v) const
{
  poly h, u1, v1;
  poly f1 = TOWER_VAL(f);
  poly g1 = TOWER_VAL(g);
  D->gcd_coefficients(h, u1, v1, f1, g1);
  u = TOWER_RINGELEM(u1);
  v = TOWER_RINGELEM(v1);
  return TOWER_RINGELEM(h);
}

int Tower::degreeInVariable(int whichvar, const ring_elem f) const
{
  poly f1 = TOWER_VAL(f);
  return D->degree(whichvar, f1);
}

ring_elem Tower::differentiate(int whichvar, const ring_elem f) const
{
  poly f1 = TOWER_VAL(f);
  poly h = 0;
  D->diff(whichvar, h, f1);
  return TOWER_RINGELEM(h);
}

int Tower::extension_degree(int firstvar) const
{
  // returns -1 if infinite
  return D->extension_degree(firstvar);
}

ring_elem Tower::power_mod(const ring_elem f,
                           mpz_srcptr n,
                           const ring_elem g) const  // f^n mod g
{
  poly f1 = TOWER_VAL(f);
  poly g1 = TOWER_VAL(g);
  poly h = 0;
  D->power_mod(h, f1, n, g1);
  return TOWER_RINGELEM(h);
}

ring_elem Tower::lowerP(const ring_elem f) const
{
  poly f1 = TOWER_VAL(f);
  poly h = 0;
  D->lowerP(h, f1);
  return TOWER_RINGELEM(h);
}

//////////////////////////////////
// top level tower gcd routines //
//////////////////////////////////

#include "relem.hpp"

const RingElement *towerGCD(const RingElement *F, const RingElement *G)
{
  const Tower *R = F->get_ring()->cast_to_Tower();
  const Tower *S = G->get_ring()->cast_to_Tower();
  if (R == 0 || R != S)
    {
      ERROR("encountered different rings");
    }

  ring_elem result = R->gcd(F->get_value(), G->get_value());
  return RingElement::make_raw(R, result);
}

const RingElement *towerExtendedGCD(const RingElement *F,
                                    const RingElement *G,
                                    const RingElement **A,
                                    const RingElement **B)
{
  const Tower *R = F->get_ring()->cast_to_Tower();
  const Tower *S = G->get_ring()->cast_to_Tower();
  if (R == 0 || R != S)
    {
      ERROR("encountered different rings");
    }

  ring_elem u, v;
  ring_elem result = R->gcd_extended(F->get_value(), G->get_value(), u, v);
  *A = RingElement::make_raw(R, u);
  *B = RingElement::make_raw(R, v);
  return RingElement::make_raw(R, result);
}

/////////////////////////////////////////////////////////
// top level translation to polynomials in other rings //
/////////////////////////////////////////////////////////

#include "polyring.hpp"

ring_elem Tower::translate(const PolynomialRing *R, ring_elem fR) const
{
  // create a poly in the Tower T, return it.

  const Monoid *M = R->getMonoid();
  const Ring *K = R->getCoefficients();
  int nvars = R->n_vars();
  poly result = 0;
  exponents exp = new int[nvars];
  for (Nterm *t = fR; t != 0; t = t->next)
    {
      M->to_expvector(t->monom, exp);

      std::pair<bool, long> res = K->coerceToLongInteger(t->coeff);
      assert(res.first);
      int c1 = static_cast<int>(res.second);

      D->add_term(result, c1, exp);
    }
  delete[] exp;
  return TOWER_RINGELEM(result);
}

#if 0
ring_elem DPoly::translateFromTower(int level, const PolynomialRing *P, poly f) const
{
  if (f == 0) return 0;
  if (level == 0)
    {
      // create a univariate poly in variable P_0
    }
  else
    {
      ring_elem b = P->var(level);
      for (int i=0; i<=f->deg; i++)
        {
          if (f->arr.polys[i] != 0)
            {
              ring_elem a = translateFromTower(level-1, P, f->arr.polys[i]);
              ring_elem c = P->power(b,i);
              ring_elem d = P->mult(c,a);
              result = P->add(result, d);
            }
        }
      return result;
    }
}

ring_elem DRing::translateFromTower(const PolynomialRing *P, poly f) const
{
  // create a polynomial in P from the tower poly fT
  poly f = TOWER_VAL(fT);
  return D.translateFromTower(level, P, f);
}

ring_elem Tower::translateFromTower(const PolynomialRing *P, ring_elem fT) const
{
  // create a polynomial in P from the tower poly fT
  poly f = TOWER_VAL(fT);
  return D->translateFromTower(P, f);
}
#endif

extern "C" // TODO: remove when this function is in e/interface
const RingElement *rawTowerTranslatePoly(const Ring *newRing,
                                         const RingElement *F)
{
  // either: F is an element in a Tower, or is an element in a PolynomialRing.
  // In both cases: the number of variables and the characteristic should
  // be the same.
  const PolynomialRing *P = F->get_ring()->cast_to_PolynomialRing();
  const Tower *T = newRing->cast_to_Tower();
  if (P != 0 && T != 0)
    {
      if (P->n_vars() != T->n_vars())
        {
          ERROR("expected rings with the same number of variables");
          return 0;
        }
      if (P->characteristic() != T->characteristic())
        {
          ERROR("expected rings with the same characteristic");
          return 0;
        }
      ring_elem a = T->translate(P, F->get_value());
      return RingElement::make_raw(T, a);
    }
#if 0
  P = newRing->cast_to_PolynomialRing();
  T = F->get_ring()->cast_to_Tower();
  if (P != 0 && T != 0)
    {
      ring_elem a = T->translateFromTower(P, F->get_value());
      return RingElement::make_raw(P, a);
    }
#endif
  ERROR("expected an element of a TowerRing or a PolynomialRing");
  return 0;
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
