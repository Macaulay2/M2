// Copyright 2010 Michael E. Stillman

#include "tower.hpp"
#include "dpoly.hpp"
#include "ring.hpp"

Tower::~Tower()
{
}

bool Tower::initialize(int charac0, M2_ArrayString names0, const VECTOR(ring_elem) &extensions)
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
      for (int i=0; i<extensions.size(); i++)
	exts[i] = TOWER_VAL(extensions[i]);
      D = DRing::create(charac0, nvars, exts);
      delete [] exts;
    }

  zeroV = from_int(0);
  oneV = from_int(1);
  minus_oneV = from_int(-1);

  return true;
}
Tower * Tower::create(int charac, M2_ArrayString names)
{
  Tower *result = new Tower;
  VECTOR(ring_elem) extensions;
  if (!result->initialize(charac, names, extensions))
    return 0;
  return result;
}

Tower * Tower::create(const Tower *R, M2_ArrayString new_names)
{
  return 0;
}

Tower * Tower::create(const Tower *R, VECTOR(ring_elem) &extensions)
{
  Tower *result = new Tower;
  if (!result->initialize(R->charac(), R->names, extensions))
    return 0;
  return result;
}

void Tower::text_out(buffer &o) const
{
  o << "Tower[ZZ/" << charac() << "[";
  for (int i=0; i<nvars-1; i++)
    o << names->array[i] << ",";
  if (nvars > 0)
    o << names->array[nvars-1];
  o << "]]";
  D->extensions_text_out(o, names);
}

ring_elem Tower::from_int(int n) const
{
  poly f;
  D->set_from_int(f, n);
  return TOWER_RINGELEM(f);
}

ring_elem Tower::from_int(mpz_ptr n) const
{
  poly f;
  D->set_from_int(f, n);
  return TOWER_RINGELEM(f);
}

ring_elem Tower::from_rational(mpq_ptr q) const
{
  poly f;
  D->set_from_rational(f, q);
  return TOWER_RINGELEM(f);
}

ring_elem Tower::var(int v) const
{
  poly f;
  D->set_var(f, v);
  return TOWER_RINGELEM(f);
}

bool Tower::is_unit(const ring_elem f) const
{
  poly f1 = TOWER_VAL(f);
  //TODO: finish
  // Compute inverse of f, see if it is null...
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
  //TODO: finish
  // call is_equal routine
}

ring_elem Tower::copy(const ring_elem f) const
{
  return f;
  //TODO: do the copy?
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
  if (!D->invert(h, f1))
    ERROR("element not invertible");
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

ring_elem Tower::eval(const RingMap *map, const ring_elem f, int first_var) const
{
  poly f1 = TOWER_VAL(f);

  //TODO: finish
}

bool Tower::promote(const Ring *Rf, const ring_elem f, ring_elem &result) const
{
  poly f1 = TOWER_VAL(f);
  // What Rf should be allowed?

  //TODO: finish

  return false;
}

bool Tower::lift(const Ring *Rg, const ring_elem f, ring_elem &result) const
{
  poly f1 = TOWER_VAL(f);
  // What Rg should be allowed?

  //TODO: finish

  return false;

}

void Tower::syzygy(const ring_elem a, const ring_elem b,
		   ring_elem &x, ring_elem &y) const
{
  //TODO: finish
}

ring_elem Tower::gcd(const ring_elem f, const ring_elem g) const
{
  poly h;
  poly f1 = TOWER_VAL(f);
  poly g1 = TOWER_VAL(g);
  D->gcd(h, f1, g1);
  return TOWER_RINGELEM(h);
}

ring_elem Tower::gcd_extended(const ring_elem f, const ring_elem g,
			      ring_elem &u, ring_elem &v) const
{
  poly h, u1, v1;
  poly f1 = TOWER_VAL(f);
  poly g1 = TOWER_VAL(g);
  D->gcd_coefficients(h, u1, v1, f1, g1);
  u = TOWER_RINGELEM(u1);
  v = TOWER_RINGELEM(v1);
  return TOWER_RINGELEM(h);
}


//////////////////////////////////
// top level tower gcd routines //
//////////////////////////////////

#include "relem.hpp"

const RingElement *towerGCD(const RingElement *F, 
			    const RingElement *G)
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


// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
