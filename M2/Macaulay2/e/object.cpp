// (c) 1994 Michael E. Stillman

#include "object.hpp"
#include "monomial.hpp"
#include "respoly.hpp"
#include "res.hpp"
#include "relem.hpp"
#include "vector.hpp"
#include "freemod.hpp"
#include "matrix.hpp"
#include "monideal.hpp"
#include "termideal.hpp"
#include "ringmap.hpp"
#include "hermite.hpp"
#include "gauss.hpp"
#include "hilb.hpp"
#include "respoly.hpp"
#include "gb.hpp"
#include "gbinhom.hpp"
#include "gbbinom.hpp"
#include "gbZZ.hpp"
#include "sagbi.hpp"
#include "gb2.hpp"
#include "respoly2.hpp"
#include "res2.hpp"

caster_oil caster;

void object_element::debug_out(buffer &o) const
{
  o << type_name() 
    << '(' << refcount  << ')';
}

bool object_element::equals(const object_element * /* o */ ) const
{
  ERROR("internal error: object_element::equals called");
  return false;
}

object_element *object_element::intern(object_element *obj)
{
  // MESXX: this will soon insert 'obj' uniquely into a hash table,
  // and return the resulting object.
  return obj;
}

Monomial    object_element::cast_to_Monomial()  { return Monomial(0,caster); }
MonomialIdeal   object_element::cast_to_MonomialIdeal()  { return MonomialIdeal(0,caster); }
RingElement object_element::cast_to_RingElement() { return RingElement(0,caster); }
Vector     object_element::cast_to_Vector()     { return Vector(0,caster); }
Matrix     object_element::cast_to_Matrix()     { return Matrix(0,caster); }

object      object_element::index_of(int) { return (object_element *)0; }

#include "obj_int.hpp"
#include "obj_iarr.hpp"
#include "obj_str.hpp"
#include "handles.hpp"

stash *object_int::mystash;
stash *object_intarray::mystash;
stash *object_string::mystash;

void i_stashes()
{
  doubles                  = new doubling_stash;
  object_int::mystash      = new stash("int", sizeof(object_int));
  intarray::mystash        = new stash("intarray", sizeof(intarray));
  object_intarray::mystash = new stash("obj intarray", sizeof(object_intarray));
  object_string::mystash   = new stash("string", sizeof(object_string));
  handle::mystash          = new stash("handle", sizeof(handle));
  int_bag::mystash         = new stash("int_bag", sizeof(int_bag));
  MonomialIdeal_rec::mystash   = new stash("MonomialIdeal", sizeof(MonomialIdeal_rec));
  Nmi_node::mystash        = new stash("Nmi_node", sizeof(Nmi_node));
  monomial_rec::mystash    = new stash("monomial", sizeof(monomial_rec));
  RingElement_rec::mystash = new stash("Ringelem", sizeof(RingElement_rec));
  Vector_rec::mystash     = new stash("Vector", sizeof(Vector_rec));
  FreeModule::mystash     = new stash("FreeModule", sizeof(FreeModule));
  Matrix_rec::mystash     = new stash("Matrix", sizeof(Matrix_rec));
  RingMap::mystash        = new stash("Ringmap", sizeof(RingMap));

  TermIdeal::mystash       = new stash("TermIdeal", sizeof(TermIdeal));
  mon_term::mystash        = new stash("monterm", sizeof(mon_term));

  res_degree::mystash      = new stash("resDegree", sizeof(res_degree));
  res_level::mystash       = new stash("resLevel", sizeof(res_level));
  res_comp::mystash        = new stash("res comp", sizeof(res_comp));
  res_pair::mystash        = new stash("respair", sizeof(res_pair));

  hilb_comp::mystash       = new stash("hilb_comp", sizeof(hilb_comp));

  hm_elem::mystash         = new stash("hm_elem", sizeof(hm_elem));
  HermiteComputation::mystash   = new stash("hermite", sizeof(HermiteComputation));

  gm_elem::mystash         = new stash("gm_elem", sizeof(gm_elem));
  GaussElimComputation::mystash   = new stash("gauss", sizeof(GaussElimComputation));

  monideal_pair::mystash   = new stash("monideal_pair", sizeof(monideal_pair));
  s_pair::mystash          = new stash("spair", sizeof(s_pair));
  gb_elem::mystash         = new stash("gbelem", sizeof(gb_elem));
  GB_comp::mystash         = new stash("GB", sizeof(GB_comp));
  GBZZ_comp::mystash       = new stash("GBZZ", sizeof(GBZZ_comp));
  GBinhom_comp::mystash    = new stash("GBinhom", sizeof(GBinhom_comp));

  gbres_comp::mystash       = new stash("gbres_comp",sizeof(gbres_comp));

  res2_comp::mystash       = new stash("res2_comp",sizeof(res2_comp));
  res2_pair::mystash        = new stash("respair2", sizeof(res2_pair));
  auto_reduce_node::mystash = new stash("autoreduce", sizeof(auto_reduce_node));

  S_pair::mystash          = new stash("Spair", sizeof(S_pair));
  gen_pair::mystash        = new stash("genpair", sizeof(gen_pair));
  GB_elem::mystash         = new stash("GBelem", sizeof(GB_elem));
  s_pair_bunch::mystash    = new stash("spair_bunch", sizeof(s_pair_bunch));
  s_pair_set::mystash      = new stash("spair_set", sizeof(s_pair_set));

  binomialGB_comp::mystash = new stash("binomialGB", sizeof(binomialGB_comp));
  sagbi_comp::mystash = new stash("sagbi", sizeof(sagbi_comp));
}


