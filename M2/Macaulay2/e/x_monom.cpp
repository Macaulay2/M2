// (c) 1994 Michael E. Stillman

#include "stack.hpp"

#include "obj_int.hpp"
#include "obj_str.hpp"
#include "obj_iarr.hpp"

#include "interp.hpp"

#include "monomial.hpp"

#if 0
void cmd_monoid_var(object &oF, object &ov, object &oe)
{
  const Monoid *F = oF->cast_to_Monoid();
  int v = ov->int_of();
  int e = oe->int_of();
  gStack.insert(Monomial(F,v,e));
}

void cmd_monoid_monomial(object &oM)
{
  const Monoid *M = oM->cast_to_Monoid();
  gStack.insert(Monomial(M, gInput, gInputLen));
}
#endif

void cmd_monoid_var(object &ov, object &oe)
{
  int v = ov->int_of();
  int e = oe->int_of();
  gStack.insert(Monomial(v,e));
}

void cmd_monoid_monomial()
{
  gStack.insert(Monomial(gInput, gInputLen));
}

void cmd_monoid_isequal(object &oa, object &ob)
{
  Monomial a = oa->cast_to_Monomial();
  Monomial b = ob->cast_to_Monomial();
  gStack.insert(make_object_int(a.is_equal(b)));
}

void cmd_monoid_isone(object &oa)
{
  Monomial a = oa->cast_to_Monomial();
  gStack.insert(make_object_int(a.is_one()));
}

void cmd_monoid_compare(object &oa, object &ob)
{
  Monomial a = oa->cast_to_Monomial();
  Monomial b = ob->cast_to_Monomial();
  gStack.insert(make_object_int(a.compare(b)));
}
void cmd_monoid_divides(object &oa, object &ob)
{
  Monomial a = oa->cast_to_Monomial();
  Monomial b = ob->cast_to_Monomial();
  gStack.insert(make_object_int(a.divides(b)));
}
void cmd_monoid_degree(object &oa)
{
  Monomial a = oa->cast_to_Monomial();
  gStack.insert(make_object_int(a.simple_degree()));
}
void cmd_monoid_mult(object &oa, object &ob)
{
  Monomial a = oa->cast_to_Monomial();
  Monomial b = ob->cast_to_Monomial();
  gStack.insert(a*b);
}
void cmd_monoid_div(object &oa, object &ob)
{
  Monomial a = oa->cast_to_Monomial();
  Monomial b = ob->cast_to_Monomial();
  gStack.insert(a/b);
}
void cmd_monoid_monsyz(object &oa, object &ob)
{
  Monomial a = oa->cast_to_Monomial();
  Monomial b = ob->cast_to_Monomial();
  Monomial c(0);
  Monomial d(0);
  a.monsyz(b,c,d);
  gStack.insert(c);
  gStack.insert(d);
}
void cmd_monoid_lcm(object &oa, object &ob)
{
  Monomial a = oa->cast_to_Monomial();
  Monomial b = ob->cast_to_Monomial();
  gStack.insert(a.lcm(b));
}
void cmd_monoid_gcd(object &oa, object &ob)
{
  Monomial a = oa->cast_to_Monomial();
  Monomial b = ob->cast_to_Monomial();
  gStack.insert(a.gcd(b));
}
void cmd_monoid_power(object &oa, object &on)
{
  Monomial a = oa->cast_to_Monomial();
  int n = on->int_of();
  gStack.insert(a.power(n));
}
void cmd_monoid_sat(object &oa, object &ob)
{
  Monomial a = oa->cast_to_Monomial();
  Monomial b = ob->cast_to_Monomial();
  gStack.insert(a.erase(b));
}
void cmd_monoid_radical(object &oa)
{
  Monomial a = oa->cast_to_Monomial();
  gStack.insert(a.radical());
}

void i_monomial_cmds(void)
{
  // Informational
  install(ggisequal, cmd_monoid_isequal, TY_MONOMIAL, TY_MONOMIAL);
  install(ggiszero, cmd_monoid_isone, TY_MONOMIAL);
  install(ggdivides, cmd_monoid_divides, TY_MONOMIAL, TY_MONOMIAL);
  install(ggcompare, cmd_monoid_compare, TY_MONOMIAL, TY_MONOMIAL);
  install(ggdegree, cmd_monoid_degree, TY_MONOMIAL);

  // Monomial commands
//  install(ggvar, cmd_monoid_var, TY_MONOID, TY_INT, TY_INT);
//  install(ggMONOMIAL, cmd_monoid_monomial, TY_MONOID);

  install(ggvar, cmd_monoid_var, TY_INT, TY_INT);
  install(ggMONOMIAL, cmd_monoid_monomial);

  install(ggmult, cmd_monoid_mult, TY_MONOMIAL, TY_MONOMIAL);
  install(ggdiv, cmd_monoid_div, TY_MONOMIAL, TY_MONOMIAL);
  install(ggpower, cmd_monoid_power, TY_MONOMIAL, TY_INT);
  install(gglcm, cmd_monoid_lcm, TY_MONOMIAL, TY_MONOMIAL);
  install(gggcd, cmd_monoid_gcd, TY_MONOMIAL, TY_MONOMIAL);
  install(ggmonsyz, cmd_monoid_monsyz, TY_MONOMIAL, TY_MONOMIAL);

  install(ggsat, cmd_monoid_sat, TY_MONOMIAL, TY_MONOMIAL);
  install(ggradical, cmd_monoid_radical, TY_MONOMIAL);
}
