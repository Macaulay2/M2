// (c) 1994 Michael E. Stillman

#include "interp.hpp"

#include "monoid.hpp"
#include "monorder.hpp"

void cmd_zero_monoid()
{
  gStack.insert(trivial_monoid);
}

void cmd_Monoid(object &omo, object &ostr, 
		object &odegmonoid, object &odegs, 
		object &ooptions) // opts[0] = isgroup, 
				  // opts[1] = MonomialSize, 
				  // opts[2] = isskew
{
  const mon_order *mo = omo->cast_to_mon_order()->mon_order_of();
  char *varnames = ostr->string_of();
  int len_varnames;
  if (varnames == NULL) 
    {
      varnames = "";
      len_varnames = 0;
    }
  else
    len_varnames = ostr->length_of();
  Monoid *D = odegmonoid->cast_to_Monoid();
  intarray *degs = odegs->intarray_of();
  intarray *opts = ooptions->intarray_of();
  if (opts->length() != 3)
    {
      *gError << "Monoid: expected three options";
      return;
    }
  bool is_group = (*opts)[0] != 0;
  int nbits = (*opts)[1];
  bool is_skew = (*opts)[2] != 0;

  // Time to check the consistency of all of these options
  if (nbits <= 0 || nbits > 16)
    {
      *gError << "MonomialSize must be in the range 1..16 bits";
      return;
    }
  int n = degs->length();
  if (n != mo->n_vars() * D->n_vars())
    {
      *gError << "Degree list should be of length " << mo->n_vars()*D->n_vars();
      return;
    }
  monoid_info *moninf = new monoid_info(mo, varnames, len_varnames, 
					D, *degs, is_group, is_skew);
  Monoid *M = new Monoid(moninf, nbits);
  bump_up(M);
  gStack.insert(M);
}

int check_all_positive(const intarray &degs)
{
  for (int i=0; i<degs.length(); i++)
    if (degs[i] <= 0)
      {
	*gError << "all primary(first) degrees must be strictly positive";
	return 0;
      }
  return 1;
}
void cmd_mo_grevlex(object &oa)
{
  intarray *a = oa->intarray_of();
  if (!check_all_positive(*a)) return;
  const mon_order *mo = mon_order::grlex(*a);
  if (mo == NULL)
    *gError << "invalid arguments for constructing monomial order";
  else
    gStack.insert(new object_mon_order(mo));
}
void cmd_mo_revlex(object &oa)
{
  intarray *a = oa->intarray_of();
  if (!check_all_positive(*a)) return;
  const mon_order *mo = mon_order::rlex(*a);
  if (mo == NULL)
    *gError << "invalid arguments for constructing monomial order";
  else
    gStack.insert(new object_mon_order(mo));
}
void cmd_mo_glex(object &oa)
{
  intarray *a = oa->intarray_of();
  if (!check_all_positive(*a)) return;
  const mon_order *mo = mon_order::glex(*a);
  if (mo == NULL)
    *gError << "invalid arguments for constructing monomial order";
  else
    gStack.insert(new object_mon_order(mo));
}
void cmd_mo_lex(object &oa)
{
  intarray *a = oa->intarray_of();
  if (!check_all_positive(*a)) return;
  const mon_order *mo = mon_order::lex(*a);
  if (mo == NULL)
    *gError << "invalid arguments for constructing monomial order";
  else
    gStack.insert(new object_mon_order(mo));
}
void cmd_mo_elim(object &oa, object &on)
{
  intarray *a = oa->intarray_of();
  if (!check_all_positive(*a)) return;
  int n = on->int_of();
  const mon_order *mo = mon_order::elim(*a, n);
  if (mo == NULL)
    *gError << "invalid arguments for constructing monomial order";
  else
    gStack.insert(new object_mon_order(mo));
}
void cmd_mo_product1(object &oa, object &on)
{
  intarray *a = oa->intarray_of();
  intarray *n = on->intarray_of();
  if (!check_all_positive(*a)) return;
  if (!check_all_positive(*n)) return;
  const mon_order *mo = mon_order::product(*a, *n);
  if (mo == NULL)
    *gError << "invalid arguments for constructing monomial order";
  else
    gStack.insert(new object_mon_order(mo));
}

void cmd_mo_product(object &om1, object &om2)
{
  const mon_order *m1 = om1->cast_to_mon_order()->mon_order_of();
  const mon_order *m2 = om2->cast_to_mon_order()->mon_order_of();
  if (m1 == NULL || m2 == NULL)
    {
      *gError << "monorder product: invalid monomial order";
      return;
    }
  const mon_order *mo = mon_order::product(m1,m2);
  if (mo == NULL)
    *gError << "invalid arguments for constructing monomial order";
  else
    gStack.insert(new object_mon_order(mo));
}
void cmd_mo_general(object &odegs, object &oorder, object &oinv, object &oinvdegs)
{
  intarray *degs = odegs->intarray_of();
  intarray *order = oorder->intarray_of();
  intarray *inv = oinv->intarray_of();
  intarray *invdegs = oinvdegs->intarray_of();
  if (!check_all_positive(*invdegs)) return;
  const mon_order *mo = mon_order::general_order(*degs, *order, *inv, *invdegs);
  if (mo == NULL)
    *gError << "invalid arguments for constructing monomial order";
  else
    gStack.insert(new object_mon_order(mo));
}

void i_monoid_cmds(void)
{
  trivial_monoid = new Monoid(new monoid_info, sizeof(int)*8);
  bump_up((Monoid *)trivial_monoid);

  // Construction of monomial orders
  install(ggMOgrevlex, cmd_mo_grevlex, TY_INTARRAY);
  install(ggMOrevlex, cmd_mo_revlex, TY_INTARRAY);
  install(ggMOglex, cmd_mo_glex, TY_INTARRAY);
  install(ggMOlex, cmd_mo_lex, TY_INTARRAY);
  install(ggMOelim, cmd_mo_elim, TY_INTARRAY, TY_INT);
  install(ggMOproduct, cmd_mo_product1, TY_INTARRAY, TY_INTARRAY);
  install(ggMOproduct, cmd_mo_product, TY_MON_ORDER, TY_MON_ORDER);
  install(ggMOgeneral, cmd_mo_general, 
	  TY_INTARRAY, TY_INTARRAY, TY_INTARRAY, TY_INTARRAY);
#if 0
  install(ggMOelim, cmd_mo_elim, TY_MON_ORDER, TY_MON_ORDER);
  install(ggMOgproduct, cmd_mo_gproduct, TY_MON_ORDER, TY_MON_ORDER);
#endif

  // Construction of new monoid objects
  install(ggzeromonoid, cmd_zero_monoid);
  install(ggmonoid, cmd_Monoid,
	  TY_MON_ORDER, TY_STRING, TY_MONOID, TY_INTARRAY, TY_INTARRAY);

}
