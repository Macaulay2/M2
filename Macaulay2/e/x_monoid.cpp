// (c) 1994 Michael E. Stillman

// Only one of these at a time, please!
#define MIKE_OLD_MONOID
#undef MIKE_EMONOID

#ifdef MIKE_OLD_MONOID
#include "monoid.hpp"
#include "monorder.hpp"
#endif

#ifdef MIKE_EMONOID
#include "Emonorder.hpp"
#include "Emonoid.hpp"
#endif

#undef MIKE_NEWMONORDER
#ifdef MIKE_NEWMONORDER
// This version is not new, and will go away very soon.
#include "newmonorder.hpp"
#endif

#include "interp.hpp"
//Monoid *trivial_monoid;

void cmd_Monoid(object &omo, object &ostr, 
		object &odegmonoid, object &odegs, 
		object &ooptions)  // opts[0] = isgroup, opts[1] = MonomialSize
				// opts[2] = isskew:0 none. 1:all odd degree variables, 2:
				// opts[3]..opts[length-1]: the list of the skew comm vars,
				// sorted in ascending order.
{
#ifdef MIKE_OLD_MONOID
  mon_order *mo = omo->cast_to_mon_order()->grab_mon_order();
  if (mo == 0) return; // Error message already given.
#else
#ifdef MIKE_EMONOID
  const EMonomialOrder *mo = omo->cast_to_EMonomialOrder();
#endif
#endif
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

  if (opts->length() <= 2)
    {
      gError << "Monoid: expected three options";
      return;
    }
  bool is_group = (*opts)[0] != 0;
  int nbits = (*opts)[1];

  // Time to check the consistency of all of these options
  if (nbits <= 0 || nbits > 16)
    {
      gError << "MonomialSize must be in the range 1..16 bits";
      return;
    }
  int n = degs->length();
  if (n != mo->n_vars() * D->n_vars())
    {
      gError << "Degree list should be of length " << mo->n_vars()*D->n_vars();
      return;
    }

  // Check that the first degree for each variable is positive
  if (D->n_vars() != 0)
  for (int i=0; i<mo->n_vars(); i++)
    if ((*degs)[i * D->n_vars()] <= 0)
      {
	gError << "All primary (first) degrees should be positive";
	return;
      }

  // Set the skew variable information needed.
  intarray skewvars;
  int is_skew = (*opts)[2]; // hopefully 0, 1 or 2.
  if (is_skew == 1)
    {
      // Put the variables which have first degree odd into skewvars.
      if (D->n_vars() != 0)
	for (int i=0; i<mo->n_vars(); i++)
	  if (((*degs)[i * D->n_vars()] % 2) != 0)
	    skewvars.append(i);
      
    }
  else if (is_skew >= 2)
    {
      // The variables appear in 'opts'
      for (int i=3; i<opts->length(); i++)
	skewvars.append((*opts)[i]);
    }

#ifdef MIKE_OLD_MONOID
  monoid_info *moninf = new monoid_info(mo, varnames, len_varnames, 
					D, *degs, is_group, skewvars);
  Monoid *M = new Monoid(moninf, nbits);
#else
#ifdef MIKE_EMONOID
  // is_group: what should be done with that?  MES
  const char **var_names = Monoid::make_name_array(mo->n_vars(), varnames, len_varnames);
  intarray print_order;
  for (int i=0; i<mo->n_vars(); i++)
    print_order.append(i);

  Monoid *M = Monoid::create(mo, print_order.raw(), var_names, D, *degs, skewvars, is_group);

  // Remove var_names:
  for (int i=0; i<mo->n_vars(); i++)
    delete [] var_names[i];
  delete [] var_names;
#endif
#endif
  //  bump_up(M);
  gStack.insert(M);
}

#ifdef MIKE_OLD_MONOID
void cmd_zero_monoid()
{
  gStack.insert(trivial_monoid);
}

int check_all_positive(const intarray &degs)
{
  for (int i=0; i<degs.length(); i++)
    if (degs[i] <= 0)
      {
	gError << "all primary(first) degrees must be strictly positive";
	return 0;
      }
  return 1;
}
void cmd_mo_grevlex(object &ow, object &oa)
{
  intarray *a = oa->intarray_of();
  if (!check_all_positive(*a)) return;
  intarray *wts = ow->intarray_of();
  mon_order *mo = mon_order::grlex(*a,*wts);
  if (mo == NULL)
    gError << "invalid arguments for constructing monomial order";
  else
    gStack.insert(new object_mon_order(mo));
}
void cmd_mo_revlex(object &ow, object &oa)
{
  intarray *a = oa->intarray_of();
  if (!check_all_positive(*a)) return;
  intarray *wts = ow->intarray_of();
  mon_order *mo = mon_order::rlex(*a,*wts);
  if (mo == NULL)
    gError << "invalid arguments for constructing monomial order";
  else
    gStack.insert(new object_mon_order(mo));
}
void cmd_mo_glex(object &ow, object &oa)
{
  intarray *a = oa->intarray_of();
  if (!check_all_positive(*a)) return;
  intarray *wts = ow->intarray_of();
  mon_order *mo = mon_order::glex(*a,*wts);
  if (mo == NULL)
    gError << "invalid arguments for constructing monomial order";
  else
    gStack.insert(new object_mon_order(mo));
}
void cmd_mo_lex(object &ow, object &oa)
{
  intarray *a = oa->intarray_of();
  if (!check_all_positive(*a)) return;
  intarray *wts = ow->intarray_of();
  mon_order *mo = mon_order::lex(*a,*wts);
  if (mo == NULL)
    gError << "invalid arguments for constructing monomial order";
  else
    gStack.insert(new object_mon_order(mo));
}
void cmd_mo_elim(object &ow, object &oa, object &on)
{
  intarray *a = oa->intarray_of();
  if (!check_all_positive(*a)) return;
  intarray *wts = ow->intarray_of();
  int n = on->int_of();
  mon_order *mo = mon_order::elim(*a, n, *wts);
  if (mo == NULL)
    gError << "invalid arguments for constructing monomial order";
  else
    gStack.insert(new object_mon_order(mo));
}
void cmd_mo_product1(object &ow, object &oa, object &on)
{
  intarray *a = oa->intarray_of();
  intarray *n = on->intarray_of();
  if (!check_all_positive(*a)) return;
  if (!check_all_positive(*n)) return;
  intarray *wts = ow->intarray_of();
  mon_order *mo = mon_order::product(*a, *n, *wts);
  if (mo == NULL)
    gError << "invalid arguments for constructing monomial order";
  else
    gStack.insert(new object_mon_order(mo));
}
#if 0
void cmd_mo_product(object &om1, object &om2)
{
  const mon_order *m1 = om1->cast_to_mon_order()->mon_order_of();
  const mon_order *m2 = om2->cast_to_mon_order()->mon_order_of();
  if (m1 == NULL || m2 == NULL)
    {
      gError << "monorder product: invalid monomial order";
      return;
    }
  const mon_order *mo = mon_order::product(m1,m2);
  if (mo == NULL)
    gError << "invalid arguments for constructing monomial order";
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
    gError << "invalid arguments for constructing monomial order";
  else
    gStack.insert(new object_mon_order(mo));
}
#endif
#endif // MIKE_OLD_MONOID

#ifdef MIKE_NEWMONORDER
void cmd_mo_make(object &oa)
{
  intarray *a = oa->intarray_of();
  const new_mon_order *mo = new new_mon_order(a->raw());
  gStack.insert(new object_new_mon_order(mo));
}
#endif

#ifdef MIKE_EMONOID
/* TO DO: to get new monoids working:
 *   - finish monoid creation code
 *   - finish monomial operations
 *   - [ZZ^n]: needs to be redone from front end?
 *   - front end needs to call the new monoid command.  This will
 *     be automatic if whenever a "NewMonomialOrder" option is given,
 *     then that monomial order is given when constructing a monoid.
 *     ONLY problem: with [ZZ^n].
 *   - front-end needs to emulate MonomialOrder, Weights options.
 *   - what else?
 */

void cmd_zero_monoid()
{
  gStack.insert(Monoid::trivial_monoid());
}

/////////////////////////////
// Monomial Order Routines //
/////////////////////////////
static void cmd_EMO_init()
{
  EMonomialOrder *mo = EMonomialOrder::make();
  gStack.insert(mo);
}
static void cmd_EMO_clone(object &o1)
{
  EMonomialOrder *mo = o1->cast_to_EMonomialOrder();
  gStack.insert(mo->clone());
}
static void cmd_EMO_revlex(object &o1, object &o2, object &o3)
{
  int nvars = o1->int_of();
  int isgroup = o2->int_of();
  EMonomialOrder *mo = o3->cast_to_EMonomialOrder();
  mo->revlex(nvars, isgroup);
}
static void cmd_EMO_lex(object &o1, object &o2, object &o3)
{
  int nvars = o1->int_of();
  int isgroup = o2->int_of();
  EMonomialOrder *mo = o3->cast_to_EMonomialOrder();
  mo->lex(nvars,isgroup);
}
static void cmd_EMO_revlexWeights(object &o1, object &o2, object &o3)
{
  intarray *a = o1->intarray_of();
  int isgroup = o2->int_of();
  EMonomialOrder *mo = o3->cast_to_EMonomialOrder();
  mo->revlexWeights(a->length(),a->raw(),isgroup);
}

static void cmd_EMO_component(object &o1)
{
  EMonomialOrder *mo = o1->cast_to_EMonomialOrder();
  mo->component();
}
static void cmd_EMO_weightFunction(object &o1, object &o2)
{
  intarray *a = o1->intarray_of();
  EMonomialOrder *mo = o2->cast_to_EMonomialOrder();
  mo->weightFunction(a->length(),a->raw());
}
static void cmd_EMO_product(object &o1, object &o2)
{
  EMonomialOrder *mo = o1->cast_to_EMonomialOrder();
  EMonomialOrder *mo2 = o2->cast_to_EMonomialOrder();
  mo->product(mo2);
}
static void cmd_EMO_NClex(object &o1, object &o2)
{
  int nvars = o1->int_of();
  EMonomialOrder *mo = o2->cast_to_EMonomialOrder();
  mo->NClex(nvars);
}
static void cmd_EMO_test(object &o1, object &o2, object &o3)
{
  // As written, the number of variables, or encoded size, should
  // be < 100 ints long.
  EMonomialOrder *mo = o3->cast_to_EMonomialOrder();
  int action = o1->int_of();
  intarray *a = o2->intarray_of();
  intarray b;
  int i;
  
  switch (action) {
  case 1: 
    // Take the exponent vector, and encode it in an integer array
    // a should be of length #vars.  Output should be of length mo->n_slots()
    for (i=0; i<mo->n_slots(); i++)
      b.append(0);
    mo->encode_commutative(a->raw(), b.raw());
    break;
  case 2:
    // Take an encoded vector, and decode it to an exponent vector.
    for (i=0; i<mo->n_vars(); i++)
      b.append(0);
    mo->decode(a->raw(), b.raw());
    break;
  default:
    gError << "Unknown test";
    return;
  }
  object_intarray *result = new object_intarray(b);
  gStack.insert(result);
}
#endif  // MIKE_EMONOID

void cmd_monheap_test(void)
{
  extern void monheap_make_sure_works();

  //  monheap_make_sure_works();
}

void i_monoid_cmds(void)
{
  // Construction of new monoid objects
  install(ggzeromonoid, cmd_zero_monoid);

  // install(ggtest, cmd_monheap_test);

#ifdef MIKE_OLD_MONOID
  trivial_monoid = new Monoid(new monoid_info, sizeof(int)*8);
  bump_up((Monoid *)trivial_monoid);

  install(ggmonoid, cmd_Monoid,
	  TY_MON_ORDER, TY_STRING, TY_MONOID, TY_INTARRAY, TY_INTARRAY);

  // Construction of monomial orders
  install(ggMOgrevlex, cmd_mo_grevlex, TY_INTARRAY, TY_INTARRAY);
  install(ggMOrevlex, cmd_mo_revlex, TY_INTARRAY, TY_INTARRAY);
  install(ggMOglex, cmd_mo_glex, TY_INTARRAY, TY_INTARRAY);
  install(ggMOlex, cmd_mo_lex, TY_INTARRAY, TY_INTARRAY);
  install(ggMOelim, cmd_mo_elim, TY_INTARRAY, TY_INTARRAY, TY_INT);
  install(ggMOproduct, cmd_mo_product1, TY_INTARRAY, TY_INTARRAY, TY_INTARRAY);
#if 0
    install(ggMOproduct, cmd_mo_product, TY_MON_ORDER, TY_MON_ORDER);
    install(ggMOgeneral, cmd_mo_general, 
	  TY_INTARRAY, TY_INTARRAY, TY_INTARRAY, TY_INTARRAY);
#endif
#if 0
  install(ggMOelim, cmd_mo_elim, TY_MON_ORDER, TY_MON_ORDER);
  install(ggMOgproduct, cmd_mo_gproduct, TY_MON_ORDER, TY_MON_ORDER);
#endif
#endif // MIKE_OLD_MONOID

#ifdef MIKE_EMONOID
  trivial_monoid = Monoid::trivial_monoid();

  install(ggmonoid, cmd_Monoid,
	  TY_EMonomialOrder, TY_STRING, TY_MONOID, TY_INTARRAY, TY_INTARRAY);

  //////////////////////////
  // New Monomial Orders ///
  //////////////////////////

  install(ggMOinit, cmd_EMO_init);
  install(ggMOclone, cmd_EMO_clone, TY_EMonomialOrder);

  install(ggMOrevlex, cmd_EMO_revlex, TY_INT, TY_INT, TY_EMonomialOrder);
  install(ggMOrevlex, cmd_EMO_revlexWeights, TY_INTARRAY, TY_INT, TY_EMonomialOrder);
  install(ggMOlex, cmd_EMO_lex, TY_INT, TY_INT, TY_EMonomialOrder);

  install(ggMOcomponent, cmd_EMO_component, TY_EMonomialOrder);
  install(ggMOwtfcn, cmd_EMO_weightFunction, TY_INTARRAY, TY_EMonomialOrder);
  install(ggMOproduct, cmd_EMO_product, TY_EMonomialOrder, TY_EMonomialOrder);
  install(ggMONClex, cmd_EMO_NClex, TY_INT, TY_EMonomialOrder);

  
  // install(ggtest, cmd_EMO_test, TY_INT, TY_INTARRAY, TY_EMonomialOrder);
#endif

#ifdef MIKE_NEWMONORDER
  install(ggzeromonoid, cmd_mo_make, TY_INTARRAY);
#endif

}

#if 0
////////////////////////////////////////////////////////
-- Top level test of latest monomial order code
-- Code to create monomial orderings is in m2/orderings.m2
mo = monomialOrdering {Weights => {1,0,0,0}, RevLex => 4, Lex => 3}
mo = monomialOrdering {Weights => {1,0,0,0}, RevLex => 4, GroupLex => 3}
mo = monomialOrdering {Weights => {-1,0,0,0}, RevLex => {2,3,4,5}}
mo = monomialOrdering {Weights => {-1,0,0,0}, GroupRevLex => {2,3,4,5}}
mo = monomialOrdering {Component, Weights => {-1,0,0,0}, RevLex => {2,3,4,5}}
mo = monomialOrdering {NCLex => 4}
mo = monomialOrdering {Eliminate 4, RevLex => 8}
encode = (mo,a) -> (sendgg(ggPush 1, ggPush a, ggPush mo, ggtest); eePopIntarray())
decode = (mo,a) -> (sendgg(ggPush 2, ggPush a, ggPush mo, ggtest); eePopIntarray())

testit = (mo, a) -> (
     << "a = " << a << endl;
     b := encode(mo,a);
     << "b = " << b << endl;
     c := decode(mo,b);
     << "c = " << c << endl;
     if a =!= c then error "encode/decode failed!"
     )

testit(mo,{1,1,0,0})
b =  encode(mo,{1,1,1,1,7,8,10})
decode(mo,b)
b =  encode(mo,{1,1,1,3,0,0,0})
decode(mo,b)
b =  encode(mo,{3,1,0,12321,1,2132134,46433436})
decode(mo,b)
////////////////////////////////////////////////////////
#endif
