// Copyright 1995 Michael E. Stillman

#include "interp.hpp"

#include "freemod.hpp"
#include "vector.hpp"
#include "matrix.hpp"

void cmd_FreeModule(object &oR, object &oa)
{
  const Ring *R = oR->cast_to_Ring();
  intarray *a = oa->intarray_of();

  int n = R->degree_monoid()->n_vars();
  int *deg = R->degree_monoid()->make_one();
  if (n == 0) n = 1;
  int rk = a->length() / n;
  if (rk * n != a->length())
    {
      gError << "inappropriate number of degrees";
      return;
    }

  FreeModule *F = R->make_FreeModule();
  for (int i=0; i<rk; i++)
    {
      R->degree_monoid()->from_expvector(a->raw() + i * n, deg);
      F->append(deg);
    }

  R->degree_monoid()->remove(deg);

  gStack.insert(F);
}

void cmd_FreeModule1(object &oR, object &on)
{
  const Ring *R = oR->cast_to_Ring();
  int n = on->int_of();
  gStack.insert(R->make_FreeModule(n));
}

FreeModule *makeSchreyerFreeModule(Ring *R, int rank, 
				   const int **degrees,  
				   const int **ordering, 
				   const int *tiebreaks) 
{
  FreeModule *F = R->make_FreeModule();
  for (int i=0; i<rank; i++)
    F->append(degrees[i],ordering[i],tiebreaks[i]);
  return F;
}

FreeModule *makeSchreyerFreeModule(const Matrix &m)
{
  int i;
  const Ring *R = m.Ring_of();
  const Monoid *M = R->Nmonoms();
  FreeModule *F = R->make_FreeModule();
  int rk = m.n_cols();
  int *base = M->make_one();
  int *tiebreaks = new int[rk];
  for (i=0; i<rk; i++)
    tiebreaks[i] = i;  // NEED TO FIND REAL TIEBREAKS
  for (i=0; i<rk; i++)
    {
      vec v = m[i];
      if (v == NULL) 
	M->one(base);
      else 
	M->copy(v->monom, base);

      F->append(m.cols()->degree(i), base, tiebreaks[i]);
    }

  M->remove(base);
  delete [] tiebreaks;
  return F;
}

void cmd_FreeModule2(object &oR, object &oa, object &om, object &onums)
{
  const Ring *R = oR->cast_to_Ring();
  if (R->cast_to_PolynomialRing() == NULL)
    {
      gError << "polynomial ring required for Schreyer order";
      return;
    }
  const Monoid *M = R->Nmonoms();
  if (M == NULL)
    {
      assert(0);
      return;
    }
  intarray *a = oa->intarray_of();
  Matrix m = om->cast_to_Matrix();
  intarray *nums = onums->intarray_of();

  int rk = m.n_cols();
  if (rk != nums->length())
    {
      gError << "inconsistent rank";
      return;
    }
  int n = R->degree_monoid()->n_vars();
  if (a->length() != n * rk)
    {
      gError << "inappropriate number of degrees";
      return;
    }
  int *deg = R->degree_monoid()->make_one();
  int *base = M->make_one();

  FreeModule *F = R->make_FreeModule();
  for (int i=0; i<rk; i++)
    {
      R->degree_monoid()->from_expvector(a->raw() + i * n, deg);
      vec v = m[i];
      if (v == NULL) 
	M->one(base);
      else 
	M->copy(v->monom, base);
      
      F->append(deg, base, (*nums)[i]);
    }

  R->degree_monoid()->remove(deg);
  M->remove(base);

  gStack.insert(F);
}

void cmd_FreeModule3(object &om)
{
  Matrix m = om->cast_to_Matrix();
  gStack.insert(makeSchreyerFreeModule(m));
}
void cmd_Nfree_sum(object &oV, object &oW)
{
  const FreeModule *V = oV->cast_to_FreeModule();
  const FreeModule *W = oW->cast_to_FreeModule();
  gStack.insert(V->direct_sum(W));
}

void cmd_Nfree_isequal(object &oV, object &oW)
{
  const FreeModule *V = oV->cast_to_FreeModule();
  const FreeModule *W = oW->cast_to_FreeModule();
  gStack.insert(make_object_int(V->is_equal(W)));
}

void cmd_Nfree_tensor(object &oV, object &oW)
{
  const FreeModule *V = oV->cast_to_FreeModule();
  const FreeModule *W = oW->cast_to_FreeModule();
  gStack.insert(V->tensor(W));
}

void cmd_Nfree_dual(object &oV)
{
  const FreeModule *V = oV->cast_to_FreeModule();
  gStack.insert(V->transpose());
}

void cmd_Nfree_shift(object &oV, object &oa)
{
  const FreeModule *V = oV->cast_to_FreeModule();
  intarray *a = oa->intarray_of();
  gStack.insert(V->shift(a->raw()));
}

void cmd_Nfree_submodule(object &oV, object &oa)
{
  const FreeModule *V = oV->cast_to_FreeModule();
  intarray *a = oa->intarray_of();
  gStack.insert(V->sub_space(*a));
}

void cmd_Nfree_symm(object &oV, object &on)
{
  const FreeModule *V = oV->cast_to_FreeModule();
  int n = on->int_of();
  gStack.insert(V->symm(n));
}

void cmd_Nfree_exterior(object &oV, object &on)
{
  const FreeModule *V = oV->cast_to_FreeModule();
  int n = on->int_of();
  gStack.insert(V->exterior(n));
}

void cmd_Nfree_lcm(object &oV)
{
  const FreeModule *V = oV->cast_to_FreeModule();
  object_intarray *deg = new object_intarray;
  V->lcm(*deg->intarray_of());
  gStack.insert(deg);
}
void cmd_Nfree_gcd(object &oV)
{
  const FreeModule *V = oV->cast_to_FreeModule();
  object_intarray *deg = new object_intarray;
  V->gcd(*deg->intarray_of());
  gStack.insert(deg);
}

static int check_elems(int num, int typ)
     // Return whether the top 'num' elements on the stack have type 'typ'.
{
  for (int i=num-1; i >= 0 ; i--) 
    if (!gStack.in_bounds(i) || gStack[i]->type_id() != typ)
      return 0;
  return 1;
}

void cmd_Nvec_make(object &oF)
     // Create a vector from elements on the stack.
     // On stack: r0 r1 r2 ... r(c-1) F:freemodule  >> vector
{
  const FreeModule *F = oF->cast_to_FreeModule();
  int rk = F->rank();
  if (!check_elems(rk, TY_RING_ELEM))
      {
	gError << "vector creation: expected ring element on the stack";
	return;
      }

  Vector result(F);
  for (int i=0; i<rk; i++)
    result = result + Vector(F, gStack[rk-1-i]->cast_to_RingElement(), i);

  gStack.poppem(rk);
  gStack.insert(result);
}

void cmd_Nvec_sparse(object &oF, object &orows)
     // Create a vector from elements on the stack.
     // On stack: r0 r1 r2 ... r(c-1) F:FreeModule rows:intarray  >> vector
{
  const FreeModule *F = oF->cast_to_FreeModule();
  intarray *rows = orows->intarray_of();
  int rk = F->rank();
  int n_on_stack = rows->length();
  if (!check_elems(n_on_stack, TY_RING_ELEM))
    {
      gError << "vector creation: expected ring element on the stack";
      return;
    }
  Vector result(F);
  for (int i=0; i<n_on_stack; i++)
    {
      int n = (*rows)[i];
      if ((n >= 0) && (n < rk))
	result = result + 
	  Vector(F, gStack[n_on_stack-1-i]->cast_to_RingElement(), n);
    }
  gStack.poppem(n_on_stack);
  gStack.insert(result);
}

void cmd_Nvec_from_int(object &oF, object &on)
{
  const FreeModule *F = oF->cast_to_FreeModule();
  int n = on->int_of();
  vec v = F->e_sub_i(n);
  gStack.insert(Vector(F,v));
}

void cmd_Nvec_zero(object &oF)
{
  const FreeModule *F = oF->cast_to_FreeModule();
  gStack.insert(Vector(F));
}

void cmd_Nvec_term(object &oF, object &oelem, object &on)
{
  const FreeModule *F = oF->cast_to_FreeModule();
  RingElement r = oelem->cast_to_RingElement();
  int n = on->int_of();
  gStack.insert(Vector(F,r,n));
}

void cmd_Nvec_elem(object &ov, object &on)
{
  Vector a = ov->cast_to_Vector();
  int n = on->int_of();
  gStack.insert(a.get_coefficient(n));
}

void cmd_Nvec_lcoeff(object &ov)
{
  Vector a = ov->cast_to_Vector();
  gStack.insert(a.lead_coefficient());
}

void cmd_Nvec_lterm(object &ov)
{
  Vector a = ov->cast_to_Vector();
  gStack.insert(a.lead_term());
}

void cmd_Nvec_lcomp(object &ov)
{
  Vector a = ov->cast_to_Vector();
  gStack.insert(make_object_int(a.lead_component()));
}

void cmd_Nvec_isequal(object &oa, object &ob)
{
  Vector a = oa->cast_to_Vector();
  Vector b = ob->cast_to_Vector();
  gStack.insert(make_object_int(a.is_equal(b)));
}

void cmd_Nvec_iszero(object &oa)
{
  Vector a = oa->cast_to_Vector();
  gStack.insert(make_object_int(a.is_zero()));
}

void cmd_Nvec_add(object &oa, object &ob)
{
    Vector a = oa->cast_to_Vector();
    Vector b = ob->cast_to_Vector();
    gStack.insert(a+b);
}
void cmd_Nvec_negate(object &oa)
{
    Vector a = oa->cast_to_Vector();
    gStack.insert(-a);
}
void cmd_Nvec_subtract(object &oa, object &ob)
{
    Vector a = oa->cast_to_Vector();
    Vector b = ob->cast_to_Vector();
    gStack.insert(a-b);
}
void cmd_Nvec_mult(object &oa, object &ob)
{
    RingElement a = oa->cast_to_RingElement();
    Vector b = ob->cast_to_Vector();
    gStack.insert(b*a);
}
#if 0
// MES: BUG!!  This needs to be working! The previous routine is
// being called incorrectly in the case v*r...
void cmd_Nvec_multr(object &oa, object &ob)
{
    RingElement b = oa->cast_to_RingElement();
    Vector a = ob->cast_to_Vector();
    gStack.insert(b*a);
}
#endif
void cmd_Nvec_getterms(object &ov, object &om, object &on)
{
  Vector v = ov->cast_to_Vector();
  int m = om->int_of();
  int n = on->int_of();
  gStack.insert(v.get_terms(m,n));
}
void cmd_Nvec_subvector(object &ov, object &oF, object &oa)
{
  Vector v = ov->cast_to_Vector();
  const FreeModule *F = oF->cast_to_FreeModule();
  intarray *a = oa->intarray_of();
  gStack.insert(v.sub_vector(F,*a));
}

void cmd_Nvec_ishomogeneous(object &ov)
{
  Vector v = ov->cast_to_Vector();
  gStack.insert(make_object_int(v.is_homogeneous()));
}
void cmd_Nvec_degree(object &ov)
{
  Vector v = ov->cast_to_Vector();
  gStack.insert(new object_intarray(v.degree()));
}
void cmd_Nvec_homogenize(object &ov, object &ovar, object &owts)
{
  Vector v = ov->cast_to_Vector();
  int var = ovar->int_of();
  intarray *wts = owts->intarray_of();
  const Ring *R = v.Ring_of();
  if (var < 0 || var >= R->n_vars())
    {
      gError << "homogenization: improper ring variable";
      return;
    }
  if (wts == NULL || wts->length() != R->n_vars())
    {
      gError << "homogenization: improper weight function";
      return;
    }
  if ((*wts)[var] == 0)
    {
      gError << "homogenization: variable weight is zero";
      return;
    }
  Vector vh = v.homogenize(var, wts->raw());
  if (!error_exists())
    gStack.insert(vh);
}
void cmd_Nvec_homogenize1(object &ov, object &ovar, object &odeg, object &owts)
{
  Vector v = ov->cast_to_Vector();
  int var = ovar->int_of();
  int deg = odeg->int_of();
  intarray *wts = owts->intarray_of();
  const Ring *R = v.Ring_of();
  if (var < 0 || var >= R->n_vars())
    {
      gError << "homogenization: improper ring variable";
      return;
    }
  if (wts == NULL || wts->length() != R->n_vars())
    {
      gError << "homogenization: improper weight function";
      return;
    }
  if ((*wts)[var] == 0)
    {
      gError << "homogenization: variable weight is zero";
      return;
    }
  Vector vh = v.homogenize(var, deg, wts->raw());
  if (!error_exists())
    gStack.insert(vh);
}

void i_Vector_cmds(void)
{
  // Informational
  install(ggisequal, cmd_Nvec_isequal, TY_VECTOR, TY_VECTOR);
  install(ggiszero, cmd_Nvec_iszero, TY_VECTOR);

  // vector commands
  install(ggvector, cmd_Nvec_make, TY_FREEMODULE);
  install(ggsparsevector, cmd_Nvec_sparse, TY_FREEMODULE, TY_INTARRAY);

  install(ggzero, cmd_Nvec_zero, TY_FREEMODULE);
  install(ggfromint, cmd_Nvec_from_int, TY_FREEMODULE, TY_INT);
  install(ggterm, cmd_Nvec_term, TY_FREEMODULE, TY_RING_ELEM, TY_INT);
  install(ggelem, cmd_Nvec_elem, TY_VECTOR, TY_INT);

  install(ggleadcomp, cmd_Nvec_lcomp, TY_VECTOR);
  install(ggleadcoeff, cmd_Nvec_lcoeff, TY_VECTOR);
  install(ggleadterm, cmd_Nvec_lterm, TY_VECTOR);

  install(ggishomogeneous, cmd_Nvec_ishomogeneous, TY_VECTOR);
  install(ggdegree, cmd_Nvec_degree, TY_VECTOR);
  install(gghomogenize, cmd_Nvec_homogenize, TY_VECTOR, TY_INT, TY_INTARRAY);
  install(gghomogenize, cmd_Nvec_homogenize1, TY_VECTOR, TY_INT, TY_INT, TY_INTARRAY);
  
  install(ggadd, cmd_Nvec_add, TY_VECTOR, TY_VECTOR);
  install(ggnegate, cmd_Nvec_negate, TY_VECTOR);
  install(ggsubtract, cmd_Nvec_subtract, TY_VECTOR, TY_VECTOR);
  install(ggmult, cmd_Nvec_mult, TY_RING_ELEM, TY_VECTOR);
  install(ggmult, cmd_Nvec_mult, TY_VECTOR, TY_RING_ELEM);

  install(gggetterms, cmd_Nvec_getterms, TY_VECTOR, TY_INT, TY_INT);
  install(ggselect, cmd_Nvec_subvector, TY_VECTOR, 
	  TY_FREEMODULE, TY_INTARRAY);

  // Free module commands
  install(ggfree, cmd_FreeModule, TY_RING, TY_INTARRAY);
  install(ggfree, cmd_FreeModule1, TY_RING, TY_INT);
  install(ggfree, cmd_FreeModule2, TY_RING, TY_INTARRAY, TY_MATRIX, TY_INTARRAY);
  install(ggfree, cmd_FreeModule3, TY_MATRIX);

  install(ggisequal, cmd_Nfree_isequal, TY_FREEMODULE, TY_FREEMODULE);
  install(ggadd, cmd_Nfree_sum, TY_FREEMODULE, TY_FREEMODULE);
  install(ggmult, cmd_Nfree_tensor, TY_FREEMODULE, TY_FREEMODULE);
  install(ggtranspose, cmd_Nfree_dual, TY_FREEMODULE);
  install(ggshift, cmd_Nfree_shift, TY_FREEMODULE, TY_INTARRAY);
  install(ggsubmodule, cmd_Nfree_submodule, TY_FREEMODULE, TY_INTARRAY);
  install(ggsymm, cmd_Nfree_symm, TY_FREEMODULE, TY_INT);
  install(ggexterior, cmd_Nfree_exterior, TY_FREEMODULE, TY_INT);

  install(gglcm, cmd_Nfree_lcm, TY_FREEMODULE);
  install(gggcd, cmd_Nfree_gcd, TY_FREEMODULE);
}
