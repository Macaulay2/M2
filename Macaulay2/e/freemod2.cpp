// Copyright 1996  Michael E. Stillman

#include "freemod.hpp"
#include "comb.hpp"
#include "bin_io.hpp"
#include "polyring.hpp"
#include "serial.hpp"

//////////////////////////////////////////////
//  Construction/Destruction routines ////////
//////////////////////////////////////////////

void FreeModule::initialize(const Ring *RR)
{
  // type() should already be set.
  R = RR;

  const PolynomialRing *P = RR->cast_to_PolynomialRing();
  if (P == NULL)
    {
      ty = FREE;
      K = RR;
      M = NULL;
      is_quotient_ring = 0;
    }
  else
    {
      ty = FREE_POLY;
      K = R->Ncoeffs();
      M = R->Nmonoms();

      nf_1 = M->make_one();
      mon_1 = M->make_one();

      nf_exp = nf_exp_a.alloc(M->n_vars());
      is_quotient_ring = (P->base_ring != NULL);
      coefficients_are_ZZ = P->coefficients_are_ZZ;

      TO_EXP_monom = M->make_one();
    }

  bump_up((Ring *) R);
  bump_up((Ring *) K);
}

FreeModule::FreeModule(const Ring *RR)
: type()
{
  initialize(RR);
}

FreeModule::FreeModule(const Ring *RR, int n)
: type()
     // Create R^n, with all gradings zero.
{
  initialize(RR);

  int *deg = degree_monoid()->make_one();
  for (int i=0; i<n; i++)
    append(deg);
  degree_monoid()->remove(deg);
}

FreeModule::FreeModule(const Ring *RR, const FreeModule *F)
: type()
    // Take the degrees and monomials from F, but take the 
    // new ring/monomial information using R.
{
  initialize(RR);
  // MES: make sure that (1) nvars are equal, (2) degree_monoid's are equal.

  int rk = F->rank();

  intarray expa;
  int *exp = expa.alloc(R->n_vars());
  int *base = NULL;
  if (F->M != NULL) base = F->M->make_one();
  for (int i=0; i<rk; i++)
    {
      if (M != NULL)
	{
	  F->M->to_expvector(F->base_monom(i), exp);
	  M->from_expvector(exp, base);
	}
      append(F->degree(i), base);
    }
  if (F->M != NULL) F->M->remove(base);
  ty = F->ty;
}

FreeModule::~FreeModule()
{
  int rk = rank();
  for (int i=0; i<rk; i++)
    {
      if (M != NULL) M->remove(components[i]->base_monom);
      degree_monoid()->remove(components[i]->deg);
      delete components[i];
    }
  if (M != NULL)
    {
      M->remove(nf_1);
      M->remove(mon_1);
      M->remove(TO_EXP_monom);
    }
  bump_down((Ring *) R);
  bump_down((Ring *) K);
}

FreeModule *FreeModule::new_free() const
{
  return R->make_FreeModule();
}

#if 0
bool FreeModule::equals(const object_element *o) const
{
  if (o->class_id() != class_id())
    return false;

  const FreeModule *FF = (FreeModule *)o;
  if (ty != FF->ty) return false;
  if (!R->equals(FF->R)) return false;
  // MESXX: test rank, then for each element, test degree, base, compare_num.
  return true;
}
#endif
void FreeModule::write_object(object_writer &o) const
{
  //  o << class_id() << R;
  // MESXX: not done yet!!
}

FreeModule *FreeModule::read_object(object_reader &i)
{
  // MESXX
  return 0;
}

//////////////////////////////////////////////
//  Manipulations involving components ///////
//////////////////////////////////////////////

void FreeModule::append(const int *d, const int *base, int compare_num)
{
  index_type *p = new index_type(compare_num);
  p->deg = degree_monoid()->make_new(d);

  if (M == NULL)
    p->base_monom = NULL;
  else
    p->base_monom = M->make_new(base);

  components.append(p);

  if (ty == FREE_POLY && !M->is_one(p->base_monom))
    ty = FREE_SCHREYER;
}

void FreeModule::append(const int *d, const int *base)
{
  append(d, base, rank());
}

void FreeModule::append(const int *d)
{
  index_type *p = new index_type(rank());

  p->deg = degree_monoid()->make_new(d);

  if (M == NULL)
    p->base_monom = NULL;
  else
    p->base_monom = M->make_one();

  components.append(p);
}

bool FreeModule::is_equal(const FreeModule *F) const
{
  int i;
  if (this == F) return 1;
  if (rank() != F->rank()) return 0;

  if (degree_monoid()->n_vars() > 0)
    for (i=0; i<rank(); i++)
      if (0 != degree_monoid()->compare(degree(i), F->degree(i)))
	return 0;

  if (M != NULL)
    for (i=0; i<rank(); i++)
      if (M->compare(base_monom(i), F->base_monom(i)) != 0)
	return 0;

  return 1;
}

//////////////////////////////////////////////
//  New free modules from old ////////////////
//////////////////////////////////////////////

FreeModule *FreeModule::shift(const int *d) const
     // Shift degree by d.
{
  FreeModule *result = new_free();
  int *deg = degree_monoid()->make_one();

  for (int i=0; i<rank(); i++)
    {
      degree_monoid()->mult(degree(i), d, deg);
      result->append(deg, base_monom(i));
    }

  degree_monoid()->remove(deg);
  return result;
}

  
FreeModule *FreeModule::sub_space(int n) const
{
  if (n < 0 || n > rank())
    {
      gError << "subfreemodule: index out of bounds";
      return NULL;
    }
  FreeModule *result = new_free();
  for (int i=0; i<n; i++)
    result->append(degree(i), base_monom(i));
  return result;
}

FreeModule *FreeModule::sub_space(const intarray &a) const
{
  FreeModule *result = new_free();
  for (int i=0; i<a.length(); i++)
    if (a[i] >= 0 && a[i] < rank())
      result->append(degree(a[i]), base_monom(a[i]));
    else
      {
	gError << "subfreemodule: index out of bounds";
	delete result;
	return NULL;
      }
  return result;
}

FreeModule *FreeModule::transpose() const
{
  FreeModule *result = new_free();
  int *deg = degree_monoid()->make_one();

  for (int i=0; i<rank(); i++)
    {
      degree_monoid()->power(degree(i), -1, deg);
      result->append(deg);
    }

  degree_monoid()->remove(deg);
  return result;
}

FreeModule *FreeModule::direct_sum(const FreeModule *G) const
     // direct sum 
{
  int i;
  FreeModule *result = new_free();
  for (i=0; i<rank(); i++)
    result->append(degree(i), base_monom(i));
  for (i=0; i<G->rank(); i++)
    result->append(G->degree(i), G->base_monom(i));
  return result;
}

void FreeModule::direct_sum_to(const FreeModule *G)
{
  for (int i=0; i<G->rank(); i++)
    append(G->degree(i), G->base_monom(i));
}

FreeModule *FreeModule::tensor(const FreeModule *G) const
     // tensor product
{
//  if (R != G->R) 
//    THROW("free module tensor product: arguments must have same base ring");

  FreeModule *result = new_free();
  int *deg = degree_monoid()->make_one();
  int *base = NULL;
  if (M != NULL) base = M->make_one();

  for (int i=0; i<rank(); i++)
    for (int j=0; j<G->rank(); j++)
      {
	degree_monoid()->mult(degree(i), G->degree(j), deg);
	if (M != NULL)
	  M->mult(base_monom(i), G->base_monom(j), base);
	result->append(deg, base);
      }
  degree_monoid()->remove(deg);
  if (M != NULL) M->remove(base);
  return result;
}

FreeModule *FreeModule::exterior(int p) const
     // p th exterior power
{
  int r, c;

  FreeModule *result;

  if (p == 0) 
    result = get_ring()->make_FreeModule(1);
  result = new_free();
  if (p > rank() || p < 0) return result;

  int n = comb::binom(rank(), p);
  intarray carray(p);
  int *a = carray.raw();

  int *deg = degree_monoid()->make_one();
  int *base = NULL;
  if (M != NULL) base = M->make_one();

  for (c=0; c<n; c++)
    {
      comb::decode(c,a,p);

      degree_monoid()->one(deg);

      for (r=0; r<p; r++)
	degree_monoid()->mult(deg, degree(a[r]), deg);

      if (M != NULL)
	{
	  M->one(base);
	  for (r=0; r<p; r++)
	    M->mult(base, base_monom(a[r]), base);
	}

      result->append(deg, base);
    }

  degree_monoid()->remove(deg);
  if (M != NULL) M->remove(base);

  return result;
}

static FreeModule *symm1_result = NULL;
static int *symm1_deg = NULL;
static int *symm1_base = NULL;

void FreeModule::symm1(int lastn,	     // can use lastn..rank()-1 in product
			int pow) const   // remaining power to take
{
  if (pow == 0)
    symm1_result->append(symm1_deg, symm1_base);
  else
    {
      for (int i=lastn; i<rank(); i++)
	{
	  // increase symm1_deg, symm1_base with e_i
	  degree_monoid()->mult(symm1_deg, degree(i), symm1_deg);
	  if (ty == FREE_SCHREYER)
	    M->mult(symm1_base, base_monom(i), symm1_base);

	  symm1(i, pow-1);

	  // decrease symm1_deg, symm1_base back
	  degree_monoid()->divide(symm1_deg, degree(i), symm1_deg);
	  if (ty == FREE_SCHREYER)
	    M->divide(symm1_base, base_monom(i), symm1_base);
	}
    }
}

FreeModule *FreeModule::symm(int n) const
    // n th symmetric power
{
  symm1_result = new_free();
  if (n >= 0)
    {
      symm1_deg = degree_monoid()->make_one();
      if (M != NULL)
	symm1_base = M->make_one();
      
      symm1(0, n);
      
      degree_monoid()->remove(symm1_deg);
      if (M != NULL) M->remove(symm1_base);
    }
  FreeModule *result = symm1_result;
  symm1_result = NULL;
  return result;
}

void FreeModule::gcd(intarray &lo_deg) const
{
  int *lodeg;
  int *finaldeg = lo_deg.alloc(degree_monoid()->n_vars());
  if (rank() == 0) 
    {
      lodeg = degree_monoid()->make_one();
    }
  else 
    {
      lodeg = degree_monoid()->make_new(degree(0));
      for (int i=1; i<rank(); i++)
	degree_monoid()->gcd(lodeg, degree(i), lodeg);
    }
  degree_monoid()->to_expvector(lodeg, finaldeg);
  degree_monoid()->remove(lodeg);
}

void FreeModule::lcm(intarray &hi_deg) const
{
  int *hideg;
  int *finaldeg = hi_deg.alloc(degree_monoid()->n_vars());
  if (rank() == 0) 
    {
      hideg = degree_monoid()->make_one();
    }
  else 
    {
      hideg = degree_monoid()->make_new(degree(0));
      for (int i=1; i<rank(); i++)
	degree_monoid()->lcm(hideg, degree(i), hideg);
    }
  degree_monoid()->to_expvector(hideg, finaldeg);
  degree_monoid()->remove(hideg);
}

int FreeModule::primary_degree(int i) const
{
  int result = degree_monoid()->primary_value(degree(i));
  return result;
}

int FreeModule::lowest_primary_degree() const
{
  if (rank() == 0) return 0;
  int result = primary_degree(0);
  for (int i=1; i<rank(); i++)
    {
      if (primary_degree(i) < result)
	result = primary_degree(i);
    }
  return result;
}

int FreeModule::highest_primary_degree() const
{
  if (rank() == 0) return 0;
  int result = primary_degree(0);
  for (int i=1; i<rank(); i++)
    {
      if (primary_degree(i) > result)
	result = primary_degree(i);
    }
  return result;
}

void FreeModule::text_out(buffer &o) const
{
  int i;
  int rk = rank();
  o << "free(rank " << rk << " degrees = {";
  for (i=0; i<rk; i++)
    {
      if (i != 0) o << ", ";
      degree_monoid()->elem_text_out(o, degree(i));
    }
  o << "}";
  if (ty == FREE_SCHREYER)
    for (i=0; i<rk; i++)
      {
	if (i != 0) o << ' ';
	M->elem_text_out(o, base_monom(i));
	o << '.';
	o << compare_num(i);
      }
  o << ')';
}

void FreeModule::bin_out(buffer &o) const
{
  int rk = rank();
  int n;
  if (degree_monoid() != NULL)
    n = degree_monoid()->n_vars();
  else 
    n = 0;
  //bin_int_out(o, rk);
  int *exp = new int[n+1];  // only use 0..n-1
  bin_int_out(o, rk * n);
  for (int i=0; i<rk; i++)
    {
      degree_monoid()->to_expvector(degree(i), exp);
      for (int j=0; j<n; j++)
	bin_int_out(o, exp[j]);
    }
  delete [] exp;
}

