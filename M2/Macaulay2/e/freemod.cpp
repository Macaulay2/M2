// Copyright 1996  Michael E. Stillman

#include "freemod.hpp"
#include "comb.hpp"
#include "polyring.hpp"
#include "matrix.hpp"
#include "matrixcon.hpp"
#include "Eschreyer.hpp"
#include "gbring.hpp"

//////////////////////////////////////////////
//  Construction/Destruction routines ////////
//////////////////////////////////////////////

void FreeModule::initialize(const Ring *RR)
{
  R = RR;
  schreyer = 0;
}

FreeModule::FreeModule(const Ring *RR, int n, bool has_schreyer_order)
: immutable_object(0)
     // Create R^n, with all gradings zero.
{
  initialize(RR);

  int *deg = degree_monoid()->make_one();
  for (int i=0; i<n; i++)
    append(deg);
  degree_monoid()->remove(deg);

  if (has_schreyer_order)
    {
      const PolynomialRing *P = RR->cast_to_PolynomialRing();
      assert(P != 0);
      assert(n == 0);
      schreyer = SchreyerOrder::create(P->Nmonoms());
    }
  else
    schreyer = 0;
}

FreeModule *FreeModule::make_schreyer(const Matrix *m)
{
  int i;
  const Ring *R = m->get_ring();
  FreeModule *F = R->make_FreeModule();
  int rk = m->n_cols();
  if (rk == 0) return F;

  for (i=0; i<rk; i++)
    F->append(m->cols()->degree(i));

  F->schreyer = SchreyerOrder::create(m);

  return F;
}

FreeModule *FreeModule::make_schreyer(const GBMatrix *m)
{
  const FreeModule *F = m->get_free_module();
  const PolynomialRing *R = F->get_ring()->cast_to_PolynomialRing();
  assert(R);
  FreeModule *G = R->make_FreeModule();
  int rk = m->elems.size();
  if (rk == 0) return G;

  for (int i=0; i<rk; i++)
    {
      int *deg = R->degree_monoid()->make_one();
      gbvector * v = m->elems[i];
      if (v != 0)
	R->get_gb_ring()->gbvector_multidegree(F, v, deg);
      G->append(deg);
    }

  G->schreyer = SchreyerOrder::create(m);

  return G;
}

Matrix * FreeModule::get_induced_order() const
{
  const PolynomialRing *P = R->cast_to_PolynomialRing();
  if (!schreyer || P == 0)
    return Matrix::zero(R->make_FreeModule(0),this);
  const SchreyerOrder *S = schreyer;
  int i;
  int maxtie = 0;
  for (i=0; i<rank(); i++)
    if (S->compare_num(i) > maxtie)
      maxtie = S->compare_num(i);
  const FreeModule *F = R->make_FreeModule(maxtie+1);
  MatrixConstructor mat(F,this,0);
  for (i=0; i<rank(); i++)
    {
      ring_elem f = P->make_flat_term(P->getCoefficients()->from_int(1), 
			    S->base_monom(i));
      mat.set_entry(S->compare_num(i), i, f);
    }
  return mat.to_matrix();
}

FreeModule::~FreeModule()
{
  if (schreyer)
    deleteitem(schreyer);
}

FreeModule *FreeModule::new_free() const
{
  return R->make_FreeModule();
}

//////////////////////////////////////////////
//  Manipulations involving components ///////
//////////////////////////////////////////////

void FreeModule::append(const int *d)
{
  assert(schreyer == 0);
  int *p = degree_monoid()->make_new(d);
  components.append(p);
}

void FreeModule::append_schreyer(const int *d, const int *base, int compare_num)
{
  assert(schreyer != 0);
  int *p = degree_monoid()->make_new(d);
  components.append(p);
  schreyer->append(compare_num,base);
}

void FreeModule::change_degree(int i, const int *deg)
{ 
  // WARNING: this modifies the degree, and should only be used during
  // the construction of a free module (or matrix).
  assert(i >= 0);
  assert(i < rank());
  degree_monoid()->copy(deg, components[i]);
}

bool FreeModule::is_equal(const FreeModule *F) const
{
  int i;
  if (this == F) return true;
  if (this->get_ring() != F->get_ring()) return false;
  if (rank() != F->rank()) return false;

  const Monoid *D = degree_monoid();
  if (D->n_vars() > 0)
    for (i=0; i<rank(); i++)
      if (0 != D->compare(degree(i), F->degree(i)))
	return false;

  if (schreyer != NULL)
    return schreyer->is_equal(F->schreyer);
  if (F->schreyer != NULL) return false;

  return true;
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
      result->append(deg);
    }

  if (schreyer != NULL)
    result->schreyer = schreyer->copy();

  degree_monoid()->remove(deg);
  return result;
}

  
FreeModule *FreeModule::sub_space(int n) const
{
  if (n < 0 || n > rank())
    {
      ERROR("subfreemodule: index out of bounds");
      return NULL;
    }
  FreeModule *result = new_free();
  for (int i=0; i<n; i++)
    result->append(degree(i));

  if (schreyer != NULL)
    result->schreyer = schreyer->sub_space(n);
  return result;
}

FreeModule *FreeModule::sub_space(M2_arrayint a) const
{
  FreeModule *result = new_free();
  for (unsigned int i=0; i<a->len; i++)
    if (a->array[i] >= 0 && a->array[i] < rank())
      result->append(degree(a->array[i]));
    else
      {
	ERROR("subfreemodule: index out of bounds");
	deleteitem(result);
	return NULL;
      }
  if (schreyer != NULL)
    result->schreyer = schreyer->sub_space(a);
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

  // result has no schreyer order
  degree_monoid()->remove(deg);
  return result;
}

FreeModule *FreeModule::direct_sum(const FreeModule *G) const
     // direct sum 
{
  int i;
  if (get_ring() != G->get_ring())
    {
      ERROR("expected free modules over the same ring");
      return 0;
    }
  FreeModule *result = new_free();
  for (i=0; i<rank(); i++)
    result->append(degree(i));
  for (i=0; i<G->rank(); i++)
    result->append(G->degree(i));

  if (schreyer != NULL && G->schreyer != NULL)
    result->schreyer = schreyer->direct_sum(G->schreyer);

  return result;
}

void FreeModule::direct_sum_to(const FreeModule *G)
{
  for (int i=0; i<G->rank(); i++)
    append(G->degree(i));

  if (schreyer != NULL && G->schreyer != NULL)
    schreyer->append_order(G->schreyer);
}

FreeModule *FreeModule::tensor(const FreeModule *G) const
     // tensor product
{
  if (get_ring() != G->get_ring())
    {
      ERROR("expected free modules over the same ring");
      return 0;
    }
  FreeModule *result = new_free();
  int *deg = degree_monoid()->make_one();

  for (int i=0; i<rank(); i++)
    for (int j=0; j<G->rank(); j++)
      {
	degree_monoid()->mult(degree(i), G->degree(j), deg);
	result->append(deg);
      }

  degree_monoid()->remove(deg);
  if (schreyer != NULL && G->schreyer != NULL)
    result->schreyer = schreyer->tensor(G->schreyer);
  return result;
}

FreeModule *FreeModule::exterior(int p) const
     // p th exterior power
{
  FreeModule *result;

  int rk = rank();

  if (p == 0) 
    return get_ring()->make_FreeModule(1);
  else
    result = new_free();
  if (p > rk || p < 0) return result;

  int *a = newarray_atomic(int,p);
  for (int i=0; i<p; i++)
    a[i] = i;

  int *deg = degree_monoid()->make_one();
  do
    {
      degree_monoid()->one(deg);

      for (int r=0; r<p; r++)
	degree_monoid()->mult(deg, degree(a[r]), deg);

      result->append(deg);
    }
  while (comb::increment(p, rk, a));

  degree_monoid()->remove(deg);
  deletearray(a);

  if (schreyer != NULL)
    result->schreyer = schreyer->exterior(p);
  return result;
}

static FreeModule *symm1_result = NULL;
static int *symm1_deg = NULL;

void FreeModule::symm1(int lastn,	     // can use lastn..rank()-1 in product
			int pow) const   // remaining power to take
{
  if (pow == 0)
    symm1_result->append(symm1_deg);
  else
    {
      for (int i=lastn; i<rank(); i++)
	{
	  // increase symm1_deg, with e_i
	  degree_monoid()->mult(symm1_deg, degree(i), symm1_deg);

	  symm1(i, pow-1);

	  // decrease symm1_deg back
	  degree_monoid()->divide(symm1_deg, degree(i), symm1_deg);
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
      
      symm1(0, n);
      
      degree_monoid()->remove(symm1_deg);
    }
  FreeModule *result = symm1_result;
  symm1_result = NULL;
  if (schreyer != NULL)
    result->schreyer = schreyer->symm(n);
  return result;
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
  if (schreyer != NULL) schreyer->text_out(o);
  o << ')';
}



// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
