// Copyright 1996 Michael E. Stillman

#include <ctype.h>
#include "text_io.hpp"
#include "monoid.hpp"
#include "varpower.hpp"
#include "ntuple.hpp"
#include "../d/M2mem.h"

#include "polyring.hpp"

Monoid *Monoid::trivial_monoid = 0;


// ONLY to be called by PolyRing::get_trivial_poly_ring()
void Monoid::set_trivial_monoid_degree_ring(const PolynomialRing *DR)
{
  Monoid *M = get_trivial_monoid();
  M->degree_ring_ = DR;
  M->degree_monoid_ = M;
}

Monoid::Monoid()
  :  nvars_(0),
     varnames_(0),
     degvals_(0),
     degree_ring_(0), // will be set later
     degree_monoid_(0), // will be set later
     mo_(0),
     monorder_(0),
     monomial_size_(0),
     monomial_bound_(0),
     EXP1_(0),
     EXP2_(0),
     EXP3_(0),
     MONlocal_(0)
{
}

#if 0

static mon_order *make_mon_order(MonomialOrdering *mo)
{
  unsigned int nvars = rawNumberOfVariables(mo);
  unsigned int nwt_blocks = 0;
  unsigned int first_non_weight = 0;
  unsigned int last_non_component = 0;

  bool prev_is_wt = true;
  for (unsigned int i=0; i<mo->len; i++)
    {
      mon_part p = mo->array[i];
      if (prev_is_wt)
	{
	  if (p->type == MO_WEIGHTS)
	    nwt_blocks++;
	  else
	    {
	      first_non_weight = i;
	      prev_is_wt = false;
	    }
	}
      if (p->type != MO_POSITION_UP && p->type != MO_POSITION_DOWN)
	last_non_component = i;
    }

  // Now make the weight vector values, and grab the degrees
  unsigned int nweights = nvars * nwt_blocks;
  M2_arrayint wts = makearrayint(nweights);
  M2_arrayint degs = makearrayint(nvars);

  unsigned int nextwt = 0;
  unsigned next = 0;
  for (unsigned int i=0; i<mo->len; i++)
    {
      mon_part p = mo->array[i];
      if (p->wts == 0)
	{
	  for (int j=0; j<p->nvars; j++)
	    degs->array[next++] = 1;
	}
      else if (p->type == MO_WEIGHTS && i < nwt_blocks)
	{
	  for (int j=0; static_cast<unsigned>(j) < nvars; j++)
	    {
	      if (j < p->nvars)
		wts->array[nextwt++] = p->wts[j];
	      else
		wts->array[nextwt++] = 0;
	    }
	}
      else
	// Should only be one of the grevlex weights blocks
	{
	  for (int j=0; j<p->nvars; j++)
	    degs->array[next++] = p->wts[j];;
	}
    }

  // Check: either there is only one here, or there are several grevlex...
  if (first_non_weight == last_non_component)
    {
      // Only one block, not a product order
      switch (mo->array[first_non_weight]->type) {
      case MO_LEX:
      case MO_LEX2:
      case MO_LEX4:
      case MO_LAURENT:
	return mon_order::lex(degs,wts);
      case MO_GREVLEX:
      case MO_GREVLEX2:
      case MO_GREVLEX4:
      case MO_GREVLEX_WTS:
      case MO_GREVLEX2_WTS:
      case MO_GREVLEX4_WTS:
	return mon_order::grlex(degs,wts);
      case MO_REVLEX:
	return mon_order::rlex(degs,wts);
      case MO_NC_LEX:
	ERROR("noncommutative monomials are not yet implemented");
	return 0;
      default:
	ERROR("internal error in monorder construction");
	return 0;
      }
    }

  // Check that every part is a grevlex type
  M2_arrayint blocks = makearrayint(last_non_component - first_non_weight + 1);
  next = 0;
  for (unsigned int i=first_non_weight; i<=last_non_component; i++)
    {
      mon_part p = mo->array[i];
      if (p->type == MO_GREVLEX ||
	  p->type == MO_GREVLEX2 ||
	  p->type == MO_GREVLEX4 ||
	  p->type == MO_GREVLEX_WTS ||
	  p->type == MO_GREVLEX2_WTS ||
	  p->type == MO_GREVLEX4_WTS)
	{
	  blocks->array[next++] = p->nvars;
	}
      else
	{
	  ERROR("cannot currently handle products of non GRevLex orders");
	  return 0;
	}
    }
  return mon_order::product(degs,blocks,wts);
}
#endif


Monoid::~Monoid()
{
  deletearray(EXP1_);
  deletearray(EXP2_);
  deletearray(EXP3_);
  deletearray(MONlocal_);
}

Monoid *Monoid::get_trivial_monoid()
{
  if (trivial_monoid == 0)
    trivial_monoid = new Monoid;

  return trivial_monoid;
}

Monoid *Monoid::create(MonomialOrdering *mo,
		       M2_stringarray names,
		       const PolynomialRing *deg_ring,
		       M2_arrayint degs)
{
  unsigned int nvars = rawNumberOfVariables(mo);;
  unsigned int eachdeg = deg_ring->n_vars();
  if (degs->len != nvars * eachdeg)
    {
      ERROR("degree list should be of length %d", nvars * eachdeg);
      return 0;
    }
  if (nvars != names->len)
    {
      ERROR("expected %d variable names", nvars);
      return 0;
    }

  return new Monoid(mo,names,deg_ring,degs);
}

Monoid::Monoid(MonomialOrdering *mo,
	       M2_stringarray names,
	       const PolynomialRing *deg_ring,
	       M2_arrayint degs)
  :  nvars_(rawNumberOfVariables(mo)),
     varnames_(names),
     degvals_(degs),
     degree_ring_(deg_ring),
     degree_monoid_(deg_ring->getMonoid()),
     mo_(mo)
{

  monorder_ = monomialOrderMake(mo);

  monomial_size_ = monorder_->nslots;
  n_before_component_ = monorder_->nslots_before_component;
  n_after_component_ = monomial_size_ - n_before_component_;
  component_up_ = monorder_->component_up;

  // Now we can set nslots_
  int total = 0;
  for (int i=0; i<monorder_->nblocks; i++)
    {
      total += monorder_->blocks[i].nslots;
      nslots_.push_back(total);
    }

  EXP1_ = newarray(int,nvars_);
  EXP2_ = newarray(int,nvars_);
  EXP3_ = newarray(int,nvars_);
  MONlocal_ = newarray(int,nvars_ + monomial_size()); // MES: should be total number of words of result...

  n_invertible_vars_ = rawNumberOfInvertibleVariables(mo_);

  set_degrees();
}

void Monoid::set_degrees()
{
  if (degree_monoid_ == NULL)
    {
      degree_of_var_.append(static_cast<const_monomial>(NULL));
      return;
    }

  // Set 'degree_of_var
  int degvars = degree_monoid_->n_vars();
  int *t = degvals_->array;

  primary_degree_of_var_ = makearrayint(nvars_);

  if (degvars > 0)
    for (int i=0; i<nvars_; i++)
      {
	monomial m = degree_monoid_->make_one();
	degree_monoid_->from_expvector(t, m);
	degree_of_var_.append(m);
	primary_degree_of_var_->array[i] = t[0];
	t += degvars;
      }
  else
    {
      for (int i=0; i<nvars_; i++)
	primary_degree_of_var_->array[i] = 1;
    }
  degree_of_var_.append(degree_monoid_->make_one());
}

Monoid *Monoid::tensor_product(const Monoid *M1, const Monoid *M2)
{
  int i,v;
  MonomialOrdering_array M12 = GETMEM(MonomialOrdering_array, sizeofarray(M12,2));
  M12->len = 2;
  M12->array[0] = M1->mo_;
  M12->array[1] = M2->mo_;
  MonomialOrdering *M = rawProductMonomialOrdering(M12);

  int n1 = M1->n_vars();
  int n2 = M2->n_vars();
  int n = n1+n2;
  M2_stringarray names = GETMEM(M2_stringarray, sizeofarray(names, n));
  names->len = n;
  for (i=0; i<n1; i++)
    names->array[i] = M1->varnames_->array[i];

  for (i=0; i<n2; i++)
    names->array[n1+i] = M2->varnames_->array[i];

  const PolynomialRing *DR = M1->get_degree_ring();
  int ndegs = DR->n_vars();

  M2_arrayint degs = makearrayint(ndegs*n);

  int next = 0;
  for (v=0; v<n1; v++)
    for (i=0; i<ndegs; i++)
      degs->array[next++] = M1->degvals_->array[next++];

  for (v=0; v<n2; v++)
    for (i=0; i<ndegs; i++)
      degs->array[next++] = 1;

  return Monoid::create(M,names,DR,degs);
}

void Monoid::text_out(buffer &o) const
{
  int i;
  o << "[";
  for (i=0; i<nvars_-1; i++)
    o << varnames_->array[i] << ",";
  if (nvars_ > 0)
    o << varnames_->array[nvars_-1];

  o << "," << newline << "  Degrees => {";
  int ndegrees = degree_monoid()->monomial_size();
  if (ndegrees == 0)
      o << "}";
  else
    {
      for (i=0; i<nvars_; i++)
	{
	  if (i != 0) o << ", ";
	  if (ndegrees > 1) o << '{';
	  for (int j=0; j<ndegrees; j++)
	    {
	      if (j != 0) o << ", ";
	      o << degvals_->array[i*ndegrees+j];
	    }
	  if (ndegrees > 1) o << '}';	  
	}
      o << "}";
    }

  if (mo_ != 0)
    {
      o << "," << newline << "  ";
      o << IM2_MonomialOrdering_to_string(mo_);
    }

  o << newline << "  ]";
}

void Monoid::encode(const_exponents exp, monomial result) const
{
  monomialOrderEncode(monorder_, exp, result);
}

void Monoid::decode(const_monomial m, exponents result) const
{
  monomialOrderDecode(monorder_, m, result);
}

void Monoid::from_expvector(const_exponents exp, monomial result) const
{
  monomialOrderFromActualExponents(monorder_, exp, EXP1_);
  encode(EXP1_, result);
}

M2_arrayint Monoid::to_arrayint(const_monomial monom) const
{
  M2_arrayint result = makearrayint(n_vars());
  to_expvector(monom, result->array);
  return result;
}

void Monoid::to_expvector(const_monomial m, exponents result_exp) const
{
  decode(m, EXP1_);
  monomialOrderToActualExponents(monorder_, EXP1_, result_exp);
}

void Monoid::mult(const_monomial m, const_monomial n, monomial result) const
{
  for (int i=0; i<monomial_size_; i++)
    {
      int x = *m++;
      int y = *n++;
      int z = x+y;
      *result++ = z;
      // Check for overflow
      if ((x < 0) == (y < 0) && (x < 0) != (z < 0))
	{
	  ERROR("monomial overflow");
	  fprintf(stderr, "monomial overflow has occurred\n");
	}
    }
}

int Monoid::num_parts() const
{
  return monorder_->nblocks;
}

int Monoid::n_slots(int nparts) const
{
  if (nparts < 0) return monomial_size();
  if (nparts >= num_parts()) nparts = num_parts()-1;
  return nslots_[nparts];
}

bool Monoid::in_subring(int nslots, const_monomial m) const
{
  for (int i=0; i<nslots; i++)
    if (*m++) return false;
  return true;
}

int Monoid::compare(int nslots, const_monomial m, const_monomial n) const
{
  int i = nslots;
  if (i == 0) return EQ;
  while (1)
    {
      if (*m > *n) return GT;
      if (*m < *n) return LT;
      if (--i == 0) return EQ;
      m++, n++;
    }
}

int Monoid::compare(const_monomial m, const_monomial n) const
{
  int i = monomial_size_;
  if (i == 0) return EQ;
  while (1)
    {
      if (*m > *n) return GT;
      if (*m < *n) return LT;
      if (--i == 0) return EQ;
      m++, n++;
    }
}

int Monoid::compare(const_monomial m, int mcomp, const_monomial n, int ncomp) const
{
  for (int i= n_before_component_; i>0; --i)
    {
      if (*m > *n) return GT;
      if (*m < *n) return LT;
      m++, n++;
    }
  bool up = component_up_;
  if (up)
    {
      if (mcomp < ncomp) return LT;
      if (mcomp > ncomp) return GT;
    }
  else
    {
      if (mcomp < ncomp) return GT;
      if (mcomp > ncomp) return LT;
    }
  for (int i= n_after_component_; i>0; --i)
    {
      if (*m > *n) return GT;
      if (*m < *n) return LT;
      m++, n++;
    }
  return EQ;
}

monomial Monoid::make_new(const_monomial d) const
{
  if (nvars_ == 0) return NULL;
  monomial result = newarray(int,monomial_size());
  copy(d, result);
  return result;
}
monomial Monoid::make_one() const
{
  if (nvars_ == 0) return NULL;
  monomial result = newarray(int,monomial_size());
  one(result);
  return result;
}
void Monoid::remove(monomial d) const
{
#if 0
  deletearray(d);
#endif
}

void Monoid::one(monomial result) const
{
  for (int i=0; i<monomial_size(); i++) 
    *result++ = 0;
}

void Monoid::copy(const_monomial m, monomial result) const
{
  memcpy(result, m, monomial_size()*sizeof(int));
}

bool Monoid::divides(const_monomial m, const_monomial n) const
// Does m divide n?
{
  if (nvars_ == 0) return true;
  to_expvector(m, EXP1_);
  to_expvector(n, EXP2_);
  return ntuple::divides(nvars_, EXP1_, EXP2_);
}

#if 0
void Monoid::divide(const_monomial m, const_monomial n, monomial result) const
{
  if (nvars_ == 0) return;
  to_expvector(m, EXP1_);
  to_expvector(n, EXP2_);
  ntuple::divide(nvars_, EXP1_, EXP2_, EXP1_);
  from_expvector(EXP1_, result);
}
#endif

void Monoid::power(const_monomial m, int n, monomial result) const
{
  if (nvars_ == 0) return;
  to_expvector(m, EXP1_);
  ntuple::power(nvars_, EXP1_, n, EXP1_);
  from_expvector(EXP1_, result);
}

void Monoid::monsyz(const_monomial m, const_monomial n, monomial sm, monomial sn) const
{
  if (nvars_ == 0) return;
  to_expvector(m, EXP1_);
  to_expvector(n, EXP2_);
  for (int i=0; i<nvars_; i++)
      if (EXP1_[i] > EXP2_[i])
	{
	  EXP2_[i] = EXP1_[i] - EXP2_[i];
	  EXP1_[i] = 0;
	}
      else
	{
	  EXP1_[i] = EXP2_[i] - EXP1_[i];
	  EXP2_[i] = 0;
	}
  from_expvector(EXP1_, sm);
  from_expvector(EXP2_, sn);
}

void Monoid::gcd(const_monomial m, const_monomial n, monomial p) const
{
  if (nvars_ == 0) return;
  to_expvector(m, EXP1_);
  to_expvector(n, EXP2_);
  ntuple::gcd(nvars_, EXP1_, EXP2_, EXP1_);
  from_expvector(EXP1_, p);
}

void Monoid::lcm(const_monomial m, const_monomial n, monomial p) const
{
  if (nvars_ == 0) return;
  to_expvector(m, EXP1_);
  to_expvector(n, EXP2_);
  ntuple::lcm(nvars_, EXP1_, EXP2_, EXP1_);
  from_expvector(EXP1_, p);
}

void Monoid::elem_text_out(buffer &o, const_monomial m) const
{
  to_expvector(m, EXP1_);
  ntuple::elem_text_out(o, nvars_, EXP1_, varnames_);
}

void Monoid::multi_degree(const_monomial m, monomial result) const
{
  if (nvars_ == 0) return;
  if (degree_monoid()->n_vars() == 0) return;

  degree_monoid()->one(result);
  monomial mon1 = degree_monoid()->make_one();
  to_expvector(m, EXP1_);

  for (int i=0; i<nvars_; i++)
    if (EXP1_[i] > 0)
      {
	degree_monoid()->power(degree_of_var(i), EXP1_[i], mon1);
	degree_monoid()->mult(result, mon1, result);
      }
  degree_monoid()->remove(mon1);
}

void Monoid::degree_of_varpower(const_varpower vp, monomial result) const
{
  if (nvars_ == 0) return;
  if (degree_monoid()->n_vars() == 0) return;

  degree_monoid()->one(result);
  monomial mon1 = degree_monoid()->make_one();

  for (index_varpower j = vp; j.valid(); ++j)
      {
	int v = j.var();
	int e = j.exponent();
	degree_monoid()->power(degree_of_var(v), e, mon1);
	degree_monoid()->mult(result, mon1, result);
      }
  degree_monoid()->remove(mon1);
}

int Monoid::primary_value(const_monomial m) const
{
  // MES: rewrite!!
  if (nvars_ == 0) return 0;
  to_expvector(m, EXP1_);
  int result = EXP1_[0];
  return result;
}

int Monoid::primary_degree(const_monomial m) const
{
  return degree_weights(m, primary_degree_of_vars());
}

int Monoid::degree_weights(const_monomial m, const M2_arrayint wts) const
{
  if (nvars_ == 0) return 0;
  to_expvector(m, EXP1_);
  return ntuple::weight(nvars_, EXP1_, wts);
}

bool Monoid::is_one(const_monomial m) const
{
  for (int i=0; i<monomial_size(); i++)
    if (*m++ != 0) return false;
  return true;
}

bool Monoid::is_invertible(const_monomial m) const
// is every variable that occurs 
// in 'm' allowed to be negative?
{
  if (n_invertible_vars_ == 0)
    {
      // Only the trivial monomial is invertible in this case
      return is_one(m);
    }
  to_expvector(m, EXP1_);
  for (int i=0; i<nvars_; i++)
    if (!monorder_->is_laurent[i] && EXP1_[i] > 0)
      return false;
  return true;
}

void Monoid::from_varpower(const_varpower vp, monomial result) const
{
  intarray a;
  varpower::to_ntuple(nvars_, vp, a);
  from_expvector(a.raw(), result);
}

void Monoid::to_varpower(const_monomial m, intarray &result_vp) const
{
  to_expvector(m, EXP1_);
  varpower::from_ntuple(nvars_, EXP1_, result_vp);
}


// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
