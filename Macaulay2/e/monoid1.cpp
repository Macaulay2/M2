// Copyright 1996 Michael E. Stillman

#include <ctype.h>
#include "text_io.hpp"
#include "monoid.hpp"
#include "varpower.hpp"
#include "ntuple.hpp"
#include "../d/M2mem.h"

#include "polyring.hpp"

Monoid *Monoid::trivial_monoid = 0;

void Monoid::set_trivial_monoid_degree_ring(const PolynomialRing *DR)
{
  Monoid *M = get_trivial_monoid();
  M->moninfo->degree_ring = DR;
  M->moninfo->degree_monoid = M;
}

// ONLY to be called by PolyRing::get_trivial_poly_ring()

monoid_info::monoid_info()
: nvars(0),
  degree_ring(NULL),
  degree_monoid(NULL),
  _mo(0),
  mo(mon_order::trivial()),		// the trivial monomial order
  isgroup(1),
  use_packing(0)
{
  // varnames doesn't need to be set
  set_degrees();
}


monoid_info::monoid_info(MonomialOrdering *mo2, 
			 mon_order *mmo,
			 M2_stringarray s,
			 const PolynomialRing *deg_ring,
			 M2_arrayint degs)
  : nvars(rawNumberOfVariables(mo2)),
    varnames(s),
    degvals(degs),
    degree_ring(deg_ring),
    degree_monoid(deg_ring->Nmonoms()), 
    _mo(mo2), 
    mo(mmo)
{ 
  int ngroup = rawNumberOfInvertibleVariables(mo2);
    
  use_packing = (ngroup == 0);
  isgroup = (ngroup == nvars);
    
  set_degrees();
}

monoid_info::~monoid_info()
{
}

void monoid_info::set_degrees()
{
  if (degree_monoid == NULL)
    {
      degree_of_var.append(static_cast<const_monomial>(NULL));
      return;
    }

  // Set 'degree_of_var
  int degvars = degree_monoid->n_vars();
  int *t = degvals->array;

  primary_degree_of_var = makearrayint(nvars);

  if (degvars > 0)
    for (int i=0; i<nvars; i++)
      {
	monomial m = degree_monoid->make_one();
	degree_monoid->from_expvector(t, m);
	degree_of_var.append(m);
	primary_degree_of_var->array[i] = t[0];
	t += degvars;
      }
  else
    {
      for (int i=0; i<nvars; i++)
	primary_degree_of_var->array[i] = 1;
    }
  degree_of_var.append(degree_monoid->make_one());
}

Monoid::Monoid(monoid_info *moninf,  int nb)
{
  moninfo = moninf;
  nvars = moninfo->nvars;
  if (!moninfo->use_packing)
    {
      nbits = sizeof(int)*8;
      bit_mask = -1;
      mon_bound = (1 << (sizeof(int)*8-1));
      top_bits = 0;
    }
  else
    {
      nbits = nb;
      bit_mask = (1 << nbits) - 1;
      mon_bound = (1 << (nbits-1));
    }
  n_per_word = (sizeof(int) * 8) / nbits;
  npacked_words = (nvars+n_per_word-1)/n_per_word;
  nweights = moninfo->mo->n_weights();
  monomial_size_ = nweights + npacked_words;

  if (nvars != 0 && moninfo->use_packing)
    {
      top_bits = 0;
      for (int j=0; j<n_per_word; j++)
	top_bits = (top_bits << nbits) + (1 << (nbits-1));
    }

  if (moninfo->degree_monoid == NULL)
    moninfo->degree_monoid = const_cast<Monoid *>(this);

  EXP1 = newarray_atomic(int,nvars);
  EXP2 = newarray_atomic(int,nvars);
  EXP3 = newarray_atomic(int,nvars);
  MONlocal = newarray_atomic(int,nvars + monomial_size()); // MES: should be total number of words of result...
}

Monoid::~Monoid()
{
  deletearray(EXP1);
  deletearray(EXP2);
  deletearray(EXP3);
  deletearray(MONlocal);
  deleteitem(moninfo);  // This takes care of bump_down of degree monoid.
}

Monoid *Monoid::get_trivial_monoid()
{
  if (trivial_monoid == 0)
    trivial_monoid = new Monoid(new monoid_info, sizeof(int)*8);

  return trivial_monoid;
}

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

#if 0
  // Check that the first degree for each variable is positive
  if (eachdeg > 0)
    for (unsigned int i=0; i<nvars; i++)
      if (degs->array[i * eachdeg] <= 0)
	{
	  ERROR("All primary (first) degrees should be positive");
	  return 0;
	}
#endif

  // create internal monomial order
  mon_order *mmo = make_mon_order(mo);
  if (mmo == 0) return 0;
  monoid_info *moninf = new monoid_info(mo, mmo, names, deg_ring, degs);
  
  return new Monoid(moninf, 16);
}

Monoid *Monoid::tensor_product(const Monoid *M1, const Monoid *M2)
{
  int i,v;
  monoid_info *m1 = M1->moninfo;
  monoid_info *m2 = M2->moninfo;
  MonomialOrdering_array M12 = GETMEM(MonomialOrdering_array, sizeofarray(M12,2));
  M12->len = 2;
  M12->array[0] = m1->_mo;
  M12->array[1] = m2->_mo;
  MonomialOrdering *M = rawProductMonomialOrdering(M12);

  int n1 = M1->n_vars();
  int n2 = M2->n_vars();
  int n = n1+n2;
  M2_stringarray names = GETMEM(M2_stringarray, sizeofarray(names, n));
  names->len = n;
  for (i=0; i<n1; i++)
    names->array[i] = m1->varnames->array[i];

  for (i=0; i<n2; i++)
    names->array[n1+i] = m2->varnames->array[i];

  const PolynomialRing *DR = M1->get_degree_ring();
  int ndegs = DR->n_vars();

  M2_arrayint degs = makearrayint(ndegs*n);

  int next = 0;
  for (v=0; v<n1; v++)
    for (i=0; i<ndegs; i++)
      degs->array[next++] = m1->degvals->array[next++];

  for (v=0; v<n2; v++)
    for (i=0; i<ndegs; i++)
      degs->array[next++] = 1;

  return Monoid::create(M,names,DR,degs);
}

void Monoid::text_out(buffer &o) const
{
  int i;
  o << "[";
  for (i=0; i<nvars-1; i++)
    o << moninfo->varnames->array[i] << ",";
  if (nvars > 0)
    o << moninfo->varnames->array[nvars-1];

  o << "," << newline << "  Degrees => {";
  int ndegrees = degree_monoid()->monomial_size();
  if (ndegrees == 0)
      o << "}";
  else
    {
      for (i=0; i<nvars; i++)
	{
	  if (i != 0) o << ", ";
	  if (ndegrees > 1) o << '{';
	  for (int j=0; j<ndegrees; j++)
	    {
	      if (j != 0) o << ", ";
	      o << moninfo->degvals->array[i*ndegrees+j];
	    }
	  if (ndegrees > 1) o << '}';	  
	}
      o << "}";
    }

  if (moninfo->_mo != 0)
    {
      o << "," << newline << "  ";
      o << IM2_MonomialOrdering_to_string(moninfo->_mo);
    }

  o << newline << "  ]";
}

void Monoid::pack(const_exponents exp, monomial result) const
{
  for (int w=0; w<nweights; w++)
    *result++ = *exp++;
  int i = npacked_words-1;
  int n = 0;
  int this_word = 0;
  for (int k=nvars-1; k>=0; k--)
    {
      if (exp[k] >= mon_bound)
	{
	  ERROR("monomial overflow");
	  one(result);
	  return;
	}
      this_word += (exp[k] << n);
      n += nbits;
      if (n > (n_per_word-1) * nbits)
	{
	  n = 0;
	  result[i] = this_word;
	  this_word = 0;
	  i--;
	}
    }
  if (n > 0)
    result[i] = this_word;
}

void Monoid::unpack(const_monomial m, exponents result) const
{
  // WARNING: this ignores the weight vector values, and does NOT
  // place these into 'result'.
  m += nweights;
  int n = 0;
  int i = npacked_words - 1;
  int this_word = m[i];
  for (int k=nvars-1; k>=0; k--)
    {
      int newslot = (this_word >> n) & bit_mask;
      result[k] = newslot;
      n += nbits;
      if (n > (n_per_word-1) * nbits)
	{
	  n = 0;
	  this_word = m[--i];
	}
    }
}

void Monoid::from_expvector(const_exponents exp, monomial result) const
{
  if (nvars == 0) return;
  if (!moninfo->use_packing)
    {
      moninfo->mo->encode(exp, result);
    }
  else
    {
      moninfo->mo->encode(exp, MONlocal);
      pack(MONlocal, result);
    }
}

M2_arrayint Monoid::to_arrayint(const_monomial monom) const
{
  M2_arrayint result = makearrayint(n_vars());
  to_expvector(monom, result->array);
  return result;
}

void Monoid::to_expvector(const_monomial m, exponents result_exp) const
{
  if (nvars == 0) return;
  if (!moninfo->use_packing)
    {
      moninfo->mo->decode(m, result_exp);
    }
  else
    {
      unpack(m, MONlocal);
      moninfo->mo->decode(MONlocal, result_exp);
    }
}

void Monoid::mult(const_monomial m, const_monomial n, monomial result) const
{
#if 0
  for (int i=0; i<monomial_size(); i++, m++, n++)
    {
      *result++ = *m + *n;
      if (*m < 0)
	{
	  if (*n < 0 && *result > *m)
	    ERROR("monomial overflow");
	}
      else if (*m >= 0 && *n > 0 && *m < *result)
	{
	  ERROR("monomial overflow");
	}
    }
#endif
  for (int w=0; w<nweights; w++)
    *result++ = *m++ + *n++;
  // Now for the packed part
  for (int i=0; i<npacked_words; i++)
    {
      *result = *m++ + *n++;
      // Overflow is checked by using the bit mask
      if (((*result++) & top_bits) != 0)
	ERROR("monomial overflow");
    }
}
int Monoid::num_parts() const
{
  return nweights + nvars;
}
int Monoid::n_slots(int nparts) const
{
  int max_slots = nweights + nvars;
  if (nparts < 0) return max_slots;
  if (nparts > max_slots) return max_slots;
  return nparts;
}
bool Monoid::in_subring(int nslots, const_monomial m) const
{
  if (nvars == 0) return true;
  int rest = 0;
  int mwts;
  if (nslots <= nweights)
    mwts = nslots;
  else 
    {
      mwts = nweights;
      rest = nslots - nweights;
    }
  for (int w=0; w<mwts; w++)
    if (m[w] != 0) return false;
  if (rest > 0)
    {
      unpack(m, EXP1);
      for (int i=0; i<rest; i++)
	if (EXP1[i] != 0) return false;
    }
  return true;
}
int Monoid::compare(int nslots, const_monomial m1, const_monomial m2) const
{
  if (nvars == 0) return 1;
  int rest = 0;
  int mwts;
  if (nslots <= nweights)
    mwts = nslots;
  else 
    {
      mwts = nweights;
      rest = nslots - nweights;
    }
  for (int w=0; w<mwts; w++)
    if (m1[w] > m2[w]) return GT;
    else if (m1[w] < m2[w]) return LT;
  if (rest > 0)
    {
      unpack(m1, EXP1);
      unpack(m2, EXP2);
      for (int i=0; i<rest; i++)
	if (EXP1[i] > EXP2[i]) return GT; //-1;
	else if (EXP1[i] < EXP2[i]) return LT; //1;
    }
  return EQ;  //0;
}

monomial Monoid::make_new(const_monomial d) const
{
  if (nvars == 0) return NULL;
  monomial result = newarray_atomic(int,monomial_size());
  copy(d, result);
  return result;
}
monomial Monoid::make_one() const
{
  if (nvars == 0) return NULL;
  monomial result = newarray_atomic(int,monomial_size());
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
  //if (!moninfo->isgroup)
  if (true)
    {
      if (nvars == 0) return true;
      to_expvector(m, EXP1);
      to_expvector(n, EXP2);
      return ntuple::divides(nvars, EXP1, EXP2);
    }
  else return true;   // If this is a group, then m always divides n.
}

#if 0
void Monoid::divide(const_monomial m, const_monomial n, monomial result) const
{
  if (nvars == 0) return;
  to_expvector(m, EXP1);
  to_expvector(n, EXP2);
  ntuple::divide(nvars, EXP1, EXP2, EXP1);
  from_expvector(EXP1, result);
}
#endif

void Monoid::power(const_monomial m, int n, monomial result) const
{
  if (nvars == 0) return;
  to_expvector(m, EXP1);
  ntuple::power(nvars, EXP1, n, EXP1);
  from_expvector(EXP1, result);
}

void Monoid::monsyz(const_monomial m, const_monomial n, monomial sm, monomial sn) const
{
  if (nvars == 0) return;
  to_expvector(m, EXP1);
  to_expvector(n, EXP2);
  for (int i=0; i<nvars; i++)
      if (EXP1[i] > EXP2[i])
	{
	  EXP2[i] = EXP1[i] - EXP2[i];
	  EXP1[i] = 0;
	}
      else
	{
	  EXP1[i] = EXP2[i] - EXP1[i];
	  EXP2[i] = 0;
	}
  from_expvector(EXP1, sm);
  from_expvector(EXP2, sn);
}

void Monoid::gcd(const_monomial m, const_monomial n, monomial p) const
{
  if (nvars == 0) return;
  to_expvector(m, EXP1);
  to_expvector(n, EXP2);
  ntuple::gcd(nvars, EXP1, EXP2, EXP1);
  from_expvector(EXP1, p);
}

void Monoid::lcm(const_monomial m, const_monomial n, monomial p) const
{
  if (nvars == 0) return;
  to_expvector(m, EXP1);
  to_expvector(n, EXP2);
  ntuple::lcm(nvars, EXP1, EXP2, EXP1);
  from_expvector(EXP1, p);
}

void Monoid::elem_text_out(buffer &o, const_monomial m) const
{
  to_expvector(m, EXP1);
  ntuple::elem_text_out(o, nvars, EXP1, moninfo->varnames);
}

void Monoid::multi_degree(const_monomial m, monomial result) const
{
  if (nvars == 0) return;
  if (degree_monoid()->n_vars() == 0) return;

  degree_monoid()->one(result);
  monomial mon1 = degree_monoid()->make_one();
  to_expvector(m, EXP1);

  for (int i=0; i<nvars; i++)
    if (EXP1[i] > 0)
      {
	degree_monoid()->power(degree_of_var(i), EXP1[i], mon1);
	degree_monoid()->mult(result, mon1, result);
      }
  degree_monoid()->remove(mon1);
}

void Monoid::degree_of_varpower(const_varpower vp, monomial result) const
{
  if (nvars == 0) return;
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
  if (nvars == 0) return 0;
  to_expvector(m, EXP1);
  int result = EXP1[0];
  return result;
}

int Monoid::primary_degree(const_monomial m) const
{
  return degree_weights(m, primary_degree_of_vars());
}

int Monoid::degree_weights(const_monomial m, M2_arrayint wts) const
{
  if (nvars == 0) return 0;
  to_expvector(m, EXP1);
  int sz = (wts->len < nvars ? wts->len : nvars);
  return ntuple::weight(sz, EXP1_, wts);
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
  return is_group();
}

void Monoid::from_varpower(const_varpower vp, monomial result) const
{
  intarray a;
  varpower::to_ntuple(nvars, vp, a);
  from_expvector(a.raw(), result);
}

void Monoid::to_varpower(const_monomial m, intarray &result_vp) const
{
  to_expvector(m, EXP1);
  varpower::from_ntuple(nvars, EXP1, result_vp);
}


// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
