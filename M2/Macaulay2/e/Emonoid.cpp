// Copyright 1998 by Michael E. Stillman
#include <string.h>
#include <ctype.h>
#include "Emonoid.hpp"
#include "Emontable.hpp"
#include "random.hpp"

ECommMonoid *ECommMonoid::_trivial = 0;

const ECommMonoid *ECommMonoid::getTrivialMonoid()
{
  if (_trivial == 0)
    {
      // We make a commutative monoid with no variables, by hand.
      EMonomialOrder *mo = EMonomialOrder::make();
      _trivial = ECommMonoid::make(mo,0,0);
    }
  return _trivial;
}

char ** EMonoid::setNames(int nvars, const char *s, int slength)
{
  char thisstr[100];
  char *current;
  current = thisstr;
  char **result = new char *[nvars];
  int thisvar = 0;

  for (int i=0; i<=slength; i++) 
    {
      if (thisvar >= nvars) break;
      if (!isspace(s[i]) && i<slength) 
	*current++ = s[i];
      else 
	{
	  if (current == thisstr) continue;
	  *current++ = '\0';
	  result[thisvar] = new char[current - thisstr];
	  strcpy(result[thisvar], thisstr);
	  current = thisstr;
	  thisvar++;
	}
    }
  for ( ; thisvar<nvars; thisvar++)
    {
      char *v = new char[1];
      *v = '\0';
      result[thisvar] = v;
    }
  return result;
}

EMonoid::EMonoid(EMonomialOrder *mo, 
		 const int *print, 
		 const char **names)
  : nvars(mo->n_vars()),
    componentloc(mo->component_loc()),
    nslots(mo->n_slots()),
    monorder(mo),
    print_order(0),
    var_names(0),
    _one(0)
{
  if (nvars == 0) return;
  
  int i;
  print_order = new int[nvars];
  for (i=0; i<nvars; i++)
    print_order[i] = print[i];

  var_names = new char *[nvars];
  for (i=0; i<nvars; i++)
    {
      int len = strlen(names[i]);
      var_names[i] = new char[len+1];
      strncpy(var_names[i], names[i], len+1);
    }

  isnonnegativevar = new bool[nvars];
  for (i=0; i<nvars; i++)
    isnonnegativevar[i] = mo->isNonnegativeVariable(i);
    
  // Before using this class:
  // Make a lookup table for monomials.
  // Create _one. (done in the routine ECommMonoid::make, ENCMonoid::make)
}

EMonoid::~EMonoid()
{
  // Start deleting everything
  delete [] print_order;
  for (int i=0; i<nvars; i++)
    delete [] var_names[i];
  delete [] var_names;
  delete [] isnonnegativevar;
}

int EMonoid::degree(const monomial *a, const int *wts) const
{
  const int *exp = a->exponents;
  int result = 0;
  for (int i=0; i<nvars; i++)
    result += exp[i] * wts[i];
  return result;
}

ECommMonoid::ECommMonoid(EMonomialOrder *mo, 
	       const int *print, 
	       const char **names)
  : EMonoid(mo,print,names),
    MULT_exp(0)
{
  if (nvars == 0) return;
  MULT_exp = new int[nvars];

  hash_multiplier = new uint32[nvars];
  for (int i=0; i<nvars; i++)
    hash_multiplier[i] = (uint32) Random::random0();

  // Before using this class:
  // Make a lookup table for monomials.
  // Create _one. (done in the routine make_Monoid)
}

ECommMonoid *ECommMonoid::make(EMonomialOrder *mo, 
		     const int *print, 
		     const char **names)
{
  ECommMonoid *M = new ECommMonoid(mo,print,names);
  EMonomialTable *T = new EMonomialTable(M,MONOMIAL_HASH_SIZE);
  M->T = T;
  for (int i=0; i<M->nvars; i++) M->MULT_exp[i] = 0;
  M->_one = M->monomial_from_exponents(M->MULT_exp);
  return M;
}

ECommMonoid::~ECommMonoid()
{
  delete [] MULT_exp;
  delete [] hash_multiplier;
  // Now delete all of the monomials
  // MES
}

const int * ECommMonoid::to_exponents(const monomial *m) const
{
  return m->exponents;
}

void ECommMonoid::copy_exponents(const monomial *m, int *&result) const
{
  for (int i=0; i<nvars; i++)
    result[i] = m->exponents[i];
}

monomial *ECommMonoid::unchecked_monomial_from_exponents(const int *exp) const
{
  return T->lookup_and_insert_commutative(exp);
}
monomial *ECommMonoid::monomial_from_exponents(const int *exp) const
{
  // First make sure that each exponent is >= 0, if it needs to be...
  for (int i=0; i<nvars; i++)
    if (isnonnegativevar[i] && exp[i] < 0) 
      {
        gError << "expected non-negative exponent";
        return 0;
      }
  return unchecked_monomial_from_exponents(exp);
}

void ECommMonoid::to_variable_exponent_pairs(const monomial *m, intarray &result) const
{
  const int *exp = m->exponents;
  for (int i=0; i<nvars; i++)
    if (exp[i] != 0)
      {
	result.append(i);
	result.append(exp[i]);
      }
}
monomial *ECommMonoid::monomial_from_variable_exponent_pairs(const intarray &term) const
{
  int *exp = new int[nvars];
  for (int i=0; i<nvars; i++)
    exp[i] = 0;
  for (int j = 0; j < term.length()-1; j += 2)
    {
      int v = term[j];
      int e = term[j+1];
      if (v < 0 || v >= nvars)
	{
	  gError << "variable out of range";
	  delete [] exp;
	  return 0;
	}
      exp[v] += e;
    }
  return monomial_from_exponents(exp);
}
uint32 ECommMonoid::hash_exponents(const int *exponents) const
{
  // !! TUNE THIS !!
  uint32 result = 0;
  for (int i=0; i<nvars; i++)
    if (exponents[i] != 0) 
      result += exponents[i] * hash_multiplier[i];
  return result;
}

monomial *ECommMonoid::mult(const monomial *m, const monomial *n) const
{
  const int *m1 = m->exponents;
  const int *n1 = n->exponents;
  for (int i=0; i<nvars; i++)
    MULT_exp[i] = *m1++ + *n1++;
  return unchecked_monomial_from_exponents(MULT_exp);
}

int ECommMonoid::compare(const monomial *m, int mcomponent,
			 const monomial *n, int ncomponent) const
{
  int i,cmp;
  if (m == n) {
    cmp = mcomponent - ncomponent;
    if (cmp < 0) return LT;
    if (cmp > 0) return GT;
    return EQ;
  }
  const int *m1 = get_partial_sums(m);
  const int *n1 = get_partial_sums(n);
  m1 += nslots;
  n1 += nslots;
  for (i=nslots-1; i>=componentloc; i--) {
    cmp = *--m1 - *--n1;
    if (cmp < 0) return LT;
    if (cmp > 0) return GT;
  }
  cmp = mcomponent - ncomponent;
  if (cmp < 0) return LT;
  if (cmp > 0) return GT;
  for ( ; i>=0; i--) {
    cmp = *--m1 - *--n1;
    if (cmp < 0) return LT;
    if (cmp > 0) return GT;
  }
  return EQ;  // Should never get to here...
}
int ECommMonoid::compare(const monomial *a,
			 const monomial *b,
			 int n) const
{
  const int *a1 = get_partial_sums(a);
  const int *b1 = get_partial_sums(b);
  int m = monorder->n_slots(n);
  a1 += nslots;
  b1 += nslots;
  for (int i=0; i<m; i++)
    {
      int cmp = *--a1 - *--b1;
      if (cmp < 0) return LT;
      if (cmp > 0) return GT;
    }
  return EQ;
}


monomial *ECommMonoid::lcm(const monomial *m, const monomial *n) const
{
  const int *e1 = m->exponents;
  const int *e2 = n->exponents;
  for (int i=0; i<nvars; i++)
    if (e1[i] > e2[i])
      MULT_exp[i] = e1[i];
    else
      MULT_exp[i] = e2[i];
  return unchecked_monomial_from_exponents(MULT_exp);
}

monomial *ECommMonoid::gcd(const monomial *m, const monomial *n) const
{
  const int *e1 = m->exponents;
  const int *e2 = n->exponents;
  for (int i=0; i<nvars; i++)
    if (e1[i] < e2[i])
      MULT_exp[i] = e1[i];
    else
      MULT_exp[i] = e2[i];
  return unchecked_monomial_from_exponents(MULT_exp);
}
#if 0
monomial *ECommMonoid::divide(const monomial *m, const monomial *n) const
  // m/n
{
  const int *e1 = m->exponents;
  const int *e2 = n->exponents;
  for (int i=0; i<nvars; i++)
    if (e1[i] < e2[i])
      MULT_exp[i] = 0;
    else
      MULT_exp[i] = e1[i] - e2[i];
  return unchecked_monomial_from_exponents(MULT_exp);
}
#endif
monomial *ECommMonoid::divide(const monomial *m, const monomial *n) const
  // m/n  WARNING: only use if there exists a monomial r such that m = rn.
  // Otherwise, the result will be NOT valid...
{
  const int *e1 = m->exponents;
  const int *e2 = n->exponents;
  for (int i=0; i<nvars; i++)
      MULT_exp[i] = e1[i] - e2[i];
  return unchecked_monomial_from_exponents(MULT_exp);
}

bool ECommMonoid::divides(const monomial *m, const monomial *n) const
  // does m divide n?
{
  const int *e1 = m->exponents;
  const int *e2 = n->exponents;
  for (int i=0; i<nvars; i++)
    if (e1[i] > e2[i])
      return false;
  return true;
}

void ECommMonoid::stats() const
{
  T->stats();
}
//////////////
// ENCMonoid //
//////////////

ENCMonoid::ENCMonoid(EMonomialOrder *mo, 
	       const int *print, 
	       const char **names)
  : EMonoid(mo,print,names),
    MULT_exp(0)
{
  MULT_exp = new int[nvars+1000];
  mo->set_noncommutative_parameters(
         this->n_nc_blocks,
	 this->nclength,
	 this->is_nc_block,
	 this->is_comm);

  // Before using this class:
  // Make a lookup table for monomials.
  // Create _one. (done in the routine make_Monoid)
}

ENCMonoid *ENCMonoid::make(EMonomialOrder *mo, 
		     const int *print, 
		     const char **names)
{
  ENCMonoid *M = new ENCMonoid(mo,print,names);
  EMonomialTable *T = new EMonomialTable(M,MONOMIAL_HASH_SIZE);
  M->T = T;
  for (int i=0; i<M->nvars; i++) M->MULT_exp[i] = 0;
  M->_one = M->monomial_from_exponents(M->MULT_exp);
  return M;
}

ENCMonoid::~ENCMonoid()
{
  delete [] MULT_exp;

  delete [] nclength;
  delete [] is_nc_block;

  // Now delete all of the monomials
  // MES
}

int ENCMonoid::encoded_length(const int *encoded) const
{
  int result = nslots;
  for (int j=0; j<n_nc_blocks; j++)
    result += encoded[nclength[j]];
  return result;
}

monomial *ENCMonoid::monomial_from_exponents(const int *exp) const
{
  
  monorder->encode(exp,MULT_exp);
  return T->lookup_and_insert_noncomm_encoded(MULT_exp);
}

monomial *ENCMonoid::monomial_from_encoded(const int *encoded) const
{
  return T->lookup_and_insert_noncomm_encoded(encoded);
}

void ENCMonoid::to_variable_exponent_pairs(const monomial *m, intarray &result) const
{
  // WRITE THIS!!
}

monomial *ENCMonoid::monomial_from_variable_exponent_pairs(const intarray &term) const
{
  int *exp = new int[nvars];
  for (int i=0; i<nvars; i++)
    exp[i] = 0;
  monomial *result = one();
  for (int j = 0; j < term.length()-1; j += 2)
    {
      int v = term[j];
      int e = term[j+1];
      if (v < 0 || v >= nvars)
	{
	  gError << "variable out of range";
	  delete [] exp;
	  return 0;
	}
      exp[v] = e;
      monomial *m = monomial_from_exponents(exp);
      if (m == 0) 
	{
	  delete [] exp;
	  return 0;
	}
      result = mult(result,m);
    }
  delete [] exp;
  return result;
}

void ENCMonoid::copy_exponents(const monomial *m, int *&result) const
{
  for (int i=0; i<nvars; i++)
    result[i] = m->exponents[i];
}

const int * ENCMonoid::to_exponents(const monomial *m) const
{
  return m->exponents;
}

monomial *ENCMonoid::mult(const monomial *m, const monomial *n) const
{
  int i,j,k;
  const int *m1 = get_partial_sums(m);
  const int *n1 = get_partial_sums(n);
  for (i=0; i<nslots; i++)
    MULT_exp[i] = m1[i] + n1[i];
  // Now multiply the non-commutative part
  int firstpast = nslots;
  int mpast = nslots;
  int npast = nslots;
  for (j=0; j<n_nc_blocks; j++)
    {
      for (k=0; k<m1[nclength[j]]; k++)
	MULT_exp[firstpast++] = m1[mpast++];
      for (k=0; k<n1[nclength[j]]; k++)
	MULT_exp[firstpast++] = n1[npast++];
    }      
  return monomial_from_encoded(MULT_exp);
}

monomial *ENCMonoid::divide(const monomial *m, const monomial *n) const
{
  gError << "division not implemented for non-comm monoids";
  return clone(one());
}
int ENCMonoid::compare(const monomial *m, int mcomponent,
		       const monomial *n, int ncomponent) const
{
  // THIS IS ALL WRONG...
  int cmp;
  if (m == n) {
    cmp = mcomponent - ncomponent;
    if (cmp < 0) return LT;
    if (cmp > 0) return GT;
    return EQ;
  }
  const int *m1 = get_partial_sums(m);
  const int *n1 = get_partial_sums(n);
  int past = nslots;
  for (int i=0; i<nslots; i++) {
    if (i == componentloc)
      {
	cmp = mcomponent - ncomponent;
	if (cmp < 0) return LT;
	if (cmp > 0) return GT;
      }
    cmp = m1[i] - n1[i];
    if (cmp < 0) return LT;
    if (cmp > 0) return GT;
    if (is_nc_block[i])
      {
	// Need to check the NC block of vars
	for (int j=0; j<m1[i]; j++)
	  {
	    cmp = m1[past] - n1[past];
	    if (cmp < 0) return GT;
	    if (cmp > 0) return LT;
	    past++;
	  }
      }
  }
  return EQ;  // Should never get to here...
}

int ENCMonoid::compare(const monomial *a,
			 const monomial *b,
			 int n) const
{
  // WRITE THIS!!
  return EQ;
}

uint32 ENCMonoid::hash_encoded(const int *encoded) const
{
  int len = encoded_length(encoded);
  uint32 result = 0;
  for (int i=0; i<len; i++)
    result += encoded[i] * (17*i); 
  return result;
}

void ENCMonoid::stats() const
{
  T->stats();
}
