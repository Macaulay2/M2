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
    ncommvars(mo->n_commuting_vars()),
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

int ECommMonoid::degree(const monomial *a, const int *wts) const
{
  const int *exp = a->monom;
  int result = 0;
  for (int i=0; i<nvars; i++)
    result += exp[i] * wts[i];
  return result;
}

ECommMonoid::ECommMonoid(EMonomialOrder *mo, 
	       const int *print, 
	       const char **names)
  : EMonoid(mo,print,names)
{
  if (nvars == 0) return;

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
  ECommMonomialTable *T = new ECommMonomialTable(M,MONOMIAL_HASH_SIZE);
  M->T = T;
  intarray one;
  M->_one = M->monomial_from_variable_exponent_pairs(one);
  return M;
}

ECommMonoid::~ECommMonoid()
{
  delete [] hash_multiplier;
  // Now delete all of the monomials
  // MES
}

const int * ECommMonoid::to_exponents(const monomial *m) const
{
  return m->monom;
}

void ECommMonoid::copy_exponents(const monomial *m, int *&result) const
{
  for (int i=0; i<nvars; i++)
    result[i] = m->monom[i];
}

const monomial *ECommMonoid::unchecked_monomial_from_exponents(int *exp) const
  // This is a low level routine where 'exp' is expected to have been
  // allocated via T->allocate_tentative_monomial(nvars+nslots).
  // This routine will give back the space if needed.
{
  return T->lookup_and_insert_commutative(exp);
}
const monomial *ECommMonoid::monomial_from_exponents(const int *exp) const
{
  // First make sure that each exponent is >= 0, if it needs to be...
  int *exp1 = T->allocate_tentative_monomial(nvars+nslots);
  for (int i=0; i<nvars; i++)
    {
      if (isnonnegativevar[i] && exp[i] < 0) 
	{
	  gError << "expected non-negative exponent";
	  T->give_back(exp1);
	  return 0;
	}
      exp1[i] = exp[i];
    }
  return unchecked_monomial_from_exponents(exp1);
}

void ECommMonoid::to_variable_exponent_pairs(const monomial *m, intarray &result) const
{
  const int *exp = m->monom;
  for (int i=0; i<nvars; i++)
    if (exp[i] != 0)
      {
	result.append(i);
	result.append(exp[i]);
      }
}
const monomial *ECommMonoid::monomial_from_variable_exponent_pairs(const intarray &term) const
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
  const monomial *result = monomial_from_exponents(exp);
  delete [] exp;
  return result;
}
uint32 ECommMonoid::hash(const int *exponents) const
{
  // !! TUNE THIS !!
  uint32 result = 0;
  for (int i=0; i<nvars; i++)
    if (exponents[i] != 0) 
      result += exponents[i] * hash_multiplier[i];
  return result;
}

const monomial *ECommMonoid::mult(const monomial *m, const monomial *n) const
{
  const int *m1 = m->monom;
  const int *n1 = n->monom;
  int *result = T->allocate_tentative_monomial(nslots+nvars);
  for (int i=0; i<nvars; i++)
    result[i] = *m1++ + *n1++;
  return unchecked_monomial_from_exponents(result);
}

const monomial *ECommMonoid::power(const monomial *m, int n) const
{
  const int *m1 = m->monom;
  int *result = T->allocate_tentative_monomial(nslots+nvars);
  for (int i=0; i<nvars; i++)
    result[i] = (*m1++) * n;
  return unchecked_monomial_from_exponents(result);
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
  const int *m1 = m->monom + nvars;  // This is where the 'slots' start
  const int *n1 = n->monom + nvars;
  for (i=0; i<componentloc; i++) {
    cmp = *m1++ - *n1++;
    if (cmp < 0) return LT;
    if (cmp > 0) return GT;
  }
  cmp = mcomponent - ncomponent;
  if (cmp < 0) return LT;
  if (cmp > 0) return GT;
  for ( ; i<nslots; i++) {
    cmp = *m1++ - *n1++;
    if (cmp < 0) return LT;
    if (cmp > 0) return GT;
  }
  return EQ;  // Should never get to here...
}
int ECommMonoid::compare(const monomial *a,
			 const monomial *b,
			 int n) const
{
  const int *a1 = a->monom + nvars;
  const int *b1 = b->monom + nvars;
  int m = monorder->n_slots(n);
  for (int i=0; i<m; i++)
    {
      int cmp = *a1++ - *b1++;
      if (cmp < 0) return LT;
      if (cmp > 0) return GT;
    }
  return EQ;
}


const monomial *ECommMonoid::lcm(const monomial *m, const monomial *n) const
{
  const int *e1 = m->monom;
  const int *e2 = n->monom;
  int *result = T->allocate_tentative_monomial(nslots+nvars);
  for (int i=0; i<nvars; i++)
    if (e1[i] > e2[i])
      result[i] = e1[i];
    else
      result[i] = e2[i];
  return unchecked_monomial_from_exponents(result);
}

const monomial *ECommMonoid::gcd(const monomial *m, const monomial *n) const
{
  const int *e1 = m->monom;
  const int *e2 = n->monom;
  int *result = T->allocate_tentative_monomial(nslots+nvars);
  for (int i=0; i<nvars; i++)
    if (e1[i] < e2[i])
      result[i] = e1[i];
    else
      result[i] = e2[i];
  return unchecked_monomial_from_exponents(result);
}
const monomial *ECommMonoid::divide(const monomial *m, const monomial *n) const
  // m/n  WARNING: only use if there exists a monomial r such that m = rn.
  // Otherwise, the result will be NOT valid...
{
  const int *e1 = m->monom;
  const int *e2 = n->monom;
  int *result = T->allocate_tentative_monomial(nslots+nvars);
  for (int i=0; i<nvars; i++)
      result[i] = e1[i] - e2[i];
  return unchecked_monomial_from_exponents(result);
}

bool ECommMonoid::divides(const monomial *m, const monomial *n) const
  // does m divide n?
{
  const int *e1 = m->monom;
  const int *e2 = n->monom;
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
  : EMonoid(mo,print,names)
{
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
  ENCMonomialTable *T = new ENCMonomialTable(M,MONOMIAL_HASH_SIZE);
  M->T = T;
  intarray one;
  M->_one = M->monomial_from_variable_exponent_pairs(one);
  return M;
}

ENCMonoid::~ENCMonoid()
{
  delete [] nclength;
  delete [] is_nc_block;

  // Now delete all of the monomials
  // MES
}

//////////////////////////////////////////
// Format of a Non-Commutative monomial //
//////////////////////////////////////////
//
// Each monomial is a pointer to a sequence of integers.
//  a. length field (the length of the entire thing, including this int).
//  b. nslots: one integer per slot.  Each non-commutative block
//       has only one slot, and its value is the length of that non-comm part of
//       the monomial.
//  c. ncommvars: the exponent vector for commutative variables which appear.
//  d. length-nslots-ncommvars: the non-commutative part of the monomial
//       This is a variable length field, given by (variable,exponent),...
// 

void ENCMonoid::to_variable_exponent_pairs(const monomial *m, intarray &result) const
{
  int *encoded = m->monom;
  // First put in the commutative part:
  int len = *encoded++;
  encoded += nslots;  // This is the location of the comm exponent vector part.
  int i;
  for (i=0; i<ncommvars; i++)
    if (encoded[i] != 0)
      {
	result.append(i);
	result.append(encoded[i]);
      }
  // Now do the non-commutative part
  len -= 1+nslots+ncommvars;
  encoded += ncommvars;  // This is the location of the non-comm var exp part.
  for (i=0; i<len; i+=2)
    {
      result.append(encoded[i]);
      result.append(encoded[i+1]);
    }
}

const monomial *ENCMonoid::monomial_from_variable_exponent_pairs(const intarray &mon) const
{
  int *result = T->allocate_tentative_monomial(1 + nslots + ncommvars + mon.length());
    // This is the max possible length.
  // MES: PLACE this 'mon' into the proper order for encode_noncommutative
  monorder->encode_noncommutative(mon, result);  
  return T->lookup_and_insert_noncommutative(result);
}

const monomial *ENCMonoid::mult(const monomial *m, const monomial *n) const
{
  int i,j,k;
  const int *m1 = m->monom;
  const int *n1 = n->monom;
  int *result = T->allocate_tentative_monomial(*m1 + *n1);  // Way more than enough space
  result++;  // We will set the length field at the end.
  m1++;
  n1++;
  for (i=0; i<nslots+ncommvars; i++)
    result[i] = m1[i] + n1[i];
  // Now multiply the non-commutative part
  int firstpast = nslots+ncommvars;
  int mpast = firstpast;
  int npast = firstpast;
  for (j=0; j<n_nc_blocks; j++)
    {
      int slotlen = nclength[j];
      int len1 = m1[slotlen];
      int len2 = n1[slotlen];
      for (k=0; k<len1; k++)
	result[firstpast++] = m1[mpast++];
      if (len1 > 0 && len2 > 0 && m1[mpast-2] == n1[npast])
	{
	  result[firstpast-1] += n1[npast+1];
	  result[slotlen] -= 2;
	  npast += 2;
	  len2 -= 2;
	}
      for (k=0; k<len2; k++)
	result[firstpast++] = n1[npast++];
    }      
  --result;
  *result = firstpast+1;
  return T->lookup_and_insert_noncommutative(result);
}

const monomial *ENCMonoid::divide(const monomial *m, const monomial *n) const
{
  gError << "division not implemented for non-comm monoids";
  return clone(one());
}

const monomial *ENCMonoid::power(const monomial *a, int n) const
{
  // We can make this more efficient if it seems worth it later...
  const monomial *result = one();
  for (int i=0; i<n; i++)
    result = mult(a,result);
  return result;
}

int ENCMonoid::compare(const monomial *m, int mcomponent,
		       const monomial *n, int ncomponent) const
{
  int cmp;
  if (m == n) {
    cmp = mcomponent - ncomponent;
    if (cmp < 0) return LT;
    if (cmp > 0) return GT;
    return EQ;
  }
  const int *m1 = m->monom + 1;
  const int *n1 = n->monom + 1;
  int past = nslots+ncommvars;
  for (int i=0; i<nslots; i++) {
    if (i == componentloc)
      {
	cmp = mcomponent - ncomponent;
	if (cmp < 0) return LT;
	if (cmp > 0) return GT;
      }
    if (is_nc_block[i])
      {
	// Need to check the NC block of vars
	int len1 = m1[i];
	int len2 = n1[i];
	int len = len1;
	if (len2 < len) len = len2;
	for (int j=0; j<len; j += 2)
	  {
	    cmp = m1[past] - n1[past];  // Variable
	    if (cmp < 0) return GT;
	    if (cmp > 0) return LT;
	    past++;
	    cmp = m1[past] - n1[past];  // Exponent
	    if (cmp > 0) return GT;
	    if (cmp < 0) return LT;
	    past++;
	  }
	if (len1 > len2) return GT;
	if (len2 > len1) return LT;
      }
    else
      {
	cmp = m1[i] - n1[i];
	if (cmp < 0) return LT;
	if (cmp > 0) return GT;
      }
  }
  return EQ;  // Should never get to here...
}

int ENCMonoid::compare(const monomial *m,
			 const monomial *n,
			 int r) const
{
  int cmp;
  if (m == n) return EQ;
  int these_slots = monorder->n_slots(r);
  const int *m1 = m->monom + 1;
  const int *n1 = n->monom + 1;
  int past = nslots+ncommvars;
  for (int i=0; i<these_slots; i++) {
    if (is_nc_block[i])
      {
	// Need to check the NC block of vars
	int len1 = m1[i];
	int len2 = n1[i];
	int len = len1;
	if (len2 < len) len = len2;
	for (int j=0; j<len; j += 2)
	  {
	    cmp = m1[past] - n1[past];  // Variable
	    if (cmp < 0) return GT;
	    if (cmp > 0) return LT;
	    past++;
	    cmp = m1[past] - n1[past];  // Exponent
	    if (cmp > 0) return GT;
	    if (cmp < 0) return LT;
	    past++;
	  }
	if (len1 > len2) return GT;
	if (len2 > len1) return LT;
      }
    else
      {
	cmp = m1[i] - n1[i];
	if (cmp < 0) return LT;
	if (cmp > 0) return GT;
      }
  }
  return EQ;  // Should never get to here...
}

int ENCMonoid::degree(const monomial *a, const int *wts) const
{
  int result = 0;
  const int *exp = a->monom + 1 + nslots;
  // First get the comm part
  for (int i=0; i<ncommvars; i++)
    result += exp[i] * wts[i];
  // Now do the non-commutative part
  const int *end = a->monom + *(a->monom);
  const int *start = exp + ncommvars;
  for (const int *v = start; v < end; v += 2)
    result += wts[*v] * v[1];
  return result;
}

uint32 ENCMonoid::hash(const int *encoded) const
{
  // WRITE THIS!!
  int len = *encoded++;
  uint32 result = 0;
  for (int i=0; i<len; i++)
    result += encoded[i] * (17*i); 
  return result;
}

void ENCMonoid::stats() const
{
  T->stats();
}

// What about these routines?
const monomial *ENCMonoid::monomial_from_exponents(const int *exp) const
{
  intarray varexp;
  for (int i=0; i<nvars; i++)
    if (exp[i] != 0)
      {
	varexp.append(i);
	varexp.append(exp[i]);
      }
  return monomial_from_variable_exponent_pairs(varexp);
}

void ENCMonoid::copy_exponents(const monomial *m, int *&result) const
{
  int i;
  for (i=0; i<nvars; i++)
    result[i] = 0;

  // First do commutative part:
  const int *p = m->monom + 1 + nslots;
  for (i=0; i<ncommvars; i++)
    result[i] = p[i];

  // Now do the non-commutative part
  p += ncommvars;
  int len = m->monom + (*m->monom) - p;
  for (i=0; i<len; i += 2)
    result[p[i]] += p[i+1];
}

const int * ENCMonoid::to_exponents(const monomial *m) const
{
  assert(0);
  // WHAT??
}

