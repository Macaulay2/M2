// Copyright 1996 Michael E. Stillman

#include <ctype.h>
#include "text_io.hpp"
#include "monoid.hpp"
#include "varpower.hpp"
#include "error.hpp"
#include "ntuple.hpp"

Monoid *trivial_monoid;		// set in x_monoid.cc

monoid_info::monoid_info()
: nvars(0),
  degree_monoid(NULL),
  mo(mon_order::trivial()),		// the trivial monomial order
  isgroup(1)
{
  // varnames doesn't need to be set
  set_degrees();
}
  
monoid_info::monoid_info(const mon_order *mmo, 
			 const char *s, 
			 int len, 
			 Monoid *deg_monoid,
			 const intarray &degs,
			 bool isgrp,
			 bool isskew)
: nvars(mmo->n_vars()), degvals(degs),
  degree_monoid(deg_monoid), mo(mmo), isgroup(isgrp)
{ 
  set_names(nvars, s, len, varnames); 
  set_degrees();
  if (!isskew)
    {
      n_skew = 0;
      skew_vars = NULL;
      skew_list = NULL;
    }
  else
    {
      n_skew = 0;
      skew_vars = new int[nvars];
      skew_list = new int[nvars];
      for (int i=0; i<nvars; i++)
	if (primary_degree_of_var[i] % 2 == 0)
	  skew_vars[i] = 0;
	else
	  {
	    skew_vars[i] = 1;
	    skew_list[n_skew++] = i;
	  }
    }
}

void monoid_info::set_names(int nvars, const char *s, int slength, array<char *> &varnames)
{
  char thisstr[100];
  char *current;
  current = thisstr;
  for (int i=0; i<=slength; i++) 
    {
      if (varnames.length() >= nvars) break;
      if (!isspace(s[i]) && i<slength) 
	*current++ = s[i];
      else 
	{
	  if (current == thisstr) continue;
	  *current++ = '\0';
	  char *varname = new char[current - thisstr];
	  strcpy(varname, thisstr);
	  varnames.append(varname);
	  current = thisstr;
	}
    }
    for (int j=varnames.length()+1; j<=nvars; j++)
      {
	char *v = new char[1];
	*v = '\0';
	varnames.append(v);
      }
}

void monoid_info::set_degrees()
{
  if (degree_monoid == NULL)
    {
      degree_of_var.append((int *)NULL);
      return;
    }

  // Set 'degree_of_var
  int degvars = degree_monoid->n_vars();
  int *t = degvals.raw();

  primary_degree_of_var = new int[nvars];

  for (int i=0; i<nvars; i++)
    {
      int *m = degree_monoid->make_one();
      degree_monoid->from_expvector(t, m);
      degree_of_var.append(m);
      primary_degree_of_var[i] = t[0];
      t += degvars;
    }
  degree_of_var.append(degree_monoid->make_one());
}

Monoid::Monoid(monoid_info *moninf,  int nb)
{
  moninfo = moninf;
  nvars = moninfo->nvars;
  if (moninfo->isgroup)
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
  nwords = (nvars+n_per_word-1)/n_per_word;

  // MES: will the next line work correctly if nwords == 0?
  monom_stash = new stash("packed monoms", nwords*sizeof(int));

  if (nvars != 0 && !moninfo->isgroup)
    {
      top_bits = 0;
      for (int j=0; j<n_per_word; j++)
	top_bits = (top_bits << nbits) + (1 << (nbits-1));
    }

  if (moninfo->degree_monoid == NULL)
    moninfo->degree_monoid = (Monoid *) this;

  skew_mvars = new int[nvars];
  skew_nvars = new int[nvars];
  EXP1 = new int[nvars];
  EXP2 = new int[nvars];
  EXP3 = new int[nvars];
  MONlocal = new int[nvars + nwords]; // MES: should be total number of words of result...
}

bool Monoid::equals(const object_element *o) const
{
  // MESXX
  return false;
}

int Monoid::hash() const
{
  // MESXX
  return 0;
}
void Monoid::write_object(object_writer &o) const
{
  // MESXX
}
Monoid *Monoid::read_object(object_reader &i)
{
  // MESXX
  return 0;
}

void Monoid::text_out(buffer &o) const
{
  int i;
  if (moninfo->isgroup)
    o << "group ";
  o << "[";
  for (i=0; i<nvars-1; i++)
    o << moninfo->varnames[i] << ",";
  if (nvars > 0)
    o << moninfo->varnames[nvars-1];

  o << "; Degrees => {";
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
	      o << moninfo->degvals[i*ndegrees+j];
	    }
	  if (ndegrees > 1) o << '}';	  
	}
      o << "}";
    }

  o << "; MonomialOrder => ";
  moninfo->mo->text_out(o);

  o << "]";
}

void Monoid::pack(const int *exp, int *result) const
{
  int i = nwords-1;
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

void Monoid::unpack(const int *m, int *result) const
{
  int n = 0;
  int i = nwords-1;
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

void Monoid::from_expvector(const int *exp, int *result) const
{
  if (nvars == 0) return;
  if (moninfo->isgroup)
    {
      moninfo->mo->encode(exp, result);
    }
  else
    {
      moninfo->mo->encode(exp, MONlocal);
      pack(MONlocal, result);
    }
}

void Monoid::to_expvector(const int *m, int *result_exp) const
{
  if (nvars == 0) return;
  if (moninfo->isgroup)
    {
      moninfo->mo->decode(m, result_exp);
    }
  else
    {
      unpack(m, MONlocal);
      moninfo->mo->decode(MONlocal, result_exp);
    }
}

void Monoid::mult(const int *m, const int *n, int *result) const
{
#if 0
  for (int i=0; i<nwords; i++, m++, n++)
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
  // Now for the packed part
  for (int i=0; i<nwords; i++)
    {
      *result = *m++ + *n++;
      // Overflow is checked by using the bit mask
      if (((*result++) & top_bits) != 0)
	ERROR("monomial overflow");
    }
}
int Monoid::in_subring(int n, const int *m) const
{
  if (nvars == 0) return 1;
  unpack(m, EXP1);
  if (n >= nvars || n < 0) n = nvars;
  for (int i=0; i<n; i++)
    if (EXP1[i] != 0) return 0;
  return 1;
}
int Monoid::compare(int n, const int *m1, const int *m2) const
{
  if (nvars == 0) return 1;
  unpack(m1, EXP1);
  unpack(m2, EXP2);
  if (n >= nvars || n < 0) n = nvars;
  for (int i=0; i<n; i++)
    if (EXP1[i] > EXP2[i]) return -1;
    else if (EXP1[i] < EXP2[i]) return 1;
  return 0;
}

int *Monoid::make_new(const int *d) const
{
  if (nvars == 0) return NULL;
  int *result = (int *) monom_stash->new_elem();
  copy(d, result);
  return result;
}
int *Monoid::make_one() const
{
  if (nvars == 0) return NULL;
  int *result = (int *) monom_stash->new_elem();
  one(result);
  return result;
}
void Monoid::remove(int *d) const
{
  if (d != NULL) monom_stash->delete_elem(d);
}

void Monoid::one(int *result) const
{
  for (int i=0; i<nwords; i++) 
    *result++ = 0;
}

void Monoid::copy(const int *m, int *result) const
{
  memcpy(result, m, nwords*sizeof(int));
}

bool Monoid::divides(const int *m, const int *n) const
// Does m divide n?
{
  if (!moninfo->isgroup)
    {
      if (nvars == 0) return true;
      to_expvector(m, EXP1);
      to_expvector(n, EXP2);
      return ntuple::divides(nvars, EXP1, EXP2);
    }
  else return true;   // If this is a group, then m always divides n.
}

#if 0
void Monoid::divide(const int *m, const int *n, int *result) const
{
  if (nvars == 0) return;
  to_expvector(m, EXP1);
  to_expvector(n, EXP2);
  ntuple::divide(nvars, EXP1, EXP2, EXP1);
  from_expvector(EXP1, result);
}
#endif

void Monoid::power(const int *m, int n, int *result) const
{
  if (nvars == 0) return;
  to_expvector(m, EXP1);
  ntuple::power(nvars, EXP1, n, EXP1);
  from_expvector(EXP1, result);
}

void Monoid::monsyz(const int *m, const int *n, int *sm, int *sn) const
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

void Monoid::gcd(const int *m, const int *n, int *p) const
{
  if (nvars == 0) return;
  to_expvector(m, EXP1);
  to_expvector(n, EXP2);
  ntuple::gcd(nvars, EXP1, EXP2, EXP1);
  from_expvector(EXP1, p);
}

void Monoid::lcm(const int *m, const int *n, int *p) const
{
  if (nvars == 0) return;
  to_expvector(m, EXP1);
  to_expvector(n, EXP2);
  ntuple::lcm(nvars, EXP1, EXP2, EXP1);
  from_expvector(EXP1, p);
}

void Monoid::elem_text_out(buffer &o, const int *m) const
{
  to_expvector(m, EXP1);
  if (moninfo->isgroup)
    {
      o << '{';
      for (int i=0; i<nvars; i++)
	{
	  if (i > 0) o << ' ';
	  o << EXP1[i];
	}
      o << '}';
    }
  else
    ntuple::elem_text_out(o, nvars, EXP1, moninfo->varnames);
}
void Monoid::elem_bin_out(buffer &o, const int *m) const
{
  to_expvector(m, EXP1);
  ntuple::elem_bin_out(o, nvars, EXP1);
}

void Monoid::write_element(object_writer &o, const int *m) const
{
  // MESXX
}

void Monoid::read_element(object_reader &i, int * &result) const
{
  // MESXX
}

void Monoid::multi_degree(const int *m, int *result) const
{
  if (nvars == 0) return;
  if (degree_monoid()->n_vars() == 0) return;

  degree_monoid()->one(result);
  int *mon1 = degree_monoid()->make_one();
  to_expvector(m, EXP1);

  for (int i=0; i<nvars; i++)
    if (EXP1[i] > 0)
      {
	degree_monoid()->power(degree_of_var(i), EXP1[i], mon1);
	degree_monoid()->mult(result, mon1, result);
      }
  degree_monoid()->remove(mon1);
}

void Monoid::degree_of_varpower(const int *vp, int *result) const
{
  if (nvars == 0) return;
  if (degree_monoid()->n_vars() == 0) return;

  degree_monoid()->one(result);
  int *mon1 = degree_monoid()->make_one();

  for (index_varpower j = vp; j.valid(); ++j)
      {
	int v = j.var();
	int e = j.exponent();
	degree_monoid()->power(degree_of_var(v), e, mon1);
	degree_monoid()->mult(result, mon1, result);
      }
  degree_monoid()->remove(mon1);
}

int Monoid::primary_value(const int *m) const
{
  // MES: rewrite!!
  if (nvars == 0) return 0;
  to_expvector(m, EXP1);
  int result = EXP1[0];
  return result;
}

int Monoid::primary_degree(const int *m) const
{
  return degree_weights(m, moninfo->primary_degree_of_var);
}

int Monoid::degree_weights(const int *m, const int *wts) const
{
  if (nvars == 0) return 0;
  to_expvector(m, EXP1);
  return ntuple::weight(nvars, EXP1, wts);
}

int Monoid::is_non_negative(const int *m) const
{
  if (moninfo->isgroup)
    {
      // MES: rewrite!
      to_expvector(m, EXP1);
      int result = (EXP1[0] >= 0);
      return result;
    }

  return 1;
}

int Monoid::is_one(const int *m) const
{
  for (int i=0; i<nwords; i++)
    if (*m++ != 0) return 0;
  return 1;
}

void Monoid::from_varpower(const int *vp, int *result) const
{
  intarray a;
  varpower::to_ntuple(nvars, vp, a);
  from_expvector(a.raw(), result);
}

void Monoid::to_varpower(const int *m, intarray &result_vp) const
{
  to_expvector(m, EXP1);
  varpower::from_ntuple(nvars, EXP1, result_vp);
}

bool Monoid::is_skew() const
{
  return (moninfo->n_skew > 0);
}

int Monoid::is_skew_var(int v) const
{
  return (moninfo->skew_vars[v]);
}

int Monoid::skew_mult_sign(const int *m, const int *n) const
{
  int a = skew_vars(m, skew_mvars);
  int b = skew_vars(n, skew_nvars);
  if (a == 0 || b == 0) return 1;
  int result = 0;
  a--;
  b--;
  for (;;)
    {
      if (skew_mvars[a] < skew_nvars[b])
	{
	  b--;
	  if (b < 0)
	    {
	      return (result % 2 == 0 ? 1 : -1);
	    }
	}
      else if (skew_mvars[a] > skew_nvars[b])
	{
	  result += b+1;
	  a--;
	  if (a < 0)
	    {
	      return (result % 2 == 0 ? 1 : -1);
	    }
	}
      else 
	return 0;
    }
}

int Monoid::skew_mult(const int *m, const int *n, int *result) const
{
  int sign = skew_mult_sign(m,n);
  mult(m,n,result);
  return sign;
}

int Monoid::skew_divide(const int *m, const int *n, int *result) const
    // If the result is s (1,or -1), then m = s * result * n
{
  divide(m,n,result);
  int sign = skew_mult_sign(result,n);
  return sign;
}

int Monoid::exp_skew_vars(const int *exp, int *result) const
    // The number s of skew variables in 'm' is returned, and their
    // indices are placed in result[0], ..., result[s-1].
    // The space that 'result' points to MUST hold at least 'nvars' ints.
{
  int i;
  int next = 0;
  for (i=0; i<moninfo->n_skew; i++)
    {
      int v = moninfo->skew_list[i];
      if (exp[v] > 0)
	result[next++] = v;
    }
  return next;
}
int Monoid::skew_vars(const int *m, int *result) const
    // The number s of skew variables in 'm' is returned, and their
    // indices are placed in result[0], ..., result[s-1].
    // The space that 'result' points to MUST hold at least 'nvars' ints.
{
  to_expvector(m, result);
  return exp_skew_vars(result,result); // This aliasing is ok...
}

bool Monoid::skew_is_zero(const int *exp) const
    // Return whether any skew variable in the exponent vector has exponent >= 2
{
  for (int i=0; i<moninfo->n_skew; i++)
    {
      int v = moninfo->skew_list[i];
      if (exp[v] >= 2) return true;
    }
  return false;
}
