// Copyright 2000 by Michael E. Stillman
#include "Emonoid.hpp"

#include <ctype.h>
#include "text_io.hpp"
#include "varpower.hpp"
#include "error.hpp"
#include "ntuple.hpp"

/////////////////////
// Monoid creation //
/////////////////////

Monoid *Monoid::_trivial_monoid = 0;

Monoid *Monoid::create(EMonomialOrder *mo,
		       const int *print, 
		       const char **names,
		       const Monoid *D,
		       const intarray &degs,
		       const intarray &skewvars,   // This should really go with the ring.
		       bool is_group) // Possibly ignored?
{
  // MES: Probably not functional yet
  Monoid *M = new Monoid(mo,print,names,D,degs,skewvars,is_group);
  return M;
}

const Monoid *Monoid::trivial_monoid()
{
  if (_trivial_monoid == 0)
    {
      _trivial_monoid = new Monoid;
      // Now we must set the degree monoid to be a circular link:
      _trivial_monoid->_D = _trivial_monoid;
      bump_up(_trivial_monoid);
    }
  return _trivial_monoid;
}

Monoid::Monoid()
  : _nvars(0),
    _nwords(0),
    _is_group(false),
    _monom_stash(0),
    _one(0),
    _primary_degree_of_var(0),
    _D(0),
    _mo(0),
    _n_skew(0),
    _skew_vars(0),
    _skew_list(0),
    _skew_mvars(0),
    _skew_nvars(0),
    _EXP1(0),
    _EXP2(0),
    _EXP3(0)
{
}

Monoid::Monoid(EMonomialOrder *mo, 
	       const int *print, 
	       const char **names,
	       const Monoid *deg_monoid,
	       const intarray &degs,
	       const intarray &skewvariables,
	       bool is_group)
  : _nvars(mo->n_vars()),
    _nwords(mo->n_slots()),
    _component_loc(mo->component_loc()),
    _print_order(0),
    _is_group(is_group),
    _one(0),
    _mo(mo)
{
  int i;
  if (_nvars == 0) return;
  
  // MES: will the next line work correctly if _nwords == 0?
  _monom_stash = new stash("monomials", _nwords*sizeof(int));

  // Set the print order array
  _print_order = new int[_nvars];
  for (i=0; i<_nvars; i++)
    _print_order[i] = print[i];

  // Set the variable names array
  for (i=0; i<_nvars; i++)
    {
      int len = strlen(names[i]);
      char *s = new char[len+1];
      strncpy(s, names[i], len+1);
      _var_names.append(s);
    }

  // Set the degree information.  WARNING: might remove degrees from monoids.
  // Note: must always have a degree monoid, possibly the trivial one.
  _D = deg_monoid;
  set_degrees(degs);

  // Set the _isnonnegativevar array
  //  _isnonnegative_var = new bool[_nvars];
  //  for (i=0; i<_nvars; i++)
  //    _isnonnegative_var[i] = mo->isNonnegativeVariable(i);

  // Set the skew commutative information.  WARNING: this will migrate to polynomial
  // rings.
  set_skew_info(skewvariables);

  // Create the temporary exponent and monomial arrays.
  int n = _nvars;
  if (_nwords > _nvars) n = _nwords;
  n++; // Some (which?) routines use nvars+1 spots.
  _skew_mvars = new int[n];
  _skew_nvars = new int[n];
  _EXP1 = new int[n];
  _EXP2 = new int[n];
  _EXP3 = new int[n];

  // Create _one
  if (_nvars == 0)
    _one = NULL;
  else 
    {
      _one = (int *) _monom_stash->new_elem();;
      for (int i=0; i<_nwords; i++)
	const_cast<int *>(_one)[i] = 0;
    }
}

void Monoid::set_skew_info(const intarray &skewvariables)
{
  _n_skew = skewvariables.length();
  if (_n_skew == 0)
    {
      _n_skew = 0;
      _skew_vars = NULL;
      _skew_list = NULL;
    }
  else
    {
      _n_skew = 0;
      _skew_vars = new int[_nvars];
      _skew_list = new int[_nvars];
      for (int j=0; j<_nvars; j++)
	{
	  _skew_vars[j] = _skew_list[j] = 0;
	}
      for (int i=0; i<_n_skew; i++)
	{
	  if (skewvariables[i] >= 0 && skewvariables[i] < _nvars)
	    {
	      if (_skew_vars[skewvariables[i]] != 0)
		_n_skew--;
	      else
		{
		  _skew_list[i] = skewvariables[i];
		  _skew_vars[_skew_list[i]] = 1;
		}
	    }
	  else
	    _n_skew--;
	}

    }
}

// The following (static) member function creates the input for 'create'
char ** Monoid::make_name_array(int nvars, const char *s, int slength)
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

void Monoid::set_degrees(const intarray &degvals)
{
  // _D should be non-NULL here.
  assert(_D != NULL);

  // Set 'degree_of_var
  int degvars = _D->n_vars();

  // degvals should have the correct length
  assert(degvars * _nvars == degvals.length());

  const int *t = degvals.raw();

  _primary_degree_of_var = new int[_nvars];

  for (int i=0; i<_nvars; i++)
    {
      int *m = _D->make_one();
      _D->from_expvector(t, m);
      _degree_of_var.append(m);
      _primary_degree_of_var[i] = t[0];
      t += degvars;
    }
  _degree_of_var.append(_D->make_one());
}

Monoid::~Monoid()
{
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
  o << "[";
  for (i=0; i<_nvars-1; i++)
    o << _var_names[i] << ",";
  if (_nvars > 0)
    o << _var_names[_nvars-1];

  o << ", Degrees => {";
  int ndegrees = degree_monoid()->n_vars();
  if (ndegrees == 0)
      o << "}";
  else
    {
      // First get the degrees of every variable
      for (i=0; i<_nvars; i++)
	{
	  _D->to_expvector(_degree_of_var[i], _EXP1);
	  if (i != 0) o << ", ";
	  if (ndegrees > 1) o << '{';
	  for (int j=0; j<ndegrees; j++)
	    {
	      if (j != 0) o << ", ";
	      o << _EXP1[j];
	    }
	  if (ndegrees > 1) o << '}';	  
	}
      o << "}";
    }

  o << ", MonomialOrder => ";
  _mo->text_out(o);

  // If there are skew variables, put them here
  if (_n_skew > 0)
    {
      o << ", SkewCommutative => {" << _var_names[_skew_vars[0]];
      for (i=1; i<_n_skew; i++)
	o << "," <<  _var_names[_skew_vars[i]];
      o << "}";
    }
  o << "]";
}
/////////////////////////
// Monomial operations //
/////////////////////////

void Monoid::from_expvector(const int *exp, int *result) const
{
  if (!_is_group)
    {
      // MES: write this
      // Check whether the exponents are out of range?
    }
  _mo->encode_commutative(exp, result);
}

void Monoid::to_expvector(const int *m, int *result_exp) const
{
  _mo->decode(m, result_exp);
}

void Monoid::from_varpower(const int *vp, int *result) const
{
  intarray a;
  varpower::to_ntuple(_nvars, vp, a);
  from_expvector(a.raw(), result);
}

void Monoid::to_varpower(const int *m, intarray &result_vp) const
{
  to_expvector(m, _EXP1);
  varpower::from_ntuple(_nvars, _EXP1, result_vp);
}

bool Monoid::in_subring(int n, const int *m) const
{
  if (_nvars == 0) return true;
  int nv = _mo->n_slots(n);
  for (int i=0; i<nv; i++)
    if (m[i] > 0) return false;
  return true;
}
int Monoid::compare(int n, const int *m1, const int *m2) const
{
  if (_nvars == 0) return EQ;
  int nv = _mo->n_slots(n);
  for (int i=0; i<nv; i++)
    if (m1[i] > m2[i]) return GT;
    else if (m1[i] < m2[i]) return LT;
  return EQ;
}

int *Monoid::make_new(const int *d) const
{
  if (_nvars == 0) return NULL;
  int *result = (int *) _monom_stash->new_elem();
  copy(d, result);
  return result;
}
int *Monoid::make_one() const
{
  return make_new(_one);
}
void Monoid::remove(int *d) const
{
  if (d != NULL) _monom_stash->delete_elem(d);
}

void Monoid::one(int *result) const
{
  for (int i=0; i<_nwords; i++) 
    *result++ = 0;
}

bool Monoid::is_one(const int *m) const
{
  for (int i=0; i<_nwords; i++)
    if (*m++ != 0) return false;
  return true;
}

void Monoid::copy(const int *m, int *result) const
{
  memcpy(result, m, _nwords*sizeof(int));
}

void Monoid::mult(const int *m, const int *n, int *result) const
{
  for (int i=0; i<_nwords; i++, m++, n++)
    {
      *result++ = *m + *n;
      if (*m < 0)
	{
	  if (*n < 0 && *result > *m)
	    ERROR("monomial overflow");
	}
      else if (*n > 0 && *m < *result)
	{
	  ERROR("monomial overflow");
	}
    }
}

bool Monoid::divides(const int *m, const int *n) const
// Is every exponent of n/m non-negative?
{
  if (_nvars == 0) return true;
  to_expvector(m, _EXP1);
  to_expvector(n, _EXP2);
  return ntuple::divides(_nvars, _EXP1, _EXP2);
}

void Monoid::divide(const int *m, const int *n, int *result) const
{
  // MES: rewrite, as in mult.
  if (_nvars == 0) return;
  to_expvector(m, _EXP1);
  to_expvector(n, _EXP2);
  ntuple::divide(_nvars, _EXP1, _EXP2, _EXP1);
  from_expvector(_EXP1, result);
}

void Monoid::power(const int *m, int n, int *result) const
{
  if (_nvars == 0) return;
  to_expvector(m, _EXP1);
  ntuple::power(_nvars, _EXP1, n, _EXP1);
  from_expvector(_EXP1, result);
}

void Monoid::monsyz(const int *m, const int *n, int *sm, int *sn) const
{
  if (_nvars == 0) return;
  to_expvector(m, _EXP1);
  to_expvector(n, _EXP2);
  for (int i=0; i<_nvars; i++)
      if (_EXP1[i] > _EXP2[i])
	{
	  _EXP2[i] = _EXP1[i] - _EXP2[i];
	  _EXP1[i] = 0;
	}
      else
	{
	  _EXP1[i] = _EXP2[i] - _EXP1[i];
	  _EXP2[i] = 0;
	}
  from_expvector(_EXP1, sm);
  from_expvector(_EXP2, sn);
}

void Monoid::gcd(const int *m, const int *n, int *p) const
{
  if (_nvars == 0) return;
  to_expvector(m, _EXP1);
  to_expvector(n, _EXP2);
  ntuple::gcd(_nvars, _EXP1, _EXP2, _EXP1);
  from_expvector(_EXP1, p);
}

void Monoid::lcm(const int *m, const int *n, int *p) const
{
  if (_nvars == 0) return;
  to_expvector(m, _EXP1);
  to_expvector(n, _EXP2);
  ntuple::lcm(_nvars, _EXP1, _EXP2, _EXP1);
  from_expvector(_EXP1, p);
}

void Monoid::elem_text_out(buffer &o, const int *m) const
{
  to_expvector(m, _EXP1);
  if (_is_group)
    {
      o << '{';
      for (int i=0; i<_nvars; i++)
	{
	  if (i > 0) o << ' ';
	  o << _EXP1[i];
	}
      o << '}';
    }
  else
    ntuple::elem_text_out(o, _nvars, _EXP1, _var_names);
}
void Monoid::elem_bin_out(buffer &o, const int *m) const
{
  to_expvector(m, _EXP1);
  ntuple::elem_bin_out(o, _nvars, _EXP1);
}

void Monoid::write_element(object_writer &o, const int *m) const
{
  // MESXX
}

void Monoid::read_element(object_reader &i, int * &result) const
{
  // MESXX
}

////////////////////
// The grading /////
////////////////////

void Monoid::multi_degree(const int *m, int *result) const
{
  if (_nvars == 0) return;
  if (degree_monoid()->n_vars() == 0) return;

  degree_monoid()->one(result);
  int *mon1 = degree_monoid()->make_one();
  to_expvector(m, _EXP1);

  for (int i=0; i<_nvars; i++)
    if (_EXP1[i] > 0)
      {
	degree_monoid()->power(degree_of_var(i), _EXP1[i], mon1);
	degree_monoid()->mult(result, mon1, result);
      }
  degree_monoid()->remove(mon1);
}

void Monoid::degree_of_varpower(const int *vp, int *result) const
{
  if (_nvars == 0) return;
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
  if (_nvars == 0) return 0;
  to_expvector(m, _EXP1);
  int result = _EXP1[0];
  return result;
}

int Monoid::primary_degree(const int *m) const
{
  return degree_weights(m, _primary_degree_of_var);
}

int Monoid::degree_weights(const int *m, const int *wts) const
{
  if (_nvars == 0) return 0;
  to_expvector(m, _EXP1);
  return ntuple::weight(_nvars, _EXP1, wts);
}

///////////////////////////////
// Skew commutative routines //
///////////////////////////////
// These should REALLY be with the polynomial ring...
// MES: once packing of monomials is allowed, the mult
// routines (ans others) are being inefficient: they will
// cause several unpackings to occur.

bool Monoid::is_skew() const
{
  return (_n_skew > 0);
}

int Monoid::is_skew_var(int v) const
{
  return (_skew_vars[v]);
}

static int sort_sign(int a, int *v1, int b, int *v2)
{
  if (a == 0 || b == 0) return 1;
  int result = 0; // number of sign switches
  a--;
  b--;
  for (;;)
    {
      if (v1[a] < v2[b])
	{
	  b--;
	  if (b < 0)
	    {
	      return (result % 2 == 0 ? 1 : -1);
	    }
	}
      else if (v1[a] > v2[b])
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

int Monoid::exp_skew_mult_sign(const int *exp1, const int *exp2) const
{
  int a = exp_skew_vars(exp1, _skew_mvars);
  int b = exp_skew_vars(exp2, _skew_nvars);
  return sort_sign(a,_skew_mvars, b, _skew_nvars);
}

int Monoid::skew_mult_sign(const int *m, const int *n) const
{
  int a = skew_vars(m, _skew_mvars);
  int b = skew_vars(n, _skew_nvars);
  return sort_sign(a,_skew_mvars, b, _skew_nvars);
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
int Monoid::skew_diff(const int *m, const int *n, int *result) const
      // m acting as a differential operator on n is s * result, s = 0, 1, or -1.
{
  divide(n,m,result);
  int a = skew_vars(result, _skew_mvars);
  int b = skew_vars(m, _skew_nvars);
  int sign = sort_sign(a,_skew_mvars, b,_skew_nvars);
  int c = b % 4;
  if (c == 2 || c == 3) sign = -sign;
  return sign;
}

int Monoid::exp_skew_vars(const int *exp, int *result) const
    // The number s of skew variables in 'm' is returned, and their
    // indices are placed in result[0], ..., result[s-1].
    // The space that 'result' points to MUST hold at least 'nvars' ints.
{
  int i;
  int next = 0;
  for (i=0; i<_n_skew; i++)
    {
      int v = _skew_list[i];
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
  for (int i=0; i<_n_skew; i++)
    {
      int v = _skew_list[i];
      if (exp[v] >= 2) return true;
    }
  return false;
}
