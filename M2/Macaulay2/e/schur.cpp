// Copyright 1996 Michael E. Stillman

#include "schur.hpp"
#include <stdio.h>
#include "text-io.hpp"
#include "ZZ.hpp"

void tableau::initialize(int nvars)
{
  dim = nvars;
  maxwt = SCHUR_MAX_WT;
  wt = 0;
  lambda = 0;
  p = 0;
  xloc = newarray_atomic(int,SCHUR_MAX_WT+1);
  yloc = newarray_atomic(int,SCHUR_MAX_WT+1);
}

void tableau::resize(int max_wt)
{
  if (max_wt <= SCHUR_MAX_WT) return;
  deletearray(xloc);
  deletearray(yloc);
  maxwt = max_wt;
  wt = max_wt;
  xloc = newarray_atomic(int,maxwt+1);
  yloc = newarray_atomic(int,maxwt+1);
}

int tableau::elem(int x, int y) const
{
  // slow: only used for debugging
  for (int i=1; i<=wt; i++)
    if (xloc[i] == x && yloc[i] == y)
      return i;
  
  // otherwise perhaps throw an error
  fprintf(stderr, "tableau: location (%d,%d) out of range\n", 
	  x, y);
  return 0;
}

void tableau::fill(int *lamb, int *pp)
     // Fill the skew tableau p\lambda with 1..nboxes
     // starting at top right, moving left and then down
     // row by row.
{
  int i, j;
  p = pp;
  lambda = lamb;

  int next = 1;
  for (i=1; p[i] != 0; i++)
    {
      int a = lambda[i];
      for (j=p[i]; j>a; j--)
	{
	  xloc[next] = i;
	  yloc[next++] = j;
	}
    }
}

void tableau::display() const
{
  int i,j;

  for (i=1; p[i] != 0; i++)
    {
      for (j=1; j <= lambda[i]; j++)
	fprintf(stdout, "--  ");
      for ( ; j <= p[i]; j++)
	fprintf(stdout, "%2d  ", elem(i,j));
      fprintf(stdout, "\n");
    }
}

//////////////////////////////////////////
bool operator==(const schur_poly::iterator &a, const schur_poly::iterator &b)
{
  return a.ic == b.ic;
}
bool operator!=(const schur_poly::iterator &a, const schur_poly::iterator &b)
{
  return a.ic != b.ic;
}

void schur_poly::appendTerm(ring_elem coeff, const_schur_partition monom)
{
  coeffs.push_back(coeff);
  for (int i=0; i<monom[0]; i++)
    monoms.push_back(monom[i]);
}
void schur_poly::append(iterator &first, iterator &last)
{
  for ( ; first != last; ++first)
    appendTerm(first.getCoefficient(), first.getMonomial());
}

SchurRing2 *SchurRing2::create(const Ring *A, int n)
{
  SchurRing2 *R = new SchurRing2(A,n);
  return R;
}

SchurRing2 *SchurRing2::createInfinite(const Ring *A)
{
  SchurRing2 *R = new SchurRing2(A);
  return R;
}

SchurRing2::SchurRing2(const Ring *A, int n)
  : coefficientRing(A),
    LR(n,A),
    nvars(n)
{
}

bool SchurRing2::initialize_SchurRing2()
{
  initialize_ring(coefficientRing->charac());

  zeroV = from_int(0);
  oneV = from_int(1);
  minus_oneV = from_int(-1);

  return true;
}

bool SchurRing2::is_valid_partition(M2_arrayint part, bool set_error) const
{
  if (nvars >= 0 && part->len > nvars)
    {
      if (set_error)
	ERROR("expected a partition of size at most %d\n", nvars);
      return false;
    }
  for (int i=1; i<part->len; i++)
    if (part->array[i-1] < part->array[i])
      {
	if (set_error)
	  ERROR("expected a non-increasing sequence of integers");
	return false;
      }
  if (part->len > 0 && part->array[part->len-1] <= 0)
    {
      if (set_error)
	ERROR("expected positive integers only");
      return false;
    }
  return true;
}

ring_elem SchurRing2::from_partition(M2_arrayint part) const
{
  ring_elem result;
  schur_poly *f = new schur_poly;
  f->coeffs.push_back(coefficientRing->one());
  f->monoms.push_back(part->len + 1);
  for (int i=0; i<part->len; i++)
    f->monoms.push_back(part->array[i]);
  result.schur_poly_val = f;
  return result;
}

void SchurRing2::text_out(buffer &o) const
{
  o << "SchurRing2(";
  if (nvars >= 0)
    o << nvars << ",";
  coefficientRing->text_out(o);
  o << ")";
}

void SchurRing2::elem_text_out(buffer &o, 
			       const ring_elem f, 
			       bool p_one, 
			       bool p_plus, 
			       bool p_parens) const
{
  const schur_poly *g = f.schur_poly_val;
  int n = g->size();

  bool needs_parens = p_parens && (n > 1);
  if (needs_parens) 
    {
      if (p_plus) o << '+';
      o << '(';
      p_plus = false;
    }

  p_one = false;
  for (schur_poly::iterator i = g->begin(); i != g->end(); ++i)
    {
      const_schur_partition part = i.getMonomial();
      int len = *part++;
      int isone = (len == 1);  // the empty partition
      p_parens = !isone;
      coefficientRing->elem_text_out(o,i.getCoefficient(), p_one,p_plus,p_parens);
      o << "{";
      for (int j=0; j < len-1; j++)
	{
	  if (j > 0) o << ",";
	  o << part[j];
	}
      o << "}";
      p_plus = true;
    }
  if (needs_parens) o << ')';
}

bool SchurRing2::is_unit(const ring_elem f) const
{
  const schur_poly *g = f.schur_poly_val;
  if (g->size() != 1) return false;
  return (g->monoms.size() == 1) && (coefficientRing->is_unit(g->coeffs[0]));
}

bool SchurRing2::is_zero(const ring_elem f) const
{
  const schur_poly *g = f.schur_poly_val;
  return g->size() == 0;
}

bool SchurRing2::is_equal(const ring_elem f, const ring_elem g) const
{
  const schur_poly *f1 = f.schur_poly_val;
  const schur_poly *g1 = g.schur_poly_val;
  if (f1->size() != g1->size())
    return false;
  if (f1->monoms.size() != g1->monoms.size())
    return false;

  VECTOR(schur_word)::const_iterator m_f = f1->monoms.begin();
  VECTOR(schur_word)::const_iterator m_g = g1->monoms.begin();
  for ( ; m_f != f1->monoms.end(); ++m_f, ++m_g)
    if (*m_f != *m_g) return false;

  VECTOR(ring_elem)::const_iterator c_f = f1->coeffs.begin();
  VECTOR(ring_elem)::const_iterator c_g = g1->coeffs.begin();
  for ( ; c_f != f1->coeffs.end(); ++c_f, ++c_g)
    if (!coefficientRing->is_equal(*c_f, *c_g)) return false;

  return true;
}

bool SchurRing2::get_scalar(const schur_poly *g, ring_elem &result) const
{
  if (g->size() != 1) return false;
  if (g->monoms.size() != 1) return false;
  result = g->coeffs[0];
  return true;
}

ring_elem SchurRing2::from_coeff(ring_elem a) const
{
  ring_elem result;
  schur_poly *f = new schur_poly;
  f->coeffs.push_back(a);
  f->monoms.push_back(1);
  result.schur_poly_val = f;
  return result;
}
ring_elem SchurRing2::from_int(int n) const
{
  ring_elem a = coefficientRing->from_int(n);
  return from_coeff(a);
}
ring_elem SchurRing2::from_int(mpz_ptr n) const
{
  ring_elem a = coefficientRing->from_int(n);
  return from_coeff(a);
}
ring_elem SchurRing2::from_rational(mpq_ptr q) const
{
  ring_elem a = coefficientRing->from_rational(q);
  return from_coeff(a);
}

ring_elem SchurRing2::copy(const ring_elem f) const
{
  const schur_poly *f1 = f.schur_poly_val;

  ring_elem result;
  schur_poly *g = new schur_poly;
  result.schur_poly_val = g;

  VECTOR(ring_elem)::iterator c_result;
  for (VECTOR(ring_elem)::const_iterator c_f = f1->coeffs.begin(); c_f != f1->coeffs.end(); ++c_f)
    g->coeffs.push_back(coefficientRing->negate(*c_f));

  g->monoms.insert(g->monoms.end(), f1->monoms.begin(), f1->monoms.end());

  return result;
}

ring_elem SchurRing2::invert(const ring_elem f) const
{
  // return 0
  return zero();
}

ring_elem SchurRing2::divide(const ring_elem f, const ring_elem g) const
{
  return zero();
}

void SchurRing2::syzygy(const ring_elem a, const ring_elem b,
	    ring_elem &x, ring_elem &y) const
{
  x = zero();
  y = zero();
}

int SchurRing2::compare_partitions(const_schur_partition a, const_schur_partition b) const
{
  int len = a[0];
  if (b[0] < len) len = b[0];
  for (int i=0; i<len; i++)
    {
      int cmp = a[i] - b[i];
      if (cmp < 0) return LT;
      if (cmp > 0) return GT;
    }
  return EQ; 
}
int SchurRing2::compare_elems(const ring_elem f, const ring_elem g) const 
{
  /* write me */
}
bool SchurRing2::promote(const Ring *Rf, const ring_elem f, ring_elem &result) const
{
  // Cases:
  // 1.  Rf is ZZ
  // 2. Rf is coefficientRing
  // 3. Rf is another SchurRing2

  if (Rf == globalZZ)
    {
      from_coeff(Rf->promote(globalZZ, f, result));
      return true;
    }
  else if (Rf == coefficientRing)
    {
      result = from_coeff(f);
      return true;
    }
  else {
    const SchurRing2 *Sf = Rf->cast_to_SchurRing2();
    if (Sf != 0)
      {
	// TODO: WRITE THIS PART
	return true;
      }
  }
  return false;
}
bool SchurRing2::lift(const Ring *Rg, const ring_elem f, ring_elem &result) const
{
  const schur_poly *f1 = f.schur_poly_val;
  if (Rg == coefficientRing || Rg == globalZZ)
    {
      if (get_scalar(f1, result))
	{
	  if (Rg == globalZZ)
	    return coefficientRing->lift(globalZZ, result, result);
	  return true;
	}
    }
  else {
    const SchurRing2 *Sf = Rg->cast_to_SchurRing2();
    if (Sf != 0)
      {
	// TODO: WRITE THIS PART
	return true;
      }
  }
  return false;
}
ring_elem SchurRing2::negate(const ring_elem f) const 
{
  if (is_zero(f)) return f;
  const schur_poly *f1 = f.schur_poly_val;
  ring_elem resultRE;
  schur_poly *result = new schur_poly;
  resultRE.schur_poly_val = result;

  for (VECTOR(ring_elem)::const_iterator i = f1->coeffs.begin(); i != f1->coeffs.end(); ++i)
    result->coeffs.push_back(coefficientRing->negate(*i));

  result->monoms.insert(result->monoms.end(), f1->monoms.begin(), f1->monoms.end());
  return resultRE;
}

ring_elem SchurRing2::add(const ring_elem f, const ring_elem g) const
{
  if (is_zero(f)) return g;
  if (is_zero(g)) return f;
  const schur_poly *f1 = f.schur_poly_val;
  const schur_poly *g1 = g.schur_poly_val;

  ring_elem resultRE;
  schur_poly *result = new schur_poly;
  resultRE.schur_poly_val = result;

  schur_poly::iterator i = f1->begin();
  schur_poly::iterator j = g1->begin();
  schur_poly::iterator iend = f1->end();
  schur_poly::iterator jend = g1->end();
  
  bool done = false;
  while (!done)
    {
      int cmp = compare_partitions(i.getMonomial(), j.getMonomial());
      switch (cmp) {
      case LT:
	result->appendTerm(j.getCoefficient(), j.getMonomial());
	++j;
	if (j == jend)
	  {
	    result->append(i, iend);
	    done = true;
	  }
	break;
      case GT:
	result->appendTerm(i.getCoefficient(), i.getMonomial());
	++i;
	if (i == iend)
	  {
	    result->append(j, jend);
	    done = true;
	  }
	break;
      case EQ:
	ring_elem c = coefficientRing->add(i.getCoefficient(), j.getCoefficient());
	if (!coefficientRing->is_zero(c))
	  result->appendTerm(c, i.getMonomial());
	++j;
	++i;
	if (j == jend)
	  {
	    result->append(i, iend);
	    done = true;
	  }
	else
	  {
	    if (i == iend)
	      {
		result->append(i, iend);
		done = true;
	      }
	  }
	break;
      }
    }
  return resultRE;
}

ring_elem SchurRing2::subtract(const ring_elem f, const ring_elem g) const
{
  ring_elem h = negate(g);
  return add(f,h);
}
ring_elem SchurRing2::mult(const ring_elem f, const ring_elem g) const
{

  //  if (get_scalar(f, a))
  //    {
  //      // just mult coeffs
  //    }
  //  else 
  //    {
  //      // really want to do a heap here of some sort...
  //    }
  /* write me */ 
}
ring_elem SchurRing2::eval(const RingMap *map, const ring_elem f, int first_var) const 
{ 
  /* write me */ 
}

//////////////////////////////////////////
LittlewoodRicharsdon::LittlewoodRicharsdon(int initial_max_weight)
{
  max_weight = initial_max_weight;
  if (max_weight < 0) max_weight = 10;
  _SMtab.initialize(initial_max_weight);
  _SMfilled.initialize(initial_max_weight);
  _SMcurrent = 0;
  _SMfinalwt = 0;
  
  _SMtab.p = newarray_atomic_clear(int,max_weight+1);
}

void LittlewoodRicharsdon::bounds(int &lo, int &hi)
{
  int i, k;
  int x = _SMfilled.xloc[_SMcurrent];
  int y = _SMfilled.yloc[_SMcurrent];
  
  // First set the high bound, using info from the "one to the right"
  // in the reverse lex filled skew tableau.

  if (y == _SMfilled.p[x])	// There is not one to the right
    {
      hi = max_weight;
      for (k=1; k<=max_weight; k++)
	if (_SMtab.p[k] == 0)
	  {
	    hi = k;
	    break;
	  }
    }
  else				// note that the case _SMcurrent==1 will be handled
    {				// in the previous statement.
      hi = _SMtab.xloc[_SMcurrent-1];
    }

  // Now we set the lo bound, using info from the "one above"
  
  if (x == 1 || y <= _SMfilled.lambda[x-1])
    lo = 1;			// There is not one above
  else
    {
      int above = _SMcurrent - _SMfilled.p[x] + _SMfilled.lambda[x-1];
      int xabove = _SMtab.xloc[above];
      int yabove = _SMtab.yloc[above];
      for (i=xabove+1; i<=hi; i++)
	if (_SMtab.p[i] < yabove) break;
      lo = i;
    }
    
}

void LittlewoodRicharsdon::SM()
{
  int lo, hi;

  if (_SMcurrent == _SMfinalwt)
    {
      // partition is to be output
      append_term(_SMtab.p);
      return;
    }
  
  _SMcurrent++;
  bounds(lo, hi);
  int this_one = LARGE_NUMBER;	// larger than any entry of _SMtab
  int last_one;
  for (int i=lo; i<=hi; i++)
    {
      last_one = this_one;
      this_one = _SMtab.p[i];
      if (last_one > this_one)
	{
	  _SMtab.p[i]++;
	  _SMtab.xloc[_SMcurrent] = i;
	  _SMtab.yloc[_SMcurrent] = _SMtab.p[i];
	  SM();
	  _SMtab.p[i]--;
	}
    }
  _SMcurrent--;
}

void LittlewoodRicharsdon::skew_schur(int *lambda, int *p)
{
  _SMcurrent = 0;

  _SMfinalwt = 0;
  for (int i=1; p[i] != 0; i++)
    _SMfinalwt += (p[i] - lambda[i]);

  _SMtab.wt = _SMfinalwt;
  _SMtab.resize(_SMfinalwt);
  _SMfilled.resize(_SMfinalwt);
  _SMfilled.fill(lambda, p);
  SM();
}

void LittlewoodRicharsdon::mult(int *m, int *n)
{
  int i;

  exponents a_part = ALLOCATE_EXPONENTS(sizeof(int) * (n_rows + 1));
  exponents b_part = ALLOCATE_EXPONENTS(sizeof(int) * (n_rows + 1));
  exponents lambda = ALLOCATE_EXPONENTS(2 * sizeof(int) * (n_rows + 1));
  exponents p = ALLOCATE_EXPONENTS(2 * sizeof(int) * (n_rows + 1));

  // First: obtain the partitions  MES: TODO: translate the monomials m and n to 0 terminated partitions
  //  to_partition(m, a_part);
  //  to_partition(n, b_part);
  
  // Second: make the skew partition
  int a = b_part[1];
  for (i=1; i <= max_weight && a_part[i] != 0; i++)
    {
      p[i] = a + a_part[i];
      lambda[i] = a;
    }
  int top = i-1;
  for (i=1; i <= max_weight && b_part[i] != 0; i++)
    {
      p[top+i] = b_part[i];
      lambda[top+i] = 0;
    }
  p[top+i] = 0;
  lambda[top+i] = 0;

  // Call the SM() algorithm
  return skew_schur(lambda, p);
}

LWSchur2::LWSchur2(size_t initial_max_weight, const Ring *A0)
  : LittlewoodRicharsdon(initial_max_weight),
    coefficientRing(A0)
{
}
void LWSchur2::append_term(const_schur_partition f)
{
  // Append this partition to the answer
}
schur_poly *LWSchur2::skew_schur(const_schur_partition lambda, const_schur_partition p)
{
  // possibly increase the num rows
  // initialize the schur_poly_heap
  //  call skews_schur: should be a different name!!
  // return the answer
  return 0;
}
schur_poly *LWSchur2::mult(const_schur_partition a, const_schur_partition b)
{
  // possibly increase the num rows
  // initialize the schur_poly_heap
  //  call mult: should be a different name!!
  // return the answer
  return 0;
}


//////////////////////////////////////////

bool SchurRing::initialize_schur()
{
  _SMtab.initialize(n_vars());
  _SMfilled.initialize(n_vars());
  _SMcurrent = 0;
  _SMfinalwt = 0;
  _SMresult = 0;
  
  _SMtab.p = newarray_atomic_clear(int,nvars_+1);
  return true;
}

SchurRing * SchurRing::create(const PolynomialRing *R)
{
  SchurRing *result = new SchurRing;
  result->initialize_poly_ring(R->getCoefficients(),
			       R->getMonoid());
  if (!result->initialize_schur()) return 0;
  // No GBRing
  return result;
}

SchurRing *SchurRing::create(const Ring *A, int n)
{
  ERROR("not implemented yet");
  return 0;
}

SchurRing *SchurRing::createInfinite(const Ring *A)
{
  ERROR("not implemented yet");
  return 0;
}


void SchurRing::text_out(buffer &o) const
{
  o << "Schur(";
  K_->text_out(o);
  o << ", ";
  M_->text_out(o);
  o << ")";
}

void SchurRing::to_partition(const int *m, int *exp) const
    // exp[1]..exp[nvars] are set
{
  exponents EXP1 = ALLOCATE_EXPONENTS(exp_size); // 0..nvars-1
  // remark: exp_size is defined in an ancestor class of SchurRing
  M_->to_expvector(m, EXP1);
  exp[nvars_] = EXP1[nvars_-1];
  for (int i=nvars_-1; i>=1; i--)
    exp[i] = exp[i+1] + EXP1[i-1];
}
void SchurRing::from_partition(const int *exp, int *m) const
{
  exponents EXP1 = ALLOCATE_EXPONENTS(exp_size); // 0..nvars-1
  EXP1[nvars_-1] = exp[nvars_];
  for (int i=nvars_-1; i>0; i--)
    EXP1[i-1] = exp[i] - exp[i+1];
  M_->from_expvector(EXP1, m);
}

void SchurRing::bounds(int &lo, int &hi)
{
  int i, k;
  int x = _SMfilled.xloc[_SMcurrent];
  int y = _SMfilled.yloc[_SMcurrent];
  
  // First set the high bound, using info from the "one to the right"
  // in the reverse lex filled skew tableau.

  if (y == _SMfilled.p[x])	// There is not one to the right
    {
      hi = nvars_;
      for (k=1; k<=nvars_; k++)
	if (_SMtab.p[k] == 0)
	  {
	    hi = k;
	    break;
	  }
    }
  else				// note that the case _SMcurrent==1 will be handled
    {				// in the previous statement.
      hi = _SMtab.xloc[_SMcurrent-1];
    }

  // Now we set the lo bound, using info from the "one above"
  
  if (x == 1 || y <= _SMfilled.lambda[x-1])
    lo = 1;			// There is not one above
  else
    {
      int above = _SMcurrent - _SMfilled.p[x] + _SMfilled.lambda[x-1];
      int xabove = _SMtab.xloc[above];
      int yabove = _SMtab.yloc[above];
      for (i=xabove+1; i<=hi; i++)
	if (_SMtab.p[i] < yabove) break;
      lo = i;
    }
    
}

void SchurRing::SM()
{
  int lo, hi;

  if (_SMcurrent == _SMfinalwt)
    {
      // partition is to be output
      Nterm *f = new_term();
      f->coeff = K_->from_int(1);
      //      fprintf(stderr, "partition: ");
      //      for (int i=1; i <= nvars_; i++)
      //	fprintf(stderr, " %d", _SMtab.p[i]);
      //      fprintf(stderr, "\n");
      from_partition(_SMtab.p, f->monom);
      f->next = _SMresult;
      _SMresult = f;
      return;
    }
  
  _SMcurrent++;
  bounds(lo, hi);
  int this_one = LARGE_NUMBER;	// larger than any entry of _SMtab
  int last_one;
  for (int i=lo; i<=hi; i++)
    {
      last_one = this_one;
      this_one = _SMtab.p[i];
      if (last_one > this_one)
	{
	  _SMtab.p[i]++;
	  _SMtab.xloc[_SMcurrent] = i;
	  _SMtab.yloc[_SMcurrent] = _SMtab.p[i];
	  SM();
	  _SMtab.p[i]--;
	}
    }
  _SMcurrent--;
}

Nterm *SchurRing::skew_schur(int *lambda, int *p)
{
  _SMcurrent = 0;

  _SMfinalwt = 0;
  for (int i=1; p[i] != 0; i++)
    _SMfinalwt += (p[i] - lambda[i]);

  _SMtab.wt = _SMfinalwt;
  _SMtab.resize(_SMfinalwt);
  _SMfilled.resize(_SMfinalwt);
  _SMfilled.fill(lambda, p);
  _SMresult = NULL;
  SM();
  ring_elem result = _SMresult;
  _SMresult = NULL;
  return result;
}

ring_elem SchurRing::mult_monomials(const int *m, const int *n)
{
  int i;

  exponents a_part = ALLOCATE_EXPONENTS(sizeof(int) * (nvars_ + 1));
  exponents b_part = ALLOCATE_EXPONENTS(sizeof(int) * (nvars_ + 1));
  exponents lambda = ALLOCATE_EXPONENTS(2 * sizeof(int) * (nvars_ + 1));
  exponents p = ALLOCATE_EXPONENTS(2 * sizeof(int) * (nvars_ + 1));

  // First: obtain the partitions
  to_partition(m, a_part);
  to_partition(n, b_part);
  
  // Second: make the skew partition
  int a = b_part[1];
  for (i=1; i <= nvars_ && a_part[i] != 0; i++)
    {
      p[i] = a + a_part[i];
      lambda[i] = a;
    }
  int top = i-1;
  for (i=1; i <= nvars_ && b_part[i] != 0; i++)
    {
      p[top+i] = b_part[i];
      lambda[top+i] = 0;
    }
  p[top+i] = 0;
  lambda[top+i] = 0;

  // Call the SM() algorithm
  return skew_schur(lambda, p);
}

ring_elem SchurRing::mult_by_term(const ring_elem f, 
				    const ring_elem c, 
				    const int *m) const
{
  // return c*m*f
  ring_elem result = ZERO_RINGELEM;
  for (Nterm *t = f; t != NULL; t = t->next)
    {
      ring_elem a = K_->mult(c, t->coeff);
      ring_elem g = const_cast<SchurRing *>(this)->mult_monomials(t->monom, m);
      for (Nterm *s = g; s != NULL; s = s->next)
	{
	  ring_elem b = K_->mult(a, s->coeff);
	  s->coeff = b;
	}
      Nterm *gt = g;
      sort(gt);
      g = gt;
      add_to(result, g);
    }
  return result;
}
ring_elem SchurRing::power(const ring_elem f, mpz_t n) const
{
  if (mpz_sgn(n) < 0)
    {
      ERROR("element not invertible");
      return from_int(1);
    }
  unsigned int n1;
  if (!RingZZ::get_ui(n1, n))
    {
      ERROR("exponent too large");
      return from_int(1);
    }
  return power(f,n1);
}

ring_elem SchurRing::power(const ring_elem f, int n) const
{
  ring_elem result = from_int(1);
  if (n < 0)
    {
      ERROR("element not invertible");
      return result;
    }
  for (int i=0; i<n; i++)
    {
      ring_elem g = mult(result, f);
      remove(result);
      result = g;
    }
  return result;
}

void SchurRing::dimension(const int *exp, mpz_t result) const
    // exp: 0..nvars_
    // Return in 'result' the dimension of the irreducible
    // GL(nvars_) representation having highest weight
    // 'exp'
{
  int i,j;

  mpz_set_ui(result, 1);
  for (i=1; i<nvars_; i++)
    for (j=i+1; j<=nvars_; j++)
      if (exp[i] != exp[j])
	mpz_mul_ui(result, result, exp[i] - exp[j] + j - i);

  for (i=1; i<nvars_; i++)
    for (j=i+1; j<=nvars_; j++)
      if (exp[i] != exp[j])
	mpz_fdiv_q_ui(result, result, j - i);
}

ring_elem SchurRing::dimension(const ring_elem f) const
{
  exponents EXP = ALLOCATE_EXPONENTS(sizeof(int) * (nvars_ + 1));
  ring_elem result = K_->from_int(0);
  mpz_t dim;
  mpz_init(dim);
  for (Nterm *t = f; t != NULL; t = t->next)
    {
      to_partition(t->monom, EXP);
      dimension(EXP, dim);
      ring_elem h = K_->from_int(dim);
      ring_elem h2 = K_->mult(t->coeff, h);
      K_->add_to(result, h2);
      K_->remove(h);
    }
  return result;
}

void SchurRing::elem_text_out(buffer &o, 
			const ring_elem f, 
			bool p_one, 
			bool p_plus, 
			bool p_parens) const
{
  exponents EXP = ALLOCATE_EXPONENTS(sizeof(int) * (nvars_ + 1));
  int n = n_terms(f);

  bool needs_parens = p_parens && (n > 1);
  if (needs_parens) 
    {
      if (p_plus) o << '+';
      o << '(';
      p_plus = false;
    }

  p_one = false;
  for (Nterm *t = f; t != NULL; t = t->next)
    {
      int isone = M_->is_one(t->monom);
      p_parens = !isone;
      K_->elem_text_out(o,t->coeff, p_one,p_plus,p_parens);
      to_partition(t->monom, EXP);
      o << "{" << EXP[1];
      for (int i=2; i<=nvars_ && EXP[i] != 0; i++)
	o << "," << EXP[i];
      o << "}";
      p_plus = true;
    }
  if (needs_parens) o << ')';
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
