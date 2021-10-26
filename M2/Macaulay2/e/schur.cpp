// Copyright 1996 Michael E. Stillman

#include "schur.hpp"
#include <stdio.h>
#include "text-io.hpp"
#include "ZZ.hpp"
#include "monoid.hpp"

void tableau::initialize(int nvars)
{
  dim = nvars;
  maxwt = SCHUR_MAX_WT;
  wt = 0;
  lambda = 0;
  p = 0;
  xloc = newarray_atomic(int, SCHUR_MAX_WT + 1);
  yloc = newarray_atomic(int, SCHUR_MAX_WT + 1);
}

void tableau::resize(int max_wt)
{
  if (max_wt <= SCHUR_MAX_WT) return;
  freemem(xloc);
  freemem(yloc);
  maxwt = max_wt;
  wt = max_wt;
  xloc = newarray_atomic(int, maxwt + 1);
  yloc = newarray_atomic(int, maxwt + 1);
}

int tableau::elem(int x, int y) const
{
  // slow: only used for debugging
  for (int i = 1; i <= wt; i++)
    if (xloc[i] == x && yloc[i] == y) return i;

  // otherwise perhaps throw an error
  fprintf(stderr, "tableau: location (%d,%d) out of range\n", x, y);
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
  for (i = 1; p[i] != 0; i++)
    {
      int a = lambda[i];
      for (j = p[i]; j > a; j--)
        {
          xloc[next] = i;
          yloc[next++] = j;
        }
    }
}

void tableau::display() const
{
  int i, j;

  for (i = 1; p[i] != 0; i++)
    {
      for (j = 1; j <= lambda[i]; j++) fprintf(stdout, "--  ");
      for (; j <= p[i]; j++) fprintf(stdout, "%2d  ", elem(i, j));
      fprintf(stdout, "\n");
    }
}

bool SchurRing::initialize_schur()
{
  _SMtab.initialize(n_vars());
  _SMfilled.initialize(n_vars());
  _SMcurrent = 0;
  _SMfinalwt = 0;
  _SMresult = 0;

  _SMtab.p = newarray_atomic_clear(int, nvars_ + 1);
  return true;
}

SchurRing *SchurRing::create(const PolynomialRing *R)
{
  SchurRing *result = new SchurRing;
  result->initialize_poly_ring(R->getCoefficients(), R->getMonoid());
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
  exponents EXP1 = ALLOCATE_EXPONENTS(exp_size);  // 0..nvars-1
  // remark: exp_size is defined in an ancestor class of SchurRing
  M_->to_expvector(m, EXP1);
  exp[nvars_] = EXP1[nvars_ - 1];
  for (int i = nvars_ - 1; i >= 1; i--) exp[i] = exp[i + 1] + EXP1[i - 1];
}
void SchurRing::from_partition(const int *exp, int *m) const
{
  exponents EXP1 = ALLOCATE_EXPONENTS(exp_size);  // 0..nvars-1
  EXP1[nvars_ - 1] = exp[nvars_];
  for (int i = nvars_ - 1; i > 0; i--) EXP1[i - 1] = exp[i] - exp[i + 1];
  M_->from_expvector(EXP1, m);
}

void SchurRing::bounds(int &lo, int &hi)
{
  int i, k;
  int x = _SMfilled.xloc[_SMcurrent];
  int y = _SMfilled.yloc[_SMcurrent];

  // First set the high bound, using info from the "one to the right"
  // in the reverse lex filled skew tableau.

  if (y == _SMfilled.p[x])  // There is not one to the right
    {
      hi = nvars_;
      for (k = 1; k <= nvars_; k++)
        if (_SMtab.p[k] == 0)
          {
            hi = k;
            break;
          }
    }
  else  // note that the case _SMcurrent==1 will be handled
    {   // in the previous statement.
      hi = _SMtab.xloc[_SMcurrent - 1];
    }

  // Now we set the lo bound, using info from the "one above"

  if (x == 1 || y <= _SMfilled.lambda[x - 1])
    lo = 1;  // There is not one above
  else
    {
      int above = _SMcurrent - _SMfilled.p[x] + _SMfilled.lambda[x - 1];
      int xabove = _SMtab.xloc[above];
      int yabove = _SMtab.yloc[above];
      for (i = xabove + 1; i <= hi; i++)
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
      f->coeff = K_->from_long(1);
      //      fprintf(stderr, "partition: ");
      //      for (int i=1; i <= nvars_; i++)
      //        fprintf(stderr, " %d", _SMtab.p[i]);
      //      fprintf(stderr, "\n");
      from_partition(_SMtab.p, f->monom);
      f->next = _SMresult;
      _SMresult = f;
      return;
    }

  _SMcurrent++;
  bounds(lo, hi);
  int this_one = LARGE_NUMBER;  // larger than any entry of _SMtab
  int last_one;
  for (int i = lo; i <= hi; i++)
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
  for (int i = 1; p[i] != 0; i++) _SMfinalwt += (p[i] - lambda[i]);

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
  for (i = 1; i <= nvars_ && a_part[i] != 0; i++)
    {
      p[i] = a + a_part[i];
      lambda[i] = a;
    }
  int top = i - 1;
  for (i = 1; i <= nvars_ && b_part[i] != 0; i++)
    {
      p[top + i] = b_part[i];
      lambda[top + i] = 0;
    }
  p[top + i] = 0;
  lambda[top + i] = 0;

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
ring_elem SchurRing::power(const ring_elem f, mpz_srcptr n) const
{
  if (mpz_sgn(n) < 0) throw exc::engine_error("element not invertible");
  std::pair<bool, int> n1 = RingZZ::get_si(n);
  if (n1.first)
    return power(f, n1.second);
  else
    throw exc::engine_error("exponent too large");
}

ring_elem SchurRing::power(const ring_elem f, int n) const
{
  ring_elem result = from_long(1);
  if (n < 0)
    {
      throw exc::engine_error("element not invertible");
    }
  for (int i = 0; i < n; i++)
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
  int i, j;

  mpz_set_ui(result, 1);
  for (i = 1; i < nvars_; i++)
    for (j = i + 1; j <= nvars_; j++)
      if (exp[i] != exp[j]) mpz_mul_ui(result, result, exp[i] - exp[j] + j - i);

  for (i = 1; i < nvars_; i++)
    for (j = i + 1; j <= nvars_; j++)
      if (exp[i] != exp[j]) mpz_fdiv_q_ui(result, result, j - i);
}

ring_elem SchurRing::dimension(const ring_elem f) const
{
  exponents EXP = ALLOCATE_EXPONENTS(sizeof(int) * (nvars_ + 1));
  ring_elem result = K_->from_long(0);
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
      K_->elem_text_out(o, t->coeff, p_one, p_plus, p_parens);
      to_partition(t->monom, EXP);
      o << "{" << EXP[1];
      for (int i = 2; i <= nvars_ && EXP[i] != 0; i++) o << "," << EXP[i];
      o << "}";
      p_plus = true;
    }
  if (needs_parens) o << ')';
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
