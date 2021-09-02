// Copyright 1997 Michael E. Stillman

#include "weylalg.hpp"
#include "gbring.hpp"

#include "geopoly.hpp"
#include "text-io.hpp"

bool WeylAlgebra::initialize_weyl(M2_arrayint derivs,
                                  M2_arrayint comms,
                                  int homog_var)
{
  if (homog_var >= nvars_)
    {
      ERROR("Weyl algebra homogenizing variable out of range");
      return false;
    }
  if (homog_var < 0) _homog_var = -1;
  if (derivs->len != comms->len)
    {
      ERROR("Weyl algebra: expected arrays of the same length");
      return false;
    }
  for (unsigned int i = 0; i < derivs->len; i++)
    {
      if (derivs->array[i] < 0 || derivs->array[i] >= nvars_)
        {
          ERROR("Weyl algebra: variable out of range");
          return false;
        }
      if (comms->array[i] < 0 || comms->array[i] >= nvars_)
        {
          ERROR("Weyl algebra: variable out of range");
          return false;
        }
    }

  this->_nderivatives = derivs->len;
  this->_homogeneous_weyl_algebra = (homog_var >= 0);
  this->_homog_var = homog_var;

  this->_derivative = newarray_atomic(int, _nderivatives);
  this->_commutative = newarray_atomic(int, _nderivatives);
  for (int i = 0; i < _nderivatives; i++)
    {
      this->_derivative[i] = derivs->array[i];
      this->_commutative[i] = comms->array[i];
    }

  // Now set whether this ring is graded.  This will be the case iff
  // deg(x_i) + deg(D_i) = 2 deg(h), for every i.
  if (_homog_var < 0)
    this->setIsGraded(false);
  else
    {
      this->setIsGraded(true);
      const Monoid *D = degree_monoid();
      const int *degh = M_->degree_of_var(_homog_var);
      int *deg2h = D->make_one();
      int *degxD = D->make_one();
      D->mult(degh, degh, deg2h);

      for (int j = 0; j < _nderivatives; j++)
        {
          const int *degx = M_->degree_of_var(_commutative[j]);
          const int *degD = M_->degree_of_var(_derivative[j]);
          D->mult(degx, degD, degxD);
          if (D->compare(deg2h, degxD) != EQ)
            {
              ERROR("Weyl algebra: failed to create homogeneous Weyl algebra");
              return false;
            }
        }
    }

  // Now initialize the tables
  initialize1();
  return true;
}

WeylAlgebra *WeylAlgebra::create(const Ring *K,
                                 const Monoid *M,
                                 M2_arrayint derivs,
                                 M2_arrayint comms,
                                 int homog_var)
{
  WeylAlgebra *result = new WeylAlgebra;

  result->initialize_poly_ring(K, M);
  if (!result->initialize_weyl(derivs, comms, homog_var)) return 0;
#ifdef DEVELOPMENT
#warning "hack for ZZ and QQ coeffs in Weyl algebra: clean it up?"
#endif
  WeylAlgebra *weyl = result;
  if (K->is_QQ())
    {
      weyl = WeylAlgebra::create(globalZZ, M, derivs, comms, homog_var);
    }
  result->gb_ring_ = GBRing::create_WeylAlgebra(K, M, weyl);
  return result;
}

#if 0
// const WeylAlgebra *WeylAlgebra::createPolyRing(const Monoid *M) const
//   // creates this[M], which is commutative in M variables, but skew commutative in
//   // (some of) the variables of this
// {
//   const Monoid *newM = Monoid::tensor_product(M, getMonoid());
//   if (newM == 0) return 0;
//
//   int nvars = M->n_vars();
//   M2_arrayint new_derivs = M2_makearrayint(_nderivatives);
//   M2_arrayint new_comms = M2_makearrayint(_nderivatives);
//
//   int new_homog_var;
//   if (_homog_var >= 0)
//     new_homog_var = _homog_var + nvars;
//   else
//     new_homog_var = -1;
//
//   for (int i=0; i<_nderivatives; i++)
//     {
//       new_derivs->array[i] = (_derivative[i] >= 0 ?
//                            nvars + _derivative[i]
//                            :
//                            -1);
//       new_comms->array[i]  = (_commutative[i] >= 0 ?
//                            nvars + _commutative[i]
//                            :
//                            -1);
//     }
//
//   return create(getCoefficients(),
//              newM,
//              this,
//              M,
//              new_derivs,
//              new_comms,
//              new_homog_var);
// }
#endif

void WeylAlgebra::text_out(buffer &o) const
{
  o << "WeylAlgebra(";
  K_->text_out(o);
  M_->text_out(o);
  o << ")";
}

/////////////////
int WeylAlgebra::binomtop = 15;
int WeylAlgebra::diffcoeffstop = 10;
int **WeylAlgebra::binomtable = 0;
int **WeylAlgebra::diffcoeffstable = 0;

void WeylAlgebra::initialize1()
{
  if (binomtable == 0)
    {
      int i, j;

      binomtable = newarray(int *, binomtop + 1);
      for (i = 0; i <= binomtop; i++)
        binomtable[i] = newarray_atomic(int, i + 1);
      binomtable[0][0] = 1;
      binomtable[1][0] = 1;
      binomtable[1][1] = 1;
      for (i = 2; i <= binomtop; i++)
        {
          binomtable[i][0] = 1;
          binomtable[i][i] = 1;
          for (j = 1; j < i; j++)
            binomtable[i][j] = binomtable[i - 1][j - 1] + binomtable[i - 1][j];
        }

      diffcoeffstable = newarray(int *, diffcoeffstop + 1);
      for (i = 0; i <= diffcoeffstop; i++)
        diffcoeffstable[i] = newarray_atomic(int, i + 1);
      diffcoeffstable[0][0] = 1;
      diffcoeffstable[1][0] = 1;
      diffcoeffstable[1][1] = 1;
      for (i = 2; i <= diffcoeffstop; i++)
        {
          diffcoeffstable[i][0] = 1;
          for (j = 1; j <= i; j++)
            diffcoeffstable[i][j] = i * diffcoeffstable[i - 1][j - 1];
        }
#if 0
//       // Display the binomial tables:
//       cout << "---binom table---" << endl;
//       for (i=0; i<=binomtop; i++)
//      {
//        for (j=0; j<=i; j++)
//          cout << "  " << binomtable[i][j];
//        cout << endl;
//      }
//       cout << "---diff table---" << endl;
//       for (i=0; i<=diffcoeffstop; i++)
//      {
//        for (j=0; j<=i; j++)
//          cout << "  " << diffcoeffstable[i][j];
//        cout << endl;
//      }
#endif
    }
}

ring_elem WeylAlgebra::binomial(int top, int bottom) const
{
  // This should be located elsewhere
  // Assumption: bottom <= top, top >= 0, bottom >= 0.
  if (bottom == 0) return K_->from_long(1);
  if (bottom == 1) return K_->from_long(top);
  if (top <= binomtop) return K_->from_long(binomtable[top][bottom]);
  ring_elem result = K_->from_long(1);
  for (int a = 0; a < bottom; a++)
    {
      ring_elem b = K_->from_long(top - a);
      ring_elem result1 = K_->mult(result, b);
      K_->remove(result);
      K_->remove(b);
      ring_elem c = K_->from_long(a + 1);
      result = K_->divide(result1, c);  // exact
      K_->remove(c);
    }
  return result;
}

ring_elem WeylAlgebra::multinomial(ring_elem c,
                                   const int *top,
                                   const int *bottom) const
{
  // Assumed: top[i] >= bottom[i] for all i.
  ring_elem result = K_->copy(c);
  for (int i = 0; i < _nderivatives; i++)
    if (bottom[i] > 0)
      {
        ring_elem a = binomial(top[i], bottom[i]);
        ring_elem b = K_->mult(a, result);
        K_->remove(a);
        K_->remove(result);
        result = b;
      }
  return result;
}

bool WeylAlgebra::divides(const int *expbottom, const int *exptop) const
{
  for (int i = 0; i < _nderivatives; i++)
    if (expbottom[i] > exptop[i]) return false;
  return true;
}

bool WeylAlgebra::increment(int *current_derivative,
                            const int *top_derivative) const
{
  int i = 0;
  while (current_derivative[i] == top_derivative[i])
    {
      i++;
      if (i >= _nderivatives)
        {
          return false;
        }
    }
  for (int j = 0; j < i; j++) current_derivative[j] = 0;
  current_derivative[i]++;
  return true;
}

void WeylAlgebra::extractDerivativePart(const int *exponents,
                                        int *result_derivatives) const
{
  // exponents: 0..nvars-1
  // result_derivatives: 0.._nderivatives-1 is the result
  for (int i = 0; i < _nderivatives; i++)
    result_derivatives[i] = exponents[_derivative[i]];
}
void WeylAlgebra::extractCommutativePart(const int *exponents,
                                         int *result_exp) const
{
  // exponents: 0..nvars-1
  // result_exp: 0.._nderivatives-1 is the result
  for (int i = 0; i < _nderivatives; i++)
    result_exp[i] = exponents[_commutative[i]];
}

ring_elem WeylAlgebra::diff_coefficients(const ring_elem c,
                                         const int *derivatives,
                                         const int *exponents) const
{
  ring_elem result = K_->copy(c);
  for (int i = 0; i < _nderivatives; i++)
    {
      if (derivatives[i] == 0) continue;
      if (exponents[i] <= diffcoeffstop)
        {
          ring_elem g =
              K_->from_long(diffcoeffstable[exponents[i]][derivatives[i]]);
          ring_elem h = K_->mult(result, g);
          K_->remove(g);
          K_->remove(result);
          result = h;
          if (K_->is_zero(result)) return result;
        }
      else
        for (int j = derivatives[i] - 1; j >= 0; j--)
          {
            ring_elem g = K_->from_long(exponents[i] - j);
            ring_elem h = K_->mult(result, g);
            K_->remove(g);
            K_->remove(result);
            result = h;
            if (K_->is_zero(result)) return result;
          }
    }
  return result;
}

Nterm *WeylAlgebra::weyl_diff(const ring_elem c,
                              const int *expf,  // The exponent vector of f
                              const int *derivatives,
                              const Nterm *g) const  // An entire polynomial
{
  // This isn't really differentiation, but it is close.
  // It is the inner loop of the multiplication routine for the Weyl algebra.
  // Returns: sum of d*[n,derivative]*c*n*m/(derivative,derivatives) e_i, for
  // each
  // term d*n*e_i of v which satisfies: x part of n is >= derivatives,
  // and where the multiplication and division of monomials is in the
  // commutative
  // monoid.

  Nterm head;
  head.next = 0;
  Nterm *result = &head;

  int i;
  int *exp = newarray_atomic(int, _nderivatives);
  int *deriv_exp = newarray_atomic(int, nvars_);
  int *result_exp = newarray_atomic(int, nvars_);
  for (i = 0; i < nvars_; i++) deriv_exp[i] = 0;
  if (_homogeneous_weyl_algebra)
    {
      int sum = 0;
      for (i = 0; i < _nderivatives; i++)
        {
          sum += 2 * derivatives[i];
          deriv_exp[_derivative[i]] = derivatives[i];
          deriv_exp[_commutative[i]] = derivatives[i];
        }
      deriv_exp[_homog_var] = -sum;
    }
  else
    for (i = 0; i < _nderivatives; i++)
      {
        deriv_exp[_derivative[i]] = derivatives[i];
        deriv_exp[_commutative[i]] = derivatives[i];
      }

  for (const Nterm *t = g; t != 0; t = t->next)
    {
      // This first part checks whether the x-part of t->monom is divisible by
      // 'derivatives'.  If so, true is returned, and the resulting monomial is
      // set.
      M_->to_expvector(t->monom, result_exp);
      extractCommutativePart(result_exp, exp);
      if (divides(derivatives, exp))
        {
          ring_elem a = diff_coefficients(c, derivatives, exp);
          if (K_->is_zero(a))
            {
              K_->remove(a);
              continue;
            }
          ring_elem b = K_->mult(a, t->coeff);
          K_->remove(a);
          if (K_->is_zero(b))
            {
              K_->remove(b);
              continue;
            }
          // Now compute the new monomial:
          Nterm *tm = new_term();
          tm->coeff = b;
          for (int i2 = 0; i2 < nvars_; i2++)
            result_exp[i2] += expf[i2] - deriv_exp[i2];
          M_->from_expvector(result_exp, tm->monom);

          // Append to the result
          result->next = tm;
          result = tm;
        }
    }
  freemem(exp);
  freemem(result_exp);
  result->next = 0;
  return head.next;
}

ring_elem WeylAlgebra::mult_by_term(const ring_elem f,
                                    const ring_elem c,
                                    const int *m) const
// Computes c*m*f
{
  int *top_derivative = newarray_atomic(int, _nderivatives);
  int *current_derivative = newarray_atomic_clear(int, _nderivatives);
  int *expf = newarray_atomic(int, nvars_);
  polyheap result(this);

  M_->to_expvector(m, expf);
  extractDerivativePart(expf, top_derivative);
  // Loop over each current_derivative <= top_derivative.
  do
    {
      ring_elem d = multinomial(c, top_derivative, current_derivative);
      Nterm *h = weyl_diff(d, expf, current_derivative, f);
      K_->remove(d);
      result.add(h);
    }
  while (increment(current_derivative, top_derivative));

  freemem(expf);
  freemem(top_derivative);
  freemem(current_derivative);
  return result.value();
}

//////////////////////////////////
// gbvector multiplication ///////
//////////////////////////////////
// These are essentially identical to the two
// routines above.  Perhaps they should be
// formed from a template?
// It seems difficult, since the interfaces are somewhat different.

gbvector *WeylAlgebra::gbvector_weyl_diff(
    GBRing *GR,
    const ring_elem c,  // in K_
    int comp,           // adds this component to each component of g.
    const int *expf,    // The exponent vector of f
    const int *derivatives,
    const FreeModule
        *F,  // freemodule of g, only needed because of possible Schreyer order
    const gbvector *g) const  // An entire polynomial
{
  // This isn't really differentiation, but it is close.
  // It is the inner loop of the multiplication routine for the Weyl algebra.
  // Returns: sum of d*[n,derivative]*c*n*m/(derivative,derivatives) e_i, for
  // each
  // term d*n*e_i of v which satisfies: x part of n is >= derivatives,
  // and where the multiplication and division of monomials is in the
  // commutative
  // monoid.

  gbvector head;
  head.next = 0;
  gbvector *result = &head;

  int i;
  int *exp = newarray_atomic(int, _nderivatives);
  int *deriv_exp = newarray_atomic_clear(int, nvars_);
  int *result_exp = newarray_atomic(int, nvars_);
  if (_homogeneous_weyl_algebra)
    {
      int sum = 0;
      for (i = 0; i < _nderivatives; i++)
        {
          sum += 2 * derivatives[i];
          deriv_exp[_derivative[i]] = derivatives[i];
          deriv_exp[_commutative[i]] = derivatives[i];
        }
      deriv_exp[_homog_var] = -sum;
    }
  else
    for (i = 0; i < _nderivatives; i++)
      {
        deriv_exp[_derivative[i]] = derivatives[i];
        deriv_exp[_commutative[i]] = derivatives[i];
      }

  for (const gbvector *t = g; t != 0; t = t->next)
    {
      // This first part checks whether the x-part of t->monom is divisible by
      // 'derivatives'.  If so, true is returned, and the resulting monomial is
      // set.
      GR->gbvector_get_lead_exponents(F, t, result_exp);
      extractCommutativePart(result_exp, exp);
      if (divides(derivatives, exp))
        {
          ring_elem a = diff_coefficients(c, derivatives, exp);
          if (K_->is_zero(a))
            {
              K_->remove(a);
              continue;
            }
          ring_elem b = K_->mult(a, t->coeff);
          K_->remove(a);
          if (K_->is_zero(b))
            {
              K_->remove(b);
              continue;
            }
          // Now compute the new monomial:
          for (int i2 = 0; i2 < nvars_; i2++)
            result_exp[i2] += expf[i2] - deriv_exp[i2];

          gbvector *tm =
              GR->gbvector_term_exponents(F, b, result_exp, comp + t->comp);

          // Append to the result
          result->next = tm;
          result = tm;
        }
    }
  freemem(exp);
  freemem(result_exp);
  result->next = 0;
  return head.next;
}

gbvector *WeylAlgebra::gbvector_mult_by_term(
    gbvectorHeap &result,
    const gbvector *f,
    const ring_elem c,  // in the base K_
    const int *m,       // monomial, in M_
    int comp) const     // comp is either 0 or a real component.
// Computes c*m*f*e_comp  (where components of f and e_comp add).
{
  int *top_derivative = newarray_atomic(int, _nderivatives);
  int *current_derivative = newarray_atomic_clear(int, _nderivatives);
  int *expf = newarray_atomic(int, nvars_);

  GBRing *GR = result.get_gb_ring();
  const FreeModule *F = result.get_freemodule();
  M_->to_expvector(m, expf);
  extractDerivativePart(expf, top_derivative);
  // Loop over each current_derivative <= top_derivative.
  do
    {
      ring_elem d =
          multinomial(c, top_derivative, current_derivative);  // in K_
      gbvector *h =
          gbvector_weyl_diff(GR, d, comp, expf, current_derivative, F, f);
      K_->remove(d);
      result.add(h);
    }
  while (increment(current_derivative, top_derivative));

  freemem(expf);
  freemem(top_derivative);
  freemem(current_derivative);
  return result.value();
}

/////////////////
ring_elem WeylAlgebra::multinomial(const int *exptop, const int *exp) const
{
  ring_elem result = K_->from_long(1);
  for (int i = 0; i < nvars_; i++)
    if (exptop[i] > 0)
      {
        for (int j = exptop[i]; j > exp[i]; j--)
          {
            ring_elem c = K_->from_long(j);
            ring_elem d = K_->from_long(exptop[i] - j + 1);
            K_->mult_to(result, c);
            ring_elem e = K_->divide(result, d);  // exact
            K_->remove(c);
            K_->remove(d);
            K_->remove(result);
            result = e;
          }
      }
  return result;
}

ring_elem WeylAlgebra::power(const ring_elem f, mpz_srcptr n) const
{
  std::pair<bool, int> n1 = RingZZ::get_si(n);
  if (n1.first)
    return power(f, n1.second);
  else
    throw exc::engine_error("exponent too large");
}

ring_elem WeylAlgebra::power(const ring_elem f, int n) const
{
  return Ring::power(f, n);
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
