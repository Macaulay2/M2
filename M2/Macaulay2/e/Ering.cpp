// Copyright 1998 by Michael Stillman

#include "Ering.hpp"
#include "Efreemod.hpp"
#include "Evector.hpp"
#include "Ematrix.hpp"
#include "Ehashtab.hpp"

////////////////////
// ECommPolynomialRing //
////////////////////

ECommPolynomialRing::ECommPolynomialRing()
  : EPolynomialRing(),
    M(0)
{
}

ECommPolynomialRing::ECommPolynomialRing(
      const ECoefficientRing *KK,
      const ECommMonoid *MM,
      const EPolynomialRing *ZD,
      int *degs)  // grabbed
  : EPolynomialRing(KK,ZD,degs),
    M(MM)
{
  bump_up(M);
}

ECommPolynomialRing::~ECommPolynomialRing()
{
  bump_down(M);
}

ECommPolynomialRing *ECommPolynomialRing::make(
      const ECoefficientRing *KK,
      const ECommMonoid *MM,
      const EPolynomialRing *ZD,
      int *degs)  // grabbed: length should be exactly nvars * ndegrees.
{
  ECommPolynomialRing *R = new ECommPolynomialRing(KK,MM,ZD,degs);
  R->R1 = R->makeFreeModule(1);
  return R;
}

///////////////////////////////////
// ESkewCommPolynomialRing //
///////////////////////////////////

ESkewCommPolynomialRing::ESkewCommPolynomialRing(
	      const ECoefficientRing *KK, 
	      const ECommMonoid *MM, 
	      const EPolynomialRing *ZD,
	      int *degs, // grabbed
	      int nskew, 
	      int *skew)
  : ECommPolynomialRing(KK,MM,ZD,degs), nskew(nskew)
{
  int i;
  int n = MM->n_vars();
  this->skewvars = new bool[n];
  this->skewlist = new int[nskew];
  for (i=0; i<n; i++)
    this->skewvars[i] = false;
  for (i=0; i<nskew; i++)
    {
      this->skewlist[i] = skew[i];
      this->skewvars[skew[i]] = true;
    }
  skew_mvars = new int[nskew];
  skew_nvars = new int[nskew];
}

ESkewCommPolynomialRing::~ESkewCommPolynomialRing()
{
  delete [] skewlist;
  delete [] skewvars;
  delete [] skew_mvars;
  delete [] skew_nvars;
}

ESkewCommPolynomialRing *ESkewCommPolynomialRing::make(
              const ECoefficientRing *KK, 
	      const ECommMonoid *MM, 
	      const EPolynomialRing *ZD,
	      int *degs, // grabbed
	      int nskew, 
	      int *skew)
{
  ESkewCommPolynomialRing *R = new ESkewCommPolynomialRing(KK,MM,ZD,degs,nskew,skew);
  R->R1 = R->makeFreeModule(1);
  return R;
}

EVector *ESkewCommPolynomialRing::makeTerm(
          const EFreeModule *F, 
	  const field a, 
	  const monomial *m, 
	  int x) const
{
  const int *exponents = M->to_exponents(m);
  for (int i=0; i<nskew; i++)
    if (exponents[skewlist[i]] > 1) 
      return F->zero();
  return ECommPolynomialRing::makeTerm(F,a,m,x);
}

int ESkewCommPolynomialRing::exp_skew_vars(const int *exp, int *result) const
    // The number s of skew variables in the exponent vector 'exp' is returned, and their
    // indices are placed in result[0], ..., result[s-1].
    // The space that 'result' points to MUST hold at least 'nskew' ints.
{
  int i;
  int next = 0;
  for (i=0; i<nskew; i++)
    {
      int v = skewlist[i];
      if (exp[v] > 0)
	result[next++] = v;
    }
  return next;
}

int ESkewCommPolynomialRing::skew_vars(const monomial *m, int *result) const
    // The number s of skew variables in 'm' is returned, and their
    // indices are placed in result[0], ..., result[s-1].
    // The space that 'result' points to MUST hold at least 'nskew' ints.
{
  const int *exp = M->to_exponents(m);
  return exp_skew_vars(exp,result);
}

int ESkewCommPolynomialRing::skew_mult_sign(const monomial *m, const monomial *n) const
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

/*
 * mult1: multiply the single term 'f' by the vector 'v'.
 */

EVector *ESkewCommPolynomialRing::skew_mult1(
    const EFreeModule *resultF,
    bool component_from_f,
    const poly *f,			// A single term, either in the ring, or in resultF.
    const poly *g) const
{
  EVector *result = new EVector;
  poly head;
  poly *res = &head;
  int len = 0;
  for (const poly *h = g; h != 0; h=h->next)
    {
      int sgn = skew_mult_sign(f->monom, h->monom);
      if (sgn == 0) continue;
      len++;
      poly *tm = newTerm();
      tm->coeff = K->mult(f->coeff, h->coeff);
      if (sgn < 0)
	{
	  field a = K->negate(tm->coeff);
	  K->remove(tm->coeff);
	  tm->coeff = a;
	}
      tm->monom = M->mult(f->monom, h->monom);
      tm->component = (component_from_f ? f->component : h->component);
      res->next = tm;
      res = res->next;
    }
  res->next = 0;
  result->len = len;
  result->elems = head.next;
  result->F = resultF;
  return result;
}

EVector *ESkewCommPolynomialRing::mult(
    const EVector *f, 
    const EVector *g,
    bool component_from_f) const
{
  const EFreeModule *resultF = (component_from_f ? f->F : g->F);
  if (f->len == 0) return resultF->zero();
  if (f->len == 1) return skew_mult1(resultF,component_from_f,f->elems,g->elems);
  EVectorHeap h(resultF);
  for (poly *fterm = f->elems; fterm != 0; fterm = fterm->next)
    {
      EVector *gv = skew_mult1(resultF,component_from_f,fterm,g->elems);
      h.add(gv);
    }
  return h.value();
}

////////////////////
// EWeylAlgebra /////
////////////////////

int EWeylAlgebra::binomtop = 15;
int EWeylAlgebra::diffcoeffstop = 10;
int **EWeylAlgebra::binomtable = 0;
int **EWeylAlgebra::diffcoeffstable = 0;
EWeylAlgebra::EWeylAlgebra(const ECoefficientRing *KK, 
                           const ECommMonoid *MM, 
                           const EPolynomialRing *ZD,
                           int *degs,  // grabbed, length not checked
			   int npairs, 
			   const int *deriv, const int *comm,
			   bool is_homog, int homog_var)
  : ECommPolynomialRing(KK,MM,ZD,degs), 
    nderivatives(npairs),
    homogeneous_weyl_algebra(is_homog),
    homog_var(homog_var)
{
  derivative = new int[nderivatives];
  commutative = new int[nderivatives];
  for (int i=0; i<nderivatives; i++)
    {
      derivative[i] = deriv[i];
      commutative[i] = comm[i];
    }
  initialize();
}

EWeylAlgebra::~EWeylAlgebra()
{
  delete [] derivative;
  delete [] commutative;
}

EWeylAlgebra *EWeylAlgebra::make(const ECoefficientRing *KK, 
                                 const ECommMonoid *MM, 
                                 const EPolynomialRing *ZD,
                                 int *degs,  // grabbed, length not checked
			         int npairs, 
			         const int *deriv, const int *comm)
{
  int nvars = MM->n_vars();
  for (int i=0; i<npairs; i++)
    {
      if (deriv[i] < 0 || deriv[i] >= nvars)
	return 0;
      if (comm[i] < 0 || comm[i] >= nvars)
	return 0;
    }
  EWeylAlgebra *R = new EWeylAlgebra(KK,MM,ZD,degs,npairs,deriv,comm,false,0);
  R->R1 = R->makeFreeModule(1);
  return R;
}

EWeylAlgebra *EWeylAlgebra::make(const ECoefficientRing *KK, 
                                 const ECommMonoid *MM, 
                                 const EPolynomialRing *ZD,
                                 int *degs,  // grabbed, length not checked
			         int npairs, 
			         const int *deriv, const int *comm,
			         int homog_var)
{
  int nvars = MM->n_vars();
  if (homog_var < 0 || homog_var >= nvars)
    return 0;
  for (int i=0; i<npairs; i++)
    {
      if (deriv[i] < 0 || deriv[i] >= nvars)
	return 0;
      if (comm[i] < 0 || comm[i] >= nvars)
	return 0;
    }
  EWeylAlgebra *R = new EWeylAlgebra(KK,MM,ZD,degs,npairs,deriv,comm,true,homog_var);
  R->R1 = R->makeFreeModule(1);
  return R;
}


void EWeylAlgebra::initialize()
{
  if (binomtable == 0)
    {
      int i,j;

      binomtable = new int *[binomtop+1];
      for (i=0; i<=binomtop; i++)
	binomtable[i] = new int[i+1];
      binomtable[0][0] = 1;
      binomtable[1][0] = 1;
      binomtable[1][1] = 1;
      for (i=2; i<=binomtop; i++)
	{
	  binomtable[i][0] = 1;
	  binomtable[i][i] = 1;
	  for (j=1; j<i; j++)
	    binomtable[i][j] = binomtable[i-1][j-1] + binomtable[i-1][j];
	}

      diffcoeffstable = new int *[diffcoeffstop+1];
      for (i=0; i<=diffcoeffstop; i++)
	diffcoeffstable[i] = new int[i+1];
      diffcoeffstable[0][0] = 1;
      diffcoeffstable[1][0] = 1;
      diffcoeffstable[1][1] = 1;
      for (i=2; i<=diffcoeffstop; i++)
	{
	  diffcoeffstable[i][0] = 1;
	  for (j=1; j<=i; j++)
	    diffcoeffstable[i][j] = i * diffcoeffstable[i-1][j-1];
	}
#if 0
      // Display the binomial tables:
      cout << "---binom table---" << endl;
      for (i=0; i<=binomtop; i++)
	{
	  for (j=0; j<=i; j++)
	    cout << "  " << binomtable[i][j];
	  cout << endl;
	}
      cout << "---diff table---" << endl;
      for (i=0; i<=diffcoeffstop; i++)
	{
	  for (j=0; j<=i; j++)
	    cout << "  " << diffcoeffstable[i][j];
	  cout << endl;
	}
#endif
    }
}

field EWeylAlgebra::binomial(int top, int bottom) const
{
  // This should be located elsewhere
  // Assumption: bottom <= top, top >= 0, bottom >= 0.
  if (bottom == 0) return K->from_int(1);
  if (bottom == 1) return K->from_int(top);
  if (top <= binomtop)
    return K->from_int(binomtable[top][bottom]);
  field result = K->one();
  for (int a=0; a<bottom; a++)
    {
      field b = K->from_int(top-a);
      field result1 = K->mult(result,b);
      K->remove(result);
      K->remove(b);
      field c = K->from_int(a+1);
      result = K->divide(result1,c);
      K->remove(c);
    }
  return result;
}

field EWeylAlgebra::multinomial(field c, const int *top, const int *bottom) const
{
  // Assumed: top[i] >= bottom[i] for all i.
  field result = K->clone(c);
  for (int i=0; i<nderivatives; i++)
    if (bottom[i] > 0)
      {
	field a = binomial(top[i],bottom[i]);
	field b = K->mult(a,result);
	K->remove(a);
	K->remove(result);
	result = b;
      }
  return result;
}

bool EWeylAlgebra::divides(const int *expbottom, const int *exptop) const
{
  for (int i=0; i<nderivatives; i++)
    if (expbottom[i] > exptop[i])
      return false;
  return true;
}

bool EWeylAlgebra::increment(int *current_derivative, 
			    const int *top_derivative) const
{
  int i = 0;
  while (current_derivative[i] == top_derivative[i])
    {
      i++;
      if (i >= nderivatives)
	{
	  return false;
	}
    }
  for (int j=0; j<i; j++)
    current_derivative[j] = 0;
  current_derivative[i]++;
  return true;
}

EVector *EWeylAlgebra::mult(const EVector *f, const EVector *g, bool component_from_f) const
{
  const EFreeModule *resultF = (component_from_f ? f->F : g->F);
  if (f->len == 0) return resultF->zero();
  if (f->len == 1) return weyl_mult1(resultF,component_from_f,f->elems,g->elems);
  EVectorHeap h(resultF);
  for (poly *fterm = f->elems; fterm != 0; fterm = fterm->next)
    {
      EVector *gv = weyl_mult1(resultF,component_from_f,fterm,g->elems);
      h.add(gv);
    }
  return h.value();
}

EVector *EWeylAlgebra::weyl_mult1(
    const EFreeModule *resultF,
    bool component_from_f,
    const poly *f,			// A single term, either in the ring, or in resultF.
    const poly *g) const
{
  int *top_derivative = new int[nderivatives];
  int *current_derivative = new int[nderivatives];
  EVectorHeap result(resultF);

  extractDerivativePart(M->to_exponents(f->monom), top_derivative);
  for (int i=0; i<nderivatives; i++) current_derivative[i] = 0;
  // Loop over each current_derivative <= top_derivative.
  do {
      field c = multinomial(f->coeff, top_derivative, current_derivative);
      EVector *h = diff(resultF,component_from_f,c,f,current_derivative,g);
      K->remove(c);
      result.add(h);
  } while (increment(current_derivative, top_derivative));

  delete [] top_derivative;
  delete [] current_derivative;
  return result.value();
}

EVector *EWeylAlgebra::diff(const EFreeModule *resultF,
			  bool component_from_f,
			  const field c,
			  const poly *f, // A single monomial
			  const int *derivatives, 
			  const poly *v) const
{
  // This isn't really differentiation, but it is close.
  // It is the inner loop of the multiplication routine for the Weyl algebra.
  // Returns: sum of d*[n,derivative]*c*n*m/(derivative,derivatives) e_i, for each
  // term d*n*e_i of v which satisfies: x part of n is >= derivatives,
  // and where the multiplication and division of monomials is in the commutative
  // monoid.

  poly head;
  poly *result = &head;
  int i, len = 0;
  int *exp = new int[nderivatives];
  int *deriv_exp = new int[M->n_vars()];
  for (i=0; i<M->n_vars(); i++)
    deriv_exp[i] = 0;
  if (homogeneous_weyl_algebra)
    {
      int sum = 0;
      for (i=0; i<nderivatives; i++)
	{
	  sum += 2*derivatives[i];
	  deriv_exp[derivative[i]] = derivatives[i];
	  deriv_exp[commutative[i]] = derivatives[i];
	}
      deriv_exp[homog_var] = sum;
    }
  else
    for (i=0; i<nderivatives; i++)
      {
	deriv_exp[derivative[i]] = derivatives[i];
	deriv_exp[commutative[i]] = derivatives[i];
      }
  monomial *deriv_monomial = M->monomial_from_exponents(deriv_exp);

  for (const poly *t = v; t != 0; t = t->next)
    {
      // This first part checks whether the x-part of t->monom is divisible by
      // 'derivatives'.  If so, true is returned, and the resulting monomial is set.
      extractCommutativePart(M->to_exponents(t->monom), exp);
      if (divides(derivatives,exp))
	{
	  field a = diff_coefficients(c,derivatives,exp);
	  if (K->is_zero(a))
	    {
	      K->remove(a);
	      continue;
	    }
	  field b = K->mult(a, t->coeff);
	  K->remove(a);
	  if (K->is_zero(b))
	    {
	      K->remove(b);
	      continue;
	    }
	  // Now compute the new monomial:
	  poly *g = newTerm();
	  g->coeff = b;
	  monomial *m1 = M->mult(f->monom,t->monom);
	  g->monom = M->divide(m1,deriv_monomial);
	  g->component = (component_from_f ? f->component : t->component);
	  result->next = g;
	  result = g;
	  len++;
	}
    }
  result->next = 0;
  EVector *resultv = new EVector;
  resultv->len = len;
  resultv->F = resultF;
  resultv->elems = head.next;
  delete [] exp;
  return resultv;
}

void EWeylAlgebra::extractDerivativePart(const int *exponents, int *result_derivatives) const
{
  // exponents: 0..nvars-1
  // result_derivatives: 0..nderivatives-1 is the result
  for (int i=0; i<nderivatives; i++)
    result_derivatives[i] = exponents[derivative[i]];
}
void EWeylAlgebra::extractCommutativePart(const int *exponents, int *result_exp) const
{
  // exponents: 0..nvars-1
  // result_exp: 0..nderivatives-1 is the result
  for (int i=0; i<nderivatives; i++)
    result_exp[i] = exponents[commutative[i]];
}

field EWeylAlgebra::diff_coefficients(const field c, const int *derivatives, const int *exponents) const
{
  field result = K->clone(c);
  for (int i=0; i<nderivatives; i++)
    {
      if (derivatives[i] == 0) continue;
      if (exponents[i] <= diffcoeffstop)
	{
	  field g = K->from_int(diffcoeffstable[exponents[i]][derivatives[i]]);
	  field h = K->mult(result,g);
	  K->remove(g);
	  K->remove(result);
	  result = h;
	  if (K->is_zero(result))
	    return result;
	}
      else for (int j=derivatives[i]-1; j>=0; j--)
	{
	  field g = K->from_int(exponents[i]-j);
	  field h = K->mult(result,g);
	  K->remove(g);
	  K->remove(result);
	  result = h;
	  if (K->is_zero(result))
	    return result;
      }
    }
  return result;
}

//////////////////////
// ENCPolynomialRing //
//////////////////////
ENCPolynomialRing::ENCPolynomialRing(const ECoefficientRing *KK, 
                                     const ENCMonoid *MM,
                                     const EPolynomialRing *ZD,
                                     int *degs)  // grabbed, length not checked
:  EPolynomialRing(KK,ZD,degs), M(MM)
{
  bump_up(M);
}

ENCPolynomialRing::~ENCPolynomialRing()
{
  bump_down(M);
}

ENCPolynomialRing *ENCPolynomialRing::make(const ECoefficientRing *KK, 
                                           const ENCMonoid *MM,
                                           const EPolynomialRing *ZD,
                                           int *degs)  // grabbed, length not checked
{
  ENCPolynomialRing *R = new ENCPolynomialRing(KK,MM,ZD,degs);
  R->R1 = R->makeFreeModule(1);
  return R;
}

///////////////////////////
// EPolynomialRing //
///////////////////////////

EPolynomialRing *EPolynomialRing::_trivial = 0;

void ECommPolynomialRing::initializeTrivialRing()
{
  if (_trivial != 0) return;
  ECommPolynomialRing *triv = new ECommPolynomialRing;
  triv->D = ECommMonoid::getTrivialMonoid();
  triv->ZD = _trivial;
  triv->M = ECommMonoid::getTrivialMonoid();
  bump_up(triv->M);
  bump_up(triv);  // Now it will never go away.
  _trivial = triv;
}

const EPolynomialRing *EPolynomialRing::getTrivialRing()
  // ZZ as a polynomial ring, trivial degree ring.
{
  if (_trivial == 0)
    ECommPolynomialRing::initializeTrivialRing();
  return _trivial;
}

EPolynomialRing::EPolynomialRing()
  : K(EZZ::ZZ()),
    polyblocks(0),
    freelist(0),
    D(0),
    ZD(0),
    _degrees(0),
    R1(0)
{
  // Only used to construct the trivial ring.
}

EPolynomialRing::EPolynomialRing(const ECoefficientRing *KK, 
                                 const EPolynomialRing *ZZDD,
                                 int * degs  // grabbed
                                 )
  : K(KK),
    polyblocks(0),
    freelist(0),
    D(0),
    ZD(ZZDD),
    _degrees(degs),
    R1(0)
{
  if (ZD == 0)
    {
      ECommPolynomialRing::initializeTrivialRing();
      ZD = _trivial;
    }
  D = ZD->getMonoid();
  bump_up(K);
  bump_up(ZD);
}

EPolynomialRing::EPolynomialRing(const ECoefficientRing *KK)
  : K(KK),
    polyblocks(0),
    freelist(0),
    D(0),
    ZD(0),
    _degrees(0),
    R1(0)
{
  if (ZD == 0)
    {
      ECommPolynomialRing::initializeTrivialRing();
      ZD = _trivial;
    }
  D = ZD->getMonoid();
  bump_up(K);
  bump_up(ZD);
}

EPolynomialRing::~EPolynomialRing()
{
  bump_down(K);
}

const ENCPolynomialRing *EPolynomialRing::toENCPolynomialRing() const
{
  return 0;
}

const EPolynomialRing *EPolynomialRing::getCover() const
{
  return this;
}

EFreeModule *EPolynomialRing::makeFreeModule(int rank) const
  // Make a free module over this ring.
{
  if (rank < 0)
    {
      gError << "freemodule rank must be non-negative";
      return 0;
    }
  if (rank == 1 && R1 != 0)
    return R1;
  EFreeModule *result;
  if (isQuotient())
    {
      EFreeModule *F = getCover()->makeFreeModule(rank);
      result = new EFreeModule(this,F);
    }
  else
    {
      result = new EFreeModule(this,rank);
      result->setCover(result);
    }
  EUniqueObjects.insertEFreeModule(result);
  return result;
}

EFreeModule *EPolynomialRing::makeFreeModule(int rank, const monomial **degrees) const
  // Make a free module over this ring.
{
  if (rank < 0)
    {
      gError << "freemodule rank must be non-negative";
      return 0;
    }
  EFreeModule *result;
  if (isQuotient())
    {
      EFreeModule *F = getCover()->makeFreeModule(rank,degrees);
      result = new EFreeModule(this,F);
    }
  else
    {
      result = new EFreeModule(this,rank,degrees);
      result->setCover(result);
    }
  EUniqueObjects.insertEFreeModule(result);
  return result;
}

EFreeModule *EPolynomialRing::makeFreeModule(int rank, 
                                      const monomial **degrees,  // grabbed
                                      const monomial **ordering, // grabbed
                                      int *tiebreaks) const     // grabbed
{
  if (rank < 0)
    {
      gError << "freemodule rank must be non-negative";
      return 0;
    }
  EFreeModule *result;
  if (isQuotient())
    {
      EFreeModule *F = getCover()->makeFreeModule(rank,degrees,ordering,tiebreaks);
      result = new EFreeModule(this,F);
    }
  else
    {
      result = new EFreeModule(this,rank,degrees,ordering,tiebreaks);
      result->setCover(result);
    }
  buffer o;
  o << "Schreyer free created: ";
  result->text_out(o);
  o << newline;
  emit(o.str());
  EUniqueObjects.insertEFreeModule(result);
  return result;
}

EFreeModule *EPolynomialRing::makeFreeModule(const EMatrix *m) const
{
  const EFreeModule *G = m->getSource();
  if (G->getRing() != this)
    {
      gError << "expected a matrix over the same ring";
      return 0;
    }
  const EMonoid *M = getMonoid();
  int rank = m->n_cols();
  const monomial **degrees = new monomial *[rank];
  const monomial **ordering = new monomial *[rank];
  int *tiebreaks = new int[rank];
  for (int i=0; i<rank; i++)
    {
      degrees[i] = G->getDegree(i);
      if (m->column(i)->isZero())
        {
          ordering[i] = M->one();
          tiebreaks[i] = 0;
        }
      else
        {
          ordering[i] = m->column(i)->elems->monom;
          tiebreaks[i]  = m->column(i)->elems->component;
        }
    }
  return makeFreeModule(rank,degrees,ordering,tiebreaks);
}

const int *EPolynomialRing::getDegreeVector(int i) const
{
  if (i < 0 || i > n_degrees())
    {
      gError << "internal error: getDegreeVector";
      return _degrees;
    }
  return _degrees + n_vars() * i;
}

void EPolynomialRing::deleteTerm(poly *f) const
{
  if (f == 0) return;
  f->next = freelist;
  ((EPolynomialRing *) this)->freelist = f;
}

void EPolynomialRing::delete_terms(poly *f) const
{
  while (f != 0) {
    poly *tmp = f;
    f = f->next;
    deleteTerm(tmp);
  }
}

poly *EPolynomialRing::newBlockOfTerms() const
{
  block_polys *F = new block_polys;
  F->next = polyblocks;
  ((EPolynomialRing *) this)->polyblocks = F;
  F->blocks[N_POLYS_PER_BLOCK-1].next = 0;
  for (int i=N_POLYS_PER_BLOCK-2; i>=0; --i)
    F->blocks[i].next = &F->blocks[i+1];
  return &F->blocks[0];
}

poly *EPolynomialRing::newTerm() const
{
  if (freelist == 0)
    ((EPolynomialRing *) this)->freelist = newBlockOfTerms();

  poly *result = freelist;
  ((EPolynomialRing *) this)->freelist = freelist->next;
  result->next = 0;
  return result;
}

poly *EPolynomialRing::copy_term(const poly *t) const
{
  poly *result = newTerm();
  result->coeff = t->coeff;  // Copy this!! ??
  result->component = t->component;
  result->monom = t->monom;
  result->next = 0;
  return result;
}

int EPolynomialRing::n_terms(const poly *f) const
{
  int result = 0;
  for ( ; f != 0; f=f->next) result++;
  return result;
}

int EPolynomialRing::add_to(poly *&f, poly *&g) const
{
  // f += g, and the number of monomials of the result is returned.
  const EMonoid *M = getMonoid();
  if (g == 0) return n_terms(f);
  if (f == 0) { f = g; g = 0; return n_terms(f); }
  int len = 0;
  poly head;
  poly *result = &head;
  while (1)
    switch (M->compare(f->monom, f->component, 
		       g->monom, g->component))
      {
      case LT:
	result->next = g;
	result = result->next;
	len++;
	g = g->next;
	if (g == 0) 
	  {
	    len += n_terms(f);
	    result->next = f; 
	    f = head.next;
	    return len;
	  }
	break;
      case GT:
	result->next = f;
	result = result->next;
	len++;
	f = f->next;
	if (f == 0) 
	  {
	    len += n_terms(g);
	    result->next = g;
	    f = head.next;
	    g = 0; 
	    return len;
	  }
	break;
      case EQ:
	poly *tmf = f;
	poly *tmg = g;
	f = f->next;
	g = g->next;
	tmf->coeff = K->add(tmf->coeff, tmg->coeff);
	if (K->is_zero(tmf->coeff))
	  {
	    K->remove(tmf->coeff);
	    deleteTerm(tmf);
	  }
	else
	  {
	    result->next = tmf;
	    result = result->next;
	    len++;
	  }
	K->remove(tmg->coeff);
	deleteTerm(tmg);
	if (g == 0) 
	  {
	    len += n_terms(f);
	    result->next = f;
	    f = head.next;
	    return len;
	  }
	if (f == 0) 
	  {
	    len += n_terms(g);
	    result->next = g; 
	    f = head.next;
	    g = 0;
	    return len;
	  }
	break;
      }
}

void EPolynomialRing::addTo(EVector *&F, EVector *&G) const
{
  const EMonoid *M = getMonoid();
  poly *f = F->elems;
  poly *g = G->elems;
  G->elems = 0;
  F->len += G->len;
  G->len = 0;
  if (g == 0) return;
  if (f == 0) { F->elems = g; return; }
  poly head;
  poly *result = &head;
  while (1)
    switch (M->compare(f->monom, f->component, 
		       g->monom, g->component))
      {
      case LT:
	result->next = g;
	result = result->next;
	g = g->next;
	if (g == 0) 
	  {
	    result->next = f; 
	    F->elems = head.next;
	    return;
	  }
	break;
      case GT:
	result->next = f;
	result = result->next;
	f = f->next;
	if (f == 0) 
	  {
	    result->next = g; 
	    F->elems = head.next;
	    return;
	  }
	break;
      case EQ:
	poly *tmf = f;
	poly *tmg = g;
	f = f->next;
	g = g->next;
	tmf->coeff = K->add(tmf->coeff, tmg->coeff);
	if (K->is_zero(tmf->coeff))
	  {
	    F->len--;
	    K->remove(tmf->coeff);
	    deleteTerm(tmf);
	  }
	else
	  {
	    result->next = tmf;
	    result = result->next;
	  }
	F->len--;
	K->remove(tmg->coeff);
	deleteTerm(tmg);
	if (g == 0) 
	  {
	    result->next = f; 
	    F->elems = head.next;
	    return;
	  }
	if (f == 0) 
	  {
	    result->next = g; 
	    F->elems = head.next;
	    return;
	  }
	break;
      }
}

EVector *EPolynomialRing::makeTerm(const EFreeModule *F, const field a, const monomial *m, int x) const
{
  if (K->is_zero(a)) return F->zero();
  EVector *result = new EVector;
  result->F = F;

  result->len = 1;
  result->elems = newTerm();
  result->elems->coeff = a;
  result->elems->component = x;
  result->elems->monom = m;
  result->elems->next = 0;

  return result;
}

/*
 * mult1: multiply the single term 'f' by the vector 'g'.  The component is taken from
 * 'f' if is_left is true, otherwise the component comes from 'g'.
 */

EVector *EPolynomialRing::mult1(
    const EFreeModule *resultF,
    bool is_left,
    const poly *f, 
    const poly *g) const
{
  EVector *result = new EVector;
  poly head;
  poly *res = &head;
  int len = 0;
  for (const poly *h = g; h != 0; h=h->next)
    {
      len++;
      poly *tm = newTerm();
      tm->coeff = K->mult(f->coeff, h->coeff);
      tm->monom = getMonoid()->mult(f->monom, h->monom);
      tm->component = (is_left ? f->component : h->component);
      res->next = tm;
      res = res->next;
    }
  res->next = 0;
  result->len = len;
  result->elems = head.next;
  result->F = resultF;
  return result;
}

EVector *EPolynomialRing::mult(const EVector *f, const EVector *g, bool component_from_f) const
{
  const EFreeModule *resultF = (component_from_f ? f->F : g->F);
  if (f->len == 0) return resultF->zero();
  if (f->len == 1) return mult1(resultF,component_from_f,f->elems,g->elems);
  EVectorHeap h(resultF);
  for (poly *fterm = f->elems; fterm != 0; fterm = fterm->next)
    {
      EVector *gv = mult1(resultF,component_from_f,fterm,g->elems);
      h.add(gv);
    }
  return h.value();
}
// Multiply: One of these two must be a vector in R1.  The result is in the
// other...
EVector *EPolynomialRing::multiply(
    const EVector *f, 
    const EVector *g) const
{
  return mult(f,g,false);
}

EVector *EPolynomialRing::rightMultiply(
    const EVector *f, 
    const EVector *g) const
{
  return mult(f,g,true);
}

