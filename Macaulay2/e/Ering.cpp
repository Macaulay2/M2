// Copyright 1998 by Michael Stillman

#include "Ering.hpp"
#include "Efreemod.hpp"
#include "Evector.hpp"
#include "Ematrix.hpp"
#include "Ehashtab.hpp"

///////////
// ERing //
///////////
void ERing::initialize(
        int nvars, 
        int totalvars, 
        const ERing *K,
        const EPolynomialRing *ZZDD, // if 0, set to the trivial poly ring ZZ[].
        int *_degs   // grabbed
        )
{
  // Create the two stashes, if needed
  if (vec_stash == 0)
    {
      vec_stash = new stash("vectors", sizeof(evec0));
      vecpoly_stash = new stash("poly vectors", sizeof(evec));
    }

  this->K = K;
  bump_up(K);
  this->nvars = nvars;
  this->totalvars = totalvars;
  this->evec_stash = vec_stash;  // Polynomial rings will change this.
  if (ZZDD == 0)
    ZZDD = ECommPolynomialRing::getTrivialRing();  // this is ZZ[].
  this->ZD = ZZDD;
  bump_up(ZZDD);
  this->D = ZZDD->getMonoid()->toCommMonoid();
  this->_degrees = _degs;
  this->_cover = 0;
  this->GBring = 0;
}
ERing::~ERing()
{
  // What to remove here?
  delete _degrees;
  bump_down(ZD);
  bump_down(K);
}

const int *ERing::getDegreeVector(int i) const
{
  if (i < 0 || i > n_degrees())
    {
      gError << "internal error: getDegreeVector";
      return _degrees;
    }
  return _degrees + n_vars() * i;
}

/////////////////////////
// ECommPolynomialRing //
/////////////////////////

ECommPolynomialRing *ECommPolynomialRing::_trivial = 0;

void ECommPolynomialRing::initialize(
      const ERing *KK,
      const ECommMonoid *MM,
      const EPolynomialRing *ZD,
      int *degs)  // grabbed
{
  EPolynomialRing::initialize(KK,MM->n_vars(),ZD,degs);
  M = MM;
  bump_up(M);
}

ECommPolynomialRing::~ECommPolynomialRing()
{
  bump_down(M);
}

ECommPolynomialRing *ECommPolynomialRing::make(
      const ERing *KK,
      const ECommMonoid *MM,
      const EPolynomialRing *ZD,
      int *degs)  // grabbed: length should be exactly nvars * ndegrees.
{
  ECommPolynomialRing *R = new ECommPolynomialRing;
  R->initialize(KK,MM,ZD,degs);
  return R;
}

void ECommPolynomialRing::makeTrivialRing()
  // REWRITE!!
{
  ECommPolynomialRing *triv = new ECommPolynomialRing;
  // Set all of the fields from ERing, EPolynomialRing, ECommPolynomialRing
  const ECommMonoid *M = ECommMonoid::getTrivialMonoid();
  
  // ERing fields
  triv->K = EZZ::make();
  triv->nvars = 0;
  triv->totalvars = 0;
  triv->evec_stash = vecpoly_stash;
  triv->D = M;
  triv->ZD = triv;
  triv->_degrees = 0;
  triv->GBring = 0;
  triv->_cover = 0;

  // EPolynomialRing fields(s)
  triv->epoly_stash = new stash("polynomials", sizeof(epoly));

  // ECommPolynomialRing field(s)
  triv->M = M;

  bump_up(M);
  bump_up(triv);  // Now it will never go away.
  _trivial = triv;
}

const ECommPolynomialRing *ECommPolynomialRing::getTrivialRing()
  // ZZ as a polynomial ring, trivial degree ring.
{
  if (_trivial == 0)
    makeTrivialRing();
  return _trivial;
}

/////////////////////////////
// ESkewCommPolynomialRing //
/////////////////////////////

void ESkewCommPolynomialRing::initialize(
	      const ERing *KK, 
	      const ECommMonoid *MM, 
	      const EPolynomialRing *ZD,
	      int *degs, // grabbed
	      int nskew, 
	      int *skew)
{
  ECommPolynomialRing::initialize(KK,MM,ZD,degs);
  int i;
  int n = MM->n_vars();
  this->nskew = nskew;
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
              const ERing *KK, 
	      const ECommMonoid *MM, 
	      const EPolynomialRing *ZD,
	      int *degs, // grabbed
	      int nskew, 
	      int *skew)
{
  ESkewCommPolynomialRing *R = new ESkewCommPolynomialRing;
  R->initialize(KK,MM,ZD,degs,nskew,skew);
  return R;
}

EVector ESkewCommPolynomialRing::vec_term(
          const EFreeModule *F, 
	  const ERingElement a, 
	  const monomial *m, 
	  int x) const
{
  const int *exponents = M->to_exponents(m);
  for (int i=0; i<nskew; i++)
    if (exponents[skewlist[i]] > 1) 
      return F->zero();
  return ECommPolynomialRing::vec_term(F,a,m,x);
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
////////////////////
// EWeylAlgebra /////
////////////////////

int EWeylAlgebra::binomtop = 15;
int EWeylAlgebra::diffcoeffstop = 10;
int **EWeylAlgebra::binomtable = 0;
int **EWeylAlgebra::diffcoeffstable = 0;

void EWeylAlgebra::initialize(
        const ERing *KK, 
        const ECommMonoid *MM, 
        const EPolynomialRing *ZD,
        int *degs,  // grabbed, length not checked
	int npairs, 
        const int *deriv, const int *comm,
	bool is_homog, int homog_var)
{
  ECommPolynomialRing::initialize(KK,MM,ZD,degs);
  this->nderivatives = npairs;
  this->homogeneous_weyl_algebra = is_homog;
  this->homog_var = homog_var;

  this->derivative = new int[nderivatives];
  this->commutative = new int[nderivatives];
  for (int i=0; i<nderivatives; i++)
    {
      derivative[i] = deriv[i];
      commutative[i] = comm[i];
    }
  initialize1();
}

EWeylAlgebra::~EWeylAlgebra()
{
  delete [] derivative;
  delete [] commutative;
}

EWeylAlgebra *EWeylAlgebra::make(const ERing *KK, 
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
  EWeylAlgebra *R = new EWeylAlgebra;
  R->initialize(KK,MM,ZD,degs,npairs,deriv,comm,false,0);
  return R;
}

EWeylAlgebra *EWeylAlgebra::make(const ERing *KK, 
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
  EWeylAlgebra *R = new EWeylAlgebra;
  R->initialize(KK,MM,ZD,degs,npairs,deriv,comm,true,homog_var);
  return R;
}

void EWeylAlgebra::initialize1()
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

ERingElement EWeylAlgebra::binomial(int top, int bottom) const
{
  // This should be located elsewhere
  // Assumption: bottom <= top, top >= 0, bottom >= 0.
  if (bottom == 0) return K->from_int(1);
  if (bottom == 1) return K->from_int(top);
  if (top <= binomtop)
    return K->from_int(binomtable[top][bottom]);
  ERingElement result = K->one();
  for (int a=0; a<bottom; a++)
    {
      ERingElement b = K->from_int(top-a);
      ERingElement result1 = K->mult(result,b);
      K->remove(result);
      K->remove(b);
      ERingElement c = K->from_int(a+1);
      result = K->divide(result1,c);
      K->remove(c);
    }
  return result;
}

ERingElement EWeylAlgebra::multinomial(ERingElement c, const int *top, const int *bottom) const
{
  // Assumed: top[i] >= bottom[i] for all i.
  ERingElement result = K->clone(c);
  for (int i=0; i<nderivatives; i++)
    if (bottom[i] > 0)
      {
	ERingElement a = binomial(top[i],bottom[i]);
	ERingElement b = K->mult(a,result);
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

ERingElement EWeylAlgebra::diff_coefficients(const ERingElement c, const int *derivatives, const int *exponents) const
{
  ERingElement result = K->clone(c);
  for (int i=0; i<nderivatives; i++)
    {
      if (derivatives[i] == 0) continue;
      if (exponents[i] <= diffcoeffstop)
	{
	  ERingElement g = K->from_int(diffcoeffstable[exponents[i]][derivatives[i]]);
	  ERingElement h = K->mult(result,g);
	  K->remove(g);
	  K->remove(result);
	  result = h;
	  if (K->is_zero(result))
	    return result;
	}
      else for (int j=derivatives[i]-1; j>=0; j--)
	{
	  ERingElement g = K->from_int(exponents[i]-j);
	  ERingElement h = K->mult(result,g);
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
void ENCPolynomialRing::initialize(
      const ERing *KK,
      const ENCMonoid *MM,
      const EPolynomialRing *ZD,
      int *degs)  // grabbed
{
  EPolynomialRing::initialize(KK,MM->n_vars(),ZD,degs);
  M = MM;
  bump_up(M);
}

ENCPolynomialRing::~ENCPolynomialRing()
{
  bump_down(M);
}

ENCPolynomialRing *ENCPolynomialRing::make(
      const ERing *KK,
      const ENCMonoid *MM,
      const EPolynomialRing *ZD,
      int *degs)  // grabbed: length should be exactly nvars * ndegrees.
{
  ENCPolynomialRing *R = new ENCPolynomialRing;
  R->initialize(KK,MM,ZD,degs);
  return R;
}

///////////////////////////
// EPolynomialRing //
///////////////////////////

void EPolynomialRing::initialize(
             const ERing *K, 
             int nvars,  // only the variables from the monoid, not from K.
             const EPolynomialRing *ZZDD,
             int * _degs  // grabbed
             )
{
  ERing::initialize(nvars, 
                    nvars + K->n_vars(),
                    K,
                    ZZDD,
                    _degs);
  epoly_stash = new stash("polynomials", sizeof(epoly));
  evec_stash = vecpoly_stash;
}
EPolynomialRing::~EPolynomialRing()
{
  delete epoly_stash;
}

EFreeModule *ERing::makeFreeModule(int rank) const
  // Make a free module over this ring.
{
  if (rank < 0)
    {
      gError << "freemodule rank must be non-negative";
      return 0;
    }
  EFreeModule *result;
  if (getCover())
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

EFreeModule *ERing::makeFreeModule(int rank, const monomial **degrees) const
  // Make a free module over this ring.
{
  if (rank < 0)
    {
      gError << "freemodule rank must be non-negative";
      return 0;
    }
  EFreeModule *result;
  if (getCover())
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

EFreeModule *EPolynomialRing::makeSchreyerFreeModule(
       int rank, 
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
  if (getCover())
    {
      EFreeModule *F = getCover()->makeSchreyerFreeModule(rank,degrees,ordering,tiebreaks);
      result = new EFreeModule(this,F);
    }
  else
    {
      result = new EFreeModule(this,rank,degrees,ordering,tiebreaks);
      result->setCover(result);
    }
  EUniqueObjects.insertEFreeModule(result);
  return result;
}

EFreeModule *EPolynomialRing::makeSchreyerFreeModule(const EMatrix *m) const
{
  const EFreeModule *G = m->getSource();
  if (G->getRing() != this)
    {
      gError << "expected a matrix over the same ring";
      return 0;
    }
  const EMonoid *M = getMonoid();
  int rank = m->n_cols();
  const monomial **degrees = new const monomial *[rank];
  const monomial **ordering = new const monomial *[rank];
  int *tiebreaks = new int[rank];
  for (int i=0; i<rank; i++)
    {
      degrees[i] = G->getDegree(i);
      if (m->column(i).isZero())
        {
          ordering[i] = M->one();
          tiebreaks[i] = 0;
        }
      else
        {
          ordering[i] = m->column(i).elems->monom;
          tiebreaks[i]  = m->column(i).elems->component;
        }
    }
  return makeSchreyerFreeModule(rank,degrees,ordering,tiebreaks);
}


epoly *EPolynomialRing::divide(const epoly *a, const epoly *b) const
{
  gError << "division not yet implemented here";
  return 0;
}

epoly *EPolynomialRing::power(const epoly *f, int n) const
{
  // The exponent 'n' should be >= 0 here.
  epoly * prod = _from_int(1);
  epoly * base = clone(f);
  epoly * tmp;

  for (;;)
    {
      if ((n % 2) != 0)
	{
	  tmp = mult(base, prod);
	  remove(prod);
	  prod = tmp;
	}
      n >>= 1;
      if (n == 0)
	{
	  remove(base);
	  return prod;
	}
      else
	{
	  tmp = mult(base, base);
	  remove(base);
	  base = tmp;
	}
    }
}

ERingElement EPolynomialRing::make_ring_variable(int v, int exponent) const
{
  intarray term;
  term.append(v);
  term.append(exponent);
  const monomial *m = getMonoid()->monomial_from_variable_exponent_pairs(term);
  if (m == 0) return zero();

  return make_term(getCoefficientRing()->one(), m);
}

ERingElement EPolynomialRing::make_term(const ERingElement c, const intarray &term) const
{
  const monomial *m = getMonoid()->monomial_from_variable_exponent_pairs(term);
  //  ERingElement a = K->clone(c);
  return make_term(c, m);
}
