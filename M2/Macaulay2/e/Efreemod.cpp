#include "Efreemod.hpp"
#include "Evector.hpp"
#include "random.hpp"
#include "comb.hpp"
#include "Ematrix.hpp"

EFreeModule::EFreeModule(const ERing *RR,int rank)
  : R(RR),
    _rank(rank),
    _induced_order(false),
    _orderings(0),
    _tiebreaks(0),
    _cover(0)
{
  const EMonoid *D = getDegreeMonoid();
  _degrees = new const monomial *[rank];
  for (int i=0; i<rank; i++)
    _degrees[i] = D->one();
  bump_up(R);
}

EFreeModule::EFreeModule(const ERing *RR,int rank,const monomial **degrees)
  : R(RR),
    _rank(rank),
    _degrees(degrees),
    _induced_order(false),
    _orderings(0),
    _tiebreaks(0),
    _cover(0)
{
  bump_up(R);
}

EFreeModule::EFreeModule(const ERing *RR,
                         int rank, 
                         const monomial **degrees,  // grabbed
                         const monomial **orderings, // grabbed
                         int *tiebreaks)            // grabbed
  : R(RR),
    _rank(rank),
    _degrees(degrees),
    _induced_order(true),
    _orderings(orderings),
    _tiebreaks(tiebreaks),
    _cover(0)
{
  bump_up(R);
}

EFreeModule::EFreeModule(const ERing *RR,
                         const EFreeModule *F)
  : R(RR),
    _rank(F->_rank),
    _degrees(F->_degrees),
    _induced_order(F->_induced_order),
    _orderings(F->_orderings),
    _tiebreaks(F->_tiebreaks),
    _cover(F)
{
  bump_up(F);
}

EFreeModule::~EFreeModule()
{
  if (_cover != this)
    {
      bump_down(_cover);
    }
  else 
    {
      delete [] _degrees;
      if (_induced_order) {
        delete [] _orderings;
        delete [] _tiebreaks;
      }
      bump_down(R);
    }
}


////////////////////////////
// Free Module operations //
////////////////////////////
void EFreeModule::getDegrees(intarray &result) const
{
  for (int i=0; i<rank(); i++)
    appendMonomialToIntarray(getDegreeMonoid(),getDegree(i),result);
}

EMatrix * EFreeModule::getInducedOrder() const
{
  if (!hasInducedOrder())
    return EMatrix::zero(R->makeFreeModule(0),this);
  int i;
  int maxtie = 0;
  for (i=0; i<rank(); i++)
    if (_tiebreaks[i] > maxtie)
      maxtie = _tiebreaks[i];
  const EFreeModule *F = getRing()->makeFreeModule(maxtie+1);
  EVector * newcols = new EVector[rank()];
  const EPolynomialRing *A = getRing()->toPolynomialRing();
  assert(A != 0);  // This should never be 0 for a Schreyer order...
  for (i=0; i<rank(); i++)
    {
      EVector tmp = A->vec_term(F,getCoefficientRing()->one(),_orderings[i],_tiebreaks[i]);
      newcols[i] = tmp;
    }
    return EMatrix::make(F,this,newcols);
}

bool EFreeModule::isEqual(const EFreeModule *F) const
{
  return this == F;
}

bool EFreeModule::contentIsEqual(const EFreeModule *F) const
{
  int i;
  // Equality means complete equality: same EVERYTHING
  if (this == F) return true;
  if (R != F->getRing()) return false;
  if (rank() != F->rank()) return false;
  if (hasInducedOrder() != F->hasInducedOrder()) return false;
  for (i=0; i<rank(); i++)
    if (_degrees[i] != F->_degrees[i]) return false;
  if (hasInducedOrder())
    {
      for (i=0; i<rank(); i++)
        {
          if (_orderings[i] != F->_orderings[i]) return false;
          if (_tiebreaks[i] != F->_tiebreaks[i]) return false;
        }
     }
   return true;
}

EFreeModule *EFreeModule::makeFreeModuleFromDegrees(const ERing *R, int ncols, EVector *cols)
{
  const monomial **degs = new const monomial *[ncols];
  for (int i=0; i<ncols; i++)
    degs[i] = cols[i].degree();
  return R->makeFreeModule(ncols,degs);
}


EFreeModule * EFreeModule::subSpace(int n) const
{
  int i;
  if (n < 0 || n > rank())
    {
      gError << "subfreemodule: index out of bounds";
      return 0;
    }
  const monomial ** result_degs = new const monomial *[n];
  for (i=0; i<n; i++)
    result_degs[i] = _degrees[i];
  if (!hasInducedOrder())
    return R->makeFreeModule(n,result_degs);

  const EPolynomialRing *A = getRing()->toPolynomialRing();
  const monomial ** result_orderings = new const monomial *[n];
  int *result_tiebreaks = new int[n];
  for (i=0; i<n; i++)
    {
      result_orderings[i] = _orderings[i];
      result_tiebreaks[i] = _tiebreaks[i];
    }
  return A->makeSchreyerFreeModule(n,result_degs,result_orderings,result_tiebreaks);
}

EFreeModule * EFreeModule::subSpace(const intarray &a) const
{
  int i;
  int n = a.length();
  const monomial ** result_degs = new const monomial *[n];
  for (i=0; i<n; i++)
    if (a[i] >= 0 && a[i] < rank())
      result_degs[i] = _degrees[a[i]];
    else
      {
        gError << "subfreemodule: index out of bounds";
        delete [] result_degs;
        return 0;
      }
  if (!hasInducedOrder())
    return R->makeFreeModule(n,result_degs);

  const EPolynomialRing *A = getRing()->toPolynomialRing();
  const monomial ** result_orderings = new const monomial *[n];
  int *result_tiebreaks = new int[n];
  for (i=0; i<n; i++)
    {
      result_orderings[i] = _orderings[a[i]];
      result_tiebreaks[i] = _tiebreaks[a[i]];
    }
  return A->makeSchreyerFreeModule(n,result_degs,result_orderings,result_tiebreaks);
}

EFreeModule * EFreeModule::dual() const
{
  // The dual of a free module with an induced order will not have an induced order.
  const monomial ** result_degs = new const monomial *[rank()];
  const EMonoid *D = getDegreeMonoid();
  for (int i=0; i<rank(); i++)
    result_degs[i] = D->divide(D->one(), _degrees[i]);
  return R->makeFreeModule(rank(), result_degs);
}

EFreeModule * EFreeModule::directSum(const EFreeModule *G) const
{
  // If either this or G has an induced order, the result will.
  int i;
  if (R != G->getRing())
    {
      gError << "expected same rings";
      return 0;
    }
  int newrank = rank() + G->rank();
  const monomial ** result_degs = new const monomial *[newrank];
  for (i=0; i<rank(); i++)
    result_degs[i] = _degrees[i];
  for (i=0; i<G->rank(); i++)
    result_degs[rank() + i] = G->_degrees[i];
  if (!hasInducedOrder() && !G->hasInducedOrder())
    return R->makeFreeModule(newrank,result_degs);
  
  const EPolynomialRing *A = getRing()->toPolynomialRing();
  const EMonoid *M = A->getMonoid();
  const monomial ** result_orderings = new const monomial *[newrank];
  int *result_tiebreaks = new int[newrank];
  if (hasInducedOrder())
    for (i=0; i<rank(); i++)
    {
      result_orderings[i] = _orderings[i];
      result_tiebreaks[i] = _tiebreaks[i];
    }
  else
    for (i=0; i<rank(); i++)
    {
      result_orderings[i] = M->one();
      result_tiebreaks[i] = i;
    }
  if (G->hasInducedOrder())
    for (i=0; i<G->rank(); i++)
    {
      result_orderings[rank()+i] = _orderings[i];
      result_tiebreaks[rank()+i] = rank() + _tiebreaks[i];
    }
  else
    for (i=0; i<rank(); i++)
    {
      result_orderings[rank()+i] = M->one();
      result_tiebreaks[rank()+i] = rank() + i;
    }
    
  return A->makeSchreyerFreeModule(newrank,result_degs,result_orderings,result_tiebreaks);
}

EFreeModule * EFreeModule::shift(const monomial *d) const
{
  int i;
  int newrank = rank();
  const monomial ** result_degs = new const monomial *[newrank];
  const EMonoid *D = getDegreeMonoid();
  for (i=0; i<newrank; i++)
      result_degs[i] = D->mult(_degrees[i], d);
  if (!hasInducedOrder())
    return R->makeFreeModule(newrank,result_degs);

  // If an induced order, simply copy the previous values  
  const EPolynomialRing *A = getRing()->toPolynomialRing();
  const monomial ** result_orderings = new const monomial *[newrank];
  int *result_tiebreaks = new int[newrank];
  for (i=0; i<newrank; i++)
    {
      result_orderings[i] = _orderings[i];
      result_tiebreaks[i] = _tiebreaks[i];
    }
  return A->makeSchreyerFreeModule(newrank,result_degs,result_orderings,result_tiebreaks);
}

EFreeModule * EFreeModule::tensor(const EFreeModule *G) const
{
  int i,j;
  if (R != G->getRing())
    {
      gError << "expected same rings";
      return 0;
    }
  int newrank = rank() * G->rank();
  const monomial ** result_degs = new const monomial *[newrank];
  const EMonoid *D = getDegreeMonoid();
  int next = 0;
  for (i=0; i<rank(); i++)
    for (j=0; j<G->rank(); j++)
      result_degs[next++] = D->mult(_degrees[i], G->_degrees[j]);
  if (!hasInducedOrder() || !G->hasInducedOrder())
    return R->makeFreeModule(newrank,result_degs);
  
  // Result will have an induced order iff both this and G do.
  const EPolynomialRing *A = getRing()->toPolynomialRing();
  const EMonoid *M = A->getMonoid();
  const monomial ** result_orderings = new const monomial *[newrank];
  int *result_tiebreaks = new int[newrank];
  next = 0;
  for (i=0; i<rank(); i++)
    for (j=0; j<G->rank(); j++)
      {
        result_orderings[next] = M->mult(_orderings[i], G->_orderings[j]);
	result_tiebreaks[next] = next;
	next++;
      }
  return A->makeSchreyerFreeModule(newrank,result_degs,result_orderings,result_tiebreaks);
}

EFreeModule * EFreeModule::exterior(int p) const
{
  // By laziness, or for whatever reason, the result of this operation will NOT
  // have an induced order.
  
  if (p < 0 || p > rank()) return R->makeFreeModule(0);
  if (p == 0) return R->makeFreeModule(1);
  
  int n = comb::binom(rank(), p);
  const monomial ** result_degs = new const monomial *[n];
  const EMonoid *D = getDegreeMonoid();
  int *a = new int[p];

  for (int c=0; c<n; c++)
    {
      comb::decode(c,a,p);
      const monomial *deg = D->one();

      for (int r=0; r<p; r++)
	deg = D->mult(deg, _degrees[a[r]]);
      result_degs[c] = deg;
    }
  delete [] a;
  return R->makeFreeModule(n,result_degs);
}

EFreeModule * EFreeModule::symm(int p) const
{
  // By laziness, or for whatever reason, the result of this operation will NOT
  // have an induced order.
  return NULL;
}

///////////////////////
// Vector operations //
///////////////////////
EVector EFreeModule::zero() const
{
  return EVector(this,0,0);
}
EVector EFreeModule::basisElement(int x) const
{
  if (x < 0 || x >= rank())
    {
      gError << "index out of range";
      return zero();
    }
  ERingElement b = R->one();
  EVector result = R->vec_make(this,b,x);
  R->remove(b);
  return result;
}
EVector EFreeModule::random() const
{
  EVectorHeap result(this);
  for (int i=0; i<rank(); i++)
    {
      ERingElement a = R->from_int(Random::random0());
      EVector g = R->vec_make(this, a, i);
      R->remove(a);
      result.add(g);
    }
  return result.value();
}
EVector EFreeModule::makeVector(const ERingElement *elems) const 
{
  EVectorHeap result(this);
  for (int i=0; i<rank(); i++)
    {
      EVector tmp = R->vec_make(this, elems[i], i);
      result.add(tmp);
    }
  return result.value();
}

EVector EFreeModule::makeSparseVector(const ERingElement *elems, const intarray &rows) const 
{
  EVectorHeap result(this);
  for (int i=0; i<rows.length(); i++)
    {
      EVector tmp = R->vec_make(this, elems[i], rows[i]);
      result.add(tmp);
    }
  return result.value();
}
