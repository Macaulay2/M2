#include "Efreemod.hpp"
#include "Evector.hpp"
#include "random.hpp"
#include "comb.hpp"
#include "Ematrix.hpp"

EFreeModule::EFreeModule(const EPolynomialRing *RR,int rank)
  : R(RR),
    _rank(rank),
    _induced_order(false),
    _orderings(0),
    _tiebreaks(0),
    _cover(0)
{
  const EMonoid *D = getDegreeMonoid();
  _degrees = new monomial *[rank];
  for (int i=0; i<rank; i++)
    _degrees[i] = D->one();
  bump_up(R);
}

EFreeModule::EFreeModule(const EPolynomialRing *RR,int rank,const monomial **degrees)
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

EFreeModule::EFreeModule(const EPolynomialRing *RR,
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

EFreeModule::EFreeModule(const EPolynomialRing *RR,
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

EVector *EFreeModule::buildVector(poly *f, int len) const
{
  EVector *G = new EVector;
  G->F = this;
  G->len = len;
  G->elems = f;
  return G;
}

EVector *EFreeModule::makeTerm(const field a, const monomial *m, int x) const
{
  return getRing()->makeTerm(this,a,m,x);
}

EVector *EFreeModule::basisElement(int x) const
{
  return getRing()->makeTerm(this,getCoefficientRing()->one(),getMonoid()->one(),x);
}

EVector *EFreeModule::zero() const
{
  return EVector::zero(this);
}

void EFreeModule::sort(poly *&f) const
{
  if (f == NULL || f->next == NULL) return;
  poly *f1 = 0;
  poly *f2 = 0;
  while (f != 0)
    {
      poly *t = f;
      f = f->next;
      t->next = f1;
      f1 = t;

      if (f == 0) break;
      t = f;
      f = f->next;
      t->next = f2;
      f2 = t;
    }
  
  sort(f1);
  sort(f2);
  R->add_to(f1, f2);
  f = f1;
}

EVector *EFreeModule::makeVector(const EVector **vecs) const 
{
  EVector *result = zero();
  for (int i=0; i<rank(); i++)
    {
      EVector *tmp = translate(vecs[i],i);
      result->addTo(tmp);
    }
  return result;
}

EVector *EFreeModule::makeSparseVector(const EVector **vecs, const intarray &rows) const 
{
  EVector *result = zero();
  for (int i=0; i<rows.length(); i++)
    {
      EVector *tmp = translate(vecs[i],rows[i]);
      result->addTo(tmp);
    }
  return result;
}

EVector *EFreeModule::random() const
{
  EVector *result = zero();
  for (int i=0; i<rank(); i++)
    {
      poly *p = R->newTerm();
      p->coeff = getCoefficientRing()->from_int(Random::random0());
      p->monom = getMonoid()->one();
      p->component = i;
      p->next = 0;
      result->prepend_term(p);
    }
  result->sort();
  return result;
}
EVector *EFreeModule::subvector(const EVector *v, const intarray &r) const
{
  int *trans = new int[rank()];
  int i;
  for (i=0; i<rank(); i++)
    trans[i] = -1;
  for (i=0; i<r.length(); i++)
    if (r[i] >= 0 && r[i] < v->getFreeModule()->rank())
      trans[r[i]] = i;
  
  poly head;
  poly *result = &head;
  int len = 0;
  for (poly *a = v->elems; a != 0; a = a->next)
    if (trans[a->component] != -1)
      {
        result->next = R->copy_term(a);
        len++;
        result = result->next;
        result->component = trans[a->component];
      }
  result->next = 0;
  EVector *G = buildVector(head.next,len);
  G->sort();
  return G;
}

EVector *EFreeModule::translate(const EVector *v) const
{
  EVector *result = v->clone();
  if (this == result->F) return result;
  result->F = this;
  if (hasInducedOrder() || v->getFreeModule()->hasInducedOrder())
    result->sort();
  return result;
}
EVector *EFreeModule::translate(const EVector *v, int newcomp) const
{
  EVector *result = v->clone();
  for (poly *p=result->elems; p!=0; p=p->next)
    p->component = newcomp;
  result->F = this;
  result->sort();
  return result;
}

EVector *EFreeModule::getComponent(const EVector *v, int comp, int newcomp) const
{
  EVector *result = v->getComponent(comp);
  for (poly *p=result->elems; p!=0; p=p->next)
    p->component = newcomp;
  result->F = this;
  return result;  
}

EVector *EFreeModule::componentShift(int r, const EVector *v) const
{
  poly head;
  poly *result = &head;
  for (poly *a = v->elems; a != 0; a = a->next)
    {
      result->next = R->copy_term(a);
      result = result->next;
      result->component += r;
    }
  result->next = 0;
  EVector *G = buildVector(head.next,v->len);
  if (hasInducedOrder() || v->getFreeModule()->hasInducedOrder())
    G->sort();
  return G;
}

EVector *EFreeModule::tensorShift(int n, int m, const EVector *v) const
{
  poly head;
  poly *result = &head;
  for (poly *a = v->elems; a != 0; a = a->next)
    {
      result->next = R->copy_term(a);
      result = result->next;
      result->component = n * result->component + m;
    }
  result->next = 0;
  EVector *G = buildVector(head.next,v->len);
  if (hasInducedOrder() || v->getFreeModule()->hasInducedOrder())
    G->sort();
  return G;
}
#if 0
EVector *EFreeModule::tensor(const EVector *v, const EVector *w) const
{
  poly head;
  poly *result = &head;
  int n = w->getFreeModule()->rank();
  int newlen = 0;
  const ECoefficientRing *K = R->getCoefficientRing();
  const EMonoid *M = R->getMonoid();
  for (poly *a = v->elems; a != 0; a = a->next)
    for (poly *b = w->elems; b != 0; b = b->next)
    {
      result->next = R->newTerm();
      result = result->next;
      result->coeff = K->mult(a->coeff, b->coeff);
      result->monom = M->mult(a->monom, b->monom);
      result->component = n * a->component + b->component;
      newlen++;
    }
  result->next = 0;
  EVector *G = buildVector(head.next,newlen);
  G->sort();
  return G;
}
#endif
EVector *EFreeModule::tensor(const EVector *v, const EVector *w) const
{
  int n = w->getFreeModule()->rank();
  EVectorHeap H(this);
  const ECoefficientRing *K = R->getCoefficientRing();
  const EMonoid *M = R->getMonoid();
  for (poly *a = v->elems; a != 0; a = a->next)
    {
      EVector *v1 = makeTerm(a->coeff, a->monom, 0);
      EVector *w1 = componentShift(a->component * n, w);
      EVector *w2 = v1->multiply(w1);
      delete v1;
      delete w1;
      H.add(w2);
    }
  return H.value();
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
  EVector ** newcols = new EVector *[rank()];
  for (i=0; i<rank(); i++)
    newcols[i] = F->makeTerm(getCoefficientRing()->one(),_orderings[i],_tiebreaks[i]);
  return EMatrix::make(F,this,newcols,getDegreeMonoid()->one());
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

EFreeModule * EFreeModule::subSpace(int n) const
{
  int i;
  if (n < 0 || n > rank())
    {
      gError << "subfreemodule: index out of bounds";
      return 0;
    }
  const monomial ** result_degs = new monomial *[n];
  for (i=0; i<n; i++)
    result_degs[i] = _degrees[i];
  if (!hasInducedOrder())
    return R->makeFreeModule(n,result_degs);

  const monomial ** result_orderings = new monomial *[n];
  int *result_tiebreaks = new int[n];
  for (i=0; i<n; i++)
    {
      result_orderings[i] = _orderings[i];
      result_tiebreaks[i] = _tiebreaks[i];
    }
  return R->makeFreeModule(n,result_degs,result_orderings,result_tiebreaks);
}

EFreeModule * EFreeModule::subSpace(const intarray &a) const
{
  int i;
  int n = a.length();
  const monomial ** result_degs = new monomial *[n];
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

  const monomial ** result_orderings = new monomial *[n];
  int *result_tiebreaks = new int[n];
  for (i=0; i<n; i++)
    {
      result_orderings[i] = _orderings[a[i]];
      result_tiebreaks[i] = _tiebreaks[a[i]];
    }
  return R->makeFreeModule(n,result_degs,result_orderings,result_tiebreaks);
}

EFreeModule * EFreeModule::dual() const
{
  // The dual of a free module with an induced order will not have an induced order.
  const monomial ** result_degs = new monomial *[rank()];
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
  const monomial ** result_degs = new monomial *[newrank];
  for (i=0; i<rank(); i++)
    result_degs[i] = _degrees[i];
  for (i=0; i<G->rank(); i++)
    result_degs[rank() + i] = G->_degrees[i];
  if (!hasInducedOrder() && !G->hasInducedOrder())
    return R->makeFreeModule(newrank,result_degs);
  
  const monomial ** result_orderings = new monomial *[newrank];
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
      result_orderings[i] = getMonoid()->one();
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
      result_orderings[rank()+i] = getMonoid()->one();
      result_tiebreaks[rank()+i] = rank() + i;
    }
    
  return R->makeFreeModule(newrank,result_degs,result_orderings,result_tiebreaks);
}

EFreeModule * EFreeModule::shift(const monomial *d) const
{
  int i;
  int newrank = rank();
  const monomial ** result_degs = new monomial *[newrank];
  const EMonoid *D = getDegreeMonoid();
  for (i=0; i<newrank; i++)
      result_degs[i] = D->mult(_degrees[i], d);
  if (!hasInducedOrder())
    return R->makeFreeModule(newrank,result_degs);

  // If an induced order, simply copy the previous values  
  const monomial ** result_orderings = new monomial *[newrank];
  int *result_tiebreaks = new int[newrank];
  for (i=0; i<newrank; i++)
    {
      result_orderings[i] = _orderings[i];
      result_tiebreaks[i] = _tiebreaks[i];
    }
  return R->makeFreeModule(newrank,result_degs,result_orderings,result_tiebreaks);
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
  const monomial ** result_degs = new monomial *[newrank];
  const EMonoid *D = getDegreeMonoid();
  int next = 0;
  for (i=0; i<rank(); i++)
    for (j=0; j<G->rank(); j++)
      result_degs[next++] = D->mult(_degrees[i], G->_degrees[j]);
  if (!hasInducedOrder() || !G->hasInducedOrder())
    return R->makeFreeModule(newrank,result_degs);
  
  // Result will have an induced order iff both this and G do.
  const monomial ** result_orderings = new monomial *[newrank];
  int *result_tiebreaks = new int[newrank];
  const EMonoid *M = getMonoid();
  next = 0;
  for (i=0; i<rank(); i++)
    for (j=0; j<G->rank(); j++)
      {
        result_orderings[next] = M->mult(_orderings[i], G->_orderings[j]);
	result_tiebreaks[next] = next;
	next++;
      }
  return R->makeFreeModule(newrank,result_degs,result_orderings,result_tiebreaks);
}

EFreeModule * EFreeModule::exterior(int p) const
{
  // By laziness, or for whatever reason, the result of this operation will NOT
  // have an induced order.
  
  if (p < 0 || p > rank()) return R->makeFreeModule(0);
  if (p == 0) return R->makeFreeModule(1);
  
  int n = comb::binom(rank(), p);
  const monomial ** result_degs = new monomial *[n];
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
}

