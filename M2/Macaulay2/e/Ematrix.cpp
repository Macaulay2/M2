// Copyright 1998 by Michael Stillman

#include "Ematrix.hpp"
#include "Evector.hpp"
#include "Eringmap.hpp"

#include "text_io.hpp"
#include "comb.hpp"

EMatrix::EMatrix(const EFreeModule *F,
	         const EFreeModule *G,
	         EVector *elements,
		 int type,
	         const monomial *d)  // GRABS the array and elements in 'elements'.
: target(F),
  source(G),
  ncolumns(G->rank()),
  columns(elements),
  mapdegree(d),
  type(type),
  gb(0)
{
  bump_up(target);
  bump_up(source);
}

EMatrix::~EMatrix()
{ 
  // We allow the possibility that a matrix routine grabs the columns
  // array, sets 'columns' to 0, and deletes the matrix, keeping the
  // vectors.
  delete [] columns;
  bump_down(target); 
  bump_down(source); 
}

EMatrix *EMatrix::make(const EFreeModule *F,
	      const EFreeModule *G,
	      EVector *elements,
	      int type,
	      const monomial *d)  // GRABS the array and elements in 'elements'.
{
  if (type != left && type != right && type != both)
    type = left;
  if (d == 0) d = F->getDegreeMonoid()->one();
  EMatrix *result = new EMatrix(F,G,elements,type,d);
  return result;
}

EVector *EMatrix::allocate_columns(int c)
{
  return new EVector[c];
}
EVector *EMatrix::initialize_columns(const EFreeModule *F, int c)
{
  EVector *newcols = allocate_columns(c);
  for (int i=0; i<c; i++)
    newcols[i] = F->zero();
  return newcols;
}

void EMatrix::text_out(buffer &o) const
{
  int nrows = n_rows();
  int ncols = n_cols();

  buffer *p = new buffer[nrows];
  int r;
  for (int c=0; c<ncols; c++)
    {
      int maxcount = 0;
      for (r=0; r<nrows; r++)
	{
	  ERingElement f = entry(r,c);
	  getRing()->elem_text_out(p[r], f);
	  getRing()->remove(f);
	  if (p[r].size() > maxcount)
	    maxcount = p[r].size();
	}
      for (r=0; r<nrows; r++)
	for (int k=maxcount+1-p[r].size(); k > 0; k--)
	  p[r] << ' ';
    }
  for (r=0; r<nrows; r++)
    {
      p[r] << '\0';
      char *s = p[r].str();
      o << s << newline;
    }
  delete [] p;
}


bool EMatrix::isEqual(const EMatrix *m) const
{
  if (this == m) return true;
  if (!getTarget()->isEqual(m->getTarget()))
    return false;
  if (!getSource()->isEqual(m->getSource()))
    return false;
  if (getMatrixType() != m->getMatrixType())
    return false;
  for (int i=0; i<n_cols(); i++)
    if (! column(i).isEqual(m->column(i)))
      return false;
  return true;
}

bool EMatrix::entriesEqual(const EMatrix *m) const
{
  if (this == m) return true;
  if (n_rows() != m->n_rows())
    return false;
  if (n_cols() != m->n_cols())
    return false;
#if 0
  // MES FIX THIS!!
  for (int i=0; i<n_cols(); i++)
    if (! column(i)->entriesEqual(m->column(i)))
      return false;
#endif
  return true;
}

bool EMatrix::isZero() const
{
  for (int i=0; i<n_cols(); i++)
    if (! column(i).isZero()) return false;
  return true;
}

bool EMatrix::isGraded() const
{
  // Check: each column is either zero, or is graded
  // of degree (coldeg[i] + mapdegree)
  const EMonoid *D = getDegreeMonoid();
  for (int i=0; i<n_cols(); i++)
    {
      if (column(i).isZero()) continue;
      monomial *d;
      if (!column(i).isGraded(d))
        return false;
      if (!D->is_equal(d, D->mult(getSource()->getDegree(i), mapdegree)))
        return false;
    }
  return true;
}

EMatrix *EMatrix::zero(const EFreeModule *F, const EFreeModule *G)
{
  if (F->getRing() != G->getRing())
    {
      gError << "free modules have different base rings";
      return 0;
    }
  int ncols = G->rank();
  EVector *newcols = initialize_columns(F,ncols);
  return make(F,G,newcols);
}

EMatrix *EMatrix::identity(const EFreeModule *F)
{
  int ncols = F->rank();
  EVector *newcols = allocate_columns(ncols);
  for (int i=0; i<ncols; i++)
    newcols[i] = F->basisElement(i);
  return make(F,F,newcols);
}

ERingElement EMatrix::entry(int r, int c) const
{
  if (c < 0 || c >= n_cols()) return getRing()->zero();
  return column(c).getComponent(r);
}

EMatrix *EMatrix::add(const EMatrix *m) const
{
  int newtype = (type & m->type);
  if (newtype == 0)
    gError << "cannot add left-matrix and right-matrix";
  else if (getRing() != m->getRing())
    gError << "matrices have different base rings";
  else if (n_rows() != m->n_rows()
	   || n_cols() != m->n_cols())
    gError << "matrices have different shapes";
  else
    {
      const ERing *R = getRing();
      const EFreeModule *F = getTarget();
      const EFreeModule *G = getSource();
      
      if (!F->isEqual(m->getTarget()))
        F = R->makeFreeModule(n_rows());
      if (!G->isEqual(m->getSource()))
        G = R->makeFreeModule(n_cols());
        
      const monomial *deg = getMapDegree();
      if (!getDegreeMonoid()->is_equal(deg, m->getMapDegree()))
          deg = getDegreeMonoid()->one();
          
      int c = G->rank();
      EVector *newcols = allocate_columns(c);

      for (int i=0; i<c; i++)
	{
	  EVector v = column(i).translate(F);
	  EVector w = m->column(i).translate(F);
	  v.addTo(w);
	  newcols[i] = v;
	}
      return make(F,G,newcols,newtype,deg);
    }
  return 0;
}

EMatrix *EMatrix::subtract(const EMatrix *m) const
{
  int newtype = (type & m->type);
  if (newtype == 0)
    gError << "cannot add left-matrix and right-matrix";
  else if (getRing() != m->getRing())
    gError << "matrices have different base rings";
  else if (n_rows() != m->n_rows()
	   || n_cols() != m->n_cols())
    gError << "matrices have different shapes";
  else
    {
      const ERing *R = getRing();
      const EFreeModule *F = getTarget();
      const EFreeModule *G = getSource();
      
      if (!F->isEqual(m->getTarget()))
        F = R->makeFreeModule(n_rows());
      if (!G->isEqual(m->getSource()))
        G = R->makeFreeModule(n_cols());
        
      const monomial *deg = getMapDegree();
      if (!getDegreeMonoid()->is_equal(deg, m->getMapDegree()))
          deg = getDegreeMonoid()->one();
          
      int c = G->rank();
      EVector *newcols = allocate_columns(c);

      for (int i=0; i<c; i++)
	{
	  EVector v = column(i).translate(F);
	  EVector w = m->column(i).translate(F);
	  v.subtractTo(w);
	  newcols[i] = v;
	}
      return make(F,G,newcols,newtype,deg);
    }
  return 0;
}

EMatrix *EMatrix::negate() const
{
  EVector *newcols = allocate_columns(n_cols());
  for (int i=0; i<n_cols(); i++)
    newcols[i] = column(i).negate();
  return make(getTarget(), getSource(), newcols, type, getMapDegree());
}

EMatrix *EMatrix::submatrix(const intarray &c) const
{
  const EFreeModule *G = getSource()->subSpace(c);
  if (G == 0) return 0;
  EVector *newcols = allocate_columns(G->rank());
  for (int i=0; i<c.length(); i++)
    if (c[i] < 0 || c[i] >= n_cols())
      newcols[i] = getTarget()->zero();
    else
      newcols[i] = column(c[i]).clone();
  return make(getTarget(), G, newcols, type, getMapDegree());
}

EMatrix *EMatrix::submatrix(const intarray &r, const intarray &c) const
{
  const EFreeModule *F = getTarget()->subSpace(r);
  if (F == 0) return 0;
  const EFreeModule *G = getSource()->subSpace(c);
  if (G == 0) return 0;

  EVector *newcols = allocate_columns(G->rank());
  for (int i=0; i<c.length(); i++)
    if (c[i] < 0 || c[i] >= n_cols())
      newcols[i] = getTarget()->zero();
    else
      newcols[i] = column(c[i]).subvector(F,r);
  return make(F, G, newcols, type, getMapDegree());
}

EMatrix *EMatrix::transpose() const
{
  const ERing *R = getRing();
  const EFreeModule *F = getSource()->dual();
  const EFreeModule *G = getTarget()->dual();
  const monomial *newdeg = getDegreeMonoid()->divide(
    getDegreeMonoid()->one(), getMapDegree());

  EVector *newcols = initialize_columns(F,G->rank());
  for (int c=0; c<ncolumns; c++)
    for (EVector::iterator t = column(c); t.valid(); ++t)
      {
        evec *s = R->vec_copy_term(*t);
        int r = s->component;
        s->component = c;
        newcols[r].prepend_term(s);
      }
  for (int r=0; r<G->rank(); r++)
    newcols[r].sort();

  int newtype = both - type;
  if (newtype == 0) newtype = both;
  return make(F, G, newcols, newtype, newdeg);
}

EMatrix *EMatrix::leftMultiply(ERingElement a) const
{
  // Note: we cannot check that the ring of 'a' is the same as the ring of 'this'.
  // but this had better be the case!!
  if (type == right)
    {
      gError << "cannot multiply right-matrix by element on the left";
      return 0;
    }
  EVector *newcols = allocate_columns(n_cols());
  for (int i=0; i<n_cols(); i++)
    newcols[i] = column(i).leftMultiply(a);
  const monomial *deg = getDegreeMonoid()->mult(getMapDegree(), getRing()->degree(a));
  return make(getTarget(),getSource(),newcols,type,deg);
}
EMatrix *EMatrix::rightMultiply(ERingElement a) const
{
  // Note: we cannot check that the ring of 'a' is the same as the ring of 'this'.
  // but this had better be the case!!
  if (type == left)
    {
      gError << "cannot multiply left-matrix by element on the right";
      return 0;
    }
  EVector *newcols = allocate_columns(n_cols());
  for (int i=0; i<n_cols(); i++)
    newcols[i] = column(i).rightMultiply(a);
  const monomial *deg = getDegreeMonoid()->mult(getMapDegree(), getRing()->degree(a));
  return make(getTarget(),getSource(),newcols,type,deg);
}

EVector EMatrix::vectorImage(const EVector &v) const
{
  if (v.getFreeModule()->rank() > n_cols())
    {
      gError << "matrix vector sizes don't match";
      return getTarget()->zero();
    }
  EVectorHeap g(getTarget());
  
  // We modify w as we go
  for (EVector::iterator t = v; t.valid(); ++t)
    {
      ERingElement a = getRing()->vec_term_to_ring(*t);
      // Multiply t by the vector column(t->component):
      EVector w;
      if (type != right)
	w = column(t->component).leftMultiply(a);
      else
	w = column(t->component).rightMultiply(a);
      g.add(w);
      getRing()->remove(a);
    }
  return g.value();  
}

EMatrix *EMatrix::multiply(const EMatrix * m) const
{
  int newtype = (type & m->type);
  if (newtype == 0)
    {
      gError << "cannot compose left-matrix with a right-matrix";
      return 0;
    }
  if (getRing() != m->getRing())
    {
      gError << "matrix multiplication: different base rings";
      return 0;
    }
  if (n_cols() != m->n_rows())
    {
      gError << "matrix multiplication: matrix sizes don't match";
      return 0;
    }

  // Degree of the map is the sum of the two degrees
  const monomial *newdeg = getDegreeMonoid()->mult(getMapDegree(),
                          m->getMapDegree());

  EVector *newcols = allocate_columns(m->n_cols());
  for (int i=0; i<m->n_cols(); i++)
    newcols[i] = vectorImage(m->column(i));
  return make(getTarget(), m->getSource(), newcols, newtype, newdeg);
}

EMatrix *EMatrix::flip(const EFreeModule *F, const EFreeModule *G)
{
  const EFreeModule *H = F->tensor(G);
  EVector *newcols = allocate_columns(H->rank());
  int next = 0;
  for (int g=0; g<G->rank(); g++)
    for (int f=0; f<F->rank(); f++)
      newcols[next++] = H->basisElement(f * G->rank() + g);
  return make(H,H,newcols,both);
}

EMatrix *EMatrix::reshape(const EFreeModule *F, const EFreeModule *G) const
  // Reshape 'this' : F <--- G, where 
  // (rank F)(rank G) = (nrows this)(ncols this)
{
  if (n_rows() * n_cols() != F->rank() * G->rank()) {
    gError << "reshape: ranks of freemodules incorrect";
    return 0;
  }
  if (F->getRing() != getRing() || G->getRing() != getRing())
    {
      gError << "reshape: expected freemodules over same ring";
      return 0;
    }
  EVector *newcols = initialize_columns(F,G->rank());
  int c;
  for (c=0; c < n_cols(); c++)
    for (EVector::iterator t = column(c); t.valid(); ++t)
      {
        evec *q = getRing()->vec_copy_term(*t);
        int r = q->component;
        
        // Determine new component
        int loc = c * n_rows() + r;
        int result_col = loc / F->rank();
        int result_row = loc % F->rank();
        
        q->component = result_row;
        newcols[result_col].prepend_term(q);
      }
  for (c=0; c<G->rank(); c++)
    newcols[c].sort();
  return make(F,G,newcols,type);
}

EMatrix *EMatrix::concatenate(const EMatrix *m) const
{
  if (getRing() != m->getRing())
    {
      gError << "matrix concatenate: different base rings";
      return 0;
    }
  if (n_rows() != m->n_rows())
    {
      gError << "matrix concatenate: matrices have different numbers of rows";
      return 0;
    }

  const EFreeModule *G = getSource()->directSum(m->getSource());
  EVector *newcols = allocate_columns(G->rank());
  int nc = n_cols();
  int i;
  for (i=0; i<nc; i++)
    newcols[i] = column(i).clone();
  for (i=0; i<m->n_cols(); i++)
    newcols[nc+i] = m->column(i).translate(getTarget());

  return make(getTarget(), G, newcols, type, getMapDegree());
}

EMatrix *EMatrix::directSum(const EMatrix *m) const
{
  int newtype = (type & m->type);
  if (newtype == 0)
    {
      gError << "expected either both right-matrices or both left-matrices";
      return 0;
    }
  
  if (getRing() != m->getRing())
    {
      gError << "matrix direct sum: different base rings";
      return 0;
    }

  const monomial *mapdeg;
  if (getDegreeMonoid()->is_equal(getMapDegree(), m->getMapDegree()))
    mapdeg = getMapDegree();
  else
    mapdeg = getDegreeMonoid()->one();

  const EFreeModule *F = getTarget()->directSum(m->getTarget());
  const EFreeModule *G = getSource()->directSum(m->getSource());

  EVector *newcols = allocate_columns(G->rank());
  int i;
  int nr = n_rows();
  int nc = n_cols();
  for (i=0; i<nc; i++) 
    newcols[i] = column(i).translate(F);
  for (i=0; i<m->n_cols(); i++)
    newcols[nc+i] = m->column(i).componentShift(F,nr);

  return make(F,G,newcols,newtype,mapdeg);
}

EMatrix *EMatrix::moduleTensor(const EMatrix *m) const
{
  if (getRing() != m->getRing())
    {
      gError << "module tensor product: different base rings";
      return 0;
    }
  if (!((type & right) || (m->type & left)))
    {
      gError << "module tensor: expected left and right module";
      return 0;
    }
  int newtype = (type & left) | (m->type & right);
  if (newtype == 0)
    {
      gError << "expected bi-module";
      return 0;
    }
  EFreeModule *F = getTarget()->tensor(m->getTarget());
  EFreeModule *G1 = getTarget()->tensor(m->getSource());
  EFreeModule *G2 = m->getTarget()->tensor(getSource());
  EFreeModule *G = G1->directSum(G2);

  const monomial *mapdeg;
  if (getDegreeMonoid()->is_equal(getMapDegree(), m->getMapDegree()))
    mapdeg = getMapDegree();
  else
    mapdeg = getDegreeMonoid()->one();
  
  EVector *newcols = allocate_columns(G->rank());
  int i, j, next=0;

  for (i=0; i<n_rows(); i++)
    for (j=0; j<m->n_cols(); j++)
      newcols[next++] = m->column(j).componentShift(F, i * m->n_rows());

  for (i=0; i<m->n_rows(); i++)
    for (j=0; j<n_cols(); j++)
      newcols[next++] = column(j).tensorShift(F, m->n_rows(), i);

  return make(F,G,newcols,newtype,mapdeg);
}

EMatrix *EMatrix::random(const ERing *R, int r, int c)
{
  EFreeModule *F = R->makeFreeModule(r);
  EFreeModule *G = R->makeFreeModule(c);
  if (F == 0 || G == 0)
    {
      return 0;
    }
  EVector *newcols = allocate_columns(c);

  for (int i=0; i<c; i++)
    newcols[i] = F->random();
  return make(F,G,newcols);
}

EMatrix *EMatrix::tensor(const EMatrix *m) const
{
  // MES: What should the resulting type be?
  if (getRing() != m->getRing())
    {
      gError << "matrix tensor product: different base rings";
      return 0;
    }

  const EFreeModule *F = getTarget()->tensor(m->getTarget());
  const EFreeModule *G = getSource()->tensor(m->getSource());
  const monomial *mapdeg = getDegreeMonoid()->mult(
                              getMapDegree(), 
                              m->getMapDegree());
  EVector *newcols = allocate_columns(G->rank());
  int i, j, next = 0;
  for (i=0; i<n_cols(); i++)
    for (j=0; j<m->n_cols(); j++)
      newcols[next++] = column(i).tensor(F, m->column(j));
				 
  return make(F,G,newcols,type,mapdeg);
}

EMatrix *EMatrix::leadTerm(int n, bool same_component_only) const
    // Select those monomials in each column
    // which are maximal in the order under
    // the first n weight vectors, where the
    // component slot is considered as the nvars+1 st weight
    // vector.
{
  EVector *newcols = allocate_columns(n_cols());
  for (int i=0; i<n_cols(); i++)
    newcols[i] = column(i).leadTerm(n,same_component_only);
  return make(getTarget(), getSource(), newcols, type, getMapDegree());
}
EMatrix * EMatrix::homogenize(int v, int nwts, const int *wts) const
{
  EVector *newcols = allocate_columns(n_cols());
  for (int i=0; i<n_cols(); i++)
    {
      if (!column(i).homogenize(v,nwts,wts,newcols[i]))
        {
          // Clean up and return 0:
          delete [] newcols;
          return 0;
        }
    }
  return make(getTarget(), getSource(), newcols, type, getMapDegree());
}
EMatrix *EMatrix::evaluate(const ERingMap *f, const EFreeModule *newTarget) const
{
  EVector *newcols = initialize_columns(newTarget, n_cols());
  for (int i=0; i<n_cols(); i++)
    newcols[i] = f->evaluate(newTarget, column(i));
  EFreeModule *G = EFreeModule::makeFreeModuleFromDegrees(getRing(),n_cols(),newcols);
  return make(newTarget, G, newcols, type);
}
void EMatrix::selectInSubring(int n, intarray &result) const
{
#if 0
  // MES: write EVector::inSubring
  for (int i=0; i<n_cols(); i++)
    if (column(i)->inSubring(n))
      result.append(i);
#endif
}
#if 0
EMatrix *EMatrix::divideByVariable(int v, int maxd, int &highest) const
{
  EVector **newcols = allocate_columns(n_cols());
  highest = 0;
  for (int i=0; i<n_cols(); i++)
    {
      if (column(i)->isZero())
	newcols[i] = column(i)->clone();
      else
	{
	  int high;
	  newcols[i] = column(i)->divideByVariable(v,maxd,high);
	  if (high > highest)
	    highest = high;
	}
    }
  // Now make new monomials:
  monomial **degs = new monomial *[n_cols()];
  for (int i=0; i<n_cols(); i++)
    {
      if (column(i)->isZero())
        degs[i] = getDegreeMonoid()->clone(getSource()->getDegree(i));
      else
        {
          const monomial *d = column(i)->degree();
          const monomial *e = newcols[i]->degree();
          const monomial *f = getDegreeMonoid()->divide(d,e);
          degs[i] = getDegreeMonoid()->divide(getSource()->getDegree(i),f);
        }
    }
  EFreeModule *G = getRing()->makeFreeModule(n_cols(), degs);  // Grabs 'degs'.
  return make(getTarget(), G, newcols, getMapDegree());
}
#endif
// Used in a Koszul routine below.
static int signdivide(int n, const int *a, const int *b, int *exp)
{
  int sign = 0;
  int sum = 0;
  for (int i=0; i<n; i++)
    {
      int e = a[i] - b[i];
      if (e < 0) return 0;
      exp[i] = e;
      sign += sum*e;
      sum += b[i];
    }
  sign %= 2;
  if (sign == 0) return 1;
  return -1;
}

EMatrix *EMatrix::koszul(int p) const
{
  if (n_rows() != 1)
    {
      gError << "Koszul: expected matrix with one row";
      return 0;
    }

  EFreeModule *F = getSource()->exterior(p-1);
  EFreeModule *G = getSource()->exterior(p);
  
  EVector *newcols = initialize_columns(F,G->rank());
  if (p <= 0 || p > getRing()->n_vars())
    return make(F,G,newcols,type,getMapDegree());
  int *a = new int[p];
  for (int c=0; c < G->rank(); c++)
    {
      comb::decode(c, a,p);
      int negate = ((p % 2) != 0);
      for (int r=p-1; r>=0; r--)
	{
	  negate = !negate;
	  // swap(a[p-1], a[r]);
	  int tmp = a[p-1];
	  a[p-1] = a[r];
	  a[r] = tmp;
	  int x = comb::encode(a, p-1);
	  EVector temp = column(a[p-1]).translate(F,x);

	  if (negate)
	    newcols[c].subtractTo(temp);
	  else
	    newcols[c].addTo(temp);
	}
    }
  return make(F,G,newcols,type,getMapDegree());
}

EMatrix *EMatrix::koszul(const EMatrix *r, const EMatrix *c)
{
  // MES: check whether the ring is non-commutative.
  // If so, return 0?
  int i;
  if (r->n_rows() != 1 || c->n_rows() != 1)
    {
      gError << "koszul: expected matrices with one row";
      return 0;
    }
  // We also expect that each entry of r,c is a monomial...
  for (i=0; i<r->n_cols(); i++)
    if (r->column(i).nTerms() > 1)
      {
        gError << "koszul: expected matrix to have monomial entries";
        return 0;
      }
  for (i=0; i<c->n_cols(); i++)
    if (c->column(i).nTerms() > 1)
      {
        gError << "koszul: expected matrix to have monomial entries";
        return 0;
      }
      
  const EFreeModule *F = r->getSource();
  const EFreeModule *G = c->getSource();
  const EPolynomialRing *R = F->getRing()->toPolynomialRing();
  if (R == 0)
    {
      gError << "expected polynomial ring";
      return 0;
    }
  const EMonoid *M = R->getMonoid();
  
  EVector *newcols = initialize_columns(F,G->rank());
  
  int nvars = M->n_vars();
  int nrows = F->rank();
  int ncols = G->rank();
  const int *aexp = new int[nvars];
  const int *bexp = new int[nvars];
  int *result_exp = new int[nvars];
  for (int i=0; i<ncols; i++)
    {
      aexp = M->to_exponents(c->column(i).leadMonomial());
      for (int j=0; j<nrows; j++)
	{
	  bexp = M->to_exponents(r->column(j).leadMonomial());
	  int sign = signdivide(nvars, aexp, bexp, result_exp);
	  if (sign != 0)
	    {
	      evec *p = R->vec_new_term();
	      p->next = 0;
	      p->coeff = R->getCoefficientRing()->from_int(sign);
	      p->component = j;
	      p->monom = M->monomial_from_exponents(result_exp);
	      newcols[i].prepend_term(p);
	    }
	}
      newcols[i].sort();
    }
  delete [] aexp;
  delete [] bexp;
  delete [] result_exp;
  return make(F,G,newcols,both,r->getMapDegree());
}

EMatrix *EMatrix::exteriorProduct(int p, int q, const EFreeModule *F)
{
  const EFreeModule *Fp = F->exterior(p);
  const EFreeModule *Fq = F->exterior(q);
  const EFreeModule *Fn = F->exterior(p+q);
  const EFreeModule *G = Fp->tensor(Fq);
  const ERing *R = F->getRing();

  EVector *newcols = initialize_columns(Fn,G->rank());
  
  if (p < 0 || q < 0 || p+q >F->rank())
    {
      for (int i=0; i<G->rank(); i++)
	newcols[i] = Fn->zero();
      return make(Fn,G,newcols);
    }

  if (p == 0 || q == 0)
    {
      for (int i=0; i<G->rank(); i++)
	newcols[i] = Fn->basisElement(i);
      return make(Fn,G,newcols);
    }

  int *a = new int[p];
  int *b = new int[q];
  int *c = new int[p+q];
  int col = 0;

  for (int i=0; i<Fp->rank(); i++)
    {
      EVectorHeap v(Fn);
      comb::decode(i, a, p);
      for (int j=0; j<Fq->rank(); j++)
	{
	  comb::decode(j, b, q);
	  int sgn = comb::mult_subsets(p,a,q,b,c);
	  if (sgn == 0)
	    {
	      col++;
	      continue;
	    }
	  EVector tmp = R->vec_make(Fn,R->from_int(sgn),comb::encode(c,p+q));
	  v.add(tmp);
	}
       newcols[col++] = v.value();
    }

  delete [] a;
  delete [] b;
  delete [] c;
  return make(Fn,G,newcols);
}

int * EMatrix::sort(int degorder, int monorder) const
  // Sort the columns of 'this': Place the column indices into 'result'.
  // If degorder < 0, sort in descending degree order, if >0 ascending degree
  // If ==0, or in the event that two columns have the same (simple) degree,
  // use the monomial order: monorder > 0 means ascending, <0 means descending.
{
  int *degs = 0;
  if (degorder != 0)
    {
      degs = new int[n_cols()];
      for (int i=0; i<n_cols(); i++)
	degs[i] = getSource()->getPrimaryDegree(i);
    }

  return ESortColumnAlgorithm::sort(n_cols(), columns, degorder, monorder, degs);
}

EMatrix * EMatrix::diff(const EMatrix * m, bool use_coeff) const
{
  if (getRing() != m->getRing())
    {
      gError << "matrix diff: different base rings";
      return 0;
    }
  const EFreeModule *F1 = getTarget()->dual();
  const EFreeModule *F = F1->tensor(m->getTarget());

  const EFreeModule *G1 = getSource()->dual();
  const EFreeModule *G = G1->tensor(m->getSource());

  const monomial *mapdeg = getDegreeMonoid()->divide(
                              m->getMapDegree(),
			      getMapDegree());

  EVector *newcols = initialize_columns(F, G->rank());
  int next = 0;
  for (int i=0; i<n_cols(); i++)
    for (int j=0; j<m->n_cols(); j++)
      newcols[next++] = column(i).diff(F, m->column(j), use_coeff);      

  return make(F,G,newcols,m->getMatrixType(), mapdeg);
}

EMatrix * EMatrix::coefficients(const bool *vars, EMatrix * &result_monoms) const
{
  const EFreeModule *R1 = getRing()->makeFreeModule(1);
  // result_coeffs: will have same target as this.
  array<EVector> monoms;
  array<EVector> coeffs;
  for (int j=0; j<n_cols(); j++)
    {
      EVector f = column(j).clone();
      EVector vmonom;
      while (!f.isZero())
	{
	  EVector g = f.strip_vector(vars, R1, vmonom);
	  coeffs.append(g);
	  monoms.append(vmonom);
	}
    }
  // If monoms has length 0 (will only happen if 'this' is the zero matrix)
  // Then don't sort.

  int *perm = ESortColumnAlgorithm::sort(
                monoms.length(), 
		monoms.get_raw_array(), 
		1, -1); // increasing degree, but decreasing in the order
  EVector *new_monoms = initialize_columns(R1, monoms.length());
  EVector *new_coeffs = initialize_columns(getTarget(), monoms.length());
  for (int i=0; i<monoms.length(); i++)
    {
      new_monoms[perm[i]] = monoms[i]; // essentially removes monoms[i] as well.
      new_coeffs[perm[i]] = coeffs[i]; // and coeffs[i], so that when monoms,coeffs are destroyed,
				        // they won't take our values with them!
    }
  delete [] perm;

  result_monoms = make(R1, EFreeModule::makeFreeModuleFromDegrees(getRing(),monoms.length(),new_monoms),
		       new_monoms, getMatrixType());

  return make(getTarget(), EFreeModule::makeFreeModuleFromDegrees(getRing(),
								  monoms.length(),
								  new_coeffs),
	      new_coeffs, getMatrixType());

}

#if 0

Matrix EMatrix::random(const FreeModule *F, const FreeModule *G, 
		      int *mapdeg,	// Degree of the map, not the elements
		      int ishomog)	// Whether the map is homog.  If not
					// then degrees < given degree in any
					// entry are allowed.
{
  // For each degree in the matrix, generate a basis in that degree, or in
  // degrees <= that degree (if ishomog is false).  Don't duplicate these:
  // It is probably sufficient to check linearly the ones that we have.
  // In any case, for each column, place all of the terms on a list, and sort
  // them afterwords.
  Matrix result(F,G,mapdeg);
  array<intarray> degs;
  array<Matrix> bases;
  for (i=0; i<G->rank(); i++)
    {
      vec f = NULL;
      for (j=0; j<F->rank(); j++)
	{
	  // The degree we need:

	  // Determine whether this basis exists

	  // If not, create it, and insert into list.
	  // If ishomog is false, then do that basis.

	  // For each term in this matrix, call 'random' on
	  // the coefficient ring, and add to the 'f' list this term.
	  // Fon't forget to handle the case when F is Schreyer, or
	  // R has no monoid.

	  result[j] = F->sort(f);
	}
    }
}

EMatrix * EMatrix::lead_var_coefficient(EMatrix * &monoms) const
{
  Matrix result(rows());
  monoms = Matrix(Ring_of()->make_FreeModule(1));
  int var, exp;
  for (int i=0; i<n_cols(); i++)
    {
      vec u = elem(i);
      vec v = rows()->lead_var_coefficient(u, var, exp);
      result.append(v);
      ring_elem a = Ring_of()->var(var,exp);
      vec w = monoms.rows()->term(0,a);
      Ring_of()->remove(a);
      monoms.append(w);
    }
  return result;
}


#if 0

// determinants and Schur functors
EMatrix * EMatrix::symm(int p) const;
#endif

EMatrix * EMatrix::exterior(int p) const
{
  DetComputation *d = new DetComputation(*this,p,1);
  d->calc(-1);
  EMatrix * result = d->determinants();
  delete d;
  return result;
}



void Matrix_rec::bin_out(buffer &o) const
{
  assert(cols->rank() == entries.length());
  bin_int_out(o,entries.length());
  for (int i=0; i<entries.length(); i++)
    rows->elem_bin_out(o, entries[i]);
}
EMatrix * EMatrix::simplify(int n) const
{
  int i,j, keep;
  Matrix result(rows());

  switch (n) {
  case 1:
    for (i=0; i<n_cols(); i++)
      {
	vec f = elem(i);
	if (f == NULL) continue;
	result.append(rows()->copy(f));
      }
    break;
    //  case SIMP_SCALAR_MULTIPLES:
  case 2:
    for (i=0; i<n_cols(); i++)
      {
	vec f = elem(i);
	if (f == NULL) continue;
	keep = 1;
	for (j=i+1; j<n_cols(); j++)
	  {
	    vec g = elem(j);
	    if (g == NULL) continue;
	    if (rows()->is_scalar_multiple(f, g))
	      {
		keep = 0;
		break;
	      }
	  }
	if (keep) result.append(rows()->copy(f));
      }
    break;
  case 3:
    // Remove multiple monomial divisors (i.e. x^2*f --> x*f)
    for (i=0; i<n_cols(); i++)
      {
	vec f = elem(i);
	if (f == NULL) continue;
	result.append(rows()->monomial_squarefree(f));
      }
    break;
  case 4:
    // Remove monomial divisors (i.e. x*f --> f)
    for (i=0; i<n_cols(); i++)
      {
	vec f = elem(i);
	if (f == NULL) continue;
	result.append(rows()->remove_monomial_divisors(f));
      }
    break;
#if 0
  case SIMP_ZEROS:
    break;
  case SIMP_MULTIPLES:
    break;
  case SIMP_AUTO_REDUCE:
    break;
  case SIMP_SQUAREFREE:
    break;
  case SIMP_MONOMIAL_DIVISORS:
    break;
#endif
  default:
    gError << "bad simplification type";
    break;
  }

  return result;
}

EMatrix * EMatrix::auto_reduce() const
{
  array<vec> vecs;
  int i;
  for (i=0; i<n_cols(); i++)
    vecs.append(rows()->copy(elem(i)));
  rows()->auto_reduce(vecs);
  Matrix result(rows());
  for (i=0; i<vecs.length(); i++)
    result.append(vecs[i]);
  return result;
}


MonomialIdeal EMatrix::make_monideal(int n) const
{
  queue <Bag *> new_elems;
  for (int i=0; i<n_cols(); i++)
    {
      vec v = elem(i);
      if (rows()->is_zero(v)) continue;
      if (rows()->lead_component(v) != n) continue;
      Bag *b = new Bag(i);
      rows()->lead_varpower(v, b->monom());
      new_elems.insert(b);      
    }

  // If the base ring is a quotient ring, include these lead monomials.
  if (Ring_of()->is_quotient_poly_ring())
    {
      MonomialIdeal Rideal = Ring_of()->get_quotient_monomials();
      for (Index<MonomialIdeal> j = Rideal.first(); j.valid(); j++)
	{
	  Bag *b = new Bag(-1, Rideal[j]->monom());
	  new_elems.insert(b);
	}
    }

  MonomialIdeal result(Ring_of(), new_elems);
  return result;
}

MonomialIdeal EMatrix::make_skew_monideal(int n) const
{
  MonomialIdeal result = make_monideal(n);
  const Monoid *M = Ring_of()->Nmonoms();
  if (M != NULL && M->is_skew())
    {
      intarray vp;
      for (int i=0; i<M->n_vars(); i++)
	if (M->is_skew_var(i))
	  {
	    vp.shrink(0);
	    varpower::var(i,2,vp);
	    Bag *b = new Bag(-1, vp);
	    result.insert_minimal(b);
	  }
    }
  return result;
}

void EMatrix::append_monideal(const MonomialIdeal &mi, int k)
{
  for (Index<MonomialIdeal> i = mi.last(); i.valid(); i--)
    {
      vec v = rows()->from_varpower(mi[i]->monom().raw(), k);
      append(v);
    }
}


static int symm1_next = 0;

void EMatrix::symm1(EMatrix * &result, 
		   vec f,	     // product so far generated, consumed here
		   int lastn,	     // can use lastn..n_cols()-1 in product
		   int pow) const   // remaining power to take
{
  if (pow == 0)
    result[symm1_next++] = f;
  else
    {
      for (int i=lastn; i<n_cols(); i++)
	{
	  ring_elem g = elem(0,i);
	  vec h = result.rows()->mult(g, f);
	  Ring_of()->remove(g);
	  symm1(result, h, i, pow-1);
	}
      result.rows()->remove(f);
    }
}

EMatrix * EMatrix::symm(int n) const
    // Assumption: 'this' has one row.
    // Return the 'n'th power of the ideal
{
  const FreeModule *G = cols()->symm(n);
  int *deg = degree_monoid()->make_new(degree_shift());
  degree_monoid()->power(deg, n, deg);
  Matrix result(rows(), G, deg);
  degree_monoid()->remove(deg);

  if (n >= 0)
    {
      vec f = rows()->e_sub_i(0);
      symm1_next = 0;
      symm1(result, f, 0, n);	  // consumes f
    }
  return result;
}

static int            kb_do_trunc;
static EMatrix *         kb_result;
static MonomialIdeal  kb_monideal;
static int          * kb_deg;
static vec            kb_vec;
static int            kb_n_vars;
static int          * kb_exp;
static int          * kb_mon;
static int          * kb_exp_degree;
static const Monoid * kb_D;

void EMatrix::k_basis_insert() const
{
  Ring_of()->Nmonoms()->from_expvector(kb_exp, kb_mon);
  Ring_of()->Nmonoms()->divide(kb_mon, kb_vec->monom, kb_mon);
  ring_elem tmp = Ring_of()->term(Ring_of()->Ncoeffs()->from_int(1), kb_mon);
  kb_result.append(rows()->mult(tmp, kb_vec));
  Ring_of()->remove(tmp);
}
void EMatrix::k_basis0(int firstvar) const
    // Recursively add to the result matrix all monomials in the
    // variables 0..topvar having degree 'deg' which are not in 'mi'.
{
  for (int i=firstvar; i<kb_n_vars; i++)
    {
      if (Ring_of()->Nmonoms()->is_skew() &&
	    Ring_of()->Nmonoms()->is_skew_var(i) &&
	    kb_exp[i] >= 1)
	{
	  continue;
	}

      kb_exp[i]++;
      kb_D->mult(kb_exp_degree, Ring_of()->Nmonoms()->degree_of_var(i),
		     kb_exp_degree);

      int cmp = kb_D->primary_value(kb_exp_degree) - kb_D->primary_value(kb_deg);
      Bag *b;
      if (cmp > 0 
	  && kb_do_trunc 
	  && !kb_monideal.search_expvector(kb_exp,b))
	{
	  k_basis_insert();
	}

      if (cmp <= 0 && !kb_monideal.search_expvector(kb_exp,b))
	{
	  if (cmp == 0)
	    {
	      if (kb_D->compare(kb_exp_degree, kb_deg) == EQ)
		{
		  k_basis_insert();
		}
	    }
	  else
	    k_basis0(i);
	}

      kb_exp[i]--;
      kb_D->divide(kb_exp_degree, Ring_of()->Nmonoms()->degree_of_var(i),
		   kb_exp_degree);
    }
}

EMatrix * EMatrix::k_basis(EMatrix * bot, const int *d, int do_trunc) const
    // Only the lead monomials of the two matrices 'this' and 'bottom' are
    // considered.  Thus, you must perform the required GB's elsewhere.
    // Find a basis for (image this)/(image bottom) in degree d.
    // If 'd' is NULL, first check that (image this)/(image bottom) has
    // finite dimension, and if so, return a basis.
    // If 'd' is not NULL, it is an element of the degree monoid.
{
  kb_do_trunc = do_trunc;
  kb_result = Matrix(rows());
  kb_n_vars = Ring_of()->n_vars();
  kb_D = Ring_of()->degree_monoid();

  kb_mon = Ring_of()->Nmonoms()->make_one();
  kb_deg = kb_D->make_one();
  intarray kb_exp_a;
  kb_exp = kb_exp_a.alloc(kb_n_vars);
  kb_exp_degree = kb_D->make_one();

  int *e = kb_D->make_one();
  kb_D->from_expvector(d, e);

  for (int i=0; i<n_rows(); i++)
    {
      degree_monoid()->divide(e, rows()->degree(i), kb_deg);
      
//      if (degree_monoid()->is_non_negative(kb_deg))
//	{
	  // get the two monomial ideals
	  MonomialIdeal top = make_monideal(i);
	  MonomialIdeal bottom = bot.make_monideal(i);
	  top = top - bottom;

	  Bag *b;
	  while (top.remove(b))
	    {
	      kb_vec = elem(b->basis_elem());
	      MonomialIdeal miq = top.intersect(b->monom().raw());

	      kb_monideal = miq + bottom;
	      
	      kb_exp_a.shrink(0);
	      varpower::to_ntuple(kb_n_vars, b->monom().raw(), kb_exp_a);
	      Ring_of()->Nmonoms()->degree_of_varpower(b->monom().raw(), 
						       kb_exp_degree);

	      int cmp = kb_D->primary_value(kb_exp_degree) 
		        - kb_D->primary_value(kb_deg);
	      if ((cmp > 0 && do_trunc)
                 || (0 == kb_D->compare(kb_deg, kb_exp_degree)))
		kb_result.append(rows()->copy(kb_vec));
	      else if (cmp < 0)
		k_basis0(0);

	      delete b;
	    }
//	}
//      else if (do_trunc)
//	{
//	  kb_result.append(rows()->copy(elem(i)));
//	}
    }
  
  Matrix result = kb_result;

  kb_D->remove(kb_deg);
  kb_D->remove(kb_exp_degree);
  kb_D->remove(e);
  Ring_of()->Nmonoms()->remove(kb_mon);
  kb_result = Matrix(Ring_of()); // This is so no large global data will be laying around.

  return result;
}


void EMatrix::k_basis1(int firstvar) const
    // Recursively add to the result matrix all monomials in the
    // variables 0..topvar having degree 'deg' which are not in 'mi'.
{
  Ring_of()->Nmonoms()->from_expvector(kb_exp, kb_mon);
  Ring_of()->Nmonoms()->divide(kb_mon, kb_vec->monom, kb_mon);
  ring_elem tmp = Ring_of()->term(Ring_of()->Ncoeffs()->from_int(1), kb_mon);
  kb_result.append(rows()->mult(tmp, kb_vec));
  Ring_of()->remove(tmp);

  for (int i=firstvar; i<kb_n_vars; i++)
    {
      if (Ring_of()->Nmonoms()->is_skew() &&
	    Ring_of()->Nmonoms()->is_skew_var(i) &&
	    kb_exp[i] >= 1)
	{
	  continue;
	}

      kb_exp[i]++;
      Bag *b;
      if (!kb_monideal.search_expvector(kb_exp,b))
	k_basis1(i);

      kb_exp[i]--;
    }
}

EMatrix * EMatrix::k_basis(EMatrix * bot) const
    // Only the lead monomials of the two matrices 'this' and 'bottom' are
    // considered.  Thus, you must perform the required GB's elsewhere.
    // first check that (image this)/(image bottom) has
    // finite dimension, and if so, return a basis.
{
  kb_result = Matrix(rows());
  kb_n_vars = Ring_of()->n_vars();

  kb_mon = Ring_of()->Nmonoms()->make_one();
  intarray kb_exp_a;
  kb_exp = kb_exp_a.alloc(kb_n_vars);

  for (int i=0; i<n_rows(); i++)
    {
      // get the two monomial ideals
      MonomialIdeal top = make_monideal(i);
      MonomialIdeal bottom = bot.make_monideal(i);
      
      Bag *b, *c;
      while (top.remove(b))
	{
	  kb_vec = elem(b->basis_elem());

	  MonomialIdeal miq = top.intersect(b->monom().raw());
	  kb_monideal = miq + bottom;
	  
	  kb_exp_a.shrink(0);
	  varpower::to_ntuple(kb_n_vars, b->monom().raw(), kb_exp_a);
	  if (!kb_monideal.search(b->monom().raw(), c))
	    k_basis1(0);

	  delete b;
	}
    }

  Ring_of()->Nmonoms()->remove(kb_mon);  
  Matrix result = kb_result;
  kb_result = Matrix(Ring_of()); // This is so no large global data will be laying around.
  return result;
}

EMatrix::EMatrix(const MonomialIdeal &mi) 
{ 
  const FreeModule *r = mi.Ring_of()->make_FreeModule(1);
  int *one = r->Ring_of()->degree_monoid()->make_one();
  obj = new Matrix_rec(r,r->new_free(),one);
  r->Ring_of()->degree_monoid()->remove(one);

  append_monideal(mi,0);
}

#if 0
To do here:
  +k-basis routines
  +monideal routines (done)
  elem(r,c) makes new element, so be sure to remove it if needed

  check error conditions
  lead term routines are a bit messed up

  new routines to add: 
    exterior, symm (how best to do this?)
    random_matrix
    coeffs, 
    inpart, stdpart, mininimalpart (but call them what??)
#endif
void EMatrix::minimal_lead_terms_ZZ(intarray &result) const
{
  int x;
  intarray indices;
  intarray degs; // Not used.
  array<TermIdeal *> mis;
  const array<vec> vecs = obj->entries;
  rows()->sort(vecs, degs, 0, 1, indices);
  const PolynomialRing *P = Ring_of()->cast_to_PolynomialRing();
  const FreeModule *Rsyz = P->get_Rsyz();
  FreeModule *Gsyz = P->make_FreeModule(vecs.length());
  bump_up(Gsyz);
  for (x=0; x<n_cols(); x++)
    mis.append(new TermIdeal(P,Gsyz));
  for (int i=0; i<vecs.length(); i++)
    {
      vec v = vecs[indices[i]];
      vec gsyz, rsyz;
      if (v == NULL) continue;
      if (!mis[v->comp]->search(v->coeff, v->monom, gsyz, rsyz))
	{
	  mis[v->comp]->insert_minimal(
				       new tagged_term(P->Ncoeffs()->copy(v->coeff),
						       P->Nmonoms()->make_new(v->monom),
						       NULL,
						       NULL));
	  result.append(indices[i]);
	}
      Gsyz->remove(gsyz);
      Rsyz->remove(rsyz);
    }
  for (x=0; x<n_cols(); x++)
    delete mis[x];
  bump_down(Gsyz);
}

#if 0
Matrix EMatrix::minimal_lead_terms_ZZ() const
{
  int x;
  const PolynomialRing *P = Ring_of()->cast_to_PolynomialRing();
  FreeModule *Gsyz = P->make_FreeModule(n_cols());
  bump_up(Gsyz);
  array< queue<tagged_term *> > allterms;
  for (int i=0; i<n_cols(); i++)
    {
      vec v = elem(i);
      if (v == NULL) continue;
      allterms[v->comp].insert(
			       new tagged_term(P->Ncoeffs()->copy(v->coeff),
					       P->Nmonoms()->make_new(v->monom),
					       Gsyz->e_sub_i(i),
					       NULL));
    }
  Matrix result(rows());
  for (x=0; x<n_cols(); x++)
    {
      if (allterms[x].length() > 0)
	{
	  TermIdeal *ti = TermIdeal::make_termideal(P,Gsyz,allterms[x]);
	  // Loop through and add the corresponding elements in...
	  for (cursor_TermIdeal k(ti); k.valid(); ++k)
	    {
	      tagged_term *t = *k;
	      vec gsyz = t->_gsyz;
	      vec v = NULL;
	      rows()->apply_map(v, gsyz, obj->entries);
	      rows()->negate_to(v);
	      result.append(v);
	    }
	  delete ti;
	}
    }
  bump_down(Gsyz);
  return result;
}
#endif

void EMatrix::minimal_lead_terms(intarray &result) const
{
  if (Ring_of()->Ncoeffs()->is_Z())
    {
      minimal_lead_terms_ZZ(result);
      return;
    }
  intarray indices, vp;
  intarray degs; // Not used.
  array<MonomialIdeal> mis;
  const array<vec> vecs = obj->entries;
  rows()->sort(vecs, degs, 0, 1, indices);
  for (int x=0; x<n_cols(); x++)
    mis.append(MonomialIdeal(Ring_of()));
  for (int i=0; i<vecs.length(); i++)
    {
      vec v = vecs[indices[i]];
      if (v == NULL) continue;
      // Reduce each one in turn, and replace.
      Bag *junk_bag;
      vp.shrink(0);
      rows()->lead_varpower(v, vp);
      if (!mis[v->comp].search(vp.raw(),junk_bag))
	{
	  Bag *b = new Bag(indices[i], vp);
	  mis[v->comp].insert(b);
	  result.append(indices[i]);
	}
    }
}



#endif

////////////////////////////
// Sort a set of veectors //
////////////////////////////
int ESortColumnAlgorithm::sort_compare(int i, int j) const
{
  if (i == j) return 0;
  if (deg_ascending != 0)
    {
      int d1 = sort_degs[i];
      int d2 = sort_degs[j];
      if (d1 > d2) return -deg_ascending;
      if (d1 < d2) return deg_ascending;
    }
  evec *v1 = sort_vecs[i].elems;
  evec *v2 = sort_vecs[j].elems;
  if (v1 == 0) return -monorder_ascending;
  if (v2 == 0) return monorder_ascending;
  int cmp = R->vec_compare_terms(v1,v2);
  if (cmp > 0) return -monorder_ascending;
  if (cmp < 0) return monorder_ascending;  
  if (isZZ)
    {
      // Compare coeficients as well.
      cmp = EZ->compare(ZZVAL(v1->coeff), ZZVAL(v2->coeff));
      if (cmp < 0) return 1;
      if (cmp > 0) return -1;
    }
  return 0;
}

int ESortColumnAlgorithm::sort_partition(int lo, int hi) const
{
  int pivot = sort_vals[lo];
  int i = lo-1;
  int j = hi+1;
  for (;;)
    {
      do { j--; }
      while (sort_compare(sort_vals[j], pivot) < 0);
      do { i++; }
      while (sort_compare(sort_vals[i], pivot) > 0);

      if (i < j)
	{
	  int tmp = sort_vals[j];
	  sort_vals[j] = sort_vals[i];
	  sort_vals[i] = tmp;
	}
      else
	return j;
    }
}

void ESortColumnAlgorithm::sort_range(int lo, int hi) const
{
  if (lo < hi)
    {
      int q = sort_partition(lo, hi);
      sort_range(lo, q);
      sort_range(q+1, hi);
    }
}

ESortColumnAlgorithm::ESortColumnAlgorithm(int ncols,
		       const EVector *vecs,
		       int degorder,  // -1=descending, 0=don't use, 1=ascending
		       int monorder_ascending,  // -1=descending, 1=ascending
		       const int *degrees)  // degree of each vector, for use if degorder != 0
  : deg_ascending(degorder),
    monorder_ascending(monorder_ascending),
    ncols(ncols),
    sort_vecs(vecs),
    sort_degs(degrees)
{
  if (ncols == 0) return;

  sort_vals = new int[ncols];
  for (int i=0; i<ncols; i++)
    sort_vals[i] = i;

  if (deg_ascending && sort_degs == 0)
    {
      gError << "sort: specified degree dort, without giving degrees";
      return;
    }

  R = vecs[0].getRing();
  isZZ = (R->getCoefficientRing() == EZ);
  sort_range(0,ncols-1);
}
ESortColumnAlgorithm::~ESortColumnAlgorithm()
{
  delete [] sort_vals;  // Probably
}
int *ESortColumnAlgorithm::result()
{
  int *val = sort_vals;
  sort_vals = 0;
  return val;
}
int *ESortColumnAlgorithm::sort(int ncols,
				const EVector *vecs,
				int degorder,  // -1=descending, 0=don't use, 1=ascending
				int monorder_ascending,  // -1=descending, 1=ascending
				const int *degrees)  // degree of each vector, for use if degorder != 0
{
  ESortColumnAlgorithm A(ncols,vecs,degorder,monorder_ascending,degrees);
  return A.result();
}

int *ESortColumnAlgorithm::sort(int ncols,
				const EVector *vecs,
				int degorder,  // -1=descending, 0=don't use, 1=ascending
				int monorder_ascending)  // -1=descending, 1=ascending
{
  if (ncols == 0) return 0;
  int *degrees = 0;
  if (degorder != 0)
    {
      const EMonoid *D = vecs[0].getFreeModule()->getDegreeMonoid();
      if (D->n_vars() == 0)
	degorder = 0;
      else 
	{
	  degrees = new int[ncols];
	  for (int i=0; i<ncols; i++)
	    {
	      const monomial *d = vecs[i].degree();
	      degrees[i] = D->to_exponents(d)[0];
	    }
	}
    }
  
  ESortColumnAlgorithm A(ncols,vecs,degorder,monorder_ascending,degrees);
  return A.result();
}
