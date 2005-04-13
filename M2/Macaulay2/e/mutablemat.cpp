#include "mutablemat.hpp"
#include "sparsemat.hpp"

#include "RR.hpp"
#include "CC.hpp"
#include "text_io.hpp"
#include "coeffrings.hpp"
#include "dmatrix.hpp"

///////////////////
// MutableMatrix //
///////////////////

MutableMatrix *MutableMatrix::zero_matrix(const Ring *R, 
					  int nrows, 
					  int ncols, 
					  bool dense)
  // If the ring is RR or CC, and dense is true, then MutableMatrixRR or 
  // MutableMatrixCC will be used.
{
  const Z_mod *KZZp = R->cast_to_Z_mod();
  if (KZZp != 0)
    {
      if (dense)
	{
	  return DMatrix<CoefficientRingZZp>::zero_matrix(KZZp, nrows, ncols);
	}
      //      return SMatrix<CoefficientRingRR>::zero_matrix(globalRR, nrows, ncols);
    }
  if (R == globalRR)
    {
      if (dense)
	{
	  return DMatrix<CoefficientRingRR>::zero_matrix(globalRR, nrows, ncols);
	}
      //      return SMatrix<CoefficientRingRR>::zero_matrix(globalRR, nrows, ncols);
    }
  if (R == globalCC)
    {
      if (dense)
	{
	  return DMatrix<CoefficientRingCC>::zero_matrix(globalCC, nrows, ncols);
	}
      //      return SMatrix<CoefficientRingRR>::zero_matrix(globalRR, nrows, ncols);
    }
  if (dense)
    {
#if 0
      if (R == globalCC)
	return DenseMutableMatrixCC::zero_matrix(nrows,ncols);
      return DenseMutableMatrixRing::zero_matrix(R,nrows,ncols);
#endif
    }
  return SparseMutableMatrix::zero_matrix(R,nrows,ncols);
#if 0
  const GF *KGF = R->cast_to_GF();
  if (KGF != 0)
    {
      if (dense)
	return DenseMutableMatrix<CoefficientRingGF>(KGF, nrows, ncols);
      return SparseMutableMatrix<CoefficientRingGF>(KGF, nrows, ncols);
    }
  if (R == globalZZ)
    {
    }
  if (R == globalQQ)
    {
    }
  if (R == globalRR)
    {
    }
  if (R == globalCC)
    {
    }
  if (R == globalRRR)
    {
    }
  if (R == globalCCC)
    {
    }
#endif
}

#if 0
MutableMatrix *MutableMatrix::zero_matrix(const Ring *R, 
					  int nrows, 
					  int ncols, 
					  bool dense)
  // If the ring is RR or CC, and dense is true, then MutableMatrixRR or 
  // MutableMatrixCC will be used.
{
  if (dense)
    {
      if (R == globalRR)
	return DenseMutableMatrixRR::zero_matrix(nrows,ncols);
      if (R == globalCC)
	return DenseMutableMatrixCC::zero_matrix(nrows,ncols);
      return DenseMutableMatrixRing::zero_matrix(R,nrows,ncols);
    }
  return SparseMutableMatrix::zero_matrix(R,nrows,ncols);
}
#endif

MutableMatrix *MutableMatrix::identity(const Ring *R, int nrows, bool dense)
{
  MutableMatrix *result = MutableMatrix::zero_matrix(R,nrows,nrows,dense);
  ring_elem one = R->one();
  for (int i=0; i<nrows; i++)
    result->set_entry(i,i,one);
  return result;
}

MutableMatrix *MutableMatrix::from_matrix(const Matrix *m, bool prefer_dense)
{
  MutableMatrix *result = zero_matrix(m->get_ring(), m->n_rows(), m->n_cols(), prefer_dense);

  // Now loop through and get all (non-zero) entries.
#warning "uses internal vec knowledge"
  for (int c=0; c<m->n_cols(); c++)
    for (vec v = m->elem(c); v!=0; v=v->next)
      result->set_entry(v->comp, c, v->coeff);
  return result;
}


bool MutableMatrix::setRowChangeMatrix(MutableMatrix *rops)
{
  if (rops->get_ring() != get_ring())
    {
      ERROR("expected same ring");
      return false;
    }
  if (rops->n_cols() != n_rows())
    {
      ERROR("expected %d columns in matrix", n_rows());
      return false;
    }
  rowOps = rops;
  return true;
}
bool MutableMatrix::setColumnChangeMatrix(MutableMatrix *cops)
{
  if (cops->get_ring() != get_ring())
    {
      ERROR("expected same ring");
      return false;
    }
  if (cops->n_cols() != n_cols())
    {
      ERROR("expected %d columns in matrix", n_cols());
      return false;
    }
  colOps = cops;
  return true;
}
MutableMatrix *MutableMatrix::getRowChangeMatrix()
{
  return rowOps;
}

MutableMatrix *MutableMatrix::getColumnChangeMatrix()
{
  return colOps;
}


void MutableMatrix::text_out(buffer &o) const
{
  int nrows = n_rows();
  int ncols = n_cols();
  buffer *p = newarray(buffer,nrows);
  int r;
  for (int c=0; c<ncols; c++)
    {
      int maxcount = 0;
      for (r=0; r<nrows; r++)
	{
	  ring_elem f;
	  get_entry(r,c,f);
	  if (!R->is_zero(f))
	    R->elem_text_out(p[r], f);
	  else 
	    p[r] << ".";
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
  deletearray(p);
}


#if 0



void MutableMatrix::initialize(const Ring *KK, int nr, int nc)
{
  K = KK;
  one = K->from_int(1);
  minus_one = K->from_int(-1);
  nrows = nr;
  ncols = nc;
  V = new VectorOperations(K);
  matrix = new sparse_vector *[ncols];
  for (int c=0; c<ncols; c++)
    matrix[c] = 0;
  colSize = new int[ncols];
  rowSize = new int[nrows];
  rowOps = 0;
  colOps = 0;
  for (int i=0; i<ncols; i++)
    colSize[i] = 0;
  for (int j=0; j<nrows; j++)
    rowSize[j] = 0;
}

// Harry: tried to change following to update colSize and rowSize.

MutableMatrix::MutableMatrix(const Matrix *m)
{
  initialize(m->get_ring(), m->n_rows(), m->n_cols());
  // Now loop through and get all (non-zero) entries.
  for (int c=0; c<m->n_cols(); c++)
    {
      int last_row = -1;
      for (vec v = (*m)[c]; v!=0; v=v->next)
	if (v->comp != last_row)
	  {
	    last_row = v->comp;
	    ring_elem a = m->elem(last_row,c);
	    if (K->is_zero(a) == false)
	      {
		colSize[c]++;
		rowSize[last_row]++;
	      }
	    setEntry(last_row,c,a);
	  }
    }
}

MutableMatrix::MutableMatrix(const Ring *K0, int nrows0, int ncols0)
{
  initialize(K0,nrows0,ncols0);
}

MutableMatrix * MutableMatrix::make(const Ring *K, int nrows, int ncols)
{
  return new MutableMatrix(K,nrows,ncols);
}

MutableMatrix * MutableMatrix::make(const Matrix *m)
{
  return new MutableMatrix(m);
}

MutableMatrix::~MutableMatrix()
{
  for (int c=0; c<ncols; c++)
    V->remove(matrix[c]);
  delete [] matrix;
  delete [] rowSize;
  delete [] colSize;
  delete V;
}

Matrix *MutableMatrix::toMatrix() const
{
  FreeModule *F = K->make_FreeModule(nrows);
  Matrix *result = new Matrix(F);
  for (int c=0; c<ncols; c++)
    {
      // Create the vec v.
      vec v = 0;
      for (sparse_vector *w = matrix[c]; w!=0; w=w->next)
	{
	  vec tmp = F->raw_term(K->copy(w->coefficient), w->component);
	  F->add_to(v,tmp);
	}
      result->append(v);
    }
  return result;
}




int MutableMatrix::leadRow(int c) const  // -1 means this sparse_vector is zero.
{
  if (errorColumnBound(c)) return -1;
  sparse_vector *v = matrix[c];
  if (v == 0) return -1;
  return v->component;
}

ring_elem MutableMatrix::leadCoefficient(int c) const // Can be zero.
{
  if (errorColumnBound(c)) return K->from_int(0);
  sparse_vector *v = matrix[c];
  if (v == 0) return K->from_int(0);
  return K->copy(v->coefficient);
}

int MutableMatrix::numNonZeroRow(int r) const
{
  return -1;
}

int MutableMatrix::numNonZeroColumn(int c) const
{
  return -1;
}

bool MutableMatrix::getEntry(int r, int c, ring_elem &result) const
{
  if (errorColumnBound(c)) return false;
  if (errorRowBound(r)) return false;
  return V->getEntry(matrix[c], r, result);
}

void MutableMatrix::setEntry(int r, int c, ring_elem a)
{
  if (errorColumnBound(c)) return;
  if (errorRowBound(r)) return;
  V->setEntry(matrix[c], r, a);
}

void MutableMatrix::setRow(int r, sparse_vector *v)
{
  if (errorRowBound(r)) return;
  for ( ; v != 0; v=v->next)
    V->setEntry(matrix[v->component], r, K->copy(v->coefficient));

  // Need to set the components of the others to zero...
}

void MutableMatrix::setColumn(int c, sparse_vector *v)
{
  if (errorColumnBound(c)) return;
  sparse_vector *old = matrix[c];
  matrix[c] = v;
  V->remove(old);
}

sparse_vector *MutableMatrix::getRow(int r) const // Copies the row
{
  if (errorRowBound(r)) return 0;
  sparse_vector *result = 0;
  for (int c=0; c<ncols; c++)
    {
      ring_elem a;
      if (V->getEntry(matrix[c], r, a))
	{
	  sparse_vector *v = V->make_sparse_vector(r,a);
	  v->next = result;
	  result = v;
	}
    }
  return result;
}
sparse_vector *MutableMatrix::getColumn(int c) const // Copies the column
{
  if (errorColumnBound(c)) return 0;
  return V->clone(matrix[c]);
}

void MutableMatrix::interchangeRows(int r1, int r2, bool doRecording)
{
  if (errorRowBound(r1)) return;
  if (errorRowBound(r2)) return;
  for (int c=0; c<ncols; c++)
    V->interchangeRows(matrix[c], r1, r2);
  int n = rowSize[r1];
  rowSize[r1] = rowSize[r2];
  rowSize[r2] = n;

  if (doRecording && rowOps != 0)
    rowOps->interchangeColumns(r1,r2,false);
}

void MutableMatrix::interchangeColumns(int c1, int c2, bool doRecording)
{
  if (errorColumnBound(c1)) return;
  if (errorColumnBound(c2)) return;
  sparse_vector *tmp = matrix[c1];
  matrix[c1] = matrix[c2];
  matrix[c2] = tmp;
  int n =  colSize[c1];
  colSize[c1] = colSize[c2];
  colSize[c2] = n;

  if (doRecording && colOps != 0)
    colOps->interchangeColumns(c1,c2,false);
}

void MutableMatrix::scaleRow(int r, ring_elem a, bool doRecording)
{
  if (errorRowBound(r)) return;
  for (int c=0; c<ncols; c++)
    V->scaleRow(matrix[c], r, a);

  if (doRecording && rowOps != 0)
    rowOps->scaleColumn(r,a,false);
}

void MutableMatrix::scaleColumn(int c, ring_elem a, bool doRecording)
{
  if (errorColumnBound(c)) return;
  V->scale(matrix[c], a);

  if (doRecording && colOps != 0)
    colOps->scaleColumn(c,a,false);
}

void MutableMatrix::divideRow(int r, ring_elem a, bool doRecording)
{
  if (errorRowBound(r)) return;
  for (int c=0; c<ncols; c++)
    V->divideRow(matrix[c], r, a);

  if (doRecording && rowOps != 0)
    rowOps->divideColumn(r,a,false);
}

void MutableMatrix::divideColumn(int c, ring_elem a, bool doRecording)
{
  if (errorColumnBound(c)) return;
  V->divide(matrix[c], a);

  if (doRecording && colOps != 0)
    colOps->divideColumn(c,a,false);
}

void MutableMatrix::addRowMultiple(int r1, ring_elem a, int r, bool doRecording)
{
  if (errorRowBound(r1)) return;
  if (errorRowBound(r)) return;

  for (int c=0; c<ncols; c++)
    V->addRowMultiple(matrix[c], r1, a, r);

  if (doRecording && rowOps != 0)
    rowOps->addColumnMultiple(r1,a,r,false);
}

void MutableMatrix::addColumnMultiple(int c1, ring_elem a, int c, bool doRecording)
{
  if (errorColumnBound(c1)) return;
  if (errorColumnBound(c)) return;

  sparse_vector *tmp = V->clone(matrix[c1]);
  V->scale(tmp, a);
  V->add(matrix[c], tmp);

  int n = 0;
  for (sparse_vector *p = matrix[c]; p != 0; p = p->next) {n++;}

  colSize[c] = n;

  if (doRecording && colOps != 0)
    colOps->addColumnMultiple(c1,a,c,false);
}

ring_elem MutableMatrix::dotProduct(int c1, int c2) const
{
  return V->dotProduct(matrix[c1], matrix[c2]);
}

void MutableMatrix::normalizeColumn(int c, bool doRecording)
{
  if (errorColumnBound(c)) return;
  if (matrix[c] == 0) return;
  if (K->is_ZZ())
    {
      ring_elem a = matrix[c]->coefficient;
      mpz_ptr b = (mpz_ptr)a.poly_val;
      if (mpz_sgn(b) < 0)
	scaleColumn(c,minus_one,doRecording);
    }
  
}
void MutableMatrix::columnReduce(int c1, int c2, bool doRecording)
{
  if (errorColumnBound(c1)) return;
  if (errorColumnBound(c2)) return;

  ring_elem a1 = matrix[c1]->coefficient; // Assumed to be non-zero...
  ring_elem a2, b;
  if (!getEntry(matrix[c1]->component,c2,a2))
    return;
  if (!K->is_equal(a1,one))
    {
      ring_elem rem;
      b = K->divide(a2,a1,rem); // division algorithm...
      K->negate_to(b);
      K->remove(rem);
    }
  else
    b = K->negate(a2);
  addColumnMultiple(c1,b,c2,doRecording);
  K->remove(b);
}
void MutableMatrix::row2by2(int r1, int r2, 
				  ring_elem a1, ring_elem a2,
				  ring_elem b1, ring_elem b2,
				  bool doRecording)
{
  for (int i=0; i<ncols; i++)
    V->row2by2(matrix[i],r1,r2,a1,a2,b1,b2);

  if (doRecording && rowOps != 0)
    rowOps->column2by2(r1,r2,a1,a2,b1,b2,false);
}

void MutableMatrix::column2by2(int c1, int c2, 
				     ring_elem a1, ring_elem a2,
				     ring_elem b1, ring_elem b2,
				     bool doRecording)
{
  // do the replacements:
  // new column c1 = a1 * column[c1] + a2 * column[c2]
  // new column c2 = b1 * column[c1] + b2 * column[c2]
  // Make first column: v1 = a1*c1+a2*c2
  sparse_vector *v1 = V->clone(matrix[c1]);
  sparse_vector *v2 = V->clone(matrix[c2]);
  V->scale(v1,a1);
  V->scale(v2,a2);
  V->add(v1,v2);
  // Second column: w1 = b1*c1 + b2*c2
  sparse_vector *w1 = V->clone(matrix[c1]);
  sparse_vector *w2 = V->clone(matrix[c2]);
  V->scale(w1,b1);
  V->scale(w2,b2);
  V->add(w1,w2);
  // Set the matrices:
  V->remove(matrix[c1]);
  V->remove(matrix[c2]);
  matrix[c1] = v1;
  matrix[c2] = w1;
  // Do the recording, if needed:
  if (doRecording && colOps != 0)
    colOps->column2by2(c1,c2,a1,a2,b1,b2,false);
}

#if 0
void MutableMatrix::gcdColumnReduce(int c1, int c2, bool doRecording)
{
  if (errorColumnBound(c1)) return;
  if (errorColumnBound(c2)) return;

  if (!K->has_gcd()) return;
  ring_elem a1 = matrix[c1]->coefficient; // Assumed to be non-zero...
  ring_elem a2;
  if (!getEntry(matrix[c1]->component,c2,a2))
    return;
  ring_elem b = K->negate(a2);
  
  ring_elem x,y;
  ring_elem d = K->gcd_extended(a1,a2,x,y);

  buffer o;
  o << "gcd("; K->elem_text_out(o,a1);
  o << ","; K->elem_text_out(o,a2);
  o << ") = "; K->elem_text_out(o,d);
  o << " x="; K->elem_text_out(o,x);
  o << " y="; K->elem_text_out(o,y);
  o << newline;
  emit(o.str());

  // Make first column: matrix[column c1] = v1 = x*c1+y*c2
  sparse_vector *v1 = V->clone(matrix[c1]);
  sparse_vector *v2 = V->clone(matrix[c2]);
  V->scale(v1,x);
  V->scale(v2,y);
  V->add(v1,v2);
  // Second column: matrix[column c2] = w1 = (a1/d) c2 - (a2/d) c1
  sparse_vector *w1 = V->clone(matrix[c2]);
  sparse_vector *w2 = V->clone(matrix[c1]);
  ring_elem b1 = K->divide(a1,d); // exact division
  ring_elem b2 = K->divide(b,d); // exact division
  V->scale(w1,b1);
  V->scale(w2,b2);
  V->add(w1,w2);
  // Set the matrices:
  V->remove(matrix[c1]);
  V->remove(matrix[c2]);
  matrix[c1] = v1;
  matrix[c2] = w1;
  // Do the recording, if needed:
  if (doRecording && colOps != 0)
    {
      v1 = V->clone(colOps->matrix[c1]);
      v2 = V->clone(colOps->matrix[c2]);
      V->scale(v1,x);
      V->scale(v2,y);
      V->add(v1,v2);
      // Second column: matrix[column c2] = w1 = (a1/d) c2 - (a2/d) c1
      w1 = V->clone(colOps->matrix[c2]);
      w2 = V->clone(colOps->matrix[c1]);
      V->scale(w1,b1);
      V->scale(w2,b2);
      V->add(w1,w2);
      // Set the matrices:
      V->remove(colOps->matrix[c1]);
      V->remove(colOps->matrix[c2]);
      colOps->matrix[c1] = v1;
      colOps->matrix[c2] = w1;
    }
  // Clean up
  K->remove(b);
  K->remove(x);
  K->remove(y);
  K->remove(d);
  K->remove(b1);
  K->remove(b2);
}
#endif
void MutableMatrix::gcdRowReduce(int c, int r1, int r2, bool doRecording)
{
  if (errorColumnBound(c)) return;
  if (errorRowBound(r1)) return;
  if (errorRowBound(r2)) return;

  if (!K->has_gcd()) return;
  ring_elem a1,a2,b,b1,b2,d,x,y;
  if (!getEntry(r2,c,a2)) return;
  if (!getEntry(r1,c,a1))
    interchangeRows(r1,r2,doRecording);
  else
    {
      b = K->negate(a2);
      d = K->gcd_extended(a1,a2,x,y);
      b1 = K->divide(a1,d); // exact
      b2 = K->divide(b,d); // exact
      row2by2(r1,r2,x,y,b1,b2,doRecording);
      K->remove(b);
      K->remove(d);
      K->remove(b1);
      K->remove(b2);
      K->remove(x);
      K->remove(y);
    }
}

void MutableMatrix::gcdColumnReduce(int r, int c1, int c2, bool doRecording)
{
  if (errorColumnBound(c1)) return;
  if (errorColumnBound(c2)) return;
  if (errorRowBound(r)) return;

  if (!K->has_gcd()) return;
  ring_elem a1,a2,b,b1,b2,d,x,y;
  if (!getEntry(c2,r,a2)) return;
  if (!getEntry(c1,r,a1))
    interchangeColumns(c1,c2,doRecording);
  else
    {
      b = K->negate(a2);
      d = K->gcd_extended(a1,a2,x,y);
      b1 = K->divide(a1,d); //exact
      b2 = K->divide(b,d); // exact
      // Make first column: matrix[column c1] = v1 = x*c1+y*c2
      // Second column: matrix[column c2] = w1 = (a1/d) c2 - (a2/d) c1
      column2by2(c1,c2,x,y,b1,b2,doRecording);
      // Clean up
      K->remove(b);
      K->remove(d);
      K->remove(b1);
      K->remove(b2);
      K->remove(x);
      K->remove(y);
    }
}

void MutableMatrix::gcdColumnReduce(int c1, int c2, bool doRecording)
{
  if (errorColumnBound(c1)) return;
  if (errorColumnBound(c2)) return;

  if (matrix[c1] == 0)
    interchangeColumns(c1,c2,doRecording);
  else
    {
      int r = matrix[c1]->component;
      gcdColumnReduce(r,c1,c2,doRecording);
    }
}

void MutableMatrix::setSizes(int c_lo, int c_hi)
{
  if (errorColumnBound(c_lo)) return;
  if (errorColumnBound(c_hi)) return;

  // Sets the arrays rowSize and colSize
  int i;
  for (i=0; i<ncols; i++) colSize[i] = 0;
  for (i=0; i<nrows; i++) rowSize[i] = 0;
  for (i=c_lo; i<=c_hi; i++)
    for (sparse_vector *p=matrix[i]; p != 0; p=p->next)
      {
	colSize[i]++;
	rowSize[p->component]++;
      }
#if 0
  buffer o;
  o << "Column sizes = ";
  for (i=0; i<ncols; i++)
    {
      if (i%10 == 0) o << newline;
      o << colSize[i] << " ";
    }
  o << newline;
  o << "Row sizes = ";
  for (i=0; i<nrows; i++)
    {
      if (i%10 == 0) o << newline;
      o << rowSize[i] << " ";
    }
  o << newline;
  emit(o.str());
#endif    
}

bool MutableMatrix::findGoodUnitPivot(int c_lo, int c_hi, int &r, int &c, int &best)
{
  if (errorColumnBound(c_lo)) return false;
  if (errorColumnBound(c_hi)) return false;
  setSizes(c_lo,c_hi);
  bool found = false;
  for (int i=c_lo; i<=c_hi; i++)
    for (sparse_vector *p=matrix[i]; p != 0; p=p->next)
      {
	if (K->is_equal(one, p->coefficient)
	    || (K->is_equal(minus_one, p->coefficient)))
	  {
	    int best1 = (rowSize[p->component]-1) * (colSize[i]-1);
	    if (!found || best1 < best)
	      {
		found = true;
		r = p->component;
		c = i;
		best = best1;
		if (best == 0) return true;
	      }
	  }
      }
  return found;
}

// Harry's routines

// this routine finds units (aka pivots) in a sparse matrix, 
// clears out other columns by the pivot, and then sticks the
//  pivot into the "current" last column and last row of the matrix.

void MutableMatrix::reducePivots()
{
  // keeps track of last row and column which are not the output of a pivot
  int rowPivot = nrows-1;  
  int columnPivot = ncols-1;

  // first reduce 1's and -1's
  int i;
  for (i = 0; i < ncols; i++) 
    { 
      for (sparse_vector *p = matrix[i]; p != 0; p = p->next) 
	{
	  bool oneFlag = K->is_equal(one, p->coefficient);
	  
	  if ( oneFlag || K->is_equal(minus_one, p->coefficient)) 
	    {

	      int r = p->component;
	      
	      // case 1: pivot is only elt in its column and row
	      if (colSize[i] == 1 && rowSize[r] == 1) 
		{
		  if (oneFlag == false)
		    {
		      K->remove(matrix[i]->coefficient);
		      matrix[i]->coefficient = K->copy(one);
		    }
		  if (i == columnPivot && r == rowPivot)
		    {
		      columnPivot--;
		      rowPivot--;
		    }
		  else if (i < columnPivot || r < rowPivot)
		    {
		      interchangeColumns(i, columnPivot);
		      interchangeRows(r, rowPivot);
		      columnPivot--;
		      rowPivot--;
		      i = i-1;  //test column i again
		    }
		}
	      
	      // case 2: other elements exist in pivot's row or column
	      else
		{
		  for (int j = 0; j < ncols; j++) 
		    {
		      if (j != i) 
			{
			  for (sparse_vector *q = matrix[j]; q != 0 
				 && q->component >= r; q = q->next) 
			    {
			      if (q->component == r)
				{
				  if (oneFlag) {
				    ring_elem f = K->negate(q->coefficient);
				    addColumnMultiple(i, f, j);
				    K->remove(f);
				  }
				  else {
				    addColumnMultiple(i, q->coefficient, j);
				  }
				  break;
				}
			    }
			}
		    }
		  p = matrix[i];
		  V->remove(matrix[i]->next);
		  matrix[i]->next = 0;
		  matrix[i]->component = r;
		  K->remove(matrix[i]->coefficient);
		  matrix[i]->coefficient = K->copy(one);
		  colSize[i] = 1;
		  rowSize[r] = 1;
		  interchangeColumns(i, columnPivot);
		  interchangeRows(r, rowPivot);
		  columnPivot--;
		  rowPivot--;
		  i = -1; //start over at the 1st column
		}
	    }
	}
    }

  // now reduce other units
  for (i = 0; i < ncols; i++) 
    { 
      for (sparse_vector *p = matrix[i]; p != 0; p = p->next) 
	{
	  if ( K->is_unit(p->coefficient) && 
	       ( (colSize[i] > 1 || rowSize[p->component] > 1) ||
		 (K->is_equal(one,p->coefficient) == false) ) ) 
	    {
	      ring_elem g = K->invert(p->coefficient);
	      scaleColumn(i, g);
	      K->remove(g);
	      int r = p->component;
	      for (int j = 0; j < ncols; j++) 
		{
		  if (j != i) 
		    {
		      for (sparse_vector *q = matrix[j]; q != 0 
			     && q->component >= r; q = q->next) 
			{
			  if (q->component == r)
			    {
			      ring_elem f = K->negate(q->coefficient);
			      addColumnMultiple(i, f, j);
			      K->remove(f);
			      break;
			    }
			}
		    }
		}
	      p = matrix[i];
	      V->remove(matrix[i]->next);
	      matrix[i]->next = 0;
	      matrix[i]->component = r;
	      K->remove(matrix[i]->coefficient);
	      matrix[i]->coefficient = K->copy(one);
	      colSize[i] = 1;
	      rowSize[r] = 1;
	      interchangeColumns(i, columnPivot);
	      interchangeRows(r, rowPivot);
	      columnPivot--;
	      rowPivot--;
	      i = -1; //start over at the 1st column
	    }
	}
    }
}

MutableMatrix *MutableMatrix::identity(const Ring *K, int n)
{
  MutableMatrix *result = new MutableMatrix(K,n,n);
  for (int i=0; i<n; i++)
    result->setEntry(i,i,K->from_int(1));
  return result;
}

int MutableMatrix::compare_sparse_vectors(sparse_vector *v, sparse_vector *w)
{
  // First compare: lead component.
  // Second: if ZZ: compare using mpz_cmp
  //         if k[x]: compare using simple_degree
  // Null sparse_vectors go first
  if (v == 0)
    {
      if (w == 0) return EQ;
      return LT;
    }
  if (w == 0) return GT;
  int cmp = v->component - w->component;
  if (cmp > 0) return GT;
  if (cmp < 0) return LT;
  if (K->is_ZZ())
    {
      bool negate_a = false;
      bool negate_b = false;
      mpz_ptr a = (mpz_ptr)v->coefficient.poly_val;
      mpz_ptr b = (mpz_ptr)w->coefficient.poly_val;
      if (mpz_sgn(a) < 0)
	{
	  negate_a = true;
	  mpz_neg(a,a);
	}
      if (mpz_sgn(b) < 0)
	{
	  negate_b = true;
	  mpz_neg(b,b);
	}
      int result = mpz_cmp(a,b);
      if (negate_a) mpz_neg(a,a);
      if (negate_b) mpz_neg(b,b);
      return -result;
    }
  else
    {
      cmp = K->primary_degree(v->coefficient) - K->primary_degree(w->coefficient);
      if (cmp > 0) return GT;
      if (cmp < 0) return LT;
      return EQ;
    }
}

int MutableMatrix::sort_partition(int lo, int hi, int *sortvals)
{
  sparse_vector *pivot = matrix[sortvals[lo]];
  int i = lo-1;
  int j = hi+1;
  for (;;)
    {
      do { j--; }
      while (compare_sparse_vectors(matrix[sortvals[j]], pivot) > 0);

      do { i++; }
      while (compare_sparse_vectors(matrix[sortvals[i]], pivot) < 0);

      if (i < j)
	{
	  int tmp = sortvals[j];
	  sortvals[j] = sortvals[i];
	  sortvals[i] = tmp;
	}
      else
	return j;
    }
}

void MutableMatrix::sort1(int lo, int hi, int *sortvals)
{
  if (lo < hi)
    {
      int q = sort_partition(lo, hi, sortvals);
      sort1(lo, q, sortvals);
      sort1(q+1, hi, sortvals);
    }
}

void MutableMatrix::sortColumns(int lo, int hi, bool doRecording)
{
  if (errorColumnBound(lo)) return;
  if (errorColumnBound(hi)) return;
  // Sort columns.
  if (lo >= hi) return;
  int *sortvals = new int[ncols];
  for (int i=lo; i<=hi; i++)
    sortvals[i] = i;
  sort1(lo,hi,sortvals);

  // Now permute the columns of 'matrix', and 'colOps'.
  permuteColumns(lo,hi,sortvals,doRecording);
  delete [] sortvals;
}

void MutableMatrix::permuteColumns(int lo, int hi, int *permutation, bool doRecording)
{
  int i;
  if (errorColumnBound(lo)) return;
  if (errorColumnBound(hi)) return;
  sparse_vector **old = new sparse_vector *[hi-lo+1];
  for (i=lo; i<=hi; i++)
    old[i-lo] = matrix[i];
  for (i=lo; i<=hi; i++)
    matrix[i] = old[permutation[i]-lo];
  delete [] old;

  if (doRecording && colOps != 0)
    colOps->permuteColumns(lo,hi,permutation,false);
}

void MutableMatrix::text_out(buffer &o) const
{
  buffer *p = new buffer[nrows];
  int r;
  for (int c=0; c<ncols; c++)
    {
      int maxcount = 0;
      for (r=0; r<nrows; r++)
	{
	  ring_elem f;
	  if (getEntry(r,c,f))
	    K->elem_text_out(p[r], f);
	  else 
	    p[r] << ".";
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

void MutableMatrix::display() const
{
  buffer o;
  text_out(o);
  o << newline;
  emit(o.str());
}
#endif
// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
