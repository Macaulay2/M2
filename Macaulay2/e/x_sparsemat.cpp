// Copyright 2002 Michael E. Stillman

#include "relem.hpp"
#include "vector.hpp"
#include "matrix.hpp"
#include "sparsemat.hpp"
#include "lattice.hpp"
#include "engine.h"

typedef SparseMutableMatrix MutableMatrix;

MutableMatrix * IM2_MutableMatrix_make(const Ring *R,
				       int nrows,
				       int ncols)
{
  return MutableMatrix::make(R,nrows,ncols);
}

MutableMatrix * IM2_MutableMatrix_from_matrix(const Matrix *M)
{
  return MutableMatrix::make(M);
}

const Matrix * IM2_MutableMatrix_to_matrix(const MutableMatrix *M)
{
  return M->toMatrix();
}

const M2_string IM2_MutableMatrix_to_string(const MutableMatrix *M)
{
  buffer o;
  M->text_out(o);
  return o.to_string();
}

unsigned long  IM2_MutableMatrix_hash(const MutableMatrix *M); /* TODO */

const RingElement * IM2_MutableMatrix_get_entry(const MutableMatrix *M, int r, int c)
{
  ring_elem f,result;
  const Ring *R = M->getRing();

  if (M->getEntry(r,c,f))
    result = R->copy(f);
  else
    result = R->from_int(0);
  return RingElement::make_raw(R, result);
}

VoidOrError IM2_MutableMatrix_set_entry(MutableMatrix *M, int r, int c, const RingElement *a)
{
  const Ring *R = M->getRing();
  if (R != a->get_ring())
    {
      ERROR("expected same ring");
      return 0;
    }
  ring_elem b = R->copy(a->get_value());
  M->setEntry(r,c,b);
  return 1;
}

void IM2_MutableMatrix_row_swap(MutableMatrix *M, int r1, int r2)
{
  M->interchangeRows(r1,r2); /* ERROR conditions? */
}

void IM2_MutableMatrix_column_swap(MutableMatrix *M, int c1, int c2)
{
  M->interchangeColumns(c1,c2);
}

VoidOrError IM2_MutableMatrix_row_change(MutableMatrix *M, int row_to_change, const RingElement *a, int r)
  /* Add a times row r to row 'row_to_change' */
{
  const Ring *R = M->getRing();
  if (R != a->get_ring())
    {
      ERROR("expected same ring");
      return 0;
    }
  M->addRowMultiple(r,a->get_value(),row_to_change);
  return 1;
}

VoidOrError IM2_MutableMatrix_column_change(MutableMatrix *M, int col_to_change, const RingElement *a, int c)
  /* Add a times column c to column 'col_to_change' */
{
  const Ring *R = M->getRing();
  if (R != a->get_ring())
    {
      ERROR("expected same ring");
      return 0;
    }
  M->addColumnMultiple(c,a->get_value(),col_to_change);
  return 1;
}

VoidOrError IM2_MutableMatrix_row_scale(MutableMatrix *M, int row_to_change, const RingElement *a)
  /* Multiply row 'row_to_change' by a, on the left */
{
  const Ring *R = M->getRing();
  if (R != a->get_ring())
    {
      ERROR("expected same ring");
      return 0;
    }
  M->scaleRow(row_to_change,a->get_value());
  return 1;
}

VoidOrError IM2_MutableMatrix_column_scale(MutableMatrix *M, int col_to_change, const RingElement *a)
  /* Multiply column 'col_to_change' by a, on the left */
{
  const Ring *R = M->getRing();
  if (R != a->get_ring())
    {
      ERROR("expected same ring");
      return 0;
    }
    M->scaleColumn(col_to_change,a->get_value());
    return 1;
}

MutableMatrix * IM2_MutableMatrix_iden(const Ring *R, int nrows)
{
  return MutableMatrix::identity(R,nrows);
}

int IM2_MutableMatrix_n_rows(const MutableMatrix *M)
{
  return M->n_rows();
}

int IM2_MutableMatrix_n_cols(const MutableMatrix *M)
{
  return M->n_cols();
}

MutableMatrix * IM2_MutableMatrix_get_row_change(MutableMatrix *M)
{
  return M->getRowChangeMatrix();
}

MutableMatrix * IM2_MutableMatrix_get_col_change(MutableMatrix *M)
{
  return M->getColumnChangeMatrix();
}

void IM2_MutableMatrix_set_row_change(MutableMatrix *M,
				      MutableMatrix *rowChange)
{
  // WARNING: check that the rings are the same...!
  M->setRowChangeMatrix(rowChange);
}

void IM2_MutableMatrix_set_col_change(MutableMatrix *M,
				      MutableMatrix *colChange)
{
  // WARNING: check that the rings are the same...!
  M->setColumnChangeMatrix(colChange);
}

M2_arrayint_OrNull IM2_FF_LU_decomp(MutableMatrix *M)
{
  return FF_LUComputation::DO(M);
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e"
// End:
