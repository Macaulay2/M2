// Copyright 1995 Michael E. Stillman

#include "interp.hpp"

#include "matrix.hpp"
#include "monoid.hpp"

#include "det.hpp"
#include "pfaff.hpp"

#include "ringmap.hpp"

#include "monideal.hpp"
#include "termideal.hpp"

#include "monomial.hpp"
#include "assprime.hpp"
#include "random.hpp"

void cmd_Matrix1(object &orows, object &ocols)
     // Create a matrix from elements on the stack.
     // On stack: v0 v1 v2 ... v(c-1) rows:intarray c:int >> matrix
{
  int ncols = ocols->int_of();
  FreeModule *F = orows->cast_to_FreeModule();
  Matrix result(F);
  for (int i=ncols-1; i >= 0 ; i--) 
    {
      if (!gStack.in_bounds(i) || gStack[i]->type_of() != TY_VECTOR)
	{
	  gError << "matrix: expected vector on the stack";
	  break;
	}
      Vector v = gStack[i]->cast_to_Vector();
      if (v.Ring_of() != F->Ring_of())
	{
	  gError << "matrix: vector has incorrect base ring";
	  break;
	}
      result.append(F->translate(v.free_of(), v.get_value()));
    }
  gStack.poppem(ncols);
  gStack.insert(result);
}

void cmd_Matrix2(object &orows, object &ocols)
     // Create a matrix from elements on the stack.
     // On stack: v0 v1 v2 ... v(c-1) rows:freemodule cols:freemodule >> matrix
     // where c = rank of cols.
{
  FreeModule *F = orows->cast_to_FreeModule();
  FreeModule *G = ocols->cast_to_FreeModule();
  int ncols = G->rank();
  Matrix result(F, G);
  for (int i=ncols-1; i >= 0 ; i--) 
    {
      if (!gStack.in_bounds(i) || gStack[i]->type_of() != TY_VECTOR)
	{
	  gError << "matrix: expected vector on the stack";
	  break;
	}
      Vector v = gStack[i]->cast_to_Vector();
      if (v.Ring_of() != F->Ring_of())
	{
	  gError << "matrix: vector has incorrect base ring";
	  break;
	}
      result[ncols-1-i] = F->translate(v.free_of(), v.get_value());
    }
  gStack.poppem(ncols);
  gStack.insert(result);
}

void cmd_Matrix2a(object &orows, object &ocols, object &odeg)
     // Create a matrix from elements on the stack.
     // On stack: v0 v1 v2 ... v(c-1) rows:freemodule cols:freemodule >> matrix
     // where c = rank of cols.
{
  FreeModule *F = orows->cast_to_FreeModule();
  FreeModule *G = ocols->cast_to_FreeModule();
  int ncols = G->rank();
  Matrix result(F, G);
  intarray *deg = odeg->intarray_of();
  result.set_degree_shift(*deg);

  for (int i=ncols-1; i >= 0 ; i--) 
    {
      if (!gStack.in_bounds(i) || gStack[i]->type_of() != TY_VECTOR)
	{
	  gError << "matrix: expected vector on the stack";
	  break;
	}
      Vector v = gStack[i]->cast_to_Vector();
      if (v.Ring_of() != F->Ring_of())
	{
	  gError << "matrix: vector has incorrect base ring";
	  break;
	}
      result[ncols-1-i] = F->translate(v.free_of(), v.get_value());
    }
  gStack.poppem(ncols);
  gStack.insert(result);
}

void cmd_Matrix4(object &orows, object &ocols, object &om)
{
  FreeModule *F = orows->cast_to_FreeModule();
  FreeModule *G = ocols->cast_to_FreeModule();
  Matrix m = om->cast_to_Matrix();
  if (F->Ring_of() != G->Ring_of() || F->Ring_of() != m.Ring_of())
    {
      gError << "same base ring expected";
      return;
    }
  if (F->rank() != m.n_rows() || G->rank() != m.n_cols())
    {
      gError << "'matrix' received free modules of incorrect sizes";
      return;
    }
  Matrix result(F,G);
  int ncols = G->rank();
  for (int i=0; i<ncols; i++)
    result[i] = F->translate(m.rows(), m[i]);

  gStack.insert(result);
}
void cmd_Matrix5(object &orows, object &ocols, object &om, object &odeg)
{
  FreeModule *F = orows->cast_to_FreeModule();
  FreeModule *G = ocols->cast_to_FreeModule();
  Matrix m = om->cast_to_Matrix();
  if (F->Ring_of() != G->Ring_of() || F->Ring_of() != m.Ring_of())
    {
      gError << "same base ring expected";
      return;
    }
  if (F->rank() != m.n_rows() || G->rank() != m.n_cols())
    {
      gError << "'matrix' received free modules of incorrect sizes";
      return;
    }
  Matrix result(F,G);
  intarray *deg = odeg->intarray_of();
  result.set_degree_shift(*deg);
  int ncols = G->rank();
  for (int i=0; i<ncols; i++)
    result[i] = F->translate(m.rows(), m[i]);

  gStack.insert(result);
}

void cmd_Matrix3(object &oa)
{
  MonomialIdeal mi = oa->cast_to_MonomialIdeal();
  gStack.insert(Matrix(mi));
}

void cmd_Matrix_rows(object &oM)
{
  Matrix M = oM->cast_to_Matrix();
  gStack.insert(M.rows());
}
void cmd_Matrix_cols(object &oM)
{
  Matrix M = oM->cast_to_Matrix();
  gStack.insert(M.cols());
}

void cmd_Matrix_getshift(object &oM)
{
  Matrix M = oM->cast_to_Matrix();
  gStack.insert(new object_intarray(M.get_degree_shift()));
}
void cmd_Matrix_setshift(object &oM, object &odeg)
{
  Matrix M = oM->cast_to_Matrix();
  intarray *deg = odeg->intarray_of();
  M.set_degree_shift(*deg);
}
void cmd_Matrix_elem(object &oM, object &on)
{
  Matrix M = oM->cast_to_Matrix();
  int n = on->int_of();
  if ((n < 0) || (n >= M.n_cols()))
    gError << "matrix index " << n << " out of range 0 .. " << M.n_cols()-1;
  else
    {
      Vector result(M.rows(), M.rows()->copy(M[n]));
      gStack.insert(result);
    }
}
void cmd_Matrix_elem2(object &oM, object &on, object &om)
{
  Matrix M = oM->cast_to_Matrix();
  int n = on->int_of();
  int m = om->int_of();
  if ((n < 0) || (n >= M.n_rows()))
    gError << "matrix row index " << n << " out of range 0 .. " << M.n_rows()-1;
  else if ((m < 0) || (m >= M.n_cols()))
    gError << "matrix column index " << m << " out of range 0 .. " 
      << M.n_cols()-1;
  else
    {
      RingElement result(M.Ring_of(), M.elem(n,m));
      gStack.insert(result);
    }
}

void cmd_Matrix_isequal(object &oa, object &ob)
{
  Matrix a = oa->cast_to_Matrix();
  Matrix b = ob->cast_to_Matrix();
  gStack.insert(make_object_int(a.is_equal(b)));
}

void cmd_Matrix_iszero(object &oa)
{
  Matrix a = oa->cast_to_Matrix();
  gStack.insert(make_object_int(a.is_zero()));
}

void cmd_Matrix_ishomog(object &oa)
{
  Matrix a = oa->cast_to_Matrix();
  gStack.insert(make_object_int(a.is_homogeneous()));
}

void cmd_Matrix_add(object &oa, object &ob)
{
  Matrix a = oa->cast_to_Matrix();
  Matrix b = ob->cast_to_Matrix();
  gStack.insert(a+b);
}

void cmd_Matrix_concat(object &on)
{
  int i, j;
  int n = on->int_of();
  if (n <= 0)
    {
      gError << "directsum: expected at least one matrix";
      return;
    }
  for (i=0; i<n; i++)
    if (!gStack.in_bounds(i) || gStack[i]->type_of() != TY_MATRIX)
      {
	gError << "concat: expected " << n << " matrices";
	return;
      }
  Matrix m = gStack[n-1]->cast_to_Matrix();
  Matrix result(m.rows());
  for (i=n-1; i>=0; i--)
    {
      Matrix a = gStack[i]->cast_to_Matrix();
      if (a.Ring_of() != m.Ring_of())
	gError << "matrix concat: different base rings";
      else if (a.n_rows() != m.n_rows())
	gError << "matrix concat: row sizes are not equal";
      else 
	  for (j=0; j<a.n_cols(); j++)
	    result.append(result.rows()->translate(a.rows(), a[j]),
			  a.cols()->degree(j));
    }

  gStack.poppem(n);
  gStack.insert(result);
}

void cmd_Matrix_negate(object &oa)
{
  Matrix a = oa->cast_to_Matrix();
  gStack.insert(-a);
}

void cmd_Matrix_subtract(object &oa, object &ob)
{
  Matrix a = oa->cast_to_Matrix();
  Matrix b = ob->cast_to_Matrix();
  gStack.insert(a-b);
}

void cmd_Matrix_mult(object &oa, object &ob)
{
  Matrix a = oa->cast_to_Matrix();
  Matrix b = ob->cast_to_Matrix();
  gStack.insert(a*b);
}
void cmd_Matrix_multvec(object &oa, object &ov)
{
  Matrix a = oa->cast_to_Matrix();
  Vector v = ov->cast_to_Vector();
  if (a.n_cols() != v.free_of()->rank())
    gError << 
      "matrix multiplication: columns of matrix different from vector";
  else
    {
      vec w = a.rows()->mult_by_matrix(a,v.free_of(), v.get_value());
      gStack.insert(Vector(a.rows(), w));
    }
}
void cmd_Matrix_smult(object &oa, object &ob)
{
  RingElement a = oa->cast_to_RingElement();
  Matrix b = ob->cast_to_Matrix();
  gStack.insert(b * a.get_value());
}

void cmd_Matrix_transpose(object &oa)
{
  Matrix a = oa->cast_to_Matrix();
  gStack.insert(a.transpose());
}

void cmd_Matrix_reshape(object &oa, object &oF, object &oG)
{
  Matrix a = oa->cast_to_Matrix();
  FreeModule *F = oF->cast_to_FreeModule();
  FreeModule *G = oG->cast_to_FreeModule();
  gStack.insert(a.reshape(F,G));
}
void cmd_Matrix_flip(object &oF, object &oG)
{
  FreeModule *F = oF->cast_to_FreeModule();
  FreeModule *G = oG->cast_to_FreeModule();
  gStack.insert(Matrix::flip(F,G));
}

void cmd_Matrix_submatrix(object &oa, object &orows, object &ocols)
{
  Matrix a = oa->cast_to_Matrix();
  intarray *rows = orows->intarray_of();
  intarray *cols = ocols->intarray_of();
  gStack.insert(a.sub_matrix(*rows, *cols));
}
void cmd_Matrix_submatrix1(object &oa, object &ocols)
{
  Matrix a = oa->cast_to_Matrix();
  intarray *cols = ocols->intarray_of();
  gStack.insert(a.sub_matrix(*cols));
}

void cmd_Matrix_initial(object &oM, object &on)
{
  Matrix M = oM->cast_to_Matrix();
  int n = on->int_of();
  gStack.insert(M.lead_term(n));
}

void cmd_Matrix_initial1(object &oM)
{
  Matrix M = oM->cast_to_Matrix();
  int n = 1 + M.Ring_of()->n_vars();
  gStack.insert(M.lead_term(n));
}

void cmd_Matrix_elim(object &oM, object &on)
{
  Matrix M = oM->cast_to_Matrix();
  int n = on->int_of();
  object_intarray *result = new object_intarray;
  M.elim(n, *result->intarray_of());
  gStack.insert(result);
}
void cmd_Matrix_sat(object &oM, object &on, object &omax)
{
  Matrix M = oM->cast_to_Matrix();
  int n = on->int_of();
  int maxd = omax->int_of();
  gStack.insert(M.sat(n,maxd));
}

void cmd_Matrix_koszul(object &oa, object &on)
{
  Matrix a = oa->cast_to_Matrix();
  int p = on->int_of();
  if (a.n_rows() == 1)
    gStack.insert(a.koszul(p));
  else {
    //throw Not_Implemented_Exception("'koszul' expects a matrix with one row");
    gError << "'koszul' expects a matrix with one row";
  }
}
void cmd_Matrix_koszul2(object &orows, object &ocols)
{
  Matrix rows = orows->cast_to_Matrix();
  Matrix cols = ocols->cast_to_Matrix();
  // Check: rings are all the same
  gStack.insert(Matrix::koszul(rows,cols));
}

void cmd_Matrix_iden(object &oF)
{
  FreeModule *F = oF->cast_to_FreeModule();
  gStack.insert(Matrix::identity(F));
}

void cmd_Matrix_zero(object &orows, object &ocols)
{
  FreeModule *rows = orows->cast_to_FreeModule();
  FreeModule *cols = ocols->cast_to_FreeModule();
  gStack.insert(Matrix::zero(rows,cols));
}

void cmd_Matrix_dsum(object &oa, object &ob)
{
  Matrix a = oa->cast_to_Matrix();
  Matrix b = ob->cast_to_Matrix();
  gStack.insert(a.direct_sum(b));
}

void cmd_Matrix_dsum_several(object &on)
{
  int i;
  int n = on->int_of();
  if (n <= 0)
    {
      gError << "directsum: expected at least one matrix";
      return;
    }
  for (i=0; i<n; i++)
    if (!gStack.in_bounds(i) || gStack[i]->type_of() != TY_MATRIX)
      {
	gError << "direct sum: expected " << n << " matrices";
	return;
      }
  Matrix result = gStack[n-1]->cast_to_Matrix();
  for (i=n-2; i>=0; i--)
    {
      Matrix a = gStack[i]->cast_to_Matrix();
      if (a.Ring_of() != result.Ring_of())
	gError << "matrix directsum: different base rings";
      else 
	result = result.direct_sum(a);
    }

  gStack.poppem(n);
  gStack.insert(result);
}

void cmd_Matrix_tensor(object &oa, object &ob)
{
  Matrix a = oa->cast_to_Matrix();
  Matrix b = ob->cast_to_Matrix();
  gStack.insert(a.tensor(b));
}

void cmd_Nmodule_tensor(object &oa, object &ob)
{
  Matrix a = oa->cast_to_Matrix();
  Matrix b = ob->cast_to_Matrix();
  gStack.insert(a.module_tensor(b));
}

void cmd_Matrix_contract(object &oa, object &ob)
{
  Matrix a = oa->cast_to_Matrix();
  Matrix b = ob->cast_to_Matrix();
  gStack.insert(a.diff(b,0));
}

void cmd_Matrix_diff(object &oa, object &ob)
{
  Matrix a = oa->cast_to_Matrix();
  Matrix b = ob->cast_to_Matrix();
  gStack.insert(a.diff(b,1));
}

void cmd_Matrix_symm(object &oa, object &on)
{
  Matrix a = oa->cast_to_Matrix();
  int n = on->int_of();
  if (a.n_rows() != 1)
    gError << "ggsym unimplemented feature: matrix must have exactly one row";
  gStack.insert(a.symm(n));
}

void cmd_Matrix_exterior(object &oa, object &on)
{
  Matrix a = oa->cast_to_Matrix();
  int n = on->int_of();
  gStack.insert(a.exterior(n));
}
void cmd_Matrix_exterior_product(object &op, object &oq,
				 object &oF)
{
  int p = op->int_of();
  int q = oq->int_of();
  const FreeModule *F = oF->cast_to_FreeModule();
  gStack.insert(Matrix::wedge_product(p,q,F));
}

void cmd_Matrix_homog(object &oa, object &on, object &owts)
{
  Matrix a = oa->cast_to_Matrix();
  const Ring *R = a.Ring_of();
  int v = on->int_of();
  intarray *wts = owts->intarray_of();
  if (v < 0 || v > R->n_vars())
    {
      gError << "homogenize: improper ring variable";
      return;
    }
  if (wts == NULL || wts->length() != R->n_vars())
    {
      gError << "homogenization: improper weight function";
      return;
    }
  if ((*wts)[v] == 0)
    {
      gError << "homogenization: variable weight is zero";
      return;
    }

  Matrix result = a.homogenize(v, wts->raw());
  if (!error_exists())
    gStack.insert(result);
}

void cmd_Matrix_kbasis(object &oa, object &ob, object &od)
{
  Matrix a = oa->cast_to_Matrix();
  Matrix b = ob->cast_to_Matrix();
  if (a.Ring_of() != b.Ring_of())
    {
      gError << "'kbasis': different rings";
      return;
    }
  intarray *d = od->intarray_of();
  assert(d != NULL);
  if (a.degree_monoid()->n_vars() != d->length())
    gError << "badly formed degree for 'ggkbasis'";
  else
    gStack.insert(a.k_basis(b,d->raw(),0));
}
void cmd_Matrix_truncate(object &oa, object &ob, object &od)
{
  Matrix a = oa->cast_to_Matrix();
  Matrix b = ob->cast_to_Matrix();
  if (a.Ring_of() != b.Ring_of())
    {
      gError << "'truncate': different rings";
      return;
    }
  intarray *d = od->intarray_of();
  assert(d != NULL);
  if (a.degree_monoid()->n_vars() != d->length())
    gError << "badly formed degree for 'ggtruncate'";
  else
    gStack.insert(a.k_basis(b,d->raw(),1));
}
void cmd_Matrix_kbasis(object &oa, object &ob)
{
  Matrix a = oa->cast_to_Matrix();
  Matrix b = ob->cast_to_Matrix();
  if (a.Ring_of() != b.Ring_of())
    {
      gError << "'kbasis': different rings";
      return;
    }
  gStack.insert(a.k_basis(b));
}

#if 0
void cmd_Matrix_kbasis_out(object &oa, object &ob)
{
  Matrix a = oa->cast_to_Matrix();
  intarray *b = ob->intarray_of();
  assert(b != NULL);
  if (b->length() == 0)
    gStack.insert(a.k_basis_out(NULL));
  else if (a.degree_monoid()->n_vars() > b->length())
    gError << "badly formed degree for 'ggkbasis'";
  else
    gStack.insert(a.k_basis_out(b->raw()));
}

void cmd_Matrix_kbasis_in(object &oa, object &ob)
{
  Matrix a = oa->cast_to_Matrix();
  intarray *b = ob->intarray_of();
  assert(b != NULL);
  if (a.degree_monoid()->n_vars() > b->length())
    gError << "badly formed degree for 'ggkbasis'";
  else
    gStack.insert(a.k_basis_in(b->raw()));
}
#endif
void cmd_dets(object &om, object &op)
{
  Matrix M = om->cast_to_Matrix();
  int p = op->int_of();
  gStack.insert(new DetComputation(M,p,0));
  engine_alloc(sizeof(DetComputation));
}

void cmd_pfaffs(object &om, object &op)
{
  Matrix M = om->cast_to_Matrix();
  int p = op->int_of();
  gStack.insert(new PfaffianComputation(M,p));
  engine_alloc(sizeof(PfaffianComputation));
}
void cmd_Matrix_simplify(object &om, object &on)
{
  Matrix M = om->cast_to_Matrix();
  int n = on->int_of();
  gStack.insert(M.simplify(n));
}
void cmd_Matrix_autoreduce(object &om)
{
  Matrix M = om->cast_to_Matrix();
  gStack.insert(M.auto_reduce());
}
void cmd_Matrix_var_coeffs(object &om)
{
  Matrix M = om->cast_to_Matrix();
  Matrix monoms;
  Matrix result = M.lead_var_coefficient(monoms);
  gStack.insert(result);
  gStack.insert(monoms);
}
void cmd_Matrix_sort(object &om, object &odegorder, object &omonorder)
{
  Matrix M = om->cast_to_Matrix();
  int degorder = odegorder->int_of();
  int monorder = omonorder->int_of();
  object_intarray *result = new object_intarray;
  M.sort(degorder,monorder, *result->intarray_of());
  gStack.insert(result);
}
void cmd_Matrix_coeffs(object &om, object &op)
{
  Matrix M = om->cast_to_Matrix();
  intarray *vars = op->intarray_of();
  const PolynomialRing *R = M.Ring_of()->cast_to_poly_ring();
  if (R == NULL)
    {
      gError << "coeffs: need a polynomial ring";
      return;
    }
  int nvars = M.Ring_of()->n_vars();
  int *v = new int[nvars];
  int i;
  for (i=0; i<nvars; i++) v[i] = 0;
  for (i=0; i<vars->length(); i++)
    {
      int w = (*vars)[i];
      if (w < 0 || w >= nvars)
	{
	  gError << "'coeffs': variable index out of range";
	  delete [] v;
	  return;
	}
      v[w] = 1;
    }
  Matrix result_monoms;
  Matrix result_coeffs = M.coeffs(v, result_monoms);
  delete [] v;
  gStack.insert(result_coeffs);
  gStack.insert(result_monoms);
}

void cmd_RingMap(object &om)
{
  Matrix m = om->cast_to_Matrix();
  RingMap f = m;
  gStack.insert(f);
}
void cmd_RingMap_eval_ringelem(object &omap, object &oelem)
{
  RingMap map = omap->cast_to_RingMap();
  RingElement r = oelem->cast_to_RingElement();
  gStack.insert(map.eval(r));
}
void cmd_RingMap_eval_vector(object &omap, object &oF, object &ov)
{
  RingMap map = omap->cast_to_RingMap();
  Vector v = ov->cast_to_Vector();
  const FreeModule *F = oF->cast_to_FreeModule();
  gStack.insert(map.eval(F,v));
}
void cmd_RingMap_eval_matrix(object &omap, object &oF, object &om)
{
  RingMap map = omap->cast_to_RingMap();
  Matrix m = om->cast_to_Matrix();
  const FreeModule *F = oF->cast_to_FreeModule();
  gStack.insert(map.eval(F,m));
}
void cmd_RingElement_promote(object &oR, object &of)
{
  const Ring *R = oR->cast_to_Ring();
  RingElement f = of->cast_to_RingElement();
  RingElement result;
  if (f.promote(R, result))
    gStack.insert(result);
  else
    gError << "cannot promote given ring element";
}
void cmd_RingElement_lift(object &oR, object &of)
{
  const Ring *R = oR->cast_to_Ring();
  RingElement f = of->cast_to_RingElement();
  RingElement result;
  if (f.lift(R, result))
    gStack.insert(result);
  else
    gError << "cannot lift given ring element";
}
void cmd_Vector_promote(object & /*oF*/, object & /*ov*/)
{
#if 0
  const FreeModule *F = oF->cast_to_FreeModule();
  Vector v = ov->cast_to_Vector();
  Vector result;
  if (m.promote(F, result))
    gStack.insert(result);
  else
    gError << "cannot promote given vector";
#endif
}
void cmd_Matrix_promote(object & /*oF*/, object & /*om*/)
{
#if 0
  const FreeModule *F = oF->cast_to_FreeModule();
  Matrix m = om->cast_to_Matrix();
  Matrix result;
  if (m.promote(F, result))
    gStack.insert(result);
  else
    gError << "cannot promote given matrix";
#endif
}

#if 0
void cmd_matrix_monsyz(object &oa, object &on)
{
  // Also want: a routine that orders these elements "correctly"
  matrix a = oa->cast_to_matrix();
  int strategy = on->int_of();

  matrix result(a.cols());
  array<MonomialIdeal> mis;
  intarray mon;
  monomial m(0),n(0);
  for (int i=0; i<a.n_rows(); i++)
    mis.append(MonomialIdeal(a.ring_of()));

  for (int j=0; j<a.n_cols(); j++)
    {
      int jindex = a[j].lead_component();
      mon.shrink(0);
      a[j].lead_varpower(mon);
      MonomialIdeal newpairs = mis[jindex].quotient(mon.raw());
      Index<MonomialIdeal> p = (strategy % 2 ? newpairs.first() : newpairs.last());
      for ( ; p.valid(); (strategy % 2 ? p++ : p--))
	{
	  // Append the vector m e_j - n e_i to the result,
	  // where m leadmon(a[j]) = n leadmon(a[i]),
	  // and i = baggage of p.
	  int i = newpairs[p]->basis_elem();
	  a[j].lead_monom().monsyz(a[i].lead_monom(), m, n);
	  result.append(vector(result.rows(), m.ints(), j) 
			- vector(result.rows(), n.ints(), i));
	}
      mis[jindex].insert(new Bag(j, mon));
    }
  gStack.insert(result);
}



void cmd_matrix_monsyz1(object &oa, object &on)
{
  // Also want: a routine that orders these elements "correctly"
  matrix a = oa->cast_to_matrix();
  int strategy = on->int_of();

  matrix result(a.cols());
  array<MonomialIdeal> mis;
  intarray mon;
  monomial m(0),n(0);
  for (int i=0; i<a.n_rows(); i++)
    mis.append(MonomialIdeal(a.ring_of()));

  for (int j=0; j<a.n_cols(); j++)
    {
      int jindex = a[j].lead_component();
      mon.shrink(0);
      a[j].lead_varpower(mon);
      MonomialIdeal newpairs = mis[jindex].quotient(mon.raw());
      Index<MonomialIdeal> p = (strategy % 2 ? newpairs.first() : newpairs.last());
      for ( ; p.valid(); (strategy % 2 ? p++ : p--))
	{
	  // Append the vector m e_j - n e_i to the result,
	  // where m leadmon(a[j]) = n leadmon(a[i]),
	  // and i = baggage of p.
	  int i = newpairs[p]->basis_elem();
	  a[j].lead_monom().monsyz(a[i].lead_monom(), m, n);
	  result.append(vector(result.rows(), m.ints(), j) 
			- vector(result.rows(), n.ints(), i));
	}
      mis[jindex].insert(new Bag(j, mon));
    }
  gStack.insert(result);
}
#endif


//////////////////////////////////////////
// Monomial ideal commands ///////////////
//////////////////////////////////////////

// Monomial ideal commands

void cmd_MonomialIdeal(object &oa, object &on)
{
  Matrix m = oa->cast_to_Matrix();
  int n = on->int_of();
  gStack.insert(m.make_monideal(n));
}

void cmd_Nmi_isequal(object &oa, object &ob)
{
  MonomialIdeal a = oa->cast_to_MonomialIdeal();
  MonomialIdeal b = ob->cast_to_MonomialIdeal();
  gStack.insert(make_object_int(a.is_equal(b)));
}

void cmd_Nmi_copy(object &oa)
{
  MonomialIdeal mi = oa->cast_to_MonomialIdeal();
  gStack.insert(mi.copy());
}

void cmd_Nmi_radical(object &oa)
{
  MonomialIdeal mi = oa->cast_to_MonomialIdeal();
  gStack.insert(mi.radical());
}

void cmd_Nmi_add(object &oa, object &ob)
{
  MonomialIdeal mi1 = oa->cast_to_MonomialIdeal();
  MonomialIdeal mi2 = ob->cast_to_MonomialIdeal();
  gStack.insert(mi1 + mi2);
}

void cmd_Nmi_product(object &oa, object &ob)
{
  MonomialIdeal mi1 = oa->cast_to_MonomialIdeal();
  MonomialIdeal mi2 = ob->cast_to_MonomialIdeal();
  gStack.insert(mi1 * mi2);
}

void cmd_Nmi_intersect(object &oa, object &ob)
{
  MonomialIdeal mi1 = oa->cast_to_MonomialIdeal();
  MonomialIdeal mi2 = ob->cast_to_MonomialIdeal();
  gStack.insert(mi1.intersect(mi2));
}

void cmd_Nmi_quotient1(object &oa, object &om)
{
  MonomialIdeal mi = oa->cast_to_MonomialIdeal();
  Monomial m = om->cast_to_Monomial();
  gStack.insert(mi.quotient(m.ints()));
}

void cmd_Nmi_quotient(object &oa, object &ob)
{
  MonomialIdeal mi = oa->cast_to_MonomialIdeal();
  MonomialIdeal mi2 = ob->cast_to_MonomialIdeal();
  gStack.insert(mi.quotient(mi2));
}

void cmd_Nmi_sat1(object &oa, object &om)
{
  MonomialIdeal mi = oa->cast_to_MonomialIdeal();
  Monomial m = om->cast_to_Monomial();
  gStack.insert(mi.erase(m.ints()));
}

void cmd_Nmi_sat(object &oa, object &ob)
{
  MonomialIdeal mi = oa->cast_to_MonomialIdeal();
  MonomialIdeal mi2 = ob->cast_to_MonomialIdeal();
  gStack.insert(mi.sat(mi2));
}

void cmd_Nmi_remove(object &oa)
{
  MonomialIdeal mi = oa->cast_to_MonomialIdeal();
  Bag *b;
  if (!mi.remove(b)) return;
  gStack.insert(make_object_int(b->basis_elem()));
  gStack.insert(Monomial(b->monom().raw()));
  delete b;
}

void cmd_Nmi_borel(object &oa)
{
  MonomialIdeal mi = oa->cast_to_MonomialIdeal();
  gStack.insert(mi.borel());
}

void cmd_Nmi_isborel(object &oa)
{
  MonomialIdeal mi = oa->cast_to_MonomialIdeal();
  gStack.insert(make_object_int(mi.is_borel()));
}

void cmd_Nmi_codim(object &oa)
{
  MonomialIdeal mi = oa->cast_to_MonomialIdeal();
  AssociatedPrimes ap(mi);
  gStack.insert(make_object_int(ap.codimension()));
}

void cmd_Nmi_assprimes(object &oa)
{
  MonomialIdeal mi = oa->cast_to_MonomialIdeal();
  AssociatedPrimes ap(mi);
  gStack.insert(ap.associated_primes());
}

void cmd_ti_matrix(object &oti)
{
  TermIdeal *ti = oti->cast_to_TermIdeal();
  Matrix result(ti->Ring_of());
  ti->append_to_matrix(result, 0);
  gStack.insert(result);
}
void cmd_ti_ti(object &om, object &on)
{
  Matrix m = om->cast_to_Matrix();
  int n = on->int_of();
  TermIdeal *ti = TermIdeal::make_termideal(m, n);
  gStack.insert(ti);
}



// Random number/element generation
void cmd_random_seed(object &on)
{
  long n = on->int_of();
  Random::set_seed(n);
}
void cmd_random_maxint(object &on)
{
  gStack.insert(Random::get_max_int());
  RingElement n = on->cast_to_RingElement();
  Random::set_max_int(n);
}
void cmd_random_int()
{
  gStack.insert(Random::random());
}
void cmd_random_elem(object &oR)
{
  Ring *R = oR->cast_to_Ring();
  gStack.insert(RingElement::random(R));
}
void cmd_random(object & /*oF*/, object & /*oG*/,
		object & /*otopdeg*/, object & /*odeg*/)
{
}

void i_Matrix_cmds(void)
{
  install(ggmatrix, cmd_Matrix1, TY_FREEMODULE, TY_INT);
  install(ggmatrix, cmd_Matrix2, TY_FREEMODULE, TY_FREEMODULE);
  install(ggmatrix, cmd_Matrix2a, TY_FREEMODULE, TY_FREEMODULE, TY_INTARRAY);
  install(ggmatrix, cmd_Matrix3, TY_MONIDEAL);
  install(ggmatrix, cmd_Matrix4, TY_FREEMODULE, TY_FREEMODULE, TY_MATRIX);
  install(ggmatrix, cmd_Matrix5, TY_FREEMODULE, TY_FREEMODULE, TY_MATRIX, TY_INTARRAY);
  install(ggmonideal, cmd_MonomialIdeal, TY_MATRIX, TY_INT);

  install(ggisequal, cmd_Matrix_isequal, TY_MATRIX, TY_MATRIX);
  install(ggiszero, cmd_Matrix_iszero, TY_MATRIX);

  install(gggetrows, cmd_Matrix_rows, TY_MATRIX);
  install(gggetcols, cmd_Matrix_cols, TY_MATRIX);
  install(ggelem, cmd_Matrix_elem, TY_MATRIX, TY_INT);
  install(ggelem, cmd_Matrix_elem2, TY_MATRIX, TY_INT, TY_INT);

  install(gggetshift, cmd_Matrix_getshift, TY_MATRIX);
  install(ggsetshift, cmd_Matrix_setshift, TY_MATRIX, TY_INTARRAY);

  install(ggishomogeneous, cmd_Matrix_ishomog, TY_MATRIX);
  install(gghomogenize, cmd_Matrix_homog, TY_MATRIX, TY_INT, TY_INTARRAY);

  install(ggnegate, cmd_Matrix_negate, TY_MATRIX);
  install(ggadd, cmd_Matrix_add, TY_MATRIX, TY_MATRIX);
  install(ggsubtract, cmd_Matrix_subtract, TY_MATRIX, TY_MATRIX);
  install(ggmult, cmd_Matrix_smult, TY_RING_ELEM, TY_MATRIX);
  install(ggmult, cmd_Matrix_mult, TY_MATRIX, TY_MATRIX);
  install(ggmult, cmd_Matrix_multvec, TY_MATRIX, TY_VECTOR);
  install(ggconcat, cmd_Matrix_concat, TY_INT);

  install(ggreshape, cmd_Matrix_reshape, TY_MATRIX, 
	  TY_FREEMODULE, TY_FREEMODULE);
  install(ggflip, cmd_Matrix_flip, TY_FREEMODULE, TY_FREEMODULE);
  install(ggtranspose, cmd_Matrix_transpose, TY_MATRIX);
  install(ggsubmatrix, cmd_Matrix_submatrix, TY_MATRIX, 
	  TY_INTARRAY, TY_INTARRAY);
  install(ggsubmatrix, cmd_Matrix_submatrix1, TY_MATRIX, TY_INTARRAY);

  install(ggkoszul, cmd_Matrix_koszul, TY_MATRIX, TY_INT);
  install(ggkoszul, cmd_Matrix_koszul2, TY_MATRIX, TY_MATRIX);
  install(gginitial, cmd_Matrix_initial, TY_MATRIX, TY_INT);
  install(gginitial, cmd_Matrix_initial1, TY_MATRIX);
  install(ggelim, cmd_Matrix_elim, TY_MATRIX, TY_INT);
  install(ggsat, cmd_Matrix_sat, TY_MATRIX, TY_INT, TY_INT);

  install(ggiden, cmd_Matrix_iden, TY_FREEMODULE);
  install(ggzeromat, cmd_Matrix_zero, TY_FREEMODULE, TY_FREEMODULE);

  // Random number/element generation
  install(ggrandomseed, cmd_random_seed, TY_INT);
  install(ggrandommax, cmd_random_maxint, TY_INT);
  install(ggrandomint, cmd_random_int);
  install(ggrandom, cmd_random_elem, TY_RING);
  install(ggrandom, cmd_random, TY_FREEMODULE, TY_FREEMODULE, 
	  TY_INT, TY_INTARRAY);

  install(ggdirectsum, cmd_Matrix_dsum, TY_MATRIX, TY_MATRIX);
  install(ggdirectsum, cmd_Matrix_dsum_several, TY_INT);
  install(ggtensor, cmd_Matrix_tensor, TY_MATRIX, TY_MATRIX);
  install(ggmodtensor, cmd_Nmodule_tensor, TY_MATRIX, TY_MATRIX);

  install(ggcontract, cmd_Matrix_contract, TY_MATRIX, TY_MATRIX);
  install(ggdiff, cmd_Matrix_diff, TY_MATRIX, TY_MATRIX);
  install(ggsymm, cmd_Matrix_symm, TY_MATRIX, TY_INT);
  install(ggkbasis, cmd_Matrix_kbasis, TY_MATRIX, TY_MATRIX, TY_INTARRAY);
  install(ggtruncate, cmd_Matrix_truncate, TY_MATRIX, TY_MATRIX, TY_INTARRAY);
  install(ggkbasis, cmd_Matrix_kbasis, TY_MATRIX, TY_MATRIX);

  install(ggexterior, cmd_Matrix_exterior, TY_MATRIX, TY_INT);
  install(ggexteriorproduct, cmd_Matrix_exterior_product,
	  TY_INT, TY_INT, TY_FREEMODULE);

  install(ggdets, cmd_dets, TY_MATRIX, TY_INT);
  install(ggpfaffs, cmd_pfaffs, TY_MATRIX, TY_INT);

  install(ggcoeffs, cmd_Matrix_coeffs, TY_MATRIX, TY_INTARRAY);
  install(ggcoeffs, cmd_Matrix_var_coeffs, TY_MATRIX);

  install(ggsimplify, cmd_Matrix_simplify, TY_MATRIX, TY_INT);
  install(ggsortcolumns, cmd_Matrix_sort, TY_MATRIX, TY_INT, TY_INT);
  install(ggautoreduce, cmd_Matrix_autoreduce, TY_MATRIX);

  // ring map routines
  install(ggringmap, cmd_RingMap, TY_MATRIX);
  install(ggev, cmd_RingMap_eval_ringelem, TY_RING_MAP, TY_RING_ELEM);
  install(ggev, cmd_RingMap_eval_vector, TY_RING_MAP, TY_FREEMODULE, TY_VECTOR);
  install(ggev, cmd_RingMap_eval_matrix, TY_RING_MAP, TY_FREEMODULE, TY_MATRIX);

  install(ggpromote, cmd_Matrix_promote, TY_FREEMODULE, TY_MATRIX);
  install(ggpromote, cmd_Vector_promote, TY_FREEMODULE, TY_VECTOR);
  install(ggpromote, cmd_RingElement_promote, TY_RING, TY_RING_ELEM);

  install(gglift, cmd_RingElement_lift, TY_RING, TY_RING_ELEM);
#if 0
  install(gglift, cmd_Matrix_lift, TY_FREEMODULE, TY_MATRIX);
  install(gglift, cmd_Vector_lift, TY_FREEMODULE, TY_VECTOR);
#endif
//  install(ggmonsyz, cmd_Matrix_monsyz, TY_MATRIX, TY_INT);

  // monideal commands
  install(ggcopy, cmd_Nmi_copy, TY_MONIDEAL);
  install(ggisequal, cmd_Nmi_isequal, TY_MONIDEAL, TY_MONIDEAL);
  install(ggradical, cmd_Nmi_radical, TY_MONIDEAL);
  install(ggadd, cmd_Nmi_add, TY_MONIDEAL, TY_MONIDEAL);
  install(ggmult, cmd_Nmi_product, TY_MONIDEAL, TY_MONIDEAL);
  install(ggintersect, cmd_Nmi_intersect, TY_MONIDEAL, TY_MONIDEAL);
  install(ggdiv, cmd_Nmi_quotient1, TY_MONIDEAL, TY_MONOMIAL);
  install(ggdiv, cmd_Nmi_quotient, TY_MONIDEAL, TY_MONIDEAL);
  install(ggsat, cmd_Nmi_sat1, TY_MONIDEAL, TY_MONOMIAL);
  install(ggsat, cmd_Nmi_sat, TY_MONIDEAL, TY_MONIDEAL);

  install(ggremove, cmd_Nmi_remove, TY_MONIDEAL);

  install(ggborel, cmd_Nmi_borel, TY_MONIDEAL);
  install(ggisborel, cmd_Nmi_isborel, TY_MONIDEAL);

  install(ggcodim, cmd_Nmi_codim, TY_MONIDEAL);
  install(ggprimes, cmd_Nmi_assprimes, TY_MONIDEAL);

  // termideal routines
  install(ggmatrix, cmd_ti_matrix, TY_TERMIDEAL);
  install(ggtermideal, cmd_ti_ti, TY_MATRIX, TY_INT);
}
