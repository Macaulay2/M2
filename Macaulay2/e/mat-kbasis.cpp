#include "polyring.hpp"
#include "matrix.hpp"
#include "matrixcon.hpp"

#if 0
  const MatrixOrNull * IM2_Matrix_kbasis(const Matrix *M,
					 M2_arrayint vars,
					 M2_arrayint deg);
/* Given a matrix M:F-->G, return a matrix R^a-->G such that
   (1) image is a basis of (G/in(M))_deg, where every ring indeterminate
       outside of 'vars' is considered a unit.  If the ring R is a quotient
       ring, then in(M) contains the lead monomials of the presentation ideal of R.
       If 'vars' is missing indeterminates, then this is mathematically weird.
*/
  const MatrixOrNull * IM2_Matrix_truncate(const Matrix *M,
					   M2_arrayint vars,
					   M2_arrayint deg);

  const MatrixOrNull * IM2_Matrix_kbasis(const Matrix *M,
					 M2_arrayint vars,
					 int lodeg,
					 int hideg,
					 M2_arrayint wt);

  const MatrixOrNull * IM2_Matrix_kbasis_all(const Matrix *M,
					     M2_arrayint vars);




  const MatrixOrNull * IM2_Matrix_kbasis(const Matrix *M,
					 M2_arrayint vars,
					 M2_arrayint lo_deg,
					 M2_arrayint hi_deg); /* TODO */
  /* Construct a monomial basis of the cokernel of M, modulo the indeterminates in 'vars'
     If lodeg and hideg are not equal, then they must be each of length one. (and have
     the same length as the multi degree in the ring).
     lodeg and/or highdeg can be the empty list, in which case they refer to -infinity
     or +infinity, repsectively. */

  const MatrixOrNull * IM2_Matrix_kbasis_all(const Matrix *M,
					     M2_arrayint vars);/* TODO */

  const MatrixOrNull * IM2_Matrix_truncate(const Matrix *M,
					   M2_arrayint vars,
					   M2_arrayint deg); /* TODO */

  const MatrixOrNull * IM2_Matrix_basis_map(const Matrix *A,
					    const Matrix *B,
					    const Matrix *C,
					    M2_arrayint vars);
/* Don't really need this one: IM2_Matrix_get_coeffs(vars,C,IM2_Matrix_mult(B,A)) */
#endif

class KBasis
{
  // A class for construction of
  //  (a) kbasis of a module, in a given degree
  //  (b) kbasis of a module, which is finite
  //  (c) kbasis of a map
private:
  const PolynomialRing * P;
  const Monoid * D;
  const Monoid *M;
  const Ring * K;

  const Matrix * top_matrix;
  const Matrix * bottom_matrix;

  MatrixConstructor mat;

  bool           kb_do_trunc;
  MonomialIdeal* kb_monideal;
  int          * kb_deg;
  vec            kb_vec;
  int            kb_n_vars;
  int          * kb_exp;
  int          * kb_mon;
  int          * kb_vec_monom;
  int          * kb_exp_degree;
  intarray       kb_exp_a;

  void insert();
  void k_basis0(int firstvar);
  void k_basis1(int firstvar);

  KBasis(const Matrix *top, const Matrix *bottom, const int *wt);
  ~KBasis();

  bool compute(const int *d, bool do_trunc);
  bool compute();

  Matrix *value() { return mat.to_matrix(); }
public:
  static Matrix *k_basis(const Matrix *top, const Matrix *bot, const int *d, int do_trunc,
			 const int *wt);
  static Matrix *k_basis(const Matrix *top, const Matrix *bot, const int *wt);
};

void KBasis::insert()
{
  M->from_expvector(kb_exp, kb_mon);
  M->divide(kb_mon, kb_vec_monom, kb_mon);
  ring_elem tmp = P->term(K->from_int(1), kb_mon);
  mat.append(top_matrix->rows()->mult(tmp, kb_vec));
  P->remove(tmp);
}

void KBasis::k_basis0(int firstvar)
    // Recursively add to the result matrix all monomials in the
    // variables 0..topvar having degree 'deg' which are not in 'mi'.
{
  for (int i=firstvar; i<kb_n_vars; i++)
    {
      if (P->is_skew_commutative() &&
	    P->is_skew_var(i) &&
	    kb_exp[i] >= 1)
	{
	  continue;
	}

      kb_exp[i]++;
      D->mult(kb_exp_degree, 
		 M->degree_of_var(i),
		 kb_exp_degree);

      int cmp = D->primary_value(kb_exp_degree) - D->primary_value(kb_deg);
      Bag *b;
      if (cmp > 0 
	  && kb_do_trunc 
	  && !kb_monideal->search_expvector(kb_exp,b))
	{
	  insert();
	}

      if (cmp <= 0 && !kb_monideal->search_expvector(kb_exp,b))
	{
	  if (cmp == 0)
	    {
	      if (D->compare(kb_exp_degree, kb_deg) == EQ)
		{
		  insert();
		}
	    }
	  else
	    k_basis0(i);
	}

      kb_exp[i]--;
      D->divide(kb_exp_degree, M->degree_of_var(i),
		   kb_exp_degree);
    }
}

KBasis::KBasis(const Matrix *top, 
	       const Matrix *bottom, 
	       const int *wt)
{
  P = top->get_ring()->cast_to_PolynomialRing();
  assert(P != 0);
  D = P->degree_monoid();
  M = P->Nmonoms();
  K = P->Ncoeffs();

  top_matrix = top;
  bottom_matrix = bottom;

  kb_do_trunc = false;
  mat = MatrixConstructor(top->rows(),0,false);

  kb_n_vars = P->n_vars();

  kb_mon = M->make_one();
  kb_vec_monom = M->make_one();
  kb_deg = D->make_one();

  kb_exp = kb_exp_a.alloc(kb_n_vars);
  kb_exp_degree = D->make_one();
}

KBasis::~KBasis()
{
  D->remove(kb_deg);
  D->remove(kb_exp_degree);
  M->remove(kb_mon);
  M->remove(kb_vec_monom);  
}

bool KBasis::compute(const int *d, bool do_trunc)
    // Only the lead monomials of the two matrices 'this' and 'bottom' are
    // considered.  Thus, you must perform the required GB's elsewhere.
    // Find a basis for (image this)/(image bottom) in degree d.
    // If 'd' is NULL, first check that (image this)/(image bottom) has
    // finite dimension, and if so, return a basis.
    // If 'd' is not NULL, it is an element of the degree monoid.
{
  kb_do_trunc = do_trunc;
  int *e = D->make_one();
  D->from_expvector(d, e);

  for (int i=0; i<top_matrix->n_rows(); i++)
    {
      D->divide(e, top_matrix->rows()->degree(i), kb_deg);
      
      // get the two monomial ideals
      MonomialIdeal *top = top_matrix->make_monideal(i);
      MonomialIdeal *bottom = bottom_matrix->make_monideal(i);
      top = *top - *bottom;
      
      Bag *b;
      while (top->remove(b))
	{
	  kb_vec = top_matrix->elem(b->basis_elem());
	  M->from_varpower(b->monom().raw(),kb_vec_monom);
	  
	  MonomialIdeal *miq = top->intersect(b->monom().raw());
	  
	  kb_monideal = *miq + *bottom;
	  
	  kb_exp_a.shrink(0);
	  varpower::to_ntuple(kb_n_vars, b->monom().raw(), kb_exp_a);
	  M->degree_of_varpower(b->monom().raw(), 
				kb_exp_degree);
	  
	  int cmp = D->primary_value(kb_exp_degree) 
	    - D->primary_value(kb_deg);
	  if ((cmp > 0 && kb_do_trunc)
	      || (0 == D->compare(kb_deg, kb_exp_degree)))
	    mat.append(top_matrix->rows()->copy(kb_vec));
	  else if (cmp < 0)
	    k_basis0(0);
	  
	  deleteitem(b);
	}
      //	}
      //      else if (do_trunc)
      //	{
      //	  mat.append(rows()->copy(elem(i)));
      //	}
    }
  
  D->remove(e);
  return true;
}

Matrix *KBasis::k_basis(const Matrix *top, const Matrix *bot, const int *d, int do_trunc,
			const int *wt)
{
  KBasis KB(top,bot,wt);
  KB.compute(d,do_trunc);
  return KB.value();
}

void KBasis::k_basis1(int firstvar)
    // Recursively add to the result matrix all monomials in the
    // variables 0..topvar having degree 'deg' which are not in 'mi'.
{
  M->from_expvector(kb_exp, kb_mon);
  M->divide(kb_mon, kb_vec_monom, kb_mon);
  ring_elem tmp = P->term(K->from_int(1), kb_mon);
  mat.append(top_matrix->rows()->mult(tmp, kb_vec));
  P->remove(tmp);

  for (int i=firstvar; i<kb_n_vars; i++)
    {
      if (P->is_skew_commutative() &&
	    P->is_skew_var(i) &&
	    kb_exp[i] >= 1)
	{
	  continue;
	}

      kb_exp[i]++;
      Bag *b;
      if (!kb_monideal->search_expvector(kb_exp,b))
	k_basis1(i);

      kb_exp[i]--;
    }
}

bool KBasis::compute()
    // Only the lead monomials of the two matrices 'this' and 'bottom' are
    // considered.  Thus, you must perform the required GB's elsewhere.
    // first check that (image this)/(image bottom) has
    // finite dimension, and if so, return a basis.
{
  for (int i=0; i<top_matrix->n_rows(); i++)
    {
      // get the two monomial ideals
      MonomialIdeal *top = top_matrix->make_monideal(i);
      MonomialIdeal *bottom = bottom_matrix->make_monideal(i);
      
      Bag *b, *c;
      while (top->remove(b))
	{
	  kb_vec = top_matrix->elem(b->basis_elem());
	  M->from_varpower(b->monom().raw(),kb_vec_monom);
	  
	  MonomialIdeal *miq = top->intersect(b->monom().raw());
	  kb_monideal = *miq + *bottom;
	  
	  kb_exp_a.shrink(0);
	  varpower::to_ntuple(kb_n_vars, b->monom().raw(), kb_exp_a);
	  if (!kb_monideal->search(b->monom().raw(), c))
	    k_basis1(0);
	  
	  deleteitem(b);
	}
    }
  return true;
}

Matrix *KBasis::k_basis(const Matrix *top, const Matrix *bot, const int *wt)
{
  KBasis KB(top,bot,wt);
  KB.compute();
  return KB.value();
}

Matrix *Matrix::k_basis(const Matrix *bot, const int *d, bool do_trunc) const
{
  return KBasis::k_basis(this, bot, d, do_trunc, 0);
}

Matrix *Matrix::k_basis(const Matrix *bot) const
{
  return KBasis::k_basis(this, bot, 0);
}
