#include "polyring.hpp"
#include "matrix.hpp"
#include "matrixcon.hpp"

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

  MatrixConstructor mat;

  const Matrix *bottom_matrix;
  const int * lo_degree;
  const int * hi_degree;
  M2_arrayint wt_vector; // length is D->n_vars(), or less.
  // Dot product of a degree of a variable
  // in 'vars' will give a positive value.

  int * var_wts; // var_wts[i] is the weight of the vars->array[i] th variable
  M2_arrayint vars;
  bool do_truncation;
  int limit; // if >= 0, then stop after that number.

  int kb_comp;
  int kb_exp_weight;
  int kb_exp_lo_weight;
  int kb_exp_hi_weight;
  int          * kb_exp_degree;
  int          * kb_exp_lo_degree; // possibly NULL, meaning no low degree
                    // HOWEVER: that can only happen if this is not a multi grading
  int          * kb_exp_hi_degree; // possibly NULL, as for kb_exp_lo_degree

  bool           kb_do_trunc;
  MonomialIdeal* kb_monideal;
  int          * kb_deg;
  vec            kb_vec;
  int            kb_n_vars;
  int          * kb_exp;
  int          * kb_mon;
  int          * kb_vec_monom;
  intarray       kb_exp_a;

  int weight(const int *deg) const;
  void insert();
  void k_basis0(int firstvar);

  KBasis(const Matrix *bottom, 
	 const int * lo_degree,
	 const int * hi_degree,
	 const M2_arrayint wt, 
	 const M2_arrayint vars,
	 bool do_truncation,
	 int limit);

  ~KBasis() {}

  bool compute();

  Matrix *value() { return mat.to_matrix(); }
public:
  static Matrix *k_basis(const Matrix *bottom, 
			 const M2_arrayint lo_degree,
			 const M2_arrayint hi_degree,
			 const M2_arrayint wt, 
			 const M2_arrayint vars,
			 bool do_truncation,
			 int limit);
};

KBasis::KBasis(const Matrix *bottom, 
	       const int * lo_degree0,
	       const int * hi_degree0,
	       const M2_arrayint wt, 
	       const M2_arrayint vars0,
	       bool do_truncation0,
	       int limit0)
  : bottom_matrix(bottom),
    lo_degree(lo_degree0),
    hi_degree(hi_degree0),
    wt_vector(wt),
    vars(vars0),
    do_truncation(do_truncation0),
    limit(limit0)
{
  // Compute the (positive) weights of each of the variables in 'vars'.

  var_wts = newarray(int, vars->len);
  for (int i=0; i<vars->len; i++)
    {
      var_wts[i] = weight(M->degree_of_var(vars->array[i]));
    }

  // Set the recursion variables
  kb_n_vars = vars->len;

  kb_exp = newarray(int, P->n_vars());
  for (int i=0; i<P->n_vars(); i++)
    kb_exp[i] = 0;
  kb_exp_weight = 0;

  kb_exp_degree = newarray(int, D->n_vars());
  for (int i=0; i<D->n_vars(); i++)
    kb_exp_degree[i] = 0;

  kb_exp_weight = 0;
  if (lo_degree != 0) kb_exp_lo_weight = weight(lo_degree);
  if (hi_degree != 0) kb_exp_hi_weight = weight(hi_degree);
}

int KBasis::weight(const int *deg) const
{
  int sum = 0;
  for (int i=0; i<wt_vector->len; i++)
    sum += wt_vector->array[i] * deg[i];
  return sum;
}

void KBasis::insert()
{
  // We have a new basis element

  M->from_expvector(kb_exp, kb_mon);
  ring_elem r = P->make_flat_term(K->one(), kb_mon);
  vec v = P->make_vec(kb_comp, r);
  mat.append(v);
}

void KBasis::k_basis0(int firstvar)
    // Recursively add to the result matrix all monomials in the
    // variables 0..topvar having degree 'deg' which are not in 'mi'.
{
  for (int i=firstvar; i<kb_n_vars; i++)
    {
      int v = vars->array[v];
      if (P->is_skew_commutative() &&
	    P->is_skew_var(v) &&
	    kb_exp[v] >= 1)
	{
	  continue;
	}

      kb_exp[v]++;
      D->mult(kb_exp_degree, 
		 M->degree_of_var(v),
		 kb_exp_degree);
      kb_exp_weight += var_wts[i];

      Bag *b;
      if (kb_exp_hi_degree // this means that kb_exp_hi_weight will be set
	  && kb_exp_weight > kb_exp_hi_weight
	  && kb_do_trunc 
	  && !kb_monideal->search_expvector(kb_exp,b))
	{
	  insert();
	}
      else {
	if ((hi_degree == 0 || kb_exp_weight <= kb_exp_hi_weight)
	    && !kb_monideal->search_expvector(kb_exp,b))
	  {
	    // When do we insert it?
	    // if single degree at this point: if weight is >= kb_exp_lo_weight
	    // if multidegree: check equality
	    // or if multidegree, is monomial is identical
	    
	    if (D->n_vars() > 1)
	      {
		if (D->compare(kb_exp_degree, kb_deg) == EQ)
		  insert();
	      }
	    else
	      {
		if (lo_degree != 0 || kb_exp_weight >= kb_exp_lo_weight)
		  insert();
	      }
	    
	    // When do we recurse further
	    // when the weight is strictly less than kb_exp_hi_weight
	    if (hi_degree == 0 || kb_exp_weight <= kb_exp_hi_weight)
	      k_basis0(i);
	  }
      }
      kb_exp[v]--;
      D->divide(kb_exp_degree, M->degree_of_var(v),
		   kb_exp_degree);
      kb_exp_weight -= var_wts[i];
    }
}

bool KBasis::compute()
    // Only the lead monomials of the two matrices 'this' and 'bottom' are
    // considered.  Thus, you must perform the required GB's elsewhere.
    // Find a basis for (image this)/(image bottom) in degree d.
    // If 'd' is NULL, first check that (image this)/(image bottom) has
    // finite dimension, and if so, return a basis.
    // If 'd' is not NULL, it is an element of the degree monoid.
{
  for (int i=0; i<bottom_matrix->n_rows(); i++)
    {
      kb_comp = i;
      // Make the monomial ideal: this should contain only
      // monomials involving 'vars'.
      kb_monideal = bottom_matrix->make_monideal(i);

      // Change kb_exp_degree, kb_exp_weight to reflect the degree
      // of this row.
      D->copy(bottom_matrix->rows()->degree(i), kb_exp_degree);
      kb_exp_weight = weight(kb_exp_degree);

      // If this degree is too large, either insert it or not,
      // depending on the truncation flag
      if (hi_degree && kb_exp_weight > kb_exp_hi_weight)
	{
	  if (kb_do_trunc)
	    insert();
	  continue;
	}
      // Do the recursion
      k_basis0(0);
    }

  return true;
}

MatrixOrNull *KBasis::k_basis(const Matrix *bottom, 
			      const M2_arrayint lo_degree,
			      const M2_arrayint hi_degree,
			      const M2_arrayint wt, 
			      const M2_arrayint vars,
			      bool do_truncation,
			      int limit)
{
  // Do some checks first, return 0 if not good.
#warning "do some sanity checks here"
  KBasis KB(bottom,lo_degree->array,hi_degree->array,wt,vars,do_truncation,limit);
  KB.compute();
  return KB.value();
}

const Matrix *Matrix::basis(const M2_arrayint lo_degree,
			    const M2_arrayint hi_degree,
			    const M2_arrayint wt, 
			    const M2_arrayint vars,
			    bool do_truncation,
			    int limit) const
{
  return KBasis::k_basis(this,lo_degree,hi_degree,wt,vars,do_truncation,limit);
}

/////// BELOW THIS LINE IS OLD ///////////////////////////
#if 0

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
#warning "kbasis: handle as flat monoid, coefficients, but put in var list"
  ring_elem tmp = P->make_flat_term(K->from_int(1), kb_mon);
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

bool KBasis::compute()
    // Only the lead monomials of the two matrices 'this' and 'bottom' are
    // considered.  Thus, you must perform the required GB's elsewhere.
    // first check that (image this)/(image bottom) has
    // finite dimension, and if so, return a basis.
{
  for (int i=0; i<bottom_matrix->n_rows(); i++)
    {
      kb_bottom = bottom_matrix->make_monideal(i);
      kb_component = i;

      for (int j=0; j<nvars; j++)
	kb_exp[j] = 0;

      // Check the degree...

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
#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
