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

  MonomialIdeal* kb_monideal;
  int          * kb_exp;
  int          * kb_mon;

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

  void compute();

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
  P = bottom->get_ring()->cast_to_PolynomialRing();
  M = P->getMonoid();
  D = P->get_degree_ring()->getMonoid();
  
  // Compute the (positive) weights of each of the variables in 'vars'.

  var_wts = newarray(int, vars->len);
  for (int i=0; i<vars->len; i++)
    {
      var_wts[i] = weight(M->degree_of_var(vars->array[i]));
    }

  // Set the recursion variables
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

  kb_mon = M->make_one();

  mat = MatrixConstructor(bottom->rows(), 0, false);
}

int KBasis::weight(const int *deg) const
{
  int sum = 0;
  for (int i=0; i<wt_vector->len; i++)
    sum += wt_vector->array[i] * deg[i];
  return sum;
}

#warning "add limits"
#warning "be careful about monideals over ZZ or over non-fields"
#warning "is the initial degree set up correctly?"

void KBasis::insert()
{
  // We have a new basis element

  M->from_expvector(kb_exp, kb_mon);
  ring_elem r = P->make_flat_term(P->getCoefficients()->one(), kb_mon);
  vec v = P->make_vec(kb_comp, r);
  mat.append(v);
  if (limit > 0) limit--;
}

void KBasis::k_basis0(int firstvar)
    // Recursively add to the result matrix all monomials in the
    // variables 0..topvar having degree 'deg' which are not in 'mi'.
{
  // First: decide whether to insert this element

  // Second: decide whether to recurse


  for (int i=firstvar; i<vars->len; i++)
    {
      int v = vars->array[i];
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
      if (hi_degree // this means that kb_exp_hi_weight will be set
	  && kb_exp_weight > kb_exp_hi_weight
	  && do_truncation 
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
		if (D->compare(kb_exp_degree, lo_degree) == EQ) // lo_degree == hi_degree
		  insert();
	      }
	    else
	      {
		if (!lo_degree || kb_exp_weight >= kb_exp_lo_weight)
		  insert();
	      }
	    if (limit == 0) return;	    
	    // When do we recurse further
	    // when the weight is strictly less than kb_exp_hi_weight
	    if (hi_degree == 0 || kb_exp_weight <= kb_exp_hi_weight)
	      k_basis0(i);
	  }
      }
      if (limit == 0) return;
      kb_exp[v]--;
      D->divide(kb_exp_degree, M->degree_of_var(v),
		   kb_exp_degree);
      kb_exp_weight -= var_wts[i];
    }
}

void KBasis::compute()
    // Only the lead monomials of the two matrices 'this' and 'bottom' are
    // considered.  Thus, you must perform the required GB's elsewhere.
    // Find a basis for (image this)/(image bottom) in degree d.
    // If 'd' is NULL, first check that (image this)/(image bottom) has
    // finite dimension, and if so, return a basis.
    // If 'd' is not NULL, it is an element of the degree monoid.
{
  if (limit == 0) return;
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
	  if (do_truncation)
	    insert();
	  continue;
	}
      // Do the recursion
      k_basis0(0);
    }
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
  const int *lo = (lo_degree->len > 0 ? lo_degree->array : 0);
  const int *hi = (hi_degree->len > 0 ? hi_degree->array : 0);
  KBasis KB(bottom,lo,hi,wt,vars,do_truncation,limit);
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

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
