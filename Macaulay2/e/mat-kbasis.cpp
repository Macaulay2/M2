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
  int * kb_target_degree; // Only set or used for multi-degrees.
  
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

  kb_exp_degree = D->make_one();
  kb_exp_weight = 0;

  if (lo_degree != 0) kb_exp_lo_weight = weight(lo_degree);
  if (hi_degree != 0) kb_exp_hi_weight = weight(hi_degree);

  kb_mon = M->make_one();

  mat = MatrixConstructor(bottom->rows(), 0, false);

  if (D->n_vars() > 1)
    {
      assert(lo_degree);
      kb_target_degree = D->make_one();
      D->from_expvector(lo_degree, kb_target_degree);
    }
  else
    kb_target_degree = 0;
}

int KBasis::weight(const int *deg) const
{
  int *exp = newarray(int,D->n_vars());
  D->to_expvector(deg, exp);
  int sum = 0;
  for (int i=0; i<wt_vector->len; i++)
    sum += wt_vector->array[i] * exp[i];
  deletearray(exp);
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
  bool do_insert = false;
  Bag *b;

  if (hi_degree && kb_exp_weight > kb_exp_hi_weight)
    {
      if (!do_truncation) return;
      do_insert = true; // unless the following search_expvector fails
    }
  if (kb_monideal->search_expvector(kb_exp,b)) return;
  if (D->n_vars() > 1)
    {
      if (D->compare(kb_exp_degree, kb_target_degree) == EQ)
	do_insert = true;
    }
  else
    {
      if (!lo_degree || kb_exp_weight >= kb_exp_lo_weight)
	do_insert = true;
    }

  if (do_insert)
    {
      insert();
      if (limit == 0) return;	    
    }

  if (hi_degree && kb_exp_weight >= kb_exp_hi_weight)
    return;  // Do not recurse

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


      k_basis0(i);

      kb_exp[v]--;
      D->divide(kb_exp_degree, M->degree_of_var(v),
		   kb_exp_degree);
      kb_exp_weight -= var_wts[i];

      if (limit == 0) return;
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
  const PolynomialRing *P = bottom->get_ring()->cast_to_PolynomialRing();
  if (P == 0)
    {
      ERROR("expected polynomial ring");
      return 0;
    }
  const PolynomialRing *D = P->get_degree_ring();
  // cases:
  //   (1) neither lo_degree nor hi_degree is given
  //   (2) only one is given
  //       then: this cannot be a multidegree
  //   (3) both are given
  //       if multidegree: they must be the same, truncation must be false.
  // 
  const int *lo = (lo_degree->len > 0 ? lo_degree->array : 0);
  const int *hi = (hi_degree->len > 0 ? hi_degree->array : 0);

  if ((lo != 0 && lo_degree->len != D->n_vars())
      || (hi != 0 && hi_degree->len != D->n_vars()))
    {
      ERROR("expected degrees of length %d", D->n_vars());
    }

  if (D->n_vars() > 1 && (lo != 0 || hi != 0))
    {
      if (lo == 0 || hi == 0)
	{
	  ERROR("expected low and high degrees to be specified");
	  return 0;
	}
      for (int i=0; i<D->n_vars(); i++)
	if (lo[i] != hi[i])
	  {
	    ERROR("expected low and high degrees to be the same");
	    return 0;
	  }
      if (do_truncation)
	{
	  ERROR("cannot do truncation for multigraded rings");
	}
    }

  KBasis KB(bottom,lo,hi,wt,vars,do_truncation,limit);

  for (int i=0; i<vars->len; i++)
    if (KB.var_wts[i] <= 0)
      {
	ERROR("expected given degree vector to produce positive weights");
	return 0;
      }
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
