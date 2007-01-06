#include "polyring.hpp"
#include "matrix.hpp"
#include "matrixcon.hpp"
#include "ntuple.hpp"

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
  M2_arrayint wt_vector; // length is D->n_vars(), or less.
  // Dot product of a degree of a variable
  // in 'vars' will give a positive value.

  int * var_wts; // var_wts[i] is the weight of the vars->array[i] th variable
  M2_arrayint vars;
  bool do_truncation;
  int limit; // if >= 0, then stop after that number.

  const int * lo_degree; // if non-null, the lowest degree to collect
  const int * hi_degree; // if non-null, the highest degree to collect
  // In the singly graded case: collect every monomial whose weight lies >= weight of
  // lo_degree (kb_target_lo_weight), and <= weight of hi_degree (kb_target_hi_weight).
  // (resp -infty, infty, if lo_degree resp hi_degree is null).
  //
  // in multi-graded case, we can only collect one degree, or the entire module.
  // so: lo_degree and hi_degree must be the same (null, or same degree vector).

  int * kb_exp; // exponent vector being constructed recursively
  int kb_exp_weight; // weight of this exponent vector  

  int kb_target_lo_weight; // only valid if lo_degree is not null 
  int kb_target_hi_weight; // only valid if hi_degree is not null

  int * kb_target_multidegree; // in multigraded case this is not null, and is the
                              // degree vector which is our target.
  int * kb_exp_multidegree; // only used when kb_target_multidegree is non-null

  int kb_comp;

  int          * kb_mon;
  
  MonomialIdeal* kb_monideal;

  int weight_of_monomial(const int *deg) const; // deg is an encoded degree monomials
  int weight(const int *exp) const; // exp is an exponent vector
  void insert();
  void k_basis0(int firstvar);

  KBasis(const Matrix *bottom, 
	 const int * lo_degree,
	 const int * hi_degree,
	 M2_arrayint wt, 
	 M2_arrayint vars,
	 bool do_truncation,
	 int limit);

  ~KBasis() {}

  void compute();

  Matrix *value() { return mat.to_matrix(); }
public:
  static Matrix *k_basis(const Matrix *bottom, 
			 M2_arrayint lo_degree,
			 M2_arrayint hi_degree,
			 M2_arrayint wt, 
			 M2_arrayint vars,
			 bool do_truncation,
			 int limit);
};

KBasis::KBasis(const Matrix *bottom, 
	       const int * lo_degree0,
	       const int * hi_degree0,
	       M2_arrayint wt, 
	       M2_arrayint vars0,
	       bool do_truncation0,
	       int limit0)
  : bottom_matrix(bottom),
    wt_vector(wt),
    vars(vars0),
    do_truncation(do_truncation0),
    limit(limit0),
    lo_degree(lo_degree0),
    hi_degree(hi_degree0)
{
  P = bottom->get_ring()->cast_to_PolynomialRing();
  M = P->getMonoid();
  D = P->get_degree_ring()->getMonoid();
  
  // Compute the (positive) weights of each of the variables in 'vars'.

  var_wts = newarray_atomic(int, vars->len);
  for (int i=0; i<vars->len; i++)
    {
      var_wts[i] = weight_of_monomial(M->degree_of_var(vars->array[i]));
    }

  // Set the recursion variables
  kb_exp = newarray_atomic_clear(int, P->n_vars());
  kb_exp_weight = 0;

  if (lo_degree != 0) kb_target_lo_weight = weight(lo_degree);
  if (hi_degree != 0) kb_target_hi_weight = weight(hi_degree);

  kb_mon = M->make_one();

  mat = MatrixConstructor(bottom->rows(), 0);

  if (D->n_vars() > 1 && lo_degree)
    {
      kb_target_multidegree = D->make_one();
      D->from_expvector(lo_degree, kb_target_multidegree);
      kb_exp_multidegree = D->make_one();
    }
  else
    {
      kb_target_multidegree = 0;
      kb_exp_multidegree = 0;
    }
}

int KBasis::weight_of_monomial(const int *deg) const
{
  int *exp = newarray_atomic(int,D->n_vars());
  D->to_expvector(deg, exp);
  int result = ntuple::weight(wt_vector->len, exp, wt_vector);
  deletearray(exp);
  return result;
}

int KBasis::weight(const int *exp) const
{
  return ntuple::weight(wt_vector->len, exp, wt_vector);
}

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
  bool do_recurse;
  Bag *b;

  if (hi_degree && kb_exp_weight >= kb_target_hi_weight)
    {
      if (kb_exp_weight > kb_target_hi_weight)
	{
	  if (!do_truncation) return;
	  do_insert = true;
	}
      do_recurse = false;
    }
  else
    do_recurse = true;

  if (kb_monideal->search_expvector(kb_exp,b)) return;

  if (kb_target_multidegree)
    {
      if (kb_exp_weight < kb_target_lo_weight)
	{
	  do_insert = false;
	}
      else 
	{
	  // We have the same weight.  The case when kb_exp_weight > kb_target_lo_weight
	  // should not get to this point.
	  do_insert = (D->compare(kb_target_multidegree, kb_exp_multidegree) == EQ);
	}
    }
  else
    {
      if (!lo_degree || kb_exp_weight >= kb_target_lo_weight)
	do_insert = true;
    }

  if (do_insert)
    {
      insert();
      if (limit == 0) return;	    
    }

  if (!do_recurse) return;

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
      kb_exp_weight += var_wts[i];
      if (kb_target_multidegree)
	D->mult(kb_exp_multidegree, 
		M->degree_of_var(v),
		kb_exp_multidegree);
      
      k_basis0(i);

      kb_exp[v]--;
      kb_exp_weight -= var_wts[i];
      if (kb_target_multidegree)
	D->divide(kb_exp_multidegree, 
		  M->degree_of_var(v),
		  kb_exp_multidegree);

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

      const int *component_degree = bottom_matrix->rows()->degree(i);
      kb_exp_weight = weight_of_monomial(component_degree);

      if (kb_target_multidegree)
	D->copy(component_degree, kb_exp_multidegree);

      // Do the recursion
      k_basis0(0);
    }
}

MatrixOrNull *KBasis::k_basis(const Matrix *bottom, 
			      M2_arrayint lo_degree,
			      M2_arrayint hi_degree,
			      M2_arrayint wt, 
			      M2_arrayint vars,
			      bool do_truncation,
			      int limit)
{
  // Do some checks first, return 0 if not good.
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
	ERROR("basis: non-positive heft form encountered (consider using Heft option)");
	return 0;
      }
  KB.compute();
  return KB.value();
}

const Matrix *Matrix::basis(M2_arrayint lo_degree,
			    M2_arrayint hi_degree,
			    M2_arrayint wt, 
			    M2_arrayint vars,
			    bool do_truncation,
			    int limit) const
{
  return KBasis::k_basis(this,lo_degree,hi_degree,wt,vars,do_truncation,limit);
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
