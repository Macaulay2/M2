/* Copyright 2005, Michael E. Stillman */

#ifndef _linalgGB_h_
#define _linalgGB_h_

#include "lingb.hpp"

template<typename CoefficientRing>
class LinAlgGB : public GBComputation
{
  typedef typename CoefficientRing::elem COEFF_TYPE;
  typedef mygbelem<COEFF_TYPE> gbelem;
  typedef mygb_array<COEFF_TYPE> gb_array;
  typedef mypoly<COEFF_TYPE> poly;
  typedef row_elem<COEFF_TYPE> row_elem;
  typedef coefficient_matrix<COEFF_TYPE> coefficient_matrix;

  const PolynomialRing *originalR;
  const FreeModule *F; // determines whether the monomial order is a Schreyer order
                       // Also determines degrees of elements in F.
  M2_arrayint weights;
  CoefficientRing *coeffK;

  int n_subring; // number of GB elements in the first subring
  int n_pairs_computed;
  int n_gens_left;

  MonomialSet H; // Hash table of monomials in the ring
  SPairSet S;
  MonomialLookupTable *lookup; // This should be the monomial ideal type
  int this_degree;

  // Need: lookup table for monomials in the GB
  // Need: Hash table for (monom,int) --> column index
  // Need: allocator for vectors in the matrix
  // Need: GB itself.

  gb_array gens;
  gb_array gb;

  // The matrix
  coefficient_matrix *mat;
  int next_col_to_process;
  int next_row_to_process;
private:
  gbelem *make_gbelem(poly &g);
  void allocate_poly(poly &result, size_t len);
  void allocate_sparse_row(row_elem &result, size_t len);
  int column(monomial m);
  void sparse_row_to_poly(row_elem &r, poly &g);
  void insert_gb_element(poly &g);

  void append_row(monomial m, int gbelem);
  // Make a new row.  Should we also do
  // process_row (and therefore process_vector)  here?

  void process_row(int r);
  // Do the multiplication, and process the 
  // resulting vector.
  //   ALSO: this is where we improve row[r] = (m,gbelem)...
  //   search for any (t,newelem)'s for which t divides m have been done in a prev matrix.
  //   choose the largest t.  now process (m/t, newelem):
  //   special multiplication routine?  since newelem uses diff monomial encoding.

  void process_column(int c);
    /* If this column has been handled before, return.
       Otherwise, find a GB element whose lead term
       divides this monomial, and either mark this colum
       as not an initial element, OR append a row
    */

  void append_column(monomial m);
    /* Make a new column, insert it. */

  void process_s_pair(SPairSet::spair *p);
    /*
      3 cases:
      (a) an spair or ringpair
      find the lcm.
      do append_column for this monomial
      do 2 append_row's
      (b) a skew pair
      find the lcm
      do process_vector (?)
      (c) a generator
      either append_row, or process_vector
    */

  void set_comparisons();
    /* Sort the monomials */
    /* set the comparison values in the current matrix */
    /* Should we also go thru the matrix and set the values? */

  void make_matrix();
    /* loop through all spairs, process,
       then while there are any columns to process, do so,
       then process rows.
       Is this the best order to do it in?  Maybe not...
    */

  void LU_decompose();

  void new_GB_elements();
    /* After LU decomposition, loop through each
       row of the matrix.  If the corresponding 
       lead term is not in the initial ideal (or, at least,
       wasn't) then insert GB element (and so update spairs, etc,
       but don't do auto_reduce...)
       
       If instead the lead term is not new, then keep track of this
       information somehow: place ... into a monheap...
    */

  void s_pair_step();
  /* make_matrix, the LU_decompose, then grab GB elements */

  void show_gb_array(const gb_array &g);
  void show_row_info();
  void show_column_info();
  void show_matrix();

public:
  enum ComputationStatusCode computation_is_complete();
  virtual bool stop_conditions_ok() { return true; }

  LinAlgGB(CoefficientRing *K,
	   const Matrix *m, 
	   M2_bool collect_syz, 
	   int n_rows_to_keep,
	   M2_arrayint gb_weights,
	   int strategy, 
	   M2_bool use_max_degree,
	   int max_degree);

  virtual ~LinAlgGB();

  virtual int kind() { return 231; } // FIX THIS!!

  void start_computation();

  virtual const PolynomialRing *get_ring() { return originalR; }

  virtual ComputationOrNull *set_hilbert_function(const RingElement *h);

  virtual const MatrixOrNull *get_gb();

  virtual const MatrixOrNull *get_mingens();

  virtual const MatrixOrNull *get_change();

  virtual const MatrixOrNull *get_syzygies();

  virtual const MatrixOrNull *get_initial(int nparts);

  virtual const MatrixOrNull *matrix_remainder(const Matrix *m);

  virtual void matrix_lift(const Matrix *m,
			   MatrixOrNull **result_remainder,
			   MatrixOrNull **result_quotient
			   );

  virtual int contains(const Matrix *m);

  virtual void text_out(buffer &o); 
  /* This displays statistical information, and depends on the
     gbTrace value */

  virtual int complete_thru_degree() const;
  // The computation is complete up through this degree.
};

#if 0
#include "../comp_gb.hpp"
#include "interface.h"
#include "SPairSet.h"
#include "MonomialTable.h"
#include <map>


struct sparse_row : public our_new_delete {
  int len;
  COEFF_TYPE *coeffs;
  int *comps; // points into either S, or into a set Sd
  
  // One must be careful about adding two polynomials, since we
  // need to have an order on the monomials in this case
};

class CoefficientRing : public our_new_delete {
  const Ring *K;
public:
  CoefficientRing(const Ring *K0) : K(K0) {}
  void init_set(COEFF_TYPE *result, COEFF_TYPE *a) const { *result = *a; }
};

template<typename CoeffRing>
class CoefficientMatrix : public our_new_delete {
  typedef std::map<monomial, int> monomial_map;
  
  struct row_elem : public our_new_delete {
    monomial monom;
    int elem;
    sparse_row row;
  };

  struct column_elem : public our_new_delete {
    monomial monom;
    int comp;
    int gb_divisor; // -1 if none, otherwise >= 0.
    int ord; // Set before doing LU decomposition
  };

  M2_arrayint weights;

  const CoeffRing *coeffK;
  MonomialSet H; // Hash table of monomials in the ring
  monomial_map H0; // Hash table (well...  sort of) of
                         // monomial --> int

  std::vector<row_elem, gc_allocator<row_elem> > rows;
  std::vector<column_elem, gc_allocator<column_elem> > columns;
};

class LinearAlgebraGB : public GBComputation {
public:

  struct row_elem : public our_new_delete {
    monomial monom;
    int elem;
    sparse_row row;
  };

  struct column_elem : public our_new_delete {
    monomial monom;
    int comp;
    int gb_divisor; // -1 if none, otherwise >= 0.
    int ord; // Set before doing LU decomposition
  };

private:
  typedef std::map<monomial, int> monomial_map;

  const PolynomialRing *originalR;
  const FreeModule *F; // determines whether the monomial order is a Schreyer order
                       // Also determines degrees of elements in F.
  M2_arrayint weights;

  const CoefficientRing *coeffK;
  int n_subring; // number of GB elements in the first subring
  int n_pairs_computed;
  int n_gens_left;

  MonomialSet H; // Hash table of monomials in the ring
  monomial_map H0; // Hash table (well...  sort of) of
                         // monomial --> int
  SPairSet S;

  MonomialLookupTable *lookup; // This should be the monomial ideal type
  int this_degree;
#if 0
  const MonomialTable *ringtable;    // At most one of these two will be non-NULL.
  const MonomialTableZZ *ringtableZZ;
  MonomialTable *lookup; // This should be the monomial ideal type
  MonomialTableZZ *lookupZZ; // Only one of these two will be non-NULL.
#endif
  // Need: lookup table for monomials in the GB
  // Need: Hash table for (monom,int) --> column index
  // Need: allocator for vectors in the matrix
  // Need: GB itself.

  gb_array gens;
  gb_array gb;

  // The matrix
  std::vector<row_elem, gc_allocator<row_elem> > rows;
  std::vector<column_elem, gc_allocator<column_elem> > columns;

  int next_col_to_process;
  int next_row_to_process;

  gbelem *LinearAlgebraGB::make_gbelem(poly &g);
  void allocate_poly(poly &result, size_t len);
  void allocate_sparse_row(sparse_row &result, size_t len);
  int mult_monomials(monomial m, monomial n);
  int column(monomial m);
  void sparse_row_to_poly(sparse_row &r, poly &g);
  void insert_gb_element(poly &g);

  void append_row(monomial m, int gbelem);
  // Make a new row.  Should we also do
  // process_row (and therefore process_vector)  here?

  void process_row(int r);
  // Do the multiplication, and process the 
  // resulting vector.
  //   ALSO: this is where we improve row[r] = (m,gbelem)...
  //   search for any (t,newelem)'s for which t divides m have been done in a prev matrix.
  //   choose the largest t.  now process (m/t, newelem):
  //   special multiplication routine?  since newelem uses diff monomial encoding.

  void process_column(int c);
    /* If this column has been handled before, return.
       Otherwise, find a GB element whose lead term
       divides this monomial, and either mark this colum
       as not an initial element, OR append a row
    */

  void append_column(monomial m);
    /* Make a new column, insert it. */

  void process_s_pair(SPairSet::spair *p);
    /*
      3 cases:
      (a) an spair or ringpair
      find the lcm.
      do append_column for this monomial
      do 2 append_row's
      (b) a skew pair
      find the lcm
      do process_vector (?)
      (c) a generator
      either append_row, or process_vector
    */

  void set_comparisons();
    /* Sort the monomials */
    /* set the comparison values in the current matrix */
    /* Should we also go thru the matrix and set the values? */

  void make_matrix();
    /* loop through all spairs, process,
       then while there are any columns to process, do so,
       then process rows.
       Is this the best order to do it in?  Maybe not...
    */

  void LU_decompose();

  void new_GB_elements();
    /* After LU decomposition, loop through each
       row of the matrix.  If the corresponding 
       lead term is not in the initial ideal (or, at least,
       wasn't) then insert GB element (and so update spairs, etc,
       but don't do auto_reduce...)
       
       If instead the lead term is not new, then keep track of this
       information somehow: place ... into a monheap...
    */

  void s_pair_step();
  /* make_matrix, the LU_decompose, then grab GB elements */

private:
  enum ComputationStatusCode computation_is_complete();
  virtual bool stop_conditions_ok() { return true; }

  LinearAlgebraGB(const Matrix *m, 
		  M2_bool collect_syz, 
		  int n_rows_to_keep,
		  M2_arrayint gb_weights,
		  int strategy, 
		  M2_bool use_max_degree,
		  int max_degree);

public:
  //////////////////////////
  // Computation routines //
  //////////////////////////

  static LinearAlgebraGB * create(const Matrix *m, 
				  M2_bool collect_syz, 
				  int n_rows_to_keep,
				  M2_arrayint gb_weights,
				  int strategy, 
				  M2_bool use_max_degree,
				  int max_degree);

  virtual ~LinearAlgebraGB();

  virtual int kind() { return 231; } // FIX THIS!!

  void start_computation();

  virtual const PolynomialRing *get_ring() { return originalR; }

  virtual ComputationOrNull *set_hilbert_function(const RingElement *h);

  virtual const MatrixOrNull *get_gb();

  virtual const MatrixOrNull *get_mingens();

  virtual const MatrixOrNull *get_change();

  virtual const MatrixOrNull *get_syzygies();

  virtual const MatrixOrNull *get_initial(int nparts);

  virtual const MatrixOrNull *matrix_remainder(const Matrix *m);

  virtual void matrix_lift(const Matrix *m,
			   MatrixOrNull **result_remainder,
			   MatrixOrNull **result_quotient
			   );

  virtual int contains(const Matrix *m);

  virtual void text_out(buffer &o); 
  /* This displays statistical information, and depends on the
     gbTrace value */

  virtual int complete_thru_degree() const;
  // The computation is complete up through this degree.

  void LinearAlgebraGB::show_gb_array(const gb_array &g);
  void LinearAlgebraGB::show_row_info();
  void LinearAlgebraGB::show_column_info();
  void LinearAlgebraGB::show_matrix();
};  

class MutableMatrix;

MutableMatrix * to_M2_MutableMatrix(  
    const Ring *K,
    std::vector<LinearAlgebraGB::row_elem, gc_allocator<LinearAlgebraGB::row_elem> > &rows,
    std::vector<LinearAlgebraGB::column_elem, gc_allocator<LinearAlgebraGB::column_elem> > &columns
    );
#endif
#endif


// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
