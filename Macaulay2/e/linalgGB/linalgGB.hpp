/* Copyright 2005, Michael E. Stillman */

#ifndef _linalgGB_h_
#define _linalgGB_h_

#include "lingb.hpp"

template<typename CoeffRing>
class LinAlgGB : public GBComputation
{
  typedef typename CoeffRing::ring_type RingType;
  typedef typename CoeffRing::elem elem;

  typedef typename CoeffRing::elem COEFF_TYPE;
  typedef mygbelem<COEFF_TYPE> gbelem;
  typedef gb_array<COEFF_TYPE> gb_array;
  typedef mypoly<COEFF_TYPE> poly;
  typedef row_elem<COEFF_TYPE> row_elem;
  typedef coefficient_matrix<COEFF_TYPE> coefficient_matrix;

  const PolynomialRing *originalR;
  const FreeModule *F; // determines whether the monomial order is a 
		       // Schreyer order.
                       // Also determines degrees of elements in F.
  M2_arrayint weights;
  const RingType *K;
  CoeffRing *coeffK;

  int n_subring; // number of GB elements in the first subring
  int n_pairs_computed;
  int n_gens_left;
  int complete_thru_this_degree;

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
private:
  gbelem *make_gbelem(poly &g);
  void allocate_poly(poly &result, size_t len);
  void allocate_sparse_row(row_elem &result, size_t len);
  int find_or_append_column(monomial m);
  void sparse_row_to_poly(row_elem &r, poly &g);
  void insert_gb_element(poly &g);

  void load_gen(int which);
  void load_row(monomial monom, int which);

  void process_column(int c);
    /* If this column has been handled before, return.
       Otherwise, find a GB element whose lead term
       divides this monomial, and either mark this colum
       as not an initial element, OR append a row
    */

  void process_s_pair(SPairSet::spair *p);

  void make_matrix();
    /* loop through all spairs, process,
       then while there are any columns to process, do so,
       then process rows.
       Is this the best order to do it in?  Maybe not...
    */

  // Helper routines for LU_decompose/make_matrix
  void reorder_columns(); 
  void reorder_rows(); 
  void sparse_to_dense_row(elem *v, int r);
  void dense_to_sparse_row(int r, elem *v);
  void reduce_row(long r, long first, long last);
  void tail_reduce_row(long r, long first, long last);
  void normalize_row(long r);
  void gauss();

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

  LinAlgGB(const RingType *K,
	   const Matrix *m, 
	   M2_bool collect_syz, 
	   int n_rows_to_keep,
	   M2_arrayint gb_weights,
	   int strategy, 
	   M2_bool use_max_degree,
	   int max_degree);

  virtual ~LinAlgGB();

  virtual void remove_gb() {}

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

#endif


// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
