#ifndef __gbasis_hpp_
#define __gbasis_hpp_

#include "gbring.hpp"
#include "montable.hpp"

/* Types of minimality */
#if 0
enum gbelem_type { 
  ELEM_IN_STONE = -1,  /* These are ring elements, or do not count towards mingens */
  ELEM_TRIMMED = 0,
  ELEM_MIN_GB = 1,
  ELEM_NON_MIN_GB = 2
};
#endif
enum gbelem_type { 
  ELEM_IN_STONE,  // These are ring elements
  ELEM_TRIMMED,   // These are min GB elements which might also be min gens
  ELEM_MIN_GB,    // These are elements which are minimal GB elements
  ELEM_NON_MIN_GB // These are elements which are not minimal GB elements
};

struct POLY {
  gbvector *f;
  gbvector *fsyz;
};

class RingGBasis;

class GBasis
// This handles a GB (or "declared" GB), where the base ring
// is a field (i.e. the ring contains a field).
// The case where the base is globalZZ is handled in GBasisZZ
{
public:
  struct gbelem {
    POLY g;
    int deg;
    int alpha; // the homogenizing degree
    exponents lead; // -1..nvars-1, the -1 part is the component
    gbelem_type minlevel;
  };

  GBasis(const FreeModule *F, const FreeModule *Fsyz);
  // Grabs originalR,R,lookup from F.
  // sets gb, first_gb_index.

  ~GBasis() {}

  static gbelem *gbelem_make(const FreeModule *F,
			     gbvector *f,  // grabs f
			     gbvector *fsyz, // grabs fsyz
			     gbelem_type minlevel,
			     int deg);

  int insert(gbvector *f, gbvector *fsyz, gbelem_type minlevel, int deg);
    // returns integer index of this inserted element

  void poly_auto_reduce(vector<POLY> &mat);

  void minimalize_gb();

  int find_good_divisor(exponents e,
			int x,
			int degf, 
			int &result_alpha);

  void remainder(POLY &f, int degf);

  void remainder(POLY &f, int degf, ring_elem &denom);
  // denom must start out as an element of the base R->get_flattened_coefficients().
  // denom is multiplied by the coefficient which is multiplied to f in order to
  // reduce f.
  // i.e. the result g satisfies: g = c*f mod GB, where new(denom) = c * old(denom).

public:
  GBasis *make_minimal_gb();

  RingGBasis *make_ring_gb();

  const Matrix *get_gb();

  const Matrix *get_minimal_gb();

  const Matrix *get_minimal_gens();

  const Matrix *get_change();

  const Matrix *get_leadterms(int n);

  const FreeModule *get_free(M2_bool minimal);

  // Division algorithm routines 

  const MatrixOrNull *division(const Matrix *m);

  const MatrixOrNull *lift(const Matrix *m, MatrixOrNull * &result_quotient);

  const MatrixOrNull *matrix_remainder(const Matrix *m);

  void matrix_lift(const Matrix *m,
		   MatrixOrNull **result_remainder,
		   MatrixOrNull **result_quotient);

  int contains(const Matrix *m);

  // Routines:
  //   create
  //   division (several versions)
  //   minimalize
  //   sort
  //   auto_reduce
  //   make_RingGBasis
  //     [steps: first, make a RingGBasis, copy all the
  //       (minimal) elements to it, changing component to 0.
  //       sort it, auto_reduce it, and then set ringtable.
  //       (care must be taken in local case during auto reduction.)
private:
  const PolynomialRing *originalR; // points to a RingGBasis, in quotient case
  GBRing *R;
  int nvars;
  const FreeModule *F;  // MES: can we combine F,Fsyz?
  const FreeModule *Fsyz;

  const MonomialTable *ringtable; // Set from originalR.
  MonomialTable *lookup;
public:
  vector<gbelem *> gb; // Contains any quotient ring elements
private:
  int first_gb_element; // indices 0..first_gb_element-1 in 'gb'
			// refer to quotient ring elements
  vector<POLY> minimal_gb; // Contains NO quotient ring elements
  bool minimal_gb_valid;

  exponents _EXP; // Used in 'remainder'
};

class RingGBasis
{
  GBRing *R;
  FreeModule *R1; // Should be rank 1, degree 0 generator.
  MonomialTable *ringtable;
  vector<GBasis::gbelem *> gb;
  RingGBasis() {}

  GBasis::gbelem *gbelem_make(gbvector *f);
public:
  static RingGBasis *make(GBRing *R, vector<gbvector *> &elems);

  static RingGBasis *make_RingGBasis(const GBasis *G);
  // G should be a GB with _F having rank 1.
  // This minimalizes G (including quotient ring elems, if any).

  void normal_form(FreeModule *F, gbvector *&v, ring_elem &denom);
  // reduce v/denom, the answer is the new v/denom.

  ring_elem normal_form(ring_elem f);

  vec normal_form(FreeModule *F, vec v);

  void set_quotient(vector <GBasis::gbelem *> &gb);
};
#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
