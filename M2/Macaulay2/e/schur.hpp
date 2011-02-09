// Copyright 1996-2011 Michael E. Stillman

#ifndef _Schurring_hh_
#define _Schurring_hh_

#include <vector>
#include "poly.hpp"

const int SCHUR_MAX_WT = 100;
const int LARGE_NUMBER = 32000;

class tableau
{
  friend class SchurRing;
  friend class SchurRing2;
  friend class LittlewoodRicharsdon;

  int dim;
  int maxwt;
  int wt;
  int *lambda;			// A partition vector 0..nvars-1
  int *p;			// A partition vector 0..nvars-1
  int *xloc;			// array 1..|p|-|lambda| of the horizontal location
				// of this number in the skew table
  int *yloc;			// array 1..|p|-|lambda| of the vertical location
				// of this number in the skew table

  void initialize(int nvars);
  tableau() {}
  ~tableau() {}
  void resize(int max_wt);

  int elem(int x, int y) const;
  void fill(int *lamb, int *pp);
  void display() const;
};

typedef int schur_word;
typedef schur_word *schur_partition;
typedef const schur_word *const_schur_partition;

class schur_poly_iterator;;

class schur_poly : public our_new_delete {
  friend class SchurRing2;
  friend class schur_poly_iterator;
  VECTOR(ring_elem) coeffs;
  VECTOR(schur_word) monoms; // each monomial is a partition
  // of the form:  len a_1 a_2 ... a_len,
  // where a_1 >= a_2 >= ... >= a_len

public:
  typedef schur_poly_iterator iterator;

  iterator begin() const;
  iterator end() const;
  size_t size() const { return coeffs.size(); }

  void append(iterator &first, iterator &last);
  void appendTerm(ring_elem coeff, const_schur_partition monom);
};

class schur_poly_iterator {
  VECTOR(ring_elem)::const_iterator ic;
  VECTOR(schur_word)::const_iterator im;
  
  friend class schur_poly;
  
  schur_poly_iterator(const schur_poly &f) : ic(f.coeffs.begin()), im(f.monoms.begin()) {}
  schur_poly_iterator(const schur_poly &f, int) : ic(f.coeffs.end()), im(0) {}
public:
  void operator++() { ++ic; im += *im;  }
  ring_elem getCoefficient() { return *ic; }
  const_schur_partition getMonomial() { return &*im; }
  
  friend bool operator==(const schur_poly_iterator &a, const schur_poly_iterator &b);
  friend bool operator!=(const schur_poly_iterator &a, const schur_poly_iterator &b);
};


bool operator==(const schur_poly::iterator &a, const schur_poly::iterator &b);
bool operator!=(const schur_poly::iterator &a, const schur_poly::iterator &b);

inline schur_poly::iterator schur_poly::begin() const { return iterator(*this); }
inline schur_poly::iterator schur_poly::end() const { return iterator(*this,1); }

class LittlewoodRicharsdon
{
protected:
  int n_rows; // can be increased via resizing
  int max_weight;
  tableau _SMtab;
  tableau _SMfilled;
  int _SMcurrent;
  int _SMfinalwt;

  void bounds(int &lo, int &hi);
  void SM();

  virtual void append_term(const_schur_partition f) = 0;
public:
  LittlewoodRicharsdon() {}
  LittlewoodRicharsdon(int initial_max_weight);
  void skew_schur(int* lambda, int* p);
  void mult(int* a, int* b);
};

class LWSchur2 : public LittlewoodRicharsdon
{
  const Ring *coefficientRing;
  virtual void append_term(const_schur_partition f);
  // append this term to a poly heap
  
public:
  LWSchur2() {}
  LWSchur2(size_t initial_max_weight, const Ring *A0);

  schur_poly *skew_schur(const_schur_partition lambda, const_schur_partition p);
  schur_poly *mult(const_schur_partition a, const_schur_partition b);
};

class SchurRing2 : public Ring
{
private:
  // These are variables that are used in the recursive routine 
  // SM(), which is called from skew_schur().
  Nterm *_SMresult;

  void to_partition(const int *m, int *exp) const;
    // exp[1]..exp[nvars] are set
  void from_partition(const int *exp, int *m) const;

  void bounds(int &lo, int &hi);
  void SM();
  Nterm *skew_schur(int *lambda, int *p);
  ring_elem mult_monomials(const int *m, const int *n);

  const Ring *coefficientRing;
  LWSchur2 LR;
  int nvars; // max size of a partition, or -1 meaning infinity
protected:
  bool initialize_schur();
  SchurRing2() {}
  virtual ~SchurRing2() {}

  SchurRing2(const Ring *A, int n=-1);

  bool initialize_SchurRing2();
  int compare_partitions(const_schur_partition a, const_schur_partition b) const;
public:
  static SchurRing2 *create(const Ring *A, int n=-1);

  static SchurRing2 *createInfinite(const Ring *A);

  virtual const SchurRing2 * cast_to_SchurRing2() const { return this; }
  virtual       SchurRing2 * cast_to_SchurRing2()       { return this; }

  bool is_valid_partition(M2_arrayint part, bool set_error=true) const;
  // sets global error message by default

  ring_elem from_coeff(ring_elem a) const;
  ring_elem from_partition(M2_arrayint part) const;

  bool get_scalar(const schur_poly *f, ring_elem &result) const;

  void dimension(const int *exp, mpz_t result) const;
  ring_elem dimension(const ring_elem f) const;
  // only allowed if nvars >= 0

  // from Ring:
  virtual void text_out(buffer &o) const;
  virtual void elem_text_out(buffer &o, 
			     const ring_elem f, 
			     bool p_one=true, 
			     bool p_plus=false, 
			     bool p_parens=false) const;

  virtual ring_elem from_int(int n) const;
  virtual ring_elem from_int(mpz_ptr n) const;

  virtual ring_elem from_rational(mpq_ptr q) const;

  virtual bool promote(const Ring *R, const ring_elem f, ring_elem &result) const;
  virtual bool lift(const Ring *R, const ring_elem f, ring_elem &result) const;

  virtual bool is_unit(const ring_elem f) const;
  virtual bool is_zero(const ring_elem f) const;
  virtual bool is_equal(const ring_elem f, const ring_elem g) const; // NOT DONE...

  virtual int compare_elems(const ring_elem f, const ring_elem g) const;
  virtual ring_elem copy(const ring_elem f) const;
  virtual void remove(ring_elem &f) const { }  // let the GC do it!

  virtual ring_elem negate(const ring_elem f) const;
  virtual ring_elem add(const ring_elem f, const ring_elem g) const;
  virtual ring_elem subtract(const ring_elem f, const ring_elem g) const;
  virtual ring_elem mult(const ring_elem f, const ring_elem g) const;

  virtual ring_elem invert(const ring_elem f) const; // do nothing
  virtual ring_elem divide(const ring_elem f, const ring_elem g) const; // do nothing

  virtual void syzygy(const ring_elem a, const ring_elem b,
		      ring_elem &x, ring_elem &y) const; // do nothing

  virtual ring_elem eval(const RingMap *map, const ring_elem f, int first_var) const;

};

class SchurRing : public PolyRing
{
private:
  // These are variables that are used in the recursive routine 
  // SM(), which is called from skew_schur().
  tableau _SMtab;
  tableau _SMfilled;
  int _SMcurrent;
  int _SMfinalwt;
  Nterm *_SMresult;

  void to_partition(const int *m, int *exp) const;
    // exp[1]..exp[nvars] are set
  void from_partition(const int *exp, int *m) const;

  void bounds(int &lo, int &hi);
  void SM();
  Nterm *skew_schur(int *lambda, int *p);
  ring_elem mult_monomials(const int *m, const int *n);

protected:
  bool initialize_schur();
  SchurRing() {}
  virtual ~SchurRing() {}

public:
  static SchurRing *create(const PolynomialRing *R);

  static SchurRing *create(const Ring *A, int n);

  static SchurRing *createInfinite(const Ring *A);


  virtual const SchurRing * cast_to_SchurRing() const { return this; }
  virtual       SchurRing * cast_to_SchurRing()       { return this; }

  virtual void text_out(buffer &o) const;
  virtual void elem_text_out(buffer &o, 
			     const ring_elem f, 
			     bool p_one=true, 
			     bool p_plus=false, 
			     bool p_parens=false) const;

  void dimension(const int *exp, mpz_t result) const;
  ring_elem dimension(const ring_elem f) const;

  virtual ring_elem mult_by_term(const ring_elem f, 
				  const ring_elem c, const int *m) const;

  ring_elem power(const ring_elem f, mpz_t n) const;
  ring_elem power(const ring_elem f, int n) const;
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
