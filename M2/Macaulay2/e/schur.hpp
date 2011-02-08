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

typedef unsigned int *schur_partition;
typedef const unsigned int *const_schur_partition;

class schur_poly : public our_new_delete {
  friend class SchurRing2;

  VECTOR(ring_elem) coeffs;
  VECTOR(unsigned int) monoms; // each monomial is a partition
  // of the form:  len a_1 a_2 ... a_len,
  // where a_1 >= a_2 >= ... >= a_len

public:
  class iterator {
    VECTOR(ring_elem)::const_iterator ic;
    VECTOR(unsigned int)::const_iterator im;

    friend class schur_poly;

    iterator(const schur_poly &f) : ic(f.coeffs.begin()), im(f.monoms.begin()) {}
    iterator(const schur_poly &f, int) : ic(f.coeffs.end()), im(0) {}
  public:
    void operator++() { ++ic; im += *im;  }
    ring_elem getCoefficient() { return *ic; }
    const_schur_partition getMonomial() { return &*im; }

    friend bool operator==(const iterator &a, const iterator &b);
    friend bool operator!=(const iterator &a, const iterator &b);
  };

  iterator begin() const { return iterator(*this); }
  iterator end() const { return iterator(*this,1); }
  size_t size() const { return coeffs.size(); }
};

bool operator==(const schur_poly::iterator &a, const schur_poly::iterator &b);
bool operator!=(const schur_poly::iterator &a, const schur_poly::iterator &b);

class LittlewoodRicharsdon
{
  tableau _SMtab;
  tableau _SMfilled;
  int _SMcurrent;
  int _SMfinalwt;

  void bounds(int &lo, int &hi);
  void SM();

  void skew_schur(const_schur_partition lambda, const_schur_partition p);

protected:
  virtual void append_term(ring_elem coeff, const_schur_partition f) = 0;
public:
  
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
  int nvars; // max size of a partition, or -1 meaning infinity
protected:
  bool initialize_schur();
  SchurRing2() {}
  virtual ~SchurRing2() {}

  SchurRing2(const Ring *A, int n=-1);

  bool initialize_SchurRing2();
public:
  static SchurRing2 *create(const Ring *A, int n=-1);

  static SchurRing2 *createInfinite(const Ring *A);

  virtual const SchurRing2 * cast_to_SchurRing2() const { return this; }
  virtual       SchurRing2 * cast_to_SchurRing2()       { return this; }

  bool is_valid_partition(M2_arrayint part, bool set_error=true) const;
  // sets global error message by default

  ring_elem from_coeff(ring_elem a) const;
  ring_elem from_partition(M2_arrayint part) const;

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

  virtual bool promote(const Ring *R, const ring_elem f, ring_elem &result) const { /* write me */ }
  virtual bool lift(const Ring *R, const ring_elem f, ring_elem &result) const { /* write me */ }

  virtual bool is_unit(const ring_elem f) const;
  virtual bool is_zero(const ring_elem f) const;
  virtual bool is_equal(const ring_elem f, const ring_elem g) const; // NOT DONE...

  virtual int compare_elems(const ring_elem f, const ring_elem g) const { /* write me */ }
  virtual ring_elem copy(const ring_elem f) const;
  virtual void remove(ring_elem &f) const { }  // let the GC do it!

  virtual ring_elem negate(const ring_elem f) const { /* write me */ }
  virtual ring_elem add(const ring_elem f, const ring_elem g) const { /* write me */ }
  virtual ring_elem subtract(const ring_elem f, const ring_elem g) const { /* write me */ }
  virtual ring_elem mult(const ring_elem f, const ring_elem g) const { /* write me */ }

  virtual ring_elem invert(const ring_elem f) const; // do nothing
  virtual ring_elem divide(const ring_elem f, const ring_elem g) const; // do nothing

  virtual void syzygy(const ring_elem a, const ring_elem b,
		      ring_elem &x, ring_elem &y) const; // do nothing

  virtual ring_elem eval(const RingMap *map, const ring_elem f, int first_var) const { /* write me */ }

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
