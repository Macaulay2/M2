// Copyright 1998 by Michael Stillman
#ifndef __poly_hpp_
#define __poly_hpp_

#include "Edefs.hpp"
#include "EZZp.hpp"

typedef int field;
class stash;
class buffer;

struct poly;
struct term_list {
  term_list *next;
  int exponent;
  poly *coefficient;
};

struct poly
{
  short variable;		// If variable >= 1, then 'terms' is active, else 'coefficient' is.
  union
  {
    term_list *terms;
    field coefficient;
  } stuff;
};

class Polynomials
{
  const ECoefficientRing *K;
  static stash *poly_stash;
  static stash *term_list_stash;

  poly *new_poly() const;
  term_list *new_term_list() const;
  void delete_poly(poly *f) const;
  void delete_term_list(term_list *t) const; // Removes a single node

  void remove_term_list(term_list *t) const; // Removes the entire list
  term_list *clone_term_list(const term_list *t) const;

  term_list *add_term_list(term_list *s, term_list *t) const;
  term_list *new_term_list(int e, poly *g) const;
  poly *new_poly(int var, term_list *t) const;

public:
  Polynomials(const ECoefficientRing *K);
  ~Polynomials() {}

  // Translate 'poly' to/from the vectors we use in GB's, elsewhere.

  // Create polynomials from scratch
  poly *fromInteger(int a) const;
  poly *variablePower(int v, int e) const;

  // Removal of poly, poly_node nodes
  void remove(poly *&f) const;
  poly *clone(const poly *f) const;
  
  // Basic Arithmetic
  poly *negate(const poly *f) const;
  poly *add(poly *f, poly *g) const;
  poly *subtract(const poly *f, const poly *g) const;
  poly *multiply(const poly *f, const poly *g) const;
  poly *power(const poly *f, int n) const;
  poly *powerModF(const poly *f, int n, const poly *F) const;

  // Information
  int leadVariable(const poly *f) const;
  const poly *leadCoefficient(const poly *f) const;

  // Division: exact, over a field, not over a field.
  poly *exactDivision(const poly *f, const poly *g) const;
  poly *euclidDivision(const poly *f, const poly *g) const;
  poly *euclidDivision(const poly *f, const poly *g, poly *&result_divisor) const;
  poly *pseudoDivision(const poly *f, const poly *g) const;
  poly *pseudoDivision(const poly *f, const poly *g, poly *&divisor) const;

  // GCD, resultant type computation
  poly *content(const poly *f) const;
  poly *gcd(const poly *f, const poly *g) const;
  poly *gcdCoefficients(const poly *f, const poly *g, poly *&u, poly *&v) const;
  poly *resultant(const poly *f, const poly *g) const;

  // Display: for debugging purposes
  void text_out(buffer &o, const poly *f, 
		bool p_parens=false, bool p_one=true, bool p_plus=false) const;
  void terms_text_out(buffer &o, const term_list *t, int var,
		      bool p_parens=false, bool p_one=true, bool p_plus=false) const;
};
#endif
