// Copyright 1998 by Michael Stillman
#ifndef __poly_hpp_
#define __poly_hpp_

#include "Edefs.hpp"
#include "EZZp.hpp"

typedef int field;
class stash;
class buffer;

struct mpoly;
struct term_list {
  term_list *next;
  int exponent;
  mpoly *coefficient;
};

struct mpoly
{
  short variable;		// If variable >= 1, then 'terms' is active, else 'coefficient' is.
  union
  {
    term_list *terms;
    ERingElement coefficient;
  } stuff;
};

class Polynomials
{
  const ERing *K;
  static stash *poly_stash;
  static stash *term_list_stash;

  mpoly *new_poly() const;
  term_list *new_term_list() const;
  void delete_poly(mpoly *f) const;
  void delete_term_list(term_list *t) const; // Removes a single node

  void remove_term_list(term_list *t) const; // Removes the entire list
  term_list *clone_term_list(const term_list *t) const;

  term_list *add_term_list(term_list *s, term_list *t) const;
  term_list *new_term_list(int e, mpoly *g) const;
  mpoly *new_poly(int var, term_list *t) const;

public:
  Polynomials(const ERing *K);
  ~Polynomials() {}

  // Translate 'mpoly' to/from the vectors we use in GB's, elsewhere.

  // Create polynomials from scratch
  mpoly *fromInteger(int a) const;
  mpoly *variablePower(int v, int e) const;

  // Removal of mpoly, poly_node nodes
  void remove(mpoly *&f) const;
  mpoly *clone(const mpoly *f) const;
  
  // Basic Arithmetic
  mpoly *negate(const mpoly *f) const;
  mpoly *add(mpoly *f, mpoly *g) const;
  mpoly *subtract(const mpoly *f, const mpoly *g) const;
  mpoly *multiply(const mpoly *f, const mpoly *g) const;
  mpoly *power(const mpoly *f, int n) const;
  mpoly *powerModF(const mpoly *f, int n, const mpoly *F) const;

  // Information
  int leadVariable(const mpoly *f) const;
  const mpoly *leadCoefficient(const mpoly *f) const;

  // Division: exact, over a field, not over a field.
  mpoly *exactDivision(const mpoly *f, const mpoly *g) const;
  mpoly *euclidDivision(const mpoly *f, const mpoly *g) const;
  mpoly *euclidDivision(const mpoly *f, const mpoly *g, mpoly *&result_divisor) const;
  mpoly *pseudoDivision(const mpoly *f, const mpoly *g) const;
  mpoly *pseudoDivision(const mpoly *f, const mpoly *g, mpoly *&divisor) const;

  // GCD, resultant type computation
  mpoly *content(const mpoly *f) const;
  mpoly *gcd(const mpoly *f, const mpoly *g) const;
  mpoly *gcdCoefficients(const mpoly *f, const mpoly *g, mpoly *&u, mpoly *&v) const;
  mpoly *resultant(const mpoly *f, const mpoly *g) const;

  // Display: for debugging purposes
  void text_out(buffer &o, const mpoly *f, 
		bool p_parens=false, bool p_one=true, bool p_plus=false) const;
  void terms_text_out(buffer &o, const term_list *t, int var,
		      bool p_parens=false, bool p_one=true, bool p_plus=false) const;
};
#endif
