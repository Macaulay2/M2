// Copyright 1998 by Michael E. Stillman
#include "Epoly.hpp"
#include "mem.hpp"

stash *Polynomials::poly_stash = 0;
stash *Polynomials::term_list_stash = 0;

Polynomials::Polynomials(const ECoefficientRing *K)
  : K(K)
{
  if (poly_stash == 0)
    poly_stash = new stash("poly heads", sizeof(struct poly));
  if (term_list_stash == 0)
    term_list_stash = new stash("term lists", sizeof(struct term_list));
}

poly *Polynomials::new_poly() const
{
  return (poly *) poly_stash->new_elem();
}

term_list *Polynomials::new_term_list() const
{
  return (term_list *) term_list_stash->new_elem();
}

void Polynomials::delete_poly(poly *f) const
{
  poly_stash->delete_elem(f);
}

void Polynomials::delete_term_list(term_list *t) const
{
  term_list_stash->delete_elem(t);
}

poly *Polynomials::fromInteger(int a) const
{
  field b = K->from_int(a);
  if (K->is_zero(b))
    return 0;
  poly *result = new_poly();
  result->variable = 0;
  result->stuff.coefficient = b;
  return result;
}

poly *Polynomials::variablePower(int v, int e) const
{
  if (e == 0 || v == 0)
    return fromInteger(1);

  poly *result = new_poly();
  term_list *t = new_term_list();
  t->next = 0;
  t->exponent = e;
  t->coefficient = fromInteger(1);
  result->variable = v;
  result->stuff.terms = t;
  return result;
}

void Polynomials::remove_term_list(term_list *t) const
{
  while (t != 0)
    {
      term_list *tmp = t;
      t = t->next;
      remove(tmp->coefficient);
      delete_term_list(tmp);
    }
}
void Polynomials::remove(poly *&f) const
{
  if (f == 0) return;
  if (f->variable == 0)
    K->remove(f->stuff.coefficient);
  else
    remove_term_list(f->stuff.terms);

  delete_poly(f);
}

term_list *Polynomials::clone_term_list(const term_list *t) const
{
  term_list head;
  term_list *result = &head;
  while (t != 0)
    {
      term_list *s = new_term_list();
      s->exponent = t->exponent;
      s->coefficient = clone(t->coefficient);
      result->next = s;
      result = s;
    }
  result->next = 0;
  return head.next;
}

poly *Polynomials::clone(const poly *f) const
{
  if (f == 0) return 0;
  poly *result = new_poly();
  result->variable = f->variable;
  if (result->variable == 0)
    result->stuff.coefficient = K->clone(f->stuff.coefficient);
  else
    result->stuff.terms = clone_term_list(f->stuff.terms);
  return result;
}

int Polynomials::leadVariable(const poly *f) const
{
  if (f == 0) return 0;
  return f->variable;
}
const poly *Polynomials::leadCoefficient(const poly *f) const
{
  if (f == 0) return 0;
  if (f->variable == 0) return f;
  return f->stuff.terms->coefficient;
}

term_list *Polynomials::new_term_list(int e, poly *g) const
{
  // Consumes 'g'.
  term_list *result = new_term_list();
  result->exponent = e;
  result->coefficient = g;
  result->next = 0;
  return result;
}

poly *Polynomials::new_poly(int var, term_list *t) const
{
  // Consumes 't'.
  if (t == 0) return 0;
  poly *result;
  if (t->exponent == 0) 
    {
      result = t->coefficient;
      delete_term_list(t);
    }
  else
    {
      result = new_poly();
      result->variable = var;
      result->stuff.terms = t;
    }
  return result;
}

term_list *Polynomials::add_term_list(term_list *s, term_list *t) const
  // Consumes 's' and 't'.
{
  if (s == 0) return t;
  if (t == 0) return s;
  // Now merge the two lists together
  term_list head;
  term_list *result = &head;
  while (true)
    {
      if (s->exponent > t->exponent)
	{
	  result->next = s;
	  result = s;
	  s = s->next;
	  if (s == 0)
	    {
	      result->next = t;
	      return head.next;
	    }
	}
      else if (s->exponent < t->exponent)
	{
	  result->next = t;
	  result = t;
	  t = t->next;
	  if (t == 0)
	    {
	      result->next = s;
	      return head.next;
	    }
	}
      else
	{
	  // Equality.
	  s->coefficient = add(s->coefficient, t->coefficient);
	  result->next = s;
	  result = s;
	  s = s->next;
	  if (s == 0)
	    {
	      result->next = t;
	      return head.next;
	    }
	  term_list *tmp = t;
	  t = t->next;
	  delete_term_list(tmp);
	  if (t == 0)
	    {
	      result->next = s;
	      return head.next;
	    }
	}
    }
}

poly *Polynomials::add(poly *f, poly *g) const
{
  // Consumes 'f' and 'g'
  if (f == 0) return g;
  if (g == 0) return f;
  if (f->variable > g->variable)
    {
      f->stuff.terms = add_term_list(f->stuff.terms,
				     new_term_list(0,g));
      return f;
    }
  else if (f->variable < g->variable)
    {
      g->stuff.terms = add_term_list(g->stuff.terms,
				     new_term_list(0,f));
      return g;
    }
  else
    {
      f->stuff.terms = add_term_list(f->stuff.terms,g->stuff.terms);
      delete_poly(g);
      return f;
    }
}

void Polynomials::text_out(buffer &o, 
			   const poly *f, 
			   bool p_parens,
			   bool p_one,
			   bool p_plus) const
{
  if (f == 0) o << "0";
  else if (f->variable == 0)
    {
      int a = f->stuff.coefficient;
      if (a > 0 && p_plus) o << '+';
      else if (a < 0) 
	{
	  o << '-';
	  a = -a;
	}
      if (p_one && a == 1)
	o << "1";
      else if (a != 1)
	o << a;
    }
  else
    terms_text_out(o,f->stuff.terms,f->variable,p_parens,p_one,p_plus);
}
void Polynomials::terms_text_out(buffer &o, 
				 const term_list *t, 
				 int var,
				 bool p_parens,
				 bool p_one,
				 bool p_plus) const
{
  if (t->next == 0)
    {
      // Case 1: Only one term
      if (t->exponent == 0)
	text_out(o,t->coefficient,p_parens,p_one,p_plus);
      else
	{
	  text_out(o,t->coefficient,true,false,p_plus);
	  o << (char)('a' + var - 1);
	  o << t->exponent;
	}
    }
  else
    {
      // Case 2: At least 2 terms
      if (p_parens)
	{
	  if (p_plus)
	    o << "+";
	  o << "(";
	  p_plus = false;
	}
      // First term: will have exponent > 0.
      text_out(o,t->coefficient,true,false,p_plus);
      o << (char)('a' + var - 1);
      o << t->exponent;

      // Now do the rest of the terms
      for (t = t->next; t != 0; t=t->next)
	{
	  if (t->exponent == 0)
	    text_out(o,t->coefficient,false,true,true);
	  else
	    {
	      text_out(o,t->coefficient,p_parens,p_one,true); // First term
	      o << (char)('a' + var - 1);
	      o << t->exponent;
	    }
	}
      if (p_parens)
	o << ")";
    }
}
