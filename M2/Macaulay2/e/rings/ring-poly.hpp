// Copyright 2004 Michael E. Stillman

#ifndef __ring_poly_hpp_
#define __ring_poly_hpp_

#include <gc.h>

//void * GC_MALLOC(int x) { new char[x]; }
class FreeModule;

template <class CoeffRing, class Monoid>
class PolyRing
{
  CoeffRing *K;
  Monoid *M;

  int nvars;
  int poly_size;

 public:
  typedef typename CoeffRing::coeff_type coeff_type;
  typedef typename Monoid::monomial monomial;
  
  struct poly {
    poly *next;
    int comp;
    coeff_type coeff;
    monomial monom;
  };

private:
  poly * new_raw_term()
  {
    return static_cast<poly *>(GC_MALLOC(poly_size));
  }

  poly * new_term()
  {
    poly *a = new_raw_term();
    K->init(a->coeff);
    M->init(a->monom);
    a->next = 0;
    a->comp = 0;
    return a;
  }

  void poly_remove_term(poly *t)
  {
    char *s = reinterpret_cast<char *>(t);
    delete [] s;
  }

  poly * poly_copy_term(const poly *t)
  {
    poly *v = new_raw_term();
    v->next = 0;
    K->copy(v->coeff, t->coeff);
    M->copy(v->monom, t->monom);
    v->comp = t->comp;
    return v;
  }

  enum {LT =-1, EQ = 0, GT = 1} poly_compare(const poly *f, const poly *g);

  int (PolyRing<CoeffRing,Monoid>::*compare_fcn)(poly *f, poly *g);

  int compare1(poly *f, poly *g)
  {
    return M->compare(f->monom,f->comp,g->monom,g->comp);
  }

public:

  PolyRing(CoeffRing *K0, Monoid * M0) : K(K0), M(M0)
    {
      compare_fcn = &PolyRing<CoeffRing,Monoid>::compare1;

      // We can probably make this virtual:
      mult_by_term = &PolyRing<CoeffRing,Monoid>::mult_by_term1;
    }

  void poly_init(poly *&a)
    {
      a = 0;
    }

  bool poly_is_zero(const poly *f) const
  {
    return f == 0;
  }

  bool poly_is_equal(const poly *a, const poly *b) const
  {
    for ( ;; a = a->next, b = b->next)
      {
	if (a == 0)
	  {
	    if (b == 0) return true;
	    return false;
	  }
	if (b == 0) return false;
	if (a->comp != b->comp) return false;
	if (!K->is_equal(a->coeff, b->coeff)) return false;
	if (!M->is_equal(a->monom, b->monom))
	  return false;
      }
  }

  int poly_n_terms(const poly *v) const
  {
    int result = 0;
    for ( ; v != 0; v = v->next)
      result++;
    return result;
  }

  void poly_mult_by_coeff_to(poly *f, coeff_type u)
  {
    for ( ; f != 0; f=f->next)
      K->mult(f->coeff,f->coeff,u); // f->coeff *= u;
  }

  void poly_copy(poly *&result, const poly *f)
  {
    poly head;
    poly *b = &head;
    for ( ; f != 0; f = f->next, b = b->next)
      b->next = poly_copy_term(f);
    b->next = 0;
    result = head.next;
  }
  
  void poly_add_to(poly * &f, poly * &g)
  {
    if (g == 0) return;
    if (f == 0) { f = g; g = 0; return; }
    poly head;
    poly *result = &head;
    while (1)
      switch ((this->*compare_fcn)(f,g))
	{
	case LT:
	  result->next = g;
	  result = result->next;
	  g = g->next;
	  if (g == 0) 
	    {
	      result->next = f; 
	      f = head.next;
	      return;
	    }
	  break;
	case GT:
	  result->next = f;
	  result = result->next;
	  f = f->next;
	  if (f == 0) 
	    {
	      result->next = g; 
	      f = head.next;
	      g = 0;
	      return;
	    }
	  break;
	case EQ:
	  poly *tmf = f;
	  poly *tmg = g;
	  f = f->next;
	  g = g->next;
	  K->add(tmf->coeff, tmf->coeff, tmg->coeff);
	  if (K->is_zero(tmf->coeff))
	    {
	      poly_remove_term(tmf);
	    }
	  else
	    {
	      result->next = tmf;
	      result = result->next;
	    }
	  poly_remove_term(tmg);
	  if (g == 0) 
	    {
	      result->next = f; 
	      f = head.next;
	      return;
	    }
	  if (f == 0) 
	    {
	      result->next = g; 
	      f = head.next;
	      g = 0;
	      return;
	    }
	  break;
	}
  }

  // mult_by_term1: needs Schreyer encoding info if
  // multiplication is Weyl algebra or solvable.
  // For poly ring, skew poly ring: don't need it.

  poly *mult_by_term1(const poly *f,
		      coeff_type u,
		      monomial monom,
		      int comp)
  {
    // Remember: this multiplies w/o regard to any quotient elements
    // This works for commutative polynomial rings.
    // This works if the monomials are Schreyer encoded too.
    poly head;
    poly *inresult = &head;
    
    for (const poly *s = f; s != 0; s = s->next)
      {
	poly *t = new_raw_term();
	t->next = 0;
	t->comp = s->comp + comp;
	K->mult(t->coeff, u, s->coeff);
	M->mult(t->monom, monom, s->monom);
	inresult->next = t;
	inresult = inresult->next;
      }
    inresult->next = 0;
    return head.next;
  }

  poly *(PolyRing<CoeffRing,Monoid>::*mult_by_term)(const poly *f, 
						coeff_type u,
						monomial monom,
						int comp);

  
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
