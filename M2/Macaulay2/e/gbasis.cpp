#include <functional>
#include <algorithm>

#include "gbasis.hpp"
#include "matrix.hpp"
#include "text_io.hpp"
#include "polyring.hpp"
#include "ntuple.hpp"

extern int comp_printlevel;
// todo: minimalize_gb, remainder

GBasis::GBasis(const FreeModule *F, const FreeModule *Fsyz)
  : mem(1000),
    F(F),
    Fsyz(Fsyz)
{
  originalR = F->get_ring()->cast_to_PolynomialRing();
  if (originalR == NULL)
    {
      ERROR("ring is not a polynomial ring");
      // MES: throw an error here.
      assert(0);
    }
  R = originalR->get_gb_ring();
  nvars = R->n_vars();
  //  RingGBasis *A = originalR->get_quotient_gb();
  //  ringtable = A->get_ringtable();
  //  A->set_gb(gb);
  first_gb_element = gb.size(); 
  lookup = MonomialTable::make(R->n_vars());
  minimal_gb_valid = true;
  _EXP = R->exponents_make();
  _EXP++;
}

GBasis::gbelem *GBasis::gbelem_make(const FreeModule *F,
				    gbvector *f,  // grabs f
				    gbvector *fsyz, // grabs fsyz
				    gbelem_type minlevel,
				    int deg)
{
  const PolynomialRing *P = F->get_ring()->cast_to_PolynomialRing();
  GBRing *R = P->get_gb_ring();
  GBasis::gbelem *g = new GBasis::gbelem;
  g->g.f = f;
  g->g.fsyz = fsyz;
  g->lead = R->exponents_make();
  R->gbvector_get_lead_exponents(F, f, g->lead);
  g->deg = deg;
  g->alpha = deg - R->gbvector_term_weight(F,f);
  g->minlevel = minlevel;
  return g;
}

int GBasis::insert(gbvector *f, gbvector *fsyz, gbelem_type minlevel, int deg)
{
  gbelem *g = gbelem_make(F, f, fsyz, minlevel, deg);
  minimal_gb_valid = false;
  int me = gb.size();
  gb.push_back(g);
  
  int x = g->g.f->comp;
  lookup->insert(g->lead, x, me);

  return me;
}

#if 0
bool GBasis::find_good_divisor(exponents e,
			       int x,
			       int degf, 
			       int &result_alpha, 
			       POLY &result_g)
{
  int i, alpha, newalpha, ealpha;
  int n = 0;

  vector<MonomialTable::mon_term *> divisors;
  ealpha = degf - R->exponents_weight(e);

  /* First search for ring divisors */
  n += ringtable->find_divisors(-1, e, 1, &divisors);

  /* Next search for GB divisors */
  n += lookup->find_divisors(-1, e, x, &divisors);

  /* Now find the minimal alpha value */
  if (n == 0) 
    return false;
  MonomialTable::mon_term *t = divisors[0];
  gbelem *tg = gb[t->_val];
  alpha = tg->alpha - ealpha;
  if (alpha <= 0) 
    alpha = 0;
  else
    for (i=1; i<n; i++)
      {
	t = divisors[i];
	tg = gb[t->_val];
	newalpha = tg->alpha - ealpha;
	if (newalpha <= 0) {
	  alpha = 0;
	  break;
	} else if (newalpha < alpha) alpha = newalpha;
      }
  result_alpha = alpha;
  result_g = gb[t->_val] -> g;
  return true;
}
#endif

int GBasis::find_good_divisor(exponents e,
			      int x,
			      int degf, 
			      int &result_alpha)
  // Returns an integer w.
  // if w >=0: gb[w]'s lead term divides [e,x].
  // if w<0: no gb[w] has lead term dividing [e,x].
{
  int i, alpha, newalpha, ealpha;
  int n = 0;

  vector<MonomialTable::mon_term *> divisors;
  ealpha = degf - R->exponents_weight(e);

  /* First search for ring divisors */
  n += ringtable->find_divisors(-1, e, 1, &divisors);

  /* Next search for GB divisors */
  n += lookup->find_divisors(-1, e, x, &divisors);

  /* Now find the minimal alpha value */
  if (n == 0) 
    return -1;
  MonomialTable::mon_term *t = divisors[0];
  gbelem *tg = gb[t->_val];
  alpha = tg->alpha - ealpha;
  if (alpha <= 0) 
    alpha = 0;
  else
    for (i=1; i<n; i++)
      {
	t = divisors[i];
	tg = gb[t->_val];
	newalpha = tg->alpha - ealpha;
	if (newalpha <= 0) {
	  alpha = 0;
	  break;
	} else if (newalpha < alpha) alpha = newalpha;
      }
  result_alpha = alpha;
  return t->_val;
}

void GBasis::remainder(POLY &f, int degf)
{
  gbvector head;
  gbvector *frem = &head;
  frem->next = 0;
  int count = 0;
  POLY h = f;
  while (!R->gbvector_is_zero(h.f))
    {
      int alpha;
      R->gbvector_get_lead_exponents(F, h.f, _EXP);
      int x = h.f->comp;
      int w = find_good_divisor(_EXP,x,degf,  alpha);
        // replaced alpha, g.
      if (w < 0 || alpha > 0)
	{
	  frem->next = h.f;
	  frem = frem->next;
	  h.f = h.f->next;
	  frem->next = 0;
	}
      else
	{
	  POLY g = gb[w]->g;
	  R->gbvector_reduce_lead_term(F, Fsyz,
				       head.next,
				       h.f, h.fsyz,
				       g.f, g.fsyz);
	  count++;
	  //	  _stats_ntail++;
	  if (comp_printlevel == 10)
	    {
	      buffer o;
	      o << "  tail reducing by ";
	      R->gbvector_text_out(o,F,g.f);
	      o << "\n    giving ";
	      R->gbvector_text_out(o,F,h.f);
	      emit_line(o.str());
	    }
	  
	}
    }
  h.f = head.next;
  const Ring *K = R->get_flattened_coefficients();
  ring_elem denom = K->from_int(0);
  R->gbvector_remove_content(h.f, h.fsyz,denom);
  f.f = h.f;
  f.fsyz = h.fsyz;
  if (comp_printlevel == 3)
    {
      buffer o;
      o << "," << count;
      emit(o.str());
    }
}

void GBasis::remainder(POLY &f, int degf, ring_elem &denom)
  // find the remainder of f = [g,gsyz] wrt the GB,
  // i.e. replace f with h[h,hsyz], st
  // base not ZZ:
  //    h = f - sum(a_i * g_i),  in(f) not in in(G)
  //    hsyz = fsyz - sum(a_i * gsyz_i)
  // base is ZZ:
  //    h = c*f - sum(a_i * g_i), in(f) not in in(G),
  //    hsyz = c*fsyz - sum(a_i * gsyz_i)
  //    but a_i,h are all polynomials with ZZ coefficients (not QQ).
  // (Here: G = (g_i) is the GB, and a_i are polynomials generated
  // during division).
  // c is an integer, and is returned as 'denom'.
  // Five issues:
  // (a) if gcd(c, coeffs(f)) becomes > 1, can we divide
  //     c, f, by this gcd? If so, how often do we do this?
  // (b) do we reduce by any element of the GB, or only those whose
  //     sugar degree is no greater than degf?
  // (c) can we exclude an element of the GB from the g_i?
  //     (for use in auto reduction).
  // (d) can we reduce by the minimal GB instead of the original GB?
  //     ANSWER: NO.  Instead, use a routine to make a new GB.
  // (e) Special handling of quotient rings: none needed.
{
  gbvector head;
  gbvector *frem = &head;
  frem->next = 0;
  int count = 0;
  POLY h = f;
  while (!R->gbvector_is_zero(h.f))
    {
      int alpha;
      R->gbvector_get_lead_exponents(F, h.f, _EXP);
      int x = h.f->comp;
      int w = find_good_divisor(_EXP,x,degf,  alpha);
        // replaced alpha, g.
      if (w < 0 || alpha > 0)
	{
	  frem->next = h.f;
	  frem = frem->next;
	  h.f = h.f->next;
	  frem->next = 0;
	}
      else
	{
	  POLY g = gb[w]->g;
	  R->gbvector_reduce_lead_term_coeff(F, Fsyz,
					     head.next,
					     h.f, h.fsyz,
					     g.f, g.fsyz,
					     denom); // multiplies to denom.
	  count++;
	  //	  _stats_ntail++;
	  if (comp_printlevel == 10)
	    {
	      buffer o;
	      o << "  tail reducing by ";
	      R->gbvector_text_out(o,F,g.f);
	      o << "\n    giving ";
	      R->gbvector_text_out(o,F,h.f);
	      emit_line(o.str());
	    }
	  
	}
    }
  h.f = head.next;
  R->gbvector_remove_content(h.f, h.fsyz,denom);
  f.f = h.f;
  f.fsyz = h.fsyz;
  if (comp_printlevel == 3)
    {
      buffer o;
      o << "," << count;
      emit(o.str());
    }
}

void GBasis::poly_auto_reduce(vector<POLY> &mat)
{
  for (vector<POLY>::iterator i = mat.begin(); i != mat.end(); i++)
    for (vector<POLY>::iterator j = mat.begin(); j != i; j++)
      {
	R->gbvector_auto_reduce(F,Fsyz,
				(*i).f, (*i).fsyz,
				(*j).f, (*j).fsyz);
      }
}

struct gbelem_sorter : public binary_function<int,int,bool> {
  GBRing *R;
  const FreeModule *F;
  const vector<GBasis::gbelem *> &gb;
  gbelem_sorter(GBRing *R,
		const FreeModule *F,
		const vector<GBasis::gbelem *> &gb)
    : R(R), F(F), gb(gb) {}
  bool operator()(int xx, int yy) {
    gbvector *x = gb[xx]->g.f;
    gbvector *y = gb[yy]->g.f;
    return R->gbvector_compare(F,x,y) == LT;
  }
};

void GBasis::minimalize_gb()
{
  if (minimal_gb_valid) return;

  // Place into _minimal_gb a sorted minimal GB
  vector<exponents> exps;
  vector<int> comps;
  vector<int> positions;
  exps.reserve(gb.size());
  comps.reserve(gb.size());
  positions.reserve(gb.size());

  for (vector<gbelem *>::iterator i = gb.begin(); i != gb.end(); i++)
    {
      if ((*i)->minlevel <= ELEM_MIN_GB)
	{
	  exponents e = (*i)->lead;
	  exps.push_back(e);
	  int x = (*i)->g.f->comp; // component of this element
	  comps.push_back(x);
	}
    }

  // MES: if we modify _nvars below to only take those variables
  // which are not field vars (via a fraction field), we can obtain
  // a minimal GB in those cases.  Uniqueness is harder to compute though.

  MonomialTable::minimalize(nvars,
			    exps,
			    comps,
			    false,
			    positions);

  // Now sort 'positions'.
  sort(positions.begin(), positions.end(), gbelem_sorter(R,F,gb));

  for (vector<int>::iterator i = positions.begin(); i != positions.end(); i++)
    {
      // possibly first copy gb[*i]->g...
      minimal_gb.push_back(gb[*i]->g);
    }

  poly_auto_reduce(minimal_gb);

  minimal_gb_valid = true;
}

const Matrix *GBasis::get_minimal_gb()
  // The result contains no quotient ring elements
{
  minimalize_gb();
  Matrix *result = new Matrix(F);
  for (vector<POLY>::iterator i = minimal_gb.begin(); i != minimal_gb.end(); i++)
    result->append(R->gbvector_to_vec(F, (*i).f));
  return result;
}

const Matrix *GBasis::get_minimal_gens()
{
  Matrix *result = new Matrix(F);
  for (vector<gbelem *>::iterator i = gb.begin(); i != gb.end(); i++)
    if ((*i)->minlevel <= ELEM_TRIMMED)
      result->append(R->gbvector_to_vec(F, (*i)->g.f));
  return result;
}

const Matrix *GBasis::get_change()
{
  minimalize_gb();
  Matrix *result = new Matrix(Fsyz);
  for (vector<POLY>::iterator i = minimal_gb.begin(); i != minimal_gb.end(); i++)
    result->append(R->gbvector_to_vec(Fsyz, (*i).fsyz));
  return result;
}

const Matrix *GBasis::get_leadterms(int nparts)
{
  minimalize_gb();
  Matrix *result = new Matrix(F);
  for (vector<POLY>::iterator i = minimal_gb.begin(); i != minimal_gb.end(); i++)
    {
      gbvector *f = R->gbvector_lead_term(nparts, F, (*i).f);
      result->append(R->gbvector_to_vec(F, f));
    }
  return result;
}

const FreeModule *GBasis::get_free(M2_bool minimal)
{
  // Do we really need to include this one?
}

const MatrixOrNull *GBasis::matrix_remainder(const Matrix *m)
{
  // NEEDS WORK!!!
  if (m->n_rows() != F->rank()) {
       ERROR("expected matrices to have same number of rows");
       return 0;
  }
  Matrix *red = new Matrix(m->rows(), m->cols(), m->degree_shift());
  for (int i=0; i<m->n_cols(); i++)
    {
      ring_elem denom;
      POLY g;
      g.f = R->gbvector_from_vec(F, (*m)[i], denom);
      g.fsyz = R->gbvector_zero();

      remainder(g, 0, denom); // MES: this degree.  What should it be?

      vec fv = R->gbvector_to_vec_denom(F, g.f, denom);
      // MES: what about g.fsyz??
      (*red)[i] = fv;
    }
  return red;
}

void GBasis::matrix_lift(const Matrix *m,
			 MatrixOrNull **result_remainder,
			 MatrixOrNull **result_quotient
		 )
{
  if (m->n_rows() != F->rank()) {
       ERROR("expected matrices to have same number of rows");
      *result_remainder = 0;
      *result_quotient = 0;
  }
  *result_remainder = new Matrix(m->rows(), m->cols(), m->degree_shift());
  *result_quotient = new Matrix(Fsyz, m->cols());
  const Ring *K = R->get_flattened_coefficients();
  for (int i=0; i<m->n_cols(); i++)
    {
      ring_elem denom;
      POLY g;
      g.f = R->gbvector_from_vec(F, (*m)[i], denom);
      g.fsyz = R->gbvector_zero();

      remainder(g, 0, denom); // MES: this degree.  What should it be?

      vec fv = R->gbvector_to_vec_denom(F, g.f, denom);
      K->negate_to(denom);
      vec fsyzv = R->gbvector_to_vec_denom(Fsyz,g.fsyz, denom);
      (**result_remainder)[i] = fv;
      (**result_quotient)[i] = fsyzv;
    }
}

int GBasis::contains(const Matrix *m)
  // Return -1 if every column of 'm' reduces to zero.
  // Otherwise return the index of the first column that
  // does not reduce to zero.
{
  // Reduce each column of m one by one.
  for (int i=0; i<m->n_cols(); i++)
    {
      ring_elem denom;
      POLY g;
      g.f = R->gbvector_from_vec(F,(*m)[i], denom);
      g.fsyz = NULL;
      remainder(g,0);
      R->gbvector_remove(g.fsyz);
      if (g.f != NULL)
	{
	  R->gbvector_remove(g.f);
	  return i;
	}
    }
  return -1;
}

GBasis::gbelem *RingGBasis::gbelem_make(gbvector *f)
{
  // Creates a gbelem *, and populates it with a copy of f,
  // and also sets the components of the copy to 0.
  
  int lead, lo, hi;
  GBasis::gbelem *g = new GBasis::gbelem;
  g->g.f = R->gbvector_copy(f);
  g->g.fsyz = 0;
  R->gbvector_weight(R1,g->g.f, lead, lo, hi);
  g->deg = hi;
  g->alpha = hi-lead;
  g->lead = R->exponents_make();
  R->gbvector_get_lead_exponents(R1,g->g.f, g->lead);
  g->minlevel = ELEM_IN_STONE;
  return g;
}

RingGBasis *RingGBasis::make(GBRing *R, vector<gbvector *> &elems)
{
  RingGBasis *result = new RingGBasis;
  result->R = R;
#if 0
  result->_mem.set_size(xxx);
  R->set_mem(_mem);
#endif
  result->ringtable = MonomialTable::make(R->n_vars());
  for (int i=0; i<elems.size(); i++)
    {
      GBasis::gbelem *g = result->gbelem_make(elems[i]);
      result->ringtable->insert(g->lead, 0, i);
      result->gb.push_back(g);
    }
  return result;
}

void RingGBasis::normal_form(FreeModule *F, 
			     gbvector *&v, 
			     ring_elem &denom)
  // Multiplies denom by the ring element u st
  // NF(v) = denom * v mod GB

  // find the remainder of f = [g,gsyz] wrt the GB,
  // i.e. replace f with h[h,hsyz], st
  // base not ZZ:
  //    h = f - sum(a_i * g_i),  in(f) not in in(G)
  //    hsyz = fsyz - sum(a_i * gsyz_i)
  // base is ZZ:
  //    h = c*f - sum(a_i * g_i), in(f) not in in(G),
  //    hsyz = c*fsyz - sum(a_i * gsyz_i)
  //    but a_i,h are all polynomials with ZZ coefficients (not QQ).
  // (Here: G = (g_i) is the GB, and a_i are polynomials generated
  // during division).
  // c is an integer, and is returned as 'denom'.
  // Five issues:
  // (a) if gcd(c, coeffs(f)) becomes > 1, can we divide
  //     c, f, by this gcd? If so, how often do we do this?
  // (b) do we reduce by any element of the GB, or only those whose
  //     sugar degree is no greater than degf?
  // (c) can we exclude an element of the GB from the g_i?
  //     (for use in auto reduction).
  // (d) can we reduce by the minimal GB instead of the original GB?
  //     ANSWER: NO.  Instead, use a routine to make a new GB.
  // (e) Special handling of quotient rings: none needed.
{
#warning "normal form not yet implemented"
#if 0

  gbvector head;
  gbvector *frem = &head;
  frem->next = 0;
  exponents _EXP = R->exponents_make();
  while (!R->gbvector_is_zero(v))
    {
      int not_used;
      POLY g;
      R->gbvector_get_lead_exponents(F, v, _EXP);
      int w = ringtable->find_good_divisor(_EXP,0,0,  not_used,g);
        // replaced alpha, g.
      if (w < 0)
	{
	  frem->next = v;
	  frem = frem->next;
	  v = v->next;
	  frem->next = 0;
	}
      else
	{
	  ntuple::divide(nvars, _EXP, gb[w]->lead, _EXP);
	  R->gbvector_reduce_lead_term_coeff(F, 
					     head.next,
					     v,
					     gb[w],
					     _EXP,
					     denom); // multiplies to denom.
	}
    }
  v = head.next;
  R->gbvector_remove_content(v, 0, denom);
  R->exponents_delete(_EXP);
#endif
}
