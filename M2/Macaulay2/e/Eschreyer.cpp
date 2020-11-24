// Copyright 1999  Michael E. Stillman

#include "Eschreyer.hpp"
#include "matrix.hpp"
#include "text-io.hpp"
#include "gbring.hpp"
#include "matrix-con.hpp"

GBMatrix::GBMatrix(const FreeModule *F0) : F(F0) {}

GBMatrix::GBMatrix(const Matrix *m) : F(m->rows())
{
  const PolynomialRing *R = F->get_ring()->cast_to_PolynomialRing();
  assert(R != 0);
  for (int i = 0; i < m->n_cols(); i++)
    {
      ring_elem denom;
      gbvector *g = R->translate_gbvector_from_vec(F, m->elem(i), denom);
      append(g);
    }
}

void GBMatrix::append(gbvector *g) { elems.push_back(g); }

Matrix *GBMatrix::to_matrix()
{
  const PolynomialRing *R = F->get_ring()->cast_to_PolynomialRing();
  assert(R != 0);
  const FreeModule *G = FreeModule::make_schreyer(this);
  MatrixConstructor mat(F, G);
  for (int i = 0; i < elems.size(); i++)
    {
      vec v = R->translate_gbvector_to_vec(F, elems[i]);
      mat.set_column(i, v);
    }
  return mat.to_matrix();
}

GBKernelComputation::GBKernelComputation(const GBMatrix *m)
    : F(m->get_free_module()),
      n_ones(0),
      n_unique(0),
      n_others(0),
      total_reduce_count(0)
{
  const PolynomialRing *R0 = F->get_ring()->cast_to_PolynomialRing();
  GR = R0->get_gb_ring();
  R = R0;
  K = R->getCoefficients();
  M = R->getMonoid();

  G = FreeModule::make_schreyer(m);
  SF = F->get_schreyer_order();
  SG = G->get_schreyer_order();

  exp_size = EXPONENT_BYTE_SIZE(M->n_vars());
  monom_size = MONOMIAL_BYTE_SIZE(M->monomial_size());

  // Set 'gb'.
  strip_gb(m);
}

GBKernelComputation::~GBKernelComputation()
{
  // Remove gb
  // Remove syzygies
  // Remove mi
}

int GBKernelComputation::calc()
{
  // First find the skeleton
  for (int i = 0; i < gb.size(); i++) new_pairs(i);

  // Debug code
  GBMatrix *mm = new GBMatrix(G);
  for (int p = 0; p < syzygies.size(); p++)
    mm->append(GR->gbvector_copy(syzygies[p]));
  buffer o;
  Matrix *m = mm->to_matrix();
  if (M2_gbTrace >= 5)
    {
      o << "skeleton = " << newline;
      m->text_out(o);
      emit(o.str());
    }

  // Sort the skeleton now?

  // Now reduce each one of these elements
  for (int j = 0; j < syzygies.size(); j++)
    {
      gbvector *v = s_pair(syzygies[j]);
      reduce(v, syzygies[j]);
    }
  return COMP_DONE;
}

GBMatrix *GBKernelComputation::get_syzygies()
{
  // Make the Schreyer free module H.
  GBMatrix *result = new GBMatrix(G);
  for (int i = 0; i < syzygies.size(); i++)
    {
      result->append(syzygies[i]);
      syzygies[i] = 0;
    }
  return result;
}

//////////////////////
// Private routines //
//////////////////////
gbvector *GBKernelComputation::make_syz_term(ring_elem c,
                                             const int *m,
                                             int comp) const
// c is an element of GR->get_flattened_coefficients()
// (m,comp) is a Schreyer encoded monomial in Fsyz
{
#ifdef DEVELOPMENT
#warning "this might add in the Schreyer up"
#warning "the previous warning message is confusing, grammatically"
#endif

  return GR->gbvector_raw_term(c, m, comp);
}

void GBKernelComputation::strip_gb(const GBMatrix *m)
{
  const VECTOR(gbvector *) &g = m->elems;
  int i;
  int *components = newarray_atomic_clear(int, F->rank());
  for (i = 0; i < g.size(); i++)
    if (g[i] != 0) components[g[i]->comp - 1]++;

  for (i = 0; i < g.size(); i++)
    {
      gbvector head;
      gbvector *last = &head;
      for (gbvector *v = g[i]; v != 0; v = v->next)
        if (components[v->comp - 1] > 0)
          {
            gbvector *t = GR->gbvector_copy_term(v);
            last->next = t;
            last = t;
          }
      last->next = 0;
      gb.push_back(head.next);
    }
  for (i = 0; i < F->rank(); i++) mi.push_back(new MonomialIdeal(R));
  deletearray(components);
}

void GBKernelComputation::new_pairs(int i)
// Create and insert all of the pairs which will have lead term 'gb[i]'.
// This also places 'in(gb[i])' into the appropriate monomial ideal
{
  Index<MonomialIdeal> j;
  queue<Bag *> elems;
  intarray vp;  // This is 'p'.
  intarray thisvp;

  if (SF)
    {
      monomial PAIRS_mon = ALLOCATE_MONOMIAL(monom_size);
      SF->schreyer_down(gb[i]->monom, gb[i]->comp - 1, PAIRS_mon);
      M->to_varpower(PAIRS_mon, vp);
    }
  else
    M->to_varpower(gb[i]->monom, vp);

  // First add in syzygies arising from exterior variables
  // At the moment, there are none of this sort.

  if (R->is_skew_commutative())
    {
      int *find_pairs_exp = newarray_atomic(int, M->n_vars());

      varpower::to_ntuple(M->n_vars(), vp.raw(), find_pairs_exp);
      for (int w = 0; w < R->n_skew_commutative_vars(); w++)
        if (find_pairs_exp[R->skew_variable(w)] > 0)
          {
            thisvp.shrink(0);
            varpower::var(w, 1, thisvp);
            Bag *b = new Bag(static_cast<void *>(0), thisvp);
            elems.insert(b);
          }

      deletearray(find_pairs_exp);
    }

  // Second, add in syzygies arising from the base ring, if any
  // The baggage of each of these is NULL
  if (R->is_quotient_ring())
    {
      const MonomialIdeal *Rideal = R->get_quotient_monomials();
      for (j = Rideal->first(); j.valid(); j++)
        {
          // Compute (P->quotient_ideal->monom : p->monom)
          // and place this into a varpower and Bag, placing
          // that into 'elems'
          thisvp.shrink(0);
          varpower::quotient((*Rideal)[j]->monom().raw(), vp.raw(), thisvp);
          if (varpower::is_equal((*Rideal)[j]->monom().raw(), thisvp.raw()))
            continue;
          Bag *b = new Bag(static_cast<void *>(0), thisvp);
          elems.insert(b);
        }
    }

  // Third, add in syzygies arising from previous elements of this same level
  // The baggage of each of these is their corresponding res2_pair

  MonomialIdeal *mi_orig = mi[gb[i]->comp - 1];
  for (j = mi_orig->first(); j.valid(); j++)
    {
      Bag *b = new Bag();
      varpower::quotient((*mi_orig)[j]->monom().raw(), vp.raw(), b->monom());
      elems.insert(b);
    }

  // Make this monomial ideal, and then run through each minimal generator
  // and insert into the proper degree. (Notice that sorting does not
  // need to be done yet: only once that degree is about to begin.

  mi_orig->insert_minimal(new Bag(i, vp));

  queue<Bag *> rejects;
  Bag *b;
  MonomialIdeal *new_mi = new MonomialIdeal(R, elems, rejects);
  while (rejects.remove(b)) deleteitem(b);

  int *m = M->make_one();
  for (j = new_mi->first(); j.valid(); j++)
    {
      M->from_varpower((*new_mi)[j]->monom().raw(), m);
      M->mult(m, gb[i]->monom, m);

      gbvector *q = make_syz_term(
          GR->get_flattened_coefficients()->from_long(1), m, i + 1);
      syzygies.push_back(q);
    }
}

//////////////////////////////////////////////
//  S-pairs and reduction ////////////////////
//////////////////////////////////////////////

bool GBKernelComputation::find_ring_divisor(const int *exp,
                                            const gbvector *&result)
// If 'exp' is divisible by a ring lead term, then 1 is returned,
// and result is set to be that ring element.
// Otherwise 0 is returned.
{
  if (!R->is_quotient_ring()) return false;
  Bag *b;
  if (!R->get_quotient_monomials()->search_expvector(exp, b)) return false;
  int index = b->basis_elem();
  result = R->quotient_gbvector(index);
  return true;
}

int GBKernelComputation::find_divisor(const MonomialIdeal *this_mi,
                                      const int *exp,
                                      int &result)
{
  // Find all the posible matches, use some criterion for finding the best...
  VECTOR(Bag *) bb;
  this_mi->find_all_divisors(exp, bb);
  int ndivisors = bb.size();
  if (ndivisors == 0) return 0;
  result = bb[0]->basis_elem();
  // Now search through, and find the best one.  If only one, just return it.
  if (M2_gbTrace >= 5)
    if (this_mi->length() > 1)
      {
        buffer o;
        o << ":" << this_mi->length() << "." << ndivisors << ":";
        emit(o.str());
      }
  if (ndivisors == 1)
    {
      if (this_mi->length() == 1)
        n_ones++;
      else
        n_unique++;
      return 1;
    }
  n_others++;

  int lowest = result;
  for (int i = 1; i < ndivisors; i++)
    {
      int p = bb[i]->basis_elem();
      if (p < lowest) lowest = p;
    }
  result = lowest;
  return ndivisors;
}

gbvector *GBKernelComputation::s_pair(gbvector *gsyz)
{
  gbvector *result = NULL;
  int *si = M->make_one();
  for (gbvector *f = gsyz; f != 0; f = f->next)
    {
      SG->schreyer_down(f->monom, f->comp - 1, si);
      gbvector *h = GR->mult_by_term(F, gb[f->comp - 1], f->coeff, si, 0);
      wipe_unneeded_terms(h);
      GR->gbvector_add_to(F, result, h);
    }
  M->remove(si);
  return result;
}

void GBKernelComputation::wipe_unneeded_terms(gbvector *&f)
{
  // Remove every term of f (except the lead term)
  // which is NOT divisible by an element of mi.
  int *exp = newarray_atomic(int, GR->n_vars());
  int nterms = 1;
  int nsaved = 0;
  gbvector *g = f;
  while (g->next != 0)
    {
      // First check to see if the term g->next is in the monideal
      nterms++;
      Bag *b;
      GR->gbvector_get_lead_exponents(F, g->next, exp);
      if (mi[g->next->comp - 1]->search_expvector(exp, b))
        {
          // Want to keep the monomial
          g = g->next;
        }
      else
        {
          // Want to dump this term
          nsaved++;
          gbvector *tmp = g->next;
          g->next = tmp->next;
          tmp->next = 0;
          GR->gbvector_remove(tmp);
        }
    }
#if 0
//   if (M2_gbTrace >= 5)
//     {
//       buffer o;
//       o << "[" << nterms << ",s" << nsaved << "]";
//       emit_wrapped(o.str());
//     }
#endif
}

void GBKernelComputation::reduce(gbvector *&f, gbvector *&fsyz)
{
  exponents REDUCE_exp = ALLOCATE_EXPONENTS(exp_size);
  monomial REDUCE_mon = ALLOCATE_MONOMIAL(monom_size);

  const Ring *gbringK = GR->get_flattened_coefficients();
  ring_elem one = gbringK->from_long(1);
  gbvector *lastterm = fsyz;  // fsyz has only ONE term.
  const gbvector *r;
  int q;

  int nhits = 0;
  int nremoved = 0;
  int max_len = GR->gbvector_n_terms(f);

  int count = 0;
  if (M2_gbTrace >= 4) emit_wrapped(",");

  while (f != NULL)
    {
      if (SF)
        {
          SF->schreyer_down(f->monom, f->comp - 1, REDUCE_mon);
          M->to_expvector(REDUCE_mon, REDUCE_exp);
        }
      else
        M->to_expvector(f->monom, REDUCE_exp);
      if (find_ring_divisor(REDUCE_exp, r))
        {
          ring_elem u, v;
          // Subtract off f, leave fsyz alone
          M->divide(f->monom, r->monom, REDUCE_mon);
          gbringK->syzygy(f->coeff, r->coeff, u, v);
          gbvector *h = GR->mult_by_term(F, r, v, REDUCE_mon, f->comp);
          GR->gbvector_add_to(F, f, h);
          if (!gbringK->is_equal(u, one))
            {
              GR->gbvector_mult_by_coeff_to(fsyz, u);
              GR->gbvector_mult_by_coeff_to(f, u);
              gbringK->remove(u);
              gbringK->remove(v);
            }
          total_reduce_count++;
          count++;
        }
      else if (find_divisor(mi[f->comp - 1], REDUCE_exp, q))
        {
          ring_elem u, v;
          gbringK->syzygy(f->coeff, gb[q]->coeff, u, v);
          M->divide(f->monom, gb[q]->monom, REDUCE_mon);
          gbvector *h = GR->mult_by_term(F, gb[q], v, REDUCE_mon, 0);
          if (!gbringK->is_equal(u, one))
            {
              GR->gbvector_mult_by_coeff_to(fsyz, u);
              GR->gbvector_mult_by_coeff_to(f, u);
            }
          int n1 = GR->gbvector_n_terms(h);
          wipe_unneeded_terms(h);
          int n2 = GR->gbvector_n_terms(h);
          nremoved += (n1 - n2);
          lastterm->next = make_syz_term(v, f->monom, q + 1);  // grabs v.
          lastterm = lastterm->next;
          gbringK->remove(u);
          gbringK->remove(v);
          int n3 = GR->gbvector_n_terms(f);
          GR->gbvector_add_to(F, f, h);
          int n4 = GR->gbvector_n_terms(f);
          if (n4 > max_len) max_len = n4;
          nhits +=
              (n2 + n3 - n4 -
               2);  // the -2 is to avoid counting the lead term cancellations.
          total_reduce_count++;
          count++;
        }
      else
        {
          // To get here is an ERROR!
          f = f->next;  // Just to not have an infinite loop
          emit_line(
              "error in Schreyer reduction: element does not reduce to zero!");
        }
    }

  if (M2_gbTrace >= 4)
    {
      buffer o;
      o << count;
      o << "[" << max_len << " " << nhits << " " << nremoved << "]";
      emit_wrapped(o.str());
    }
}

void GBKernelComputation::geo_reduce(gbvector *&f, gbvector *&fsyz)
{
  exponents REDUCE_exp = ALLOCATE_EXPONENTS(exp_size);
  monomial REDUCE_mon = ALLOCATE_MONOMIAL(monom_size);

  const Ring *gbringK = GR->get_flattened_coefficients();
  ring_elem one = gbringK->from_long(1);
  gbvector *lastterm = fsyz;  // fsyz has only ONE term.
  const gbvector *r;
  gbvectorHeap fb(GR, F);
  fb.add(f);
  f = NULL;
  const gbvector *lead;
  int q;

  int count = 0;
  if (M2_gbTrace >= 4) emit_wrapped(",");

  while ((lead = fb.get_lead_term()) != NULL)
    {
      if (SF)
        {
          SF->schreyer_down(lead->monom, lead->comp - 1, REDUCE_mon);
          M->to_expvector(REDUCE_mon, REDUCE_exp);
        }
      else
        M->to_expvector(lead->monom, REDUCE_exp);
      if (find_ring_divisor(REDUCE_exp, r))
        {
          ring_elem u, v;
          // Subtract off f, leave fsyz alone
          M->divide(lead->monom, r->monom, REDUCE_mon);

          gbringK->syzygy(f->coeff, r->coeff, u, v);
          gbvector *h = GR->mult_by_term(F, r, v, REDUCE_mon, f->comp);
          fb.add(h);
          if (!gbringK->is_equal(u, one))
            {
              GR->gbvector_mult_by_coeff_to(fsyz, u);
              GR->gbvector_mult_by_coeff_to(f, u);
              gbringK->remove(u);
              gbringK->remove(v);
            }

          fb.add(h);
          total_reduce_count++;
          count++;
        }
      else if (find_divisor(mi[lead->comp - 1], REDUCE_exp, q))
        {
          ring_elem u, v;
          gbringK->syzygy(f->coeff, gb[q]->coeff, u, v);
          M->divide(lead->monom, gb[q]->monom, REDUCE_mon);
          gbvector *h = GR->mult_by_term(F, gb[q], v, REDUCE_mon, 0);
          if (!gbringK->is_equal(u, one))
            {
              GR->gbvector_mult_by_coeff_to(fsyz, u);
              GR->gbvector_mult_by_coeff_to(f, u);
            }
          wipe_unneeded_terms(h);
          lastterm->next = make_syz_term(v, lead->monom, q + 1);  // grabs v.
          lastterm = lastterm->next;
          fb.add(h);
          total_reduce_count++;
          count++;
        }
      else
        {
          // To get here is an ERROR!
          fb.remove_lead_term();
          emit_line(
              "error in Schreyer reduction: element does not reduce to zero!");
        }
    }

  if (M2_gbTrace >= 4)
    {
      buffer o;
      o << count;
      emit_wrapped(o.str());
    }
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
