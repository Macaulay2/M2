// Copyright 1997  Michael E. Stillman

#include "poly.hpp"
#include "sagbi.hpp"
#include "matrix-con.hpp"

ring_elem sagbi::subduct(int numslots,
                         const PolyRing *R,
                         ring_elem a,
                         const RingMap *phi,
                         GBComputation *J)
{
  Nterm *f = a;
  Nterm head;
  Nterm *result = &head;
  MatrixConstructor mat(R->make_FreeModule(1), 1);

  while (f != NULL)
    {
      Nterm *g = f;
      f = f->next;
      g->next = NULL;

      mat.set_entry(0, 0, g);
      Matrix *m = mat.to_matrix();
      const Matrix *n = J->matrix_remainder(m);
      ring_elem g1 = n->elem(0, 0);
      delete m;
      delete n;

      // Is g1 a monomial in the new variables?
      if (R->in_subring(numslots, g1))
        {
          g->next = f;
          f = g;
          ring_elem phi_g1 = R->eval(phi, g1, 0);
          ring_elem fr = f;
          R->internal_subtract_to(fr, phi_g1);
          f = fr;
        }
      else
        {
          result->next = g;
          result = g;
        }
    }

  result->next = NULL;
  return head.next;
}

Matrix *sagbi::subduct(int numparts, const Matrix *m, const RingMap *phi, GBComputation *J)
{
  MatrixConstructor result(m->rows(), m->cols());
  const PolyRing *R = m->get_ring()->cast_to_PolyRing();
  if (R == 0)
    {
      ERROR("expected polynomial ring");
      return 0;
    }
  int nslots = R->getMonoid()->n_slots(numparts);
  for (int i = 0; i < m->n_cols(); i++)
    {
      ring_elem a = m->elem(0, i);
      ring_elem b = subduct(nslots, R, R->copy(a), phi, J);
      result.set_entry(0, i, b);
    }
  return result.to_matrix();
}

#ifdef DEVELOPMENT
#warning "sagbi code commented out"
#endif
#if 0
// #include "sagbi.hpp"
//
// vec sagbi::subduct(const FreeModule *F,
//                 vec f,
//                 const RingMap *phi,
//                 gb_comp *J)
// {
//   vecterm head;
//   vec result = &head;
//
// #ifdef DEVELOPMENT
// #warning "subduct required Vector reduction: rewrite"
// #endif
// #if 0
// //   while (f != NULL)
// //     {
// //       vec g = f;
// //       f = f->next;
// //       g->next = NULL;
// //
// //       Vector *gv = Vector::make_raw(F,F->copy(g));
// //       Vector *junk;
// //       Vector *g1v = J->reduce(gv, junk);
// //       vec g1 = g1v->get_value();
// //
// //       // Is g1 a monomial in the new variables?
// //       if (F->in_subring(1,g1))
// //   {
// //     g->next = f;
// //     f = g;
// //     vec phi_g1 = F->eval(phi,F,g1);
// //     F->subtract_to(f, phi_g1);
// //   }
// //       else
// //   {
// //     result->next = g;
// //     result = g;
// //   }
// //     }
// #endif
//
//   result->next = NULL;
//   return head.next;
// }
//
// Matrix *sagbi::subduct(const Matrix *m,
//                      const RingMap *phi,
//                      gb_comp *J)
// {
//   Matrix *result = new Matrix(m->rows(), m->cols());
//
//   for (int i=0; i<m->n_cols(); i++)
//     (*result)[i] = subduct(m->rows(), m->rows()->copy((*m)[i]), phi, J);
//   return result;
// }
//
// #if 0
// // //////////////////
// // // pending_list //
// // //////////////////
// //
// // pending_list::pending_list(Matrix &m)
// //   : F(m.rows()),
// //     _n_held(0)
// // {
// //   _base_degree = F->lowest_primary_degree() ;
// //   _lo_degree = _base_degree-1;
// //   insert(m);
// // }
// //
// // pending_list::~pending_list()
// // {
// // }
// //
// // pending_list::insert(Matrix &m)
// // {
// //   for (int i = 0; i < m.n_cols(); i++) {
// //     vec v = m[i];
// //     if (v == NULL) continue;
// //     _n_held++;
// //     int d = F->primary_degree(v);
// //     if (d < _lo_degree) _lo_degree = d;
// //     d -= _base_degree;
// //     while (Pending.length() <= d)
// //       Pending.append(Matrix(F));
// //     Pending[d].append(F->copy(v));
// //   }
// // }
// //
// // Matrix pending_list::take_lowest_matrix()
// // {
// //   if (_lo_degree < _base_degree) return Matrix(F);
// //   int d = _lo_degree - _base_degree;
// //   Matrix result = Pending[d];
// //   n_held -= result.n_cols();
// //   Pending[d] = Matrix(F);
// //   for ( ; d < Pending.length(); d++)
// //     if (Pending[d].n_cols() > 0)
// //       {
// //   _lo_degree = d + _base_degree;
// //   return result;
// //       }
// //   _lo_degree = _base_degree - 1;
// // }
// #endif
//
// sagbi_comp::sagbi_comp(const Matrix *m) : gb_comp(COMP_SAGBI)
// {
// }
//
// sagbi_comp::~sagbi_comp()
// {
// }
//
// void sagbi_comp::enlarge(const Ring *R, int *wts)
// {
// }
//
// void sagbi_comp::add_generators(const Matrix *m)
// {
// }
//
// #if 0
// // polynomial_ring *sagbi_comp::extend_ring(const polynomial_ring *R, intarray &degs)
// // {
// //
// //   // Create the ring k[x,y], where x = variables of R, and y = new variables of
// //   // length degs.length():
// //   // monomial order = elimination order (or product order?) eliminating variables of R.
// //   // degrees of new variables: coming from 'degs'
// //   // This routine should handle the case where R is a quotient poly ring, or R is just K.
// //   const ring *K = R->getCoefficientRing();
// //   degree_monoid *D = R->degree_monoid();
// //   monorder mo =
// //   mon_info mi =
// //   monoid *M =
// //   oldRS = RS;
// //   RS = new PolynomialRing(K,M);
// // }
// #endif
// #if 0
// //
// // void sagbi_comp::append_to_basis(Matrix *m)
// // {
// //   // Each of the elements in 'm' are to be added in.
// //   if (m->n_cols() == 0) return;
// //
// //   // Append m to the basis so far.
// //
// //   // Make the ring RS = k[x,y]
// //
// //   // Make the ideal(x_i - in(f_i))
// //
// //   // Make the ring map RS --> R
// //
// //   // Extend the binomial ring
// //
// //   freemem(oldRS);
// //
// //   // Add the (xi - in(fi)) into this binomial comp.
// // }
// #endif
// #if 0
// // Matrix sagbi_comp::grab_lowest_degree()
// // {
// //
// // /* This routine assumes that lowest degree of Pending list is autosubducted.
// //    It then row reduces the lowest degree of the Pending list, possibly
// //    causing new elements of even lower degree.
// //    Once the Pending list is row reduced in the lowest degree, the routine
// //    removes the lowest degree from the Pending list and returns it.
// //    It also updates _current_degree to this lowest degree */
// //
// //
// //   // This should only be called if there are elements here...?
// //   Matrix temp = Pending->take_lowest_matrix();
// //   row_reduce(temp);
// //   Pending->insert(temp);
// //   _current_degree = Pending->lo_degree();
// //   return Pending->take_lowest_matrix();
// // }
// #endif
//
// int sagbi_comp::calc(const int *deg, const intarray &stop_conditions)
// {
// #if 0
// //   // fields: _max_gen_degree, _J_status
// //   // routines: J->is_done()
// //   intarray gbstop;
// //   int maxnloops = stop_conditions[0];
// //   nloops = 0;
// //   for (;;)
// //     {
// //       // Various ending conditions
// //       if (Pending->n_held() == 0 && J->is_done() && _current_degree > _max_gen_degree)
// //   return COMP_DONE;
// //       if (++nloops > maxnloops) return COMP_DONE_STEPS;
// //       if (*deg && (_current_degree > *deg)) return COMP_DONE_DEGREE_LIMIT;
// //       if (system_interrupted()) return COMP_INTERRUPTED;
// //
// //       // Determine S-pairs
// //       ret = find_pairs();  // sets _new_pairs, uses _current_degree.
// //       if (ret != COMP_DONE) return ret;
// //
// //       // Reduce S-pairs
// //       ret = reduce_pairs();  // sets _reduced_pairs, resets _new_pairs.
// //       if (ret != COMP_DONE) return ret;
// //
// //       // Determine S-pairs
// //       _J_status = J->calc(&_current_degree, gbstop);
// //       if (_J_status == COMP_INTERRUPTED) return COMP_INTERRUPTED;
// //       newpairs = J->subring(_current_dgree); // This only grabs minimal generators, of the given degree.
// //       newpairs = evaluatePairs(newpairs);    // Creates matrix over R.  Sends y_i to f_i.
// //       Matrix newgens = Pending->take_matrix(_current_degree);
// //       newpairs.concat(newgens);
// //
// //       // Reduce S-pairs
// //       newpairs = autosubduct(newpairs);  // This should inter-reduce elements as well...
// //       Pending->insert(newpairs);
// //       if (Pending->lo_degree() < _current_degree)
// //   _current_degree = Pending->lo_degree();
// //
// //       // At this point, the lowest degree occurring in 'Pending' should be
// //       // added to the GB.
// //       append_to_basis(Pending->take_matrix(_current_degree));
// //
// //       _current_degree++;
// //     }
// #else
//      return 0;
// #endif
// }
//
// Matrix *sagbi_comp::reduce(const Matrix *m, Matrix *&lift)
// {
// return 0;
// }
//
// int sagbi_comp::contains(const Matrix *m)
// {
// return 0;
// }
//
// bool sagbi_comp::is_equal(const gb_comp *q)
// {
// return 0;
// }
//
// // obtaining: mingens matrix, GB matrix, change of basis matrix, stats.
// Matrix *sagbi_comp::min_gens_matrix()
// {
// return 0;
// }
//
// Matrix *sagbi_comp::initial_matrix(int n)
// {
// return 0;
// }
//
// Matrix *sagbi_comp::gb_matrix()
// {
// return 0;
// }
//
// Matrix *sagbi_comp::change_matrix()
// {
// return 0;
// }
//
// Matrix *sagbi_comp::syz_matrix()
// {
// return 0;
// }
//
// void sagbi_comp::stats() const
// {
// }
#endif
// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
