/* Copyright 2007, Michael E. Stillman */

#include "gb-walk.hpp"

#include <assert.h>

#include "comp-gb-declared.hpp"
#include "error.h"
#include "gbring.hpp"
#include "interface/computation.h"
#include "interface/monomial-ordering.h"
#include "reducedgb-marked.hpp"

class Computation;
class Matrix;
class buffer;
class RingElement;

class MonomialOrderMatrix
{
  int nvars;
  long **order;

 public:
  MonomialOrderMatrix(const MonomialOrdering *mo);
  ~MonomialOrderMatrix();

  const long *part(int i) const
  {
    assert(i < nvars);
    return order[i];
  }
  int compare(long *m1, long *m2) const;

  int minpart(long *m) const;
  // returns the smallest integer i such that part(j) . m = 0 for j < i

  long value(int i, long *monom) const;
  // return part(i) . monom

  MonomialOrdering *toMonomialOrdering() const;  // TODO

  // Routines needed:
  int facet_compare(long *mon1, long *mon2);  // TODO
};

////////////////////////////////

GBWalker::GBWalker(MarkedGB *G0, long **order1, long **order2)
    : R(G0->get_gb_ring()),
      F(G0->get_ambient_FreeModule()),
      G(G0),
      monorder1(order1),  // or create this from the monomial order?
      monorder2(order2),
      ww(0)
{
  // TODO: need to set what else?
  //
}

GBWalker::GBWalker(const Matrix *gb_under_order1,
                   const MonomialOrdering *order1)
{
}

GBWalker *GBWalker::create(const Matrix *gb_under_order1,
                           const MonomialOrdering *order1)
{
  // TODO MES: TO WRITE
  return new GBWalker(gb_under_order1, order1);
}

GBWalker::~GBWalker()
{
  // TODO MES: TO WRITE
}

bool GBWalker::stop_conditions_ok()
{
  // TODO MES: TO WRITE
  return true;
}

GBComputation *GBWalker::make_gb(const Matrix *M) const
// return the GB of g, keep = 0 or 1.
{
  M2_arrayint weights = M2_makearrayint(R->n_vars());
  for (int i = 0; i < R->n_vars(); i++) weights->array[i] = 1;

  GBComputation *G0 = GBComputation::choose_gb(M,
                                               false,  // collect syz
                                               -1,
                                               weights,
                                               false,
                                               -1,
                                               0,
                                               0
                                               /* , max_reduction_count */
                                               );
  G0->set_stop_conditions(false,
                          NULL,
                          -1,
                          -1,  // syzygy limit
                          -1,
                          -1,
                          -1,
                          false,
                          NULL);
  return G0;
}

void GBWalker::initialize()
{
  // Initialize the local variables of the computation
  state = STATE_compute_w;
  ww = 0;
  inwwG = 0;
  gb_inwwG = 0;
  next_to_reduce = 0;
  // G is already set
  G1 = 0;
}

bool GBWalker::compute_next_w()
// Uses the value of w in the class, and looks at every term of every poly
// in the marked GB which is > the marked term (in order2) trying to find
// the next w to use.
// Returns: true if a w is found
{
  // TODO MES: TO WRITE
  return true;
}

//////////////////////////////////////////////////////
// GBComputation and Computation inherited routines //
//////////////////////////////////////////////////////
void GBWalker::remove_gb()
{
  // MES: TO WRITE
}

void GBWalker::start_computation()
{
  if (stop_.always_stop) return;  // don't change status

  for (;;) switch (state)
      {
        case STATE_compute_w:
          if (!compute_next_w())
            {
              // We are done!
              state = STATE_done;
              set_status(COMP_DONE);
              return;
            }
          inwwG = G->get_parallel_lead_terms(ww);
          gb_inwwG = make_gb(inwwG);
          state = STATE_do_gb;
        case STATE_do_gb:
          // Now compute the GB object.  If not interrupted, go on:
          gb_inwwG->start_computation();
          if (gb_inwwG->status() == COMP_INTERRUPTED)
            {
              set_status(COMP_INTERRUPTED);
              return;
            }
          next_to_reduce = 0;
          state = STATE_reduce;
        case STATE_reduce:
          while (next_to_reduce < 0)  // TODO: consider the top of the loop
            {
              H = G->matrix_remainder(
                  gb_inwwG->get_gb());  // Not quite: need to subtract...
              next_to_reduce++;
            }
          state = STATE_autoreduce;
        case STATE_autoreduce:
          G->remove_gb();
          delete G;
          G1 = static_cast<MarkedGB *>(
              GBDeclared::create(gb_inwwG->get_initial(-1), H, H, 0, 0));
          state = STATE_compute_w;
        case STATE_done:
          set_status(COMP_DONE);
          return;
      }
}

const PolynomialRing *GBWalker::get_ring() const
{
  // MES: TO WRITE
  return 0;
}

Computation /* or null */ *GBWalker::set_hilbert_function(const RingElement *h)
{
  // MES: TO WRITE
  return 0;
}

const Matrix /* or null */ *GBWalker::get_gb()
{
  // MES: TO WRITE
  return 0;
}

const Matrix /* or null */ *GBWalker::get_mingens()
{
  // MES: TO WRITE
  return 0;
}

const Matrix /* or null */ *GBWalker::get_change()
{
  // MES: TO WRITE
  return 0;
}

const Matrix /* or null */ *GBWalker::get_syzygies()
{
  // MES: TO WRITE
  return 0;
}

const Matrix /* or null */ *GBWalker::get_initial(int nparts)
{
  // MES: TO WRITE
  return 0;
}

const Matrix /* or null */ *GBWalker::get_parallel_lead_terms(M2_arrayint w)
{
  // MES: TO WRITE
  return 0;
}

const Matrix /* or null */ *GBWalker::matrix_remainder(const Matrix *m)
{
  // MES: TO WRITE
  return 0;
}

M2_bool GBWalker::matrix_lift(const Matrix *m,
                              const Matrix /* or null */ **result_remainder,
                              const Matrix /* or null */ **result_quotient)
{
  // MES: TO WRITE, should this be written?
  *result_remainder = 0;
  *result_quotient = 0;
  ERROR("rawGBMatrixLift not implemented for GB walks");
  return false;
}

int GBWalker::contains(const Matrix *m)
{
  // MES: TO WRITE
  return -1;
}

void GBWalker::text_out(buffer &o) const
/* This displays statistical information, and depends on the
   M2_gbTrace value */
{
  // MES: TO WRITE
}

void GBWalker::show() const
/* This displays statistical information, and depends on the
   M2_gbTrace value */
{
  // MES: TO WRITE
}

int GBWalker::complete_thru_degree() const
// The computation is complete up through this degree.
{
  // MES: TO WRITE
  return 0;
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e"
// indent-tabs-mode: nil
// End:
