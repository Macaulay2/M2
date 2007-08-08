/* Copyright 2007, Michael E. Stillman */

#include "gb-walk.hpp"
#include "matrix.hpp"
#include "monordering.h"

GBWalker::GBWalker(const Matrix *gb_under_order1,
			    const MonomialOrdering *order1)
{
}

GBWalker * GBWalker::create(const Matrix *gb_under_order1,
			    const MonomialOrdering *order1)
{
  // MES: TO WRITE
  return new GBWalker(gb_under_order1, order1);
}

GBWalker::~GBWalker()
{
  // MES: TO WRITE
}

bool GBWalker::stop_conditions_ok()
{
  // MES: TO WRITE
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
  // MES: TO WRITE
}

const PolynomialRing *GBWalker::get_ring() 
{ 
  // MES: TO WRITE
  return 0; 
}

ComputationOrNull *GBWalker::set_hilbert_function(const RingElement *h)
{
  // MES: TO WRITE
  return 0;
}

const MatrixOrNull *GBWalker::get_gb()
{
  // MES: TO WRITE
  return 0;
}

const MatrixOrNull *GBWalker::get_mingens()
{
  // MES: TO WRITE
  return 0;
}

const MatrixOrNull *GBWalker::get_change()
{
  // MES: TO WRITE
  return 0;
}

const MatrixOrNull *GBWalker::get_syzygies()
{
  // MES: TO WRITE
  return 0;
}

const MatrixOrNull *GBWalker::get_initial(int nparts)
{
  // MES: TO WRITE
  return 0;
}

const MatrixOrNull *GBWalker::get_parallel_lead_terms(M2_arrayint w)
{
  // MES: TO WRITE
  return 0;
}

const MatrixOrNull *GBWalker::matrix_remainder(const Matrix *m)
{
  // MES: TO WRITE
  return 0;
}

void GBWalker::matrix_lift(const Matrix *m,
		 MatrixOrNull **result_remainder,
		 MatrixOrNull **result_quotient
		 )
{
  // MES: TO WRITE
}

int GBWalker::contains(const Matrix *m)
{
  // MES: TO WRITE
  return -1;
}

void GBWalker::text_out(buffer &o) const
  /* This displays statistical information, and depends on the
     gbTrace value */
{
  // MES: TO WRITE
}

void GBWalker::show() const
  /* This displays statistical information, and depends on the
     gbTrace value */
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
// End:
