// Copyright 2002 Michael E. Stillman

#include "monomial.hpp"
#include "monideal.hpp"
#include "matrix.hpp"
#include "engine.h"

const MonomialIdeal *IM2_MonomialIdeal_make(const Matrix *m, int n)
{
  return m->make_monideal(n);
}

const Matrix *IM2_MonomialIdeal_to_matrix(const MonomialIdeal *I)
{
  return Matrix::make(I);
}

M2_string MonomialIdeal_to_string(const MonomialIdeal *I)
{
  buffer o;
  I->text_out(o);
  return o.to_string();
}

int IM2_MonomialIdeal_n_gens(const MonomialIdeal *I)
{
  return I->length();
}

M2_bool IM2_MonomialIdeal_is_equal(const MonomialIdeal *I1, const MonomialIdeal *I2)
{
  return I1->is_equal(*I2);
}

const MonomialIdeal *IM2_MonomialIdeal_radical(const MonomialIdeal *I)
{
  return I->radical();
}

const MonomialIdeal *IM2_MonomialIdeal_add(const MonomialIdeal *I, 
					   const MonomialIdeal *J)
{
  return (*I) + (*J);
}

const MonomialIdeal *IM2_MonomialIdeal_product(const MonomialIdeal *I, 
					       const MonomialIdeal *J)
{
  return (*I) * (*J);
}

const MonomialIdeal *IM2_MonomialIdeal_intersect(const MonomialIdeal *I, 
						 const MonomialIdeal *J)
{
  return I->intersect(*J);
}

const MonomialIdeal *IM2_MonomialIdeal_quotient1(const MonomialIdeal *I, 
						 const Monomial *a)
{
  return I->quotient(a->ints());
}

const MonomialIdeal *IM2_MonomialIdeal_quotient(const MonomialIdeal *I, 
						const MonomialIdeal *J)
{
  return I->quotient(*J);
}

const MonomialIdeal *IM2_MonomialIdeal_sat1(const MonomialIdeal *I, 
					    const Monomial *a)
{
  return I->erase(a->ints());
}

const MonomialIdeal *IM2_MonomialIdeal_sat(const MonomialIdeal *I, 
					   const MonomialIdeal *J)
{
  return I->sat(*J);
}

const MonomialIdeal *IM2_MonomialIdeal_borel(const MonomialIdeal *I)
{
  return I->borel();
}

M2_bool IM2_MonomialIdeal_is_borel(const MonomialIdeal *I)
{
  return I->is_borel();
}

int IM2_MonomialIdeal_codim(const MonomialIdeal *I)
{
  return I->codim();
}

const MonomialIdeal *IM2_MonomialIdeal_assprimes(const MonomialIdeal *I)
{
  return I->assprimes();
}


// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e"
// End:
