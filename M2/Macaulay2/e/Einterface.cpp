// Copyright 1999  Michael E. Stillman
#include "Einterface.hpp"
#include "ntuple.hpp"
#include "text_io.hpp"

EInterface::EInterface(const Ring *RR)
{
  const PolynomialRing *R1 = RR->cast_to_PolynomialRing();
  //if (R == 0) throw(expected_poly_ring);
  const PolynomialRing *R2 = R1->get_base_poly_ring();
  if (R2 == 0) 
    {
      R = R1;
      Rquotient = 0;
    }
  else
    {
      R = R2;
      Rquotient = R1;
    }
  K = R->Ncoeffs();
  M = R->Nmonoms();

  if (R1->is_quotient_ring())
    {
      Rquotient = R1;
    }
  else
    {
      Rquotient = 0;
    }

  RWeyl = R->cast_to_WeylAlgebra();

  one_ = K->from_int(1);
  minus_one_ = K->from_int(-1);

  nvars_ = M->n_vars();
}

void EInterface::divide_exponents(const exponent_vector *exp1,
				  const exponent_vector *exp2,
				  exponent_vector *result,
				  int &sign) const
{
  ntuple::divide(nvars_,exp1,exp2,result);
  if (M->is_skew())
    sign = M->skew_mult_sign(result,exp2);
}

void EInterface::exponent_syzygy(const exponent_vector *exp1,
				 const exponent_vector *exp2,
				 exponent_vector *result1,
				 exponent_vector *result2,
				 int &sign) const
{
  ntuple::syz(nvars_,exp1,exp2,result1,result2);
  // The following can be optimized
  if (M->is_skew())
    {
      emit_line("skew case not yet implemented");
      abort();
    }
}


Matrix EInterface::make_matrix(freemodule F, array< vec > &columns) const
{
  Matrix result(F);
  for (int i=0; i<columns.length(); i++)
    {
      result.append(columns[i]);
      columns[i] = 0;
    }
  return result;
}

void EInterface::cancel_lead_terms(vector_heap &fh, vector_heap &fsyzh,
				   const ringelement &coeff, 
				   const exponent_vector *mon,
				   const exponent_vector *flead,
				   const vec &g,
				   const vec &gsyz) const
{
  int *mCancelExp = new int[nvars_];
  if (true) //(!coeffs_QQ)
    {
      // In this case we assume that 'g' is monic.
      int sign = 1;  
      vec h,hsyz;
      divide_exponents(mon, flead, mCancelExp, sign);
        // sign is only set in skew commutative case.

      if (sign == 1)
	{
	  ringelement c = negate_coefficient(coeff);
	  h = mult_by_term(fh.get_target(), c, mCancelExp, g);
	  hsyz = mult_by_term(fsyzh.get_target(), c, mCancelExp, gsyz);
	  remove_coefficient(c);
	}
      else
	{
	  h = mult_by_term(fh.get_target(), coeff, mCancelExp, g);
	  hsyz = mult_by_term(fsyzh.get_target(), coeff, mCancelExp, gsyz);
	}
      fh.add(h);
      fsyzh.add(hsyz);
    }
  delete [] mCancelExp;
}

void EInterface::ring_cancel_lead_terms(vector_heap &h, vector_heap &hsyz,
					const ringelement &coeff, 
					const exponent_vector *mon,
					int comp,
					const polynomial &g) const
{
}


void EInterface::add_multiple_to(vector_heap &h, 
		     const ringelement &a, 
		     const exponent_vector *m,
		     const vec &v) const
{
  vec f = mult_by_term(h.get_target(),a,m,v);
  h.add(f);
}

void EInterface::add_ring_multiple_to(vector_heap &h, 
			  const ringelement &a, 
			  const exponent_vector *m,
			  int x,
			  const polynomial &r) const
{
  vec f = ring_mult_by_term(h.get_target(),a,m,x,r);
  h.add(f);
}

void EInterface::display_exponents(buffer &o, const exponent_vector *exp) const
{
  int *m = M->make_one();
  M->from_expvector(exp, m);
  M->elem_text_out(o,m);
  M->remove(m);
}

void EInterface::display_vector(buffer &o, const freemodule &F, const vec &f) const
{
  F->elem_text_out(o,f);
}
