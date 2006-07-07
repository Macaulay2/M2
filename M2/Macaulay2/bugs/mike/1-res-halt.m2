    R = ZZ[x,y]
    f = random(R^2,R^{4:-2})
    gbTrace = 3
    res coker f

slows to a strange halt looping forever here:

	void res_comp::reduce_gen(resterm * &f) const
	{
	  res_pair *q;
	  ring_elem rg;
	  Bag *b;

*	  while (f != NULL)
*	    {
*	      M->divide(f->monom, f->comp->base_monom, REDUCE_mon);
*	      M->to_expvector(REDUCE_mon, REDUCE_exp);
*	      if (find_ring_divisor(REDUCE_exp, rg))
		{
		  // Subtract off f, leave fsyz alone
		  Nterm *r = rg;
		  M->divide(f->monom, r->monom, REDUCE_mon);
		  R->ring_subtract_multiple_to(f, f->coeff, REDUCE_mon, f->comp, rg);
		}
*	      else if (search_mi[f->comp->me]->search_expvector(REDUCE_exp, b))
*		{
*		  q = reinterpret_cast<res_pair *>(b->basis_ptr());
*		  M->divide(f->monom, q->syz->monom, REDUCE_mon);
*		  R->subtract_multiple_to(f, f->coeff, REDUCE_mon, q->syz);
*		}
	      else
		break;			// MES: possibly auto reduce further...
	    }
	}


-----------------------------------------------------------------------------

+ M2 --no-readline --print-width 189
Macaulay 2, version 0.9.20
with packages: Classic, Elimination, LLLBases, PrimaryDecomposition, SchurRings, TangentCone

i1 : input "bug.m2"

ii2 : R = ZZ[x,y]

oo2 = R

oo2 : PolynomialRing

ii3 : f = random(R^2,R^{4:-2})

oo3 = | -9x2-9xy    4x2-10xy-4y2 9x2-5xy-6y2 -7x2+4xy-3y2 |
      | 7x2+8xy-9y2 6x2-2y2      4x2-4xy+3y2 -9x2-4xy+7y2 |

              2       4
oo3 : Matrix R  <--- R

ii4 : gbTrace = 3

oo4 = 3

ii5 : res coker f
resolution algorithm 1
   -- 
   -- {0}z