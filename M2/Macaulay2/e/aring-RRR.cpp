#include "aring-RRR.hpp"

namespace M2 {

  void ARingRRR::text_out(buffer &o) const
  {
    o << "ARRR_" << mPrecision;
  }

  void ARingRRR::elem_text_out(buffer &o,
			       ElementType &ap,
			       bool p_one,
			       bool p_plus,
			       bool p_parens) const
  {
    mpfr_ptr a = &ap;
    M2_string s = (*gmp_tostringRRpointer)(a);
    bool prepend_plus = p_plus && (s->array[0] != '-');
    bool strip_last = !p_one && (
				 (s->len == 1 && s->array[0] == '1')
				 || (s->len == 2 && s->array[1] == '1' && s->array[0] == '-'));
    
    if (prepend_plus)
      o << "+";
    if (strip_last)
      o.put(s->array, s->len-1);
    else
      o.put(s->array, s->len);
    
  }
  
}
