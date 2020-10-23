#ifndef _polyroots_hpp_
#define _polyroots_hpp_

#include "polyring.hpp"

#include "relem.hpp"
#include "aring-CC.hpp"
#include "aring-CCC.hpp"
#include "aring-glue.hpp"
#include "aring-RR.hpp"
#include "aring-RRR.hpp"

typedef M2::ConcreteRing<M2::ARingRR> RingRR;
typedef M2::ConcreteRing<M2::ARingRRR> RingRRR;
typedef M2::ConcreteRing<M2::ARingCC> RingCC;
typedef M2::ConcreteRing<M2::ARingCCC> RingCCC;

typedef cc_struct mpfc_t[1];
typedef cc_ptr mpfc_ptr;

/* FIXME: this is also defined in engine.h
extern "C" engine_RawRingElementArrayOrNull rawRoots(const RingElement *p,
                                                     long prec,
                                                     int unique);
*/

#endif /* _polyroots_hpp_ */

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e  "
// indent-tabs-mode: nil
// End:
