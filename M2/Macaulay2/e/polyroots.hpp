
#ifndef _polyroots_hpp_
#define _polyroots_hpp_

#define timer timer1

#include <pari/pari.h>

#undef timer

#include "relem.hpp"
#include "polyring.hpp"
#include "aring-CC.hpp"
#include "aring-CCC.hpp"
#include "aring-glue.hpp"
#include "aring-RR.hpp"
#include "aring-RRR.hpp"

#define abs(x)  ( ( (x) < 0) ? -(x) : (x) )
#define max(a, b)  ( ( (a) > (b) ) ? (a) : (b) )

extern "C" {
  #include "../d/pari-gnump.h"
};

typedef M2::ConcreteRing<M2::ARingRR> RingRR;
typedef M2::ConcreteRing<M2::ARingRRR> RingRRR;
typedef M2::ConcreteRing<M2::ARingCC> RingCC;
typedef M2::ConcreteRing<M2::ARingCCC> RingCCC;

typedef M2::ARingCC::complex complex;
typedef M2::ARingCCC::mpfc_struct mpfc_t[1];
typedef M2::ARingCCC::mpfc_ptr mpfc_ptr;

engine_RawRingElementArrayOrNull rawRoots(const RingElement *p, long prec,
                                          int unique);

#endif
