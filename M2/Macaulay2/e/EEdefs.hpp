// Copyright 2000  Michael E. Stillman
#ifndef _EEGBdefs_hh_
#define _EEGBdefs_hh_

// The definitions used in the Groebner basis code in the files:
// EEGB1, EEspairs, EElookup

#include "Einterface.hpp"
#define EVector vec

const int SP_SYZ = 1;
const int SP_RING = 2;
const int SP_SKEW = 3;
const int SP_GEN = 4;
const int SP_DEFERRED = 5;

enum comp_return_value
{
  COMP_ERROR = -2,
  COMP_INTERRUPTED = -1,
  COMP_DONE = 0,
  COMP_DONE_DEGREE_LIMIT = 1,
  COMP_DONE_LENGTH_LIMIT = 2,
  COMP_DONE_PAIR_LIMIT   = 4,
  COMP_DONE_GB_LIMIT     = 5,
  COMP_DONE_SYZ_LIMIT    = 6,
  COMP_DONE_CODIM        = 7,
  COMP_DONE_MIN_GENS     = 8,
  COMP_DONE_SUBRING_LIMIT= 10,
  COMP_DONE_STEPS        = 9,  // Possible Hilbert function return value
  COMP_COMPUTING = 100
};

class EStopConditions
{
public:
  bool degree;
  int degree_limit;
  int gb_limit;
  int syz_limit;
  int pair_limit;
  int mingens_limit;
  int subring_limit;
  bool codim;
  int codim_limit;  // Set if 'codim' is true.
        // Stop if the codimension of the initial submodule becomes
        // >= codim_limit.
};

extern int comp_printlevel;
extern char system_interrupted;

#endif

#if 0
each has: next, lcm, degree? <- degree is the 'sugar' degree?
SP_SYZ: egb_elem first,second;
SP_RING: egb_elem first, ering_elem second;
SP_SKEW: egb_elem first, int skewvar;
SP_GEN: (f,fsyz,denom);
SP_DEFERRED: (degree,f,fsyz,denom, ismin)

  reduce an es_pair:

    (degree,f,fsyz,denom)
    loop
      f->monom to a expvector
      at same time, return tdegree.
      search(f->comp,expvector,degree,tdegree) --> r (ering_elem) or g (egb_elem) or none, alpha
      if alpha > 0 then insert (deg,tdegree,f,fsyz,denom) into the non-minimal GB basis
      if ringelem:
        f -= xx.r
      if egb_elem:
        f -= xx.g
        fsyz -= ...
        fix denom
      if alpha > 0 then
      -- at this point:
        degree += alpha
        put the pair back, as a deferred pair.
        return
      if lead term still same: do what?
      if none:
        return.
    end loop
      
#endif
