// Copyright 1996 Michael E. Stillman.

#ifndef _gb_comp_hh_
#define _gb_comp_hh_

#include "object.hpp"

class binomialGB_comp;
class sagbi_comp;

extern "C" char system_interrupted;
extern int comp_printlevel;

// The various kinds of GB computations
const int COMP_NGB = 10;
const int COMP_GB = 1;
const int COMP_GBINHOM = 2;
const int COMP_HERMITE = 3;
const int COMP_GAUSS = 4;
const int COMP_BINOMIAL_GB = 5;
const int COMP_SAGBI = 6;

// Possible strategy flags
const int USE_HILB = 1;
const int USE_GEOBUCKET = 8;
const int USE_SORT = 16;

// These are the possible states of a GB computation
const int GB_COMP_NEWDEGREE        = 1; 
const int GB_COMP_NEED_RESIZE      = 2;
const int GB_COMP_S_PAIRS          = 3;
const int GB_COMP_GENS             = 4;
const int GB_COMP_AUTO_REDUCE      = 5;
const int GB_COMP_NEWPAIRS         = 6;
const int GB_COMP_DONE             = 7;

// The following are the return values from s_pair_step,
const int SPAIR_DONE   = 0;
const int SPAIR_GB     = 1;
const int SPAIR_SYZ    = 2;
const int SPAIR_ZERO   = 3;
const int SPAIR_MINGEN = 4;
const int SPAIR_GEN    = 5;
const int SPAIR_PAIR   = 6;
const int SPAIR_RING   = 7;
const int SPAIR_REMOVED = 8;
const int SPAIR_DEFERRED = 9;

//--- To be removed soon -----------------------------------
// These are the possible states of a GB computation
const int GB_COMP_RESIZE_MONOMIALS = 10;
const int GB_COMP_SORTPAIRS        = 11;

// The following are the three levels of a Groebner basis.
const int SYZ_GB      = 0;
const int SYZ_CHANGE  = 2;  // MES these need to be changed!!
const int SYZ_KER     = 1;

// The following are the return values from s_pair_step,
// and gen_step.
const int SPAIR_NOT_MINIMAL = 8;
const int SPAIR_BASE   = 9;

// These are the types of ending conditions
const int STOP_DONE     = 1;
const int STOP_DEGREE   = 2;
const int STOP_NEW_ELEM = 3;
const int STOP_CODIM    = 4;
const int STOP_MIN_GENS = 5;
//--- above to be removed soon -----------------------------

class gb_comp : public type
{
protected:
  int _kind;  // GB_comp:1, GBinhom_comp:2
public:
  gb_comp(int kind) : _kind(kind) {}
  virtual ~gb_comp() {}

  int kind() const { return _kind; }

  virtual int calc(const int *deg, const intarray &stop_conditions) = 0;
  
  virtual void stats() const = 0;
  virtual Matrix min_gens_matrix() = 0;
  virtual Matrix gb_matrix() = 0;
  virtual Matrix syz_matrix() = 0;
  virtual Matrix change_matrix() = 0;
  virtual Matrix initial_matrix(int n=-1) = 0;

  virtual Matrix reduce(const Matrix &m, Matrix &result_lift) = 0;
  virtual Vector reduce(const Vector &v, Vector &result_lift) = 0;

  virtual int contains(const Matrix &m) = 0;
  virtual bool is_equal(const gb_comp *q) = 0;

  // Infrastructure
  class_identifier class_id() const { return CLASS_gb_comp; }
  type_identifier  type_id () const { return TY_GB_COMP; }
  const char * type_name   () const { return "gb_comp"; }

  gb_comp * cast_to_gb_comp   () { return this; }

  // These can be overridden by the specific computation
  void text_out(buffer &o) const { o << "GB computation"; }

  virtual binomialGB_comp * cast_to_binomialGB_comp() { return 0; }
  virtual const binomialGB_comp * cast_to_binomialGB_comp() const { return 0; }
  virtual sagbi_comp * cast_to_sagbi_comp() { return 0; }
  virtual const sagbi_comp * cast_to_sagbi_comp() const { return 0; }
};

#endif
