// Copyright 1998  Michael E. Stillman
#ifndef _EGB_hh_
#define _EGB_hh_

#include "object.hpp"
#include "Ematrix.hpp"

extern "C" char system_interrupted;
extern int comp_printlevel;
extern "C" void system_spincursor(void);

// The possible return values of 'calc' in a EGroebnerComputation.
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

class EGroebnerComputation : public type
{
public:
  //void bin_out(buffer &o) const;
  //void text_out(buffer &o) const;

  class_identifier class_id() const { return CLASS_EGroebnerComputation; }
  type_identifier  type_id () const { return TY_EGroebnerComputation; }
  const char * type_name   () const { return "EGroebnerComputation"; }

  EGroebnerComputation * cast_to_EGroebnerComputation() { return this; }

public:
  // The following return false if the operation is not supported or allowed.
  virtual bool moreRelations(int lo, int hi, const EMatrix *m) { return false; }
  virtual bool moreGenerators(int lo, int hi, const EMatrix *m) { return false; }
  virtual bool extendRing(const ERing *newR) { return false; }
  virtual bool informHilbertSeries(const ERing *Hring, const ERingElement *hf) { return false; }
  
  // The return value of 'calc' is a comp_return_value.
  virtual int calc(const EStopConditions &stop) { return COMP_DONE; }
  
  // Free modules used with reduction
  virtual const EFreeModule *getTarget() const = 0;
  virtual const EFreeModule *getSource() const = 0;
  virtual EVector reduceVector(const EVector &v, int multType) const = 0;
  virtual EVector reduceVector(const EVector &v, int multType, EVector &result_lift) const = 0;
  
  // These are implemented in terms of the vector routines.
  EMatrix *reduceMatrix(const EMatrix *m) const;
  EMatrix *reduceMatrix(const EMatrix *m, EMatrix * & result_lift) const;


  // NULL is returned if the specific computation doesn't support
  // the given operation.
  virtual EMatrix *getMinimalGenerators()  { return 0; }
  virtual EMatrix *getGB()                 { return 0; }
  virtual EMatrix *getGBgenerators()       { return 0; }
  virtual EMatrix *getGBrelations()        { return 0; }
  virtual EMatrix *getChangeOfBasis()      { return 0; }
  virtual EMatrix *getSyzygies()           { return 0; }
  virtual EMatrix *getLeadTerms(int n=-1)  { return 0; }
  virtual EMatrix *getSubring(int n=-1)    { return 0; }
  virtual EMatrix *getSubringGB(int n=-1)  { return 0; }

  virtual void stats() {}

  // Read/write from disk. [checkpoint, retrieve].

};
class EDeclaredGB : public EGroebnerComputation
{
  EMatrix *gens;
  EMatrix *gb;
  EMatrix *change;
  EMatrix *syz;
protected:
  EDeclaredGB(EMatrix *gens, EMatrix *gb, EMatrix *change, EMatrix *syz);
public:
  virtual ~EDeclaredGB() {}
  static EDeclaredGB *make(EMatrix *gens, EMatrix *gb, EMatrix *change, EMatrix *syz);

  virtual const EFreeModule *getTarget() const;
  virtual const EFreeModule *getSource() const;
  virtual EVector reduceVector(const EVector &v, int multType) const;
  virtual EVector reduceVector(const EVector &v, int multType, EVector &result_lift) const;
  
  virtual EMatrix *getMinimalGenerators()  { return gens; }
  virtual EMatrix *getGB()                 { return gb; }
  virtual EMatrix *getChangeOfBasis()      { return change; }
  virtual EMatrix *getSyzygies()           { return syz; }
  virtual EMatrix *getLeadTerms(int n=-1)  { return gb->leadTerm(n,true); }
  virtual EMatrix *getSubring(int n=-1)    { return 0; }
  virtual EMatrix *getSubringGB(int n=-1)  { return 0; }
};

#endif
