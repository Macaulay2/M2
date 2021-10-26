// Copyright 2002-2016  Michael E. Stillman
#ifndef _hash_hh_
#define _hash_hh_

#include "newdelete.hpp"
#include <cassert>
#include <M2/gc-include.h>

//class EngineObject : public gc
class EngineObject : public our_new_delete
{
 private:
  mutable unsigned int mHashValue;
 public:
  EngineObject() : mHashValue(0) {}

  virtual ~EngineObject() { /* nothing to do here */ }

  unsigned int hash() const
  { 
    if (mHashValue == 0) 
      {
        mHashValue = computeHashValue();
        if (mHashValue == 0) mHashValue = 1;
      }
    return mHashValue;
  }

 protected:
  virtual unsigned int computeHashValue() const = 0;
};

class MutableEngineObject : public our_gc_cleanup
{
 private:
  static unsigned int mNextMutableHashValue;
  unsigned int mHashValue;

 public:
  MutableEngineObject() : mHashValue(mNextMutableHashValue++) {}
  virtual ~MutableEngineObject() { /* nothing to do here */ }
  unsigned int hash() const { return mHashValue; }
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
