// Copyright 2002  Michael E. Stillman
#ifndef _hash_hh_
#define _hash_hh_

#include "newdelete.hpp"
#include <cassert>

class EngineObject : public our_new_delete
{
 private:
  mutable unsigned int mHashValue;
  mutable bool mHasHash;

 public:
  EngineObject() : mHashValue(0), mHasHash(false) {}
  virtual ~EngineObject() {}  // do we need to do anything here?
  void intern() const {}
  unsigned int hash() const
  {
    if (not mHasHash)
      {
        mHashValue = computeHashValue();
        mHasHash = true;
      }
    return mHashValue;
  }

 protected:
  virtual unsigned int computeHashValue() const = 0;
};

class MutableEngineObject : public our_new_delete
{
 private:
  static unsigned int mNextMutableHashValue;
  unsigned int mHashValue;

 public:
  MutableEngineObject() : mHashValue(mNextMutableHashValue++) {}
  unsigned int hash() const { return mHashValue; }
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
