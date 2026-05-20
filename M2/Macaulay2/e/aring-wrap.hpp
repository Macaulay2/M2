// Copyright 2011 Michael E. Stillman

#ifndef _aring_wrap_hpp_
#define _aring_wrap_hpp_

/**
 * @file aring-wrap.hpp
 * @brief `RElementWrap<RingType>` and `AConcreteRing<RingType>` --- abandoned scaffolding for an abstract `ARing` / `RElement` base.
 *
 * Declares two templates that sketch an alternative aring
 * type-erasure design: `RElementWrap<RingType>` inherits from
 * `RElement` and stores one `RingType::ElementType` value, and
 * `AConcreteRing<RingType>` inherits from `ARing`, holds a
 * `RingType` by value, and forwards `init_set` / `add_to` to it
 * through the `RELEM(RingType, ...)` cast macros. Both templates
 * `friend bool ARing::converter(const ARing*, const ARing*,
 * const RElement&, RElement&)` --- but neither `RElement`,
 * `ARing`, nor `ARing::converter` is defined anywhere in the
 * engine tree; the two templates are not referenced by any
 * other file. Treat this header as orphan scaffolding kept
 * around for historical / sketch purposes rather than as part
 * of the live aring framework, whose actual ring- and
 * element-level bridges live in `aring-glue.hpp` (vertical
 * bridge to `Ring*`) and the `mypromote` / `mylift` family in
 * `aring-translate.hpp`.
 *
 * @see aring.hpp
 * @see aring-glue.hpp
 * @see aring-translate.hpp
 */

#include "aring.hpp"

namespace M2 {

/**
\ingroup rings
*/
template <class RingType>
class RElementWrap : public RElement
{
  friend bool ARing::converter(const ARing *sourceR,
                               const ARing *targetR,
                               const RElement &a,
                               RElement &b);

 public:
  typedef typename RingType::ElementType element_type;
  ~RElementWrap() {}
  RElementWrap() {}
  RElementWrap(const element_type &a) : val_(a) {}
  RElementWrap(const RElement &a)
      : val_(static_cast<const RElementWrap &>(a).val_)
  {
  }

 private:
  friend class AConcreteRing<RingType>;
  element_type val_;
};

/**
\ingroup rings
*/
template <class RingType>  // RingType should inherit from RingInterface
class AConcreteRing : public ARing
{
  friend bool ARing::converter(const ARing *sourceR,
                               const ARing *targetR,
                               const RElement &a,
                               RElement &b);

 public:
  typedef typename RingType::ElementType element_type;
  typedef RElementWrap<RingType> ringelem_type;

  AConcreteRing() {}
  AConcreteRing(RingType R) : R_(R) {}
  virtual RingID getRingID() const { return RingType::ringID; }
  RingType &getInternalRing() { return R_; }
  const RingType &getInternalRing() const { return R_; }
  virtual void init_set(RElement &a, long val) const
  {
    R_.init_set(RELEM(RingType, a), val);
  }

  virtual void add_to(RElement &a, const RElement &b) const
  {
    R_.add_to(RELEM(RingType, a), constRELEM(RingType, b));
  }

 private:
  RingType R_;
};
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e  "
// indent-tabs-mode: nil
// End:
