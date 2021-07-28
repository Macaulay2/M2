#ifndef MATHIC_K_D_ENTRY_ARRAY_GUARD
#define MATHIC_K_D_ENTRY_ARRAY_GUARD

#include "stdinc.h"

#include "DivMask.h"
#include "Comparer.h"

#include <stdexcept>
#include "memtailor/memtailor.h"

namespace mathic {
  template<class C, class EE>
  class KDEntryArray : public DivMask::HasDivMask<C::UseTreeDivMask> {
  public:
    typedef typename C::Entry Entry;
    typedef typename C::Exponent Exponent;
    typedef EE ExtEntry;
    typedef EE* iterator;
    typedef const EE* const_iterator;
    typedef const EE& const_reference;
    typedef EE value_type;
    typedef DivMask::Calculator<C> DivMaskCalculator;

    using DivMask::HasDivMask<C::UseTreeDivMask>::updateToLowerBound;

    KDEntryArray(memt::Arena& arena, const C& conf);

    template<class Iter>
    KDEntryArray(Iter begin, Iter end, memt::Arena& arena,
      const DivMaskCalculator& calc, const C& conf);
    template<class Iter>
    KDEntryArray(Iter begin, Iter end, memt::Arena& arena, const C& conf);

    void clear();

    void recalculateTreeDivMask();
#ifdef MATHIC_DEBUG
    bool debugIsValid() const;
#endif

    iterator begin() {return reinterpret_cast<ExtEntry*>(_beginMemory);}
    const_iterator begin() const {
      return reinterpret_cast<const ExtEntry*>(_beginMemory);
    }
    iterator end() {return _end;}
    const_iterator end() const {return _end;}

    bool empty() const {return begin() == end();}
    size_t size() const {return std::distance(begin(), end());}
    EE& front() {MATHIC_ASSERT(!empty()); return *begin();}
    const EE& front() const {MATHIC_ASSERT(!empty()); return *begin();}
    EE& back() {MATHIC_ASSERT(!empty()); return *(_end - 1);}
    const EE& back() const {MATHIC_ASSERT(!empty()); return *(_end - 1);}

    void push_back(const EE& entry);
    void pop_back();
    void insert(iterator it, const EE& entry);
    void insert(const EE& entry, const C& conf);

    /** Returns how many were removed. */
    template<class EM, class MO>
    size_t removeMultiples(const EM& monomial, MO& out, const C& conf);

    template<class M>
    bool removeElement(const M& monomial, const C& conf);

    template<class EM>
    inline iterator findDivisor(const EM& extMonomial, const C& conf);

    template<class EM, class DO>
    inline bool findAllDivisors(const EM& extMonomial, DO& out, const C& conf);

    template<class EM, class Output>
    inline bool findAllMultiples
      (const EM& extMonomial, Output& out, const C& conf);

    template<class EO>
    bool forAll(EO& eo);

    bool allStrictlyGreaterThan(size_t var, Exponent exp, const C& conf);
    bool allLessThanOrEqualTo(size_t var, Exponent exp, const C& conf);

    /** Reorders [begin, iter) and chooses a var and an exp such that
     if mid is the returned value, then [begin, mid) has less than or
     equal to exp exponent of var and [mid,end) has strictly greater
     exponent. The initial value of var is a hint that var + 1 could
     be a good place to start the search for a suitable value of var.
     begin must not equal end.
     
     If not null, the entry os taken into account when deciding which
     split to perform. The Entry is NOT inserted   into [begin, end). */
    template<class Iter>
    static Iter split(
      Iter begin,
      Iter end,
      size_t& var,
      Exponent& exp,
      const C& conf,
      const ExtEntry* extEntry = 0
    );

    using DivMask::HasDivMask<C::UseTreeDivMask>::resetDivMask;
    using DivMask::HasDivMask<C::UseTreeDivMask>::getDivMask;

  private:
    static Entry& getEntry(Entry& e) {return e;}
    static const Entry& getEntry(const Entry& e) {return e;}
    static Entry& getEntry(ExtEntry& e) {return e.get();}
    static const Entry& getEntry(const ExtEntry& e) {return e.get();}

    KDEntryArray(const KDEntryArray&); // unavailable
    void operator=(const KDEntryArray&); // unavailable

    class SplitEqualOrLess;


    /** This is the memory for the entries. It is kept as a raw char*
     to avoid constructing all the entries right away. */
    char _beginMemory[C::LeafSize * sizeof(ExtEntry)];
    iterator _end; // points into _beginMemory
#ifdef MATHIC_DEBUG
    const bool _sortOnInsertDebug;
#endif
  };

  template<class C, class EE>
  class KDEntryArray<C, EE>::SplitEqualOrLess {
  public:
    typedef typename C::Exponent Exponent;
    typedef typename C::Entry Entry;
  SplitEqualOrLess(size_t var, const Exponent& exp, const C& conf):
    _var(var), _exp(exp), _conf(conf) {}
    bool operator()(const Entry& entry) const {
      return !(_exp < _conf.getExponent(entry, _var));
    }
    bool operator()(const ExtEntry& entry) const {
      return !(_exp < _conf.getExponent(entry.get(), _var));
    }
  private:
    size_t _var;
    const Exponent& _exp;
    const C& _conf;
  };

  template<class C, class EE>
  template<class Iter>
  Iter KDEntryArray<C, EE>::split(
    Iter begin,
    Iter end,
    size_t& var,
    Exponent& exp,
    const C& conf,
    const ExtEntry* extEntry
  ) {
    MATHIC_ASSERT(begin != end);
    const size_t varCount = conf.getVarCount();
    for (size_t i = 0; i < varCount; ++i) {
      var = (var + 1) % conf.getVarCount();

      typename C::Exponent min;
      typename C::Exponent max;
      if (extEntry == 0) {
        min = conf.getExponent(getEntry(*begin), var);
        max = conf.getExponent(getEntry(*begin), var);
      }  else {
        min = conf.getExponent(getEntry(*extEntry), var);
        max = conf.getExponent(getEntry(*extEntry), var);
      }
      for (Iter it = begin; it != end; ++it) {
        min = std::min(min, conf.getExponent(getEntry(*it), var));
        max = std::max(max, conf.getExponent(getEntry(*it), var));
      }
      if (min == max)
        continue;
      // this formula for the average avoids overflow
      exp = min + (max - min) / 2;
      SplitEqualOrLess cmp(var, exp, conf);
      return std::partition(begin, end, cmp);
    }
    MATHIC_ASSERT(false);
    throw std::logic_error("ERROR: Inserted duplicate entry into a KD tree.");
  }

  template<class C, class EE>
  KDEntryArray<C, EE>::KDEntryArray(memt::Arena& arena, const C& conf)
#ifdef MATHIC_DEBUG
    :_sortOnInsertDebug(conf.getSortOnInsert())
#endif
  {
    _end = begin();
  }

  template<class C, class EE>
  template<class Iter>
  KDEntryArray<C, EE>::KDEntryArray(
    Iter begin,
    Iter end,
    memt::Arena& arena,
    const DivMaskCalculator& calc,
    const C& conf) 
#ifdef MATHIC_DEBUG
  :_sortOnInsertDebug(conf.getSortOnInsert())
#endif
  {
    MATHIC_ASSERT(static_cast<size_t>(std::distance(begin, end)) <=
                  C::LeafSize);
    _end = this->begin();
    // cannot directly copy as memory is not constructed.
    for (; begin != end; ++begin)
      push_back(EE(*begin, calc, conf));
    if (conf.getSortOnInsert())
      std::sort(this->begin(), this->end(), Comparer<C>(conf));
  }

  template<class C, class EE>
  template<class Iter>
  KDEntryArray<C, EE>::KDEntryArray(
    Iter begin,
    Iter end,
    memt::Arena& arena,
    const C& conf) 
#ifdef MATHIC_DEBUG
  :_sortOnInsertDebug(conf.getSortOnInsert())
#endif
  {
    MATHIC_ASSERT(static_cast<size_t>(std::distance(begin, end)) <=
                  C::LeafSize);
    _end = this->begin();
    // cannot directly copy as memory is not constructed.
    for (; begin != end; ++begin)
      push_back(*begin);
    if (conf.getSortOnInsert())
      std::sort(this->begin(), this->end(), Comparer<C>(conf));
  }

  template<class C, class EE>
    void KDEntryArray<C, EE>::push_back(const EE& entry) {
    MATHIC_ASSERT(size() < C::LeafSize);
    new (_end) EE(entry);
    updateToLowerBound(entry);
    ++_end;
  }

  template<class C, class EE>
    void KDEntryArray<C, EE>::pop_back() {
    MATHIC_ASSERT(!empty());
    --_end;
    _end->~EE();
  }

  template<class C, class EE>
    void KDEntryArray<C, EE>::insert(iterator it, const EE& entry) {
    MATHIC_ASSERT(size() < C::LeafSize);
    if (it == end()) {
      push_back(entry);
      return;
    }
    push_back(back());
    iterator moveTo = end();
    for (--moveTo; moveTo != it; --moveTo)
      *moveTo = *(moveTo - 1);
    updateToLowerBound(entry);
    *it = entry;
  }

  template<class C, class EE>
    void KDEntryArray<C, EE>::insert(const EE& entry, const C& conf) {
    MATHIC_ASSERT(size() < C::LeafSize);
    if (!conf.getSortOnInsert())
      push_back(entry);
    else {
      iterator it = std::upper_bound(begin(), end(), entry, Comparer<C>(conf));
      insert(it, entry);
    }
  }

  template<class C, class EE>
    void KDEntryArray<C, EE>::clear() {
      while (!empty())
        pop_back();
  }

  template<class C, class EE>
  template<class EM, class MO>
  size_t KDEntryArray<C, EE>::removeMultiples
    (const EM& monomial, MO& out, const C& conf) {
    MATHIC_ASSERT(C::AllowRemovals);
    if (C::LeafSize == 1) { // special case for performance
      if (empty() || !monomial.divides(*begin(), conf))
        return 0;
      out.push_back(begin()->get());
      pop_back();
      return 1;
    }

    iterator it = begin();
    iterator oldEnd = end();
    for (; it != oldEnd; ++it) {
      if (monomial.divides(*it, conf)) {
        out.push_back(it->get());
        break;
      }
    }
    if (it == oldEnd)
      return 0;
    iterator newEnd = it;
    for (++it; it != oldEnd; ++it) {
      if (!monomial.divides(*it, conf)) {
        *newEnd = *it;
        ++newEnd;
      } else
        out.push_back(it->get());
    }
    // cannot just set _end directly as the superfluous
    // entries at the end need to be destructed.
    const size_t newSize = std::distance(begin(), newEnd);
    const size_t removedCount = size() - newSize;
    MATHIC_ASSERT(newSize < size());
    do {
      pop_back();
    } while (newSize < size());
    MATHIC_ASSERT(size() == newSize);
    return removedCount;
  }

  template<class C, class EE>
  template<class M>
  bool KDEntryArray<C, EE>::removeElement(const M& monomial, const C& conf) {
    const size_t varCount = conf.getVarCount();
    const_iterator stop = end();
    for (iterator it = begin(); it != stop; ++it) {
      for (size_t var = 0; var < varCount; ++var)
        if (conf.getExponent(monomial, var) != conf.getExponent(it->get(), var))
          goto skip;
      if (it != end()) {
        const_iterator next = it;
        for (++next; next != end(); ++it, ++next)
          *it = *next;
      }
      pop_back();
      return true;
    skip:;
    }
    return false;
  }

  template<class C, class EE>
  template<class EM>
  typename KDEntryArray<C, EE>::iterator
  KDEntryArray<C, EE>::findDivisor(const EM& extMonomial, const C& conf) {
    if (C::UseTreeDivMask &&
      C::LeafSize > 1 && // no reason to do it for just 1 leaf
      !getDivMask().canDivide(extMonomial.getDivMask()))
      return end();

    if (C::LeafSize == 1) { // special case for performance
      MATHIC_ASSERT(C::AllowRemovals || !empty());
      if ((!C::AllowRemovals || !empty()) &&
        begin()->divides(extMonomial, conf))
        return begin();
      else
        return end();
    }
    else if (!conf.getSortOnInsert()) {
      const iterator stop = end();
      for (iterator it = begin(); it != stop; ++it)
        if (it->divides(extMonomial, conf))
          return it;
      return stop;
    } else {
      iterator rangeEnd =
        std::upper_bound(begin(), end(), extMonomial, Comparer<C>(conf));
      iterator it = begin();
      for (; it != rangeEnd; ++it)
        if (it->divides(extMonomial, conf))
          return it;
      return end();
    }
  }

  template<class C, class EE>
  template<class EM, class DO>
  bool KDEntryArray<C, EE>::
    findAllDivisors(const EM& extMonomial, DO& out, const C& conf) {
    if (C::UseTreeDivMask &&
      C::LeafSize > 1 && // no reason to do it for just 1 leaf
      !getDivMask().canDivide(extMonomial.getDivMask()))
      return end();

    if (C::LeafSize == 1) { // special case for performance
      MATHIC_ASSERT(C::AllowRemovals || !empty());
      return (C::AllowRemovals && empty()) ||
        !begin()->divides(extMonomial, conf) ||
        out.proceed(begin()->get());
    } else  if (!conf.getSortOnInsert()) {
      const iterator stop = end();
      for (iterator it = begin(); it != stop; ++it)
        if (it->divides(extMonomial, conf))
          if (!out.proceed(it->get()))
            return false;
    } else {
      iterator rangeEnd =
        std::upper_bound(begin(), end(), extMonomial, Comparer<C>(conf));
      iterator it = begin();
      for (; it != rangeEnd; ++it)
        if (it->divides(extMonomial, conf))
          if (!out.proceed(it->get()))
            return false;
    }
    return true;
  }

  template<class C, class EE>
  template<class EM, class DO>
  bool KDEntryArray<C, EE>::
  findAllMultiples(const EM& extMonomial, DO& out, const C& conf) {
    if (C::LeafSize == 1) { // special case for performance
      MATHIC_ASSERT(C::AllowRemovals || !empty());
      return (C::AllowRemovals && empty()) ||
        !extMonomial.divides(*begin(), conf) ||
        out.proceed(begin()->get());
    }

    // todo: consider making sorted version
    const iterator stop = end();
    for (iterator it = begin(); it != stop; ++it)
      if (extMonomial.divides(*it, conf))
        if (!out.proceed(it->get()))
          return false;
    return true;
  }

  template<class C, class EE>
  template<class EO>
  bool KDEntryArray<C, EE>::forAll(EO& output) {
    if (C::LeafSize == 1) { // special case for performance
      MATHIC_ASSERT(C::AllowRemovals || !empty());
      return (C::AllowRemovals && empty()) || output.proceed(begin()->get());
    }
    const iterator stop = end();
    for (iterator it = begin(); it != stop; ++it)
      if (!output.proceed(it->get()))
        return false;
    return true;
  }

  template<class C, class EE>
  bool KDEntryArray<C, EE>::allStrictlyGreaterThan(
    size_t var,
    Exponent exp,
    const C& conf
  ) {
    for (const_iterator it = begin(); it != end(); ++it)
      if (!(exp < conf.getExponent(it->get(), var)))
        return false;
    return true;
  }

  template<class C, class EE>
  bool KDEntryArray<C, EE>::allLessThanOrEqualTo(
    size_t var,
    Exponent exp,
    const C& conf
  ) {
    for (const_iterator it = begin(); it != end(); ++it)
      if (exp < conf.getExponent(it->get(), var))
        return false;
    return true;
  }

  template<class C, class EE>
  void KDEntryArray<C, EE>::recalculateTreeDivMask() {
    if (!C::UseTreeDivMask)
      return;
    resetDivMask();
    for (const_iterator it = begin(); it != end(); ++it)
      updateToLowerBound(*it);
  }
#ifdef MATHIC_DEBUG
  template<class C, class EE>
  bool KDEntryArray<C, EE>::debugIsValid() const {
    MATHIC_ASSERT(C::AllowRemovals || !empty());
    MATHIC_ASSERT(static_cast<size_t>(end() - begin()) <= C::LeafSize);
    if (C::UseTreeDivMask && C::LeafSize > 1) {
      for (const_iterator it = begin(); it != end(); ++it) {
        MATHIC_ASSERT(getDivMask().canDivide(it->getDivMask()));
      }
    }
    return true;
  }
#endif
}

#endif
