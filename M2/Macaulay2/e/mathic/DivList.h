#ifndef MATHIC_DIV_ARRAY_GUARD
#define MATHIC_DIV_ARRAY_GUARD

#include "stdinc.h"
#include "DivMask.h"
#include "Comparer.h"

#include "memtailor/memtailor.h"
#include <vector>
#include <string>
#include <list>
#include <algorithm>
#include <sstream>

namespace mathic {
  /** An object that supports queries for divisors of a monomial using
      an array of monomials. See DivFinder for more documentation.

      Extra fields for Configuration:

      * static const bool UseLinkedList
      Use a linked list if true, otherwise use an array.

      * static const bool UseDivMask
      Use div masks if true.

      * bool getSortOnInsert() const
      Keep the monomials sorted to speed up queries.
  */
  template<class Configuration>
    class DivList;

  namespace DivListHelper {
    // Implementation details for DivList.

    template<bool B, class E>
      struct ListImpl;

    template<class Entry>
      struct ListImpl<false, Entry> {
      typedef std::vector<Entry> Impl;
    };
    template<class Entry>
      struct ListImpl<true, Entry> {
      typedef std::list<Entry> Impl;
    };
  }

  template<class C>
    class DivList {
  public:
    typedef C Configuration;
    typedef typename C::Entry Entry;
    typedef typename C::Monomial Monomial;
    typedef typename C::Exponent Exponent;

    static const bool UseLinkedList = C::UseLinkedList;
    static const bool UseDivMask = C::UseDivMask;

  private:
    typedef typename DivMask::Extender<Entry, C::UseDivMask> ExtEntry;
    typedef typename DivMask::Extender<const Monomial&,C::UseDivMask> ExtMonoRef;
    typedef typename DivMask::Calculator<C> DivMaskCalculator;

    typedef typename DivListHelper::ListImpl<C::UseLinkedList, ExtEntry>::Impl
      List;
    typedef typename List::iterator ListIter;
    typedef typename List::const_iterator CListIter;

  public:
    class iterator;
    class const_iterator;

    DivList(const C& configuration);

    template<class Iter>
      void insert(Iter begin, Iter end);
    void insert(const Entry& entry);

    bool removeMultiples(const Monomial& monomial);
    template<class MultipleOutput>
      bool removeMultiples
      (const Monomial& monomial, MultipleOutput& out);

    bool removeElement(const Monomial& monomial);

    iterator findDivisorIterator(const Monomial& monomial);

    Entry* findDivisor(const Monomial& monomial);
    const Entry* findDivisor(const Monomial& monomial) const;

    template<class DO>
    void findAllDivisors(const Monomial& monomial, DO& out);
    template<class DO>
    void findAllDivisors(const Monomial& monomial, DO& out) const;

    template<class DO>
    void findAllMultiples(const Monomial& monomial, DO& out);

    template<class Output>
    void findAllMultiples(const Monomial& monomial, Output& output) const {
      ConstEntryOutput<Output> constOutput(output);
      const_cast<DivList<C>&>(*this).findAllMultiples(monomial, constOutput);
    }

    template<class EntryOutput>
    void forAll(EntryOutput& output);
    template<class EntryOutput>
    void forAll(EntryOutput& output) const;

    iterator begin() {return iterator(_list.begin());}
    const_iterator begin() const {return const_iterator(_list.begin());}
    iterator end() {return iterator(_list.end());}
    const_iterator end() const {return const_iterator(_list.end());}

    bool empty() const {return _list.empty();}
    size_t size() const {return _list.size();}

    std::string getName() const;

    C& getConfiguration() {return _conf;}
    const C& getConfiguration() const {return _conf;}

    void moveToFront(iterator pos);

    void rebuild();

	/** Returns the number of bytes allocated by this object. Does not
		include sizeof(*this), does not include any additional memory
		that the configuration may have allocated and does not include
		any memory that an Entry may point to. Does include
		sizeof(Entry) as well as unused memory that is being kept to
		avoid frequent allocations. */
    size_t getMemoryUse() const;

  private:
    DivList(const DivList<C>&); // unavailable
    void operator=(const DivList<C>&); // unavailable

    void resetNumberOfChangesTillRebuild();
    void reportChanges(size_t changesMadeCount);

    template<class DO>
    class ConstEntryOutput {
    public:
    ConstEntryOutput(DO& out): _out(out) {}
      bool proceed(const Entry& entry) {return _out.proceed(entry);}
    private:
      DO& _out;
    };
    class DummyMultipleOutput {
    public:
      void push_back(Entry& e) {}
    };

    List _list;
    C _conf;
    DivMaskCalculator _divMaskCalculator;
    size_t _changesTillRebuild; /// Update using reportChanges().
  };

  template<class C>
    class DivList<C>::iterator :
  public std::iterator<std::bidirectional_iterator_tag, Entry> {
  public:
    friend class DivList<C>;
    explicit iterator(ListIter it): _it(it) {}
    operator const_iterator() const {return const_iterator(_it);}

    bool operator==(iterator it) {return _it == it._it;}
    bool operator!=(iterator it) {return _it != it._it;}
    bool operator==(const_iterator it); // {return it == const_iterator(*this);}
    bool operator!=(const_iterator it); // {return it != const_iterator(*this);}

    iterator& operator++() {++_it; return *this;}
    iterator operator++(int) {iterator tmp = *this; operator++(); return tmp;}
    iterator& operator--() {--_it; return *this;}
    iterator operator--(int) {iterator tmp = *this; operator--(); return tmp;}

    Entry& operator*() const {return _it->get();}
    Entry* operator->() const {return &_it->get();}

  protected:
    ListIter getInternal() {return _it;}

  private:
    ListIter _it;
  };

  template<class C>
    class DivList<C>::const_iterator :
  public std::iterator<std::bidirectional_iterator_tag, const Entry> {
  public:
    friend class DivList<C>;
    explicit const_iterator(CListIter it): _it(it) {}
    bool operator==(const_iterator it) {return _it == it._it;}
    bool operator!=(const_iterator it) {return _it != it._it;}
    bool operator==(iterator it) {return *this == const_iterator(it);}
    bool operator!=(iterator it) {return *this != const_iterator(it);}

    const Entry& operator*() const {return _it->get();}
    const Entry* operator->() const {return &_it->get();}

    const_iterator& operator++() {++_it; return *this;}
    const_iterator operator++(int) {
      const_iterator tmp = *this;
      operator++();
      return tmp;
    }
    const_iterator& operator--() {--_it; return *this;}
    const_iterator operator--(int) {
      const_iterator tmp = *this;
      operator--();
      return tmp;
    }

  protected:
    ListIter getInternal() {return _it;}

  private:
    CListIter _it;
  };

  template<class C>
    bool DivList<C>::iterator::operator==(const_iterator it) {return it == const_iterator(*this);}
  template<class C>
    bool DivList<C>::iterator::operator!=(const_iterator it) {return it != const_iterator(*this);}

  namespace DivListHelper {
    template<class C, class E, class M, class MO>
      size_t removeMultiples
      (C& conf, std::vector<E>& list, const M& monomial, MO& out) {
      typedef typename std::vector<E>::iterator iterator;
      iterator it = list.begin();
      iterator oldEnd = list.end();
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
      const size_t origSize = list.size();
      const size_t newSize = std::distance(list.begin(), newEnd);
      MATHIC_ASSERT(newSize < list.size());
      list.resize(newSize);
      return origSize - newSize;
    }

    template<class C, class E, class M, class MO>
    size_t removeMultiples(C& conf,
                           std::list<E>& list,
                           const M& monomial,
                           MO& out) {
#ifdef MATHIC_DEBUG
      const size_t origSize = list.size();
#endif
      typedef typename std::list<E>::iterator iterator;
      iterator it = list.begin();
      iterator oldEnd = list.end();
      size_t removedCount = 0;
      while (it != oldEnd) {
        if (monomial.divides(*it, conf)) {
          out.push_back(it->get());
          ++removedCount;
          it = list.erase(it);
        } else
          ++it;
      }
      MATHIC_ASSERT(list.size() + removedCount == origSize);
      return removedCount;
    }

    template<class E, class It>
      void moveToFront(std::vector<E>& list, It pos) {
      E valueToMove = *pos;
      It begin = list.begin();
      while (pos != begin) {
        It prev = pos;
        --pos;
        *prev = *pos;
      }
      list.front() = valueToMove;
    }

    template<class E, class It>
      void moveToFront(std::list<E>& list, It pos) {
      list.splice(list.begin(), list, pos);
    }

    template<class C, class E>
      typename std::list<E>::iterator
      insertSort(C& conf, std::list<E>& list, const E& entry) {
      typedef typename std::list<E>::iterator iterator;
      iterator end = list.end();
      iterator it = list.begin();
      for (; it != end; ++it)
        if (conf.isLessThan(entry.get(), it->get()))
          break;
      return list.insert(it, entry);
    }

    template<class C, class E>
      typename std::vector<E>::iterator
      insertSort(C& conf, std::vector<E>& list, const E& entry) {
      typedef typename std::vector<E>::iterator iterator;
      iterator it = std::upper_bound(list.begin(), list.end(), entry,
                                     Comparer<C>(conf));
      return list.insert(it, entry);
    }

    template<class C, class E>
      void sortAll(C& conf, std::list<E>& list) {
      list.sort(Comparer<C>(conf));
    }

    template<class C, class E>
      void sortAll(C& conf, std::vector<E>& list) {
      std::sort(list.begin(), list.end(), Comparer<C>(conf));
    }

    template<class C, class E, class M>
      typename std::vector<E>::iterator
      findDivisorSorted(C& conf, std::vector<E>& list, const M& monomial) {
      typedef typename std::vector<E>::iterator iterator;
      iterator rangeEnd =
        std::upper_bound(list.begin(), list.end(), monomial, Comparer<C>(conf));
      iterator it = list.begin();
      for (; it != rangeEnd; ++it)
        if (it->divides(monomial, conf))
          return it;
      return list.end();
    }

    template<class C, class E, class M>
      typename std::list<E>::iterator
      findDivisorSorted(C& conf, std::list<E>& list, const M& monomial) {
      typedef typename std::list<E>::iterator iterator;
      iterator end = list.end();
      iterator it = list.begin();
      size_t count = 0;
      for (; it != end; ++it) {
        ++count;
        if (count == 35) {
          count = 0;
          if (conf.isLessThan(monomial.get(), it->get()))
            break;
        }
        if (it->divides(monomial, conf))
          return it;
      }
      return end;
    }

    template<class C, class E, class M, class DO>
      void findAllDivisorsSorted
      (C& conf, std::vector<E>& list, const M& monomial, DO& out) {
      typedef typename std::vector<E>::iterator iterator;
      iterator rangeEnd =
        std::upper_bound(list.begin(), list.end(), monomial, Comparer<C>(conf));
      iterator it = list.begin();
      for (; it != rangeEnd; ++it)
        if (it->divides(monomial, conf))
          if (!out.proceed(it->get()))
            break;
    }

    template<class C, class E, class M, class O>
      void findAllDivisorsSorted
      (C& conf, std::list<E>& list, const M& monomial, O& out) {
      typedef typename std::list<E>::iterator iterator;
      iterator end = list.end();
      iterator it = list.begin();
      size_t count = 0;
      for (; it != end; ++it) {
        ++count;
        if (count == 35) {
          count = 0;
          if (conf.isLessThan(monomial.get(), it->get()))
            break;
        }
        if (it->divides(monomial, conf))
          if (!out.proceed(it->get()))
            break;
      }
    }
  }

  template<class C>
    DivList<C>::DivList(const C& configuration):
  _conf(configuration),
    _divMaskCalculator(configuration) {
      resetNumberOfChangesTillRebuild();
    }

  template<class C>
    void DivList<C>::insert(const Entry& entry) {
    ExtEntry extEntry(entry, _divMaskCalculator, _conf);

    if (!_conf.getSortOnInsert())
      _list.push_back(extEntry);
    else
      DivListHelper::insertSort(_conf, _list, extEntry);
    reportChanges(1);
  }

  template<class C>
    template<class Iter>
    void DivList<C>::insert(Iter rangeBegin, Iter rangeEnd) {
    if (!empty()) {
      for (; rangeBegin != rangeEnd; ++rangeBegin)
        insert(*rangeBegin);
      return;
    }
    if (rangeBegin == rangeEnd)
      return;

    _divMaskCalculator.rebuild(rangeBegin, rangeEnd, _conf);
    for (; rangeBegin != rangeEnd; ++rangeBegin)
      _list.push_back(ExtEntry(*rangeBegin, _divMaskCalculator, _conf));
    if (_conf.getSortOnInsert())
      DivListHelper::sortAll(_conf, _list);
    resetNumberOfChangesTillRebuild();
  }

  template<class C>
  template<class MO>
  bool DivList<C>::removeMultiples(const Monomial& monomial, MO& out) {
    ExtMonoRef extMonomial(monomial, _divMaskCalculator, _conf);
#ifdef MATHIC_DEBUG
    const size_t origSize = size();
#endif
    const size_t removedCount =
      DivListHelper::removeMultiples(_conf, _list, extMonomial, out);
    MATHIC_ASSERT_NO_ASSUME(size() + removedCount == origSize);
    reportChanges(removedCount);
    return removedCount > 0;
  }

  template<class C>
  bool DivList<C>::removeMultiples(const Monomial& monomial) {
    DummyMultipleOutput out;
    return removeMultiples(monomial, out);
  }

  template<class C>
  bool DivList<C>::removeElement(const Monomial& monomial) {
    const size_t varCount = _conf.getVarCount();
    for (ListIter it = _list.begin(); it != _list.end(); ++it) {
      for (size_t var = 0; var < varCount; ++var) {
        if (_conf.getExponent(monomial, var) !=
          _conf.getExponent(it->get(), var)) {
          goto skip;
        }
      }
      _list.erase(it);
      return true;
    skip:;
    }
    return false;
  }

  template<class C>
  typename DivList<C>::iterator
  DivList<C>::findDivisorIterator(const Monomial& monomial) {
    ExtMonoRef extMonomial(monomial, _divMaskCalculator, _conf);

    if (!_conf.getSortOnInsert()) {
      ListIter listEnd = _list.end();
      for (ListIter it = _list.begin(); it != listEnd; ++it) {
        if (it->divides(extMonomial, _conf))
          return iterator(it);
      }
      return iterator(listEnd);
    } else
      return iterator(DivListHelper::findDivisorSorted(_conf, _list, extMonomial));
  }

  template<class C>
  typename DivList<C>::Entry*
  DivList<C>::findDivisor(const Monomial& monomial) {
    ExtMonoRef extMonomial(monomial, _divMaskCalculator, _conf);

    if (!_conf.getSortOnInsert()) {
      const ListIter listEnd = _list.end();
      for (ListIter it = _list.begin(); it != listEnd; ++it) {
        if (it->divides(extMonomial, _conf))
          return &it->get();
      }
      return 0;
    } else {
      ListIter it = DivListHelper::findDivisorSorted(_conf, _list, extMonomial);
      return it == _list.end() ? 0 : &it->get();
    }
  }

  template<class C>
  const typename DivList<C>::Entry*
  DivList<C>::findDivisor(const Monomial& monomial) const {
    return const_cast<DivList<C>&>(*this).findDivisor(monomial);
  }

  template<class C>
  template<class DO>
  void DivList<C>::findAllDivisors(const Monomial& monomial, DO& out) {
    ExtMonoRef extMonomial(monomial, _divMaskCalculator, _conf);
    if (!_conf.getSortOnInsert()) {
      const ListIter listEnd = _list.end();
      for (ListIter it = _list.begin(); it != listEnd; ++it)
        if (it->divides(extMonomial, _conf))
          if (!out.proceed(it->get()))
            break;
    } else
      DivListHelper::findAllDivisorsSorted(_conf, _list, extMonomial, out);
  }

  template<class C>
  template<class DO>
  void DivList<C>::findAllMultiples(const Monomial& monomial, DO& out) {
    // todo: consider doing sorted version
    ExtMonoRef extMonomial(monomial, _divMaskCalculator, _conf);
    const ListIter listEnd = _list.end();
    for (ListIter it = _list.begin(); it != listEnd; ++it)
      if (extMonomial.divides(*it, _conf))
        if (!out.proceed(it->get()))
          break;
  }

  template<class C>
  template<class DO>
  void DivList<C>::findAllDivisors(const Monomial& monomial,
                                     DO& output) const {
    ConstEntryOutput<DO> constOutput(output);
    const_cast<DivList<C>&>(*this).findAllDivisors(monomial, constOutput);
  }

  template<class C>
  template<class EntryOutput>
  void DivList<C>::forAll(EntryOutput& output) {
    iterator stop = end();
    for (iterator it = begin(); it != stop; ++it)
      if (!output.proceed(*it))
        return;
  }

  template<class C>
  template<class EntryOutput>
  void DivList<C>::forAll(EntryOutput& output) const {
    ConstEntryOutput<EntryOutput> constOutput(output);
    const_cast<DivList<C>&>(*this).forAll(constOutput);
  }

  template<class C>
  std::string DivList<C>::getName() const {
    std::ostringstream out;
    out <<"DivList"
        << (UseLinkedList ? " linked" : " array");
    if (UseDivMask && _conf.getDoAutomaticRebuilds()) {
      out << " autob:" << _conf.getRebuildRatio()
          << '/' << _conf.getRebuildMin();
    }
    out << (_conf.getSortOnInsert() ? " sort" : "")
        << (UseDivMask ? " dmask" : "");
    return out.str();
  }

  template<class C>
    void DivList<C>::moveToFront(iterator pos) {
    DivListHelper::moveToFront(_list, pos.getInternal());
  }

  template<class C>
    void DivList<C>::rebuild() {
    const size_t totalSize = size();
    typedef memt::ArenaVector<Entry, true> TmpContainer;
    TmpContainer tmpCopy(memt::Arena::getArena(), totalSize);
    std::copy(begin(), end(), std::back_inserter<TmpContainer>(tmpCopy));
    _divMaskCalculator.rebuild(tmpCopy.begin(), tmpCopy.end(), _conf);
    ListIter listEnd = _list.end();
    for (ListIter it = _list.begin(); it != listEnd; ++it)
      it->recalculateDivMask(_divMaskCalculator, _conf);
    resetNumberOfChangesTillRebuild();
  }

  template<class C>
    void DivList<C>::resetNumberOfChangesTillRebuild() {
    if (!_conf.getDoAutomaticRebuilds())
      return;
    MATHIC_ASSERT(_conf.getRebuildRatio() > 0);
    _changesTillRebuild = std::max
      (static_cast<size_t>(size() * _conf.getRebuildRatio()),
       _conf.getRebuildMin());
  }

  template<class C>
    void DivList<C>::reportChanges(size_t changesMadeCount) {
    // note how negative value/overflow of _changesTillRebuild cannot
    // happen this way.
    if (!_conf.getDoAutomaticRebuilds())
      return;
    if (_changesTillRebuild > changesMadeCount)
      _changesTillRebuild -= changesMadeCount;
    else
      rebuild();
  }

  template<class C>
  size_t DivList<C>::getMemoryUse() const {
	return _list.capacity() * sizeof(_list.front());
  }
}

#endif
