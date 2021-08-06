#ifndef MATHIC_K_D_TREE_GUARD
#define MATHIC_K_D_TREE_GUARD

#include "stdinc.h"
#include "DivMask.h"
#include "BinaryKDTree.h"
#include "PackedKDTree.h"
#include "memtailor/memtailor.h"
#include <list>
#include <string>
#include <algorithm>
#include <iterator>
#include <sstream>
#include <vector>

namespace mathic {
  /** An object that supports queries for divisors of a monomial using
      a KD Tree (K Dimensional Tree). See DivFinder.h for more documentation.

      Extra fields for Configuration:

      * bool getSortOnInsert() const
      Return true to keep the monomials in leaves sorted to speed up queries.

      * size_t getLeafSize() const
      Return the fixed maximal size of a leaf.

      * static const bool AllowRemovals
      If false, it is an error to call methods that remove elements from
      the data structure. This can be a slight speed up in some cases.
      Clear and rebuild is still allowed even if this field is false.
  */
  template<class Configuration>
  class KDTree;

  namespace KDTreeInternal {
    // cannot do this inside KDTree as template partial specializations
    // cannot be done inside a class.
    template<class C, bool Pack>
    struct SelectTree {
      typedef BinaryKDTree<C> Tree;
    };
    template<class C>
    struct SelectTree<C, true> {
      typedef PackedKDTree<C> Tree;
    };
  }

  template<class C>
  class KDTree {
  private:
    // for figuring out which kd tree implementation to use
    typedef typename KDTreeInternal::SelectTree<C, C::PackedTree>::Tree Tree;
    typedef typename Tree::ExtEntry ExtEntry;
    typedef typename Tree::ExtMonoRef ExtMonoRef;
  public:
    typedef C Configuration;

    /** Constructs an object with the given configuration. The configuration
        is copied into the object, so a reference to the passed-in object is
        not kept. The configuration is not copied other than the initial copy. */
    KDTree(const C& configuration):
      _divMaskCalculator(configuration),
      _tree(configuration),
      _size(0) {
      resetNumberOfChangesTillRebuild();
      if (getConfiguration().getUseDivisorCache())
        _divisorCache = 0;
    }

    static const bool UseDivMask = C::UseDivMask;
    typedef typename C::Monomial Monomial;
    typedef typename C::Entry Entry;
    typedef typename C::Exponent Exponent;

    /** Returns whether there are any entries. */
    bool empty() const {return size() == 0;}

    /** Returns the number of entries. */
    size_t size() const {return _size;}

    /** Returns a string that describes the data structure. */
    std::string getName() const;

    /** Returns a reference to this object's configuration object. */
    C& getConfiguration() {
      return _tree.getConfiguration();
    }

    /** Returns a reference to this object's configuration object. */
    const C& getConfiguration() const {
      return const_cast<Tree&>(_tree).getConfiguration();
    }

    /** Removes all multiples of monomial. A duplicate counts
        as a multiple. Returns true if any multiples were removed. */
    bool removeMultiples(const Monomial& monomial) {
      MATHIC_ASSERT(C::AllowRemovals);
      if (!C::AllowRemovals)
        throw std::logic_error("Removal request while removals disabled.");
      DummyMultipleOutput out;
      return removeMultiples(monomial, out);
    }

    /** Removes all multiples of monomial. A duplicate counts
        as a multiple. Returns true if any multiples were removed.
        Calls out.push_back(entry) for each entry that is removed. */
    template<class MultipleOutput>
    bool removeMultiples(const Monomial& monomial, MultipleOutput& out) {
      MATHIC_ASSERT(C::AllowRemovals);
      if (!C::AllowRemovals)
        throw std::logic_error("Removal request while removals disabled.");
      ExtMonoRef extMonomial(monomial, _divMaskCalculator, getConfiguration());
      size_t removedCount = _tree.removeMultiples(extMonomial, out);
      reportChanges(0, removedCount);
      return removedCount > 0;
    }

    /** Calls out.proceed(entry) for each entry that monomial divides.
        The method returns if proceed returns false, otherwise the
        search for divisors proceeds. */
    template<class Output>
    void findAllMultiples(const Monomial& monomial, Output& out) {
      ExtMonoRef extMonomial(monomial, _divMaskCalculator, getConfiguration());
      _tree.findAllMultiples(extMonomial, out);
    }

    /** Calls out.proceed(entry) for each entry that monomial divides.
        The method returns if proceed returns false, otherwise the
        search for divisors proceeds. */
    template<class Output>
    void findAllMultiples(const Monomial& monomial, Output& output) const {
      ConstEntryOutput<Output> constOutput(output);
      const_cast<KDTree<C>&>(*this).findAllMultiples(monomial, constOutput);
    }

    /** Inserts entry into the data structure. Does NOT remove multiples
        of entry and entry is inserted even if it is a multiple of another
        entry. */
    void insert(const Entry& entry) {
      ExtEntry extEntry(entry, _divMaskCalculator, getConfiguration());
      _tree.insert(extEntry);
      reportChanges(1, 0);
    }

    /** Inserts the entries in the range [begin, end) into the data
        structure. Does NOT remove multiples of entry and entry is inserted
        even if it is a multiple of another entry.

        The elements in the range [begin, end) may be rearranged by this
        function, so the range must be mutable. If that is not acceptable,
        call the one element insert method for each element. */
    template<class Iter>
    void insert(Iter begin, Iter end) {
      if (begin == end)
        return;
      const size_t inserted = std::distance(begin, end); 
      if (!empty()) {
        for (; begin != end; ++begin)
          _tree.insert(*begin);
      } else {
        _tree.insert(begin, end);
        // insert into empty container is equivalent to rebuild
        resetNumberOfChangesTillRebuild();
      }
      reportChanges(inserted, 0);
    }

    /** Removes an element whose exponents are equal to monomial's. Returns
      if there are no such monomials in the data structure. */
    bool removeElement(const Monomial& monomial) {
      MATHIC_ASSERT(C::AllowRemovals);
      if (!C::AllowRemovals)
        throw std::logic_error("Removal request while removals disabled.");
      return _tree.removeElement(monomial);
    }

    /** Returns a pointer to an entry that divides monomial. Returns null if no
        entries divide monomial. */
    inline Entry* findDivisor(const Monomial& monomial) {
      // todo: do this on extended monomials. requires cache to be extended.
      const C& conf = getConfiguration();
      if (conf.getUseDivisorCache() &&
        _divisorCache != 0 &&
        conf.divides(*_divisorCache, monomial))
        return _divisorCache;

      ExtMonoRef extMonomial(monomial, _divMaskCalculator, getConfiguration());
      Entry* divisor = _tree.findDivisor(extMonomial);
      if (conf.getUseDivisorCache() && divisor != 0)
        _divisorCache = divisor;
      return divisor;
    }

    /** Returns the position of a divisor of monomial. Returns null if no
        entries divide monomial. */
    inline const Entry* findDivisor(const Monomial& monomial) const {
      return const_cast<KDTree<C>&>(*this).findDivisor(monomial);
    }

    /** Calls out.proceed(entry) for each entry that divides monomial.
        The method returns if proceed returns false, otherwise the
        search for divisors proceeds. */
    template<class DivisorOutput>
    void findAllDivisors(const Monomial& monomial, DivisorOutput& out) {
      ExtMonoRef extMonomial(monomial, _divMaskCalculator, getConfiguration());
      _tree.findAllDivisors(extMonomial, out);
    }

    /** Calls output.proceed(entry) for each entry that divides monomial.
        The method returns if proceed returns false, otherwise the
        search for divisors proceeds. */
    template<class DivisorOutput>
    void findAllDivisors(const Monomial& monomial, DivisorOutput& output) const {
      ConstEntryOutput<DivisorOutput> constOutput(output);
      const_cast<KDTree<C>&>(*this).findAllDivisors(monomial, constOutput);
    }

    /** Calls output.proceed(entry) for each entry.
        The method returns if proceed returns false, otherwise the
        search for divisors proceeds. */
    template<class EntryOutput>
    void forAll(EntryOutput& out) {
      _tree.forAll(out);
    }

    /** Calls out.proceed(entry) for each entry.
        The method returns if proceed returns false, otherwise the
        search for divisors proceeds. */
    template<class EntryOutput>
    void forAll(EntryOutput& output) const {
      ConstEntryOutput<EntryOutput> constOutput(output);
      const_cast<KDTree<C>&>(*this).forAll(constOutput);
    }

    /** Removes all entries. Does not reset the configuration object. */
    void clear() {
      _tree.clear();
      resetNumberOfChangesTillRebuild();
      _tree.calc().rebuildDefault(getConfiguration());
    }

    /** Rebuilds the data structure. */
    void rebuild() {
      EntryRecorder recorder(memt::Arena::getArena(), size());
      _tree.forAll(recorder);
      _divMaskCalculator.rebuild
        (recorder.begin(), recorder.end(), getConfiguration());
      _tree.reset(recorder.begin(), recorder.end(), _divMaskCalculator);
      resetNumberOfChangesTillRebuild();
    }

	/** Returns the number of bytes allocated by this object. Does not
		include sizeof(*this), does not include any additional memory
		that the configuration may have allocated and does not include
		any memory that an Entry may point to. Does include
		sizeof(Entry) as well as unused memory that is being kept to
		avoid frequent allocations. */
    size_t getMemoryUse() const {return _tree.getMemoryUse();}

  private:
    KDTree(const KDTree<C>&); // unavailable
    void operator=(const KDTree<C>&); // unavailable

    // For recording all entries in the tree using forAll.
    class EntryRecorder {
    public:
      EntryRecorder(memt::Arena& arena, size_t capacity):
        _entries(arena, capacity) {}
      bool proceed(const Entry& entry) {
        _entries.push_back(entry);
       return true;
      }
      Entry* begin() {return _entries.begin();}
      Entry* end() {return _entries.end();}

    private:
      memt::ArenaVector<Entry, true> _entries;
    };

    /// makes the parameter given to proceed be const.
    template<class EntryOutput>
    class ConstEntryOutput {
    public:
    ConstEntryOutput(EntryOutput& out): _out(out) {}
      bool proceed(const Entry& entry) {return _out.proceed(entry);}
    private:
      EntryOutput& _out;
    };

    /// Ignores everything passed to it. */
    class DummyMultipleOutput {
    public:
      void push_back(Entry& e) {}
    };

    void reportChanges(size_t additions, size_t removals);
    void resetNumberOfChangesTillRebuild();
    bool reportChangesRebuild(size_t additions, size_t removals);
    size_t _changesTillRebuild; /// Update using reportChanges().

    Entry* _divisorCache; /// The divisor in the previous query. Can be null.

    // All DivMasks calculated using this.
    typename Tree::DivMaskCalculator _divMaskCalculator;
    Tree _tree;
    size_t _size;
  };

  template<class C>
  std::string KDTree<C>::getName() const {
    std::stringstream out;
    const C& conf = getConfiguration();
    out << "KDTree(" << (C::PackedTree ? "packed" : "binary") << ')';
    out << " leaf:" << C::LeafSize;
    if (UseDivMask && conf.getDoAutomaticRebuilds()) {
      out << " autob:" << conf.getRebuildRatio()
          << '/' << conf.getRebuildMin();
    }
    out << (C::UseDivMask && !C::UseTreeDivMask ? " dmask" : "")
        << (C::UseTreeDivMask ? " tree-dmask" : "")
        << (conf.getSortOnInsert() ? " sort" : "")
        << (conf.getUseDivisorCache() ? " cache" : "")
        << (C::AllowRemovals ? "" : " no-removals");
    return out.str();
  }

  template<class C>
  void KDTree<C>::resetNumberOfChangesTillRebuild() {
    const C& conf = getConfiguration();
    if (conf.getUseDivisorCache())
      _divisorCache = 0;
    if (!conf.getDoAutomaticRebuilds())
      return;
    MATHIC_ASSERT(conf.getRebuildRatio() > 0);
    _changesTillRebuild = std::max
      (static_cast<size_t>(size() * conf.getRebuildRatio()),
       conf.getRebuildMin());
  }

  template<class C>
  void KDTree<C>::reportChanges(size_t additions, size_t removals) {
    if (getConfiguration().getUseDivisorCache() && (additions | removals) != 0)
      _divisorCache = 0;
    if (reportChangesRebuild(additions, removals))
      rebuild();
  }

  template<class C>
  bool KDTree<C>::reportChangesRebuild(size_t additions, size_t removals) {
    // note how negative value/overflow of _changesTillRebuild cannot
    // happen this way.
    MATHIC_ASSERT(removals <= size() + additions);
    _size = (size() + additions) - removals;
    if (!getConfiguration().getDoAutomaticRebuilds())
      return false;
    const size_t changesMadeCount = additions + removals;
    if (_changesTillRebuild > changesMadeCount) {
      _changesTillRebuild -= changesMadeCount;
      return false;
    } else
      return true;
  }
}

#endif
