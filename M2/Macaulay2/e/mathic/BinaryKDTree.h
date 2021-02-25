#ifndef MATHIC_BINARY_K_D_TREE_GUARD
#define MATHIC_BINARY_K_D_TREE_GUARD

#include "stdinc.h"
#include "DivMask.h"
#include "KDEntryArray.h"
#include "memtailor/memtailor.h"
#include <ostream>

namespace mathic {
  template<class C>
  class BinaryKDTree {
  public:
    typedef typename C::Monomial Monomial;
    typedef typename C::Entry Entry;
    typedef typename C::Exponent Exponent;
    typedef typename DivMask::Extender<Entry, C::UseDivMask> ExtEntry;
    typedef typename DivMask::Extender<const Monomial&,C::UseDivMask> ExtMonoRef;
    typedef typename DivMask::Calculator<C> DivMaskCalculator;

    struct ExpOrder {
    ExpOrder(size_t var, const C& conf): _var(var), _conf(conf) {}
      bool operator()(const ExtEntry& a, const ExtEntry& b) const {
        return _conf.getExponent(a.get(), _var) < _conf.getExponent(b.get(), _var);
      }
    private:
      const size_t _var;
      const C& _conf;
    };

    private:
    /** A helper class for KDTree. A node in the tree. The ExtEntry
        comes from the KdTree. */
    class KDTreeNode;

    /** A helper class for KDTree. An interior node in the tree. The ExtEntry
        comes from the KdTree. @todo: rename to KDTreeInternal. */
    class KDTreeInterior;

    /** A helper class for KDTree. Represents a leaf in the tree. Leaves
        hold the monomials. The Configuration is as for KdTree. The ExtEntry
        comes from the KdTree. */
    class KDTreeLeaf;

    typedef KDTreeNode Node;
    typedef KDTreeInterior Interior;
    typedef KDTreeLeaf Leaf;
    typedef C Configuration;
    static const bool UseDivMask = C::UseDivMask;

    class KDTreeNode {
    public:
      bool isLeaf() const {return _isLeaf;}
      const Leaf& asLeaf() const {
        MATHIC_ASSERT(isLeaf());
        return static_cast<const Leaf&>(*this);
      }
      Leaf& asLeaf() {
        MATHIC_ASSERT(isLeaf());
        return static_cast<Leaf&>(*this);
      }

      bool isInterior() const {return !isLeaf();}
      const Interior& asInterior() const {
        MATHIC_ASSERT(isInterior());
        return static_cast<Interior&>(*this);
      }
      Interior& asInterior() {
        MATHIC_ASSERT(isInterior());
        return static_cast<Interior&>(*this);
      }

    protected:
      KDTreeNode(bool leaf): _isLeaf(leaf) {}

    private:
      KDTreeNode(const KDTreeNode&); // unavailable
      void operator=(const KDTreeNode&); // unavailable

      class SplitEqualOrLess;
      const bool _isLeaf;
    };

    class KDTreeInterior : public KDTreeNode,
      public mathic::DivMask::HasDivMask<C::UseTreeDivMask> {
    public:
      typedef typename C::Exponent Exponent;
      typedef KDTreeInterior Interior;
      typedef KDTreeLeaf Leaf;
      typedef KDTreeNode Node;

      KDTreeInterior
        (Node& equalOrLess,
         Node& strictlyGreater,
         size_t var,
         Exponent exponent):
      Node(false),
        _equalOrLess(&equalOrLess),
        _strictlyGreater(&strictlyGreater),
        _var(var),
        _exponent(exponent) {
        }
      KDTreeInterior
        (size_t var,
         Exponent exponent):
      Node(false),
        _equalOrLess(0),
        _strictlyGreater(0),
        _var(var),
        _exponent(exponent) {
        }
      size_t getVar() const {return _var;}
      Exponent getExponent() const {return _exponent;}

      Node& getEqualOrLess() {return *_equalOrLess;}
      const Node& getEqualOrLess() const {return *_equalOrLess;}
      void setEqualOrLess(Node* equalOrLess) {_equalOrLess = equalOrLess;}

      Node& getStrictlyGreater() {return *_strictlyGreater;}
      const Node& getStrictlyGreater() const {return *_strictlyGreater;}
      void setStrictlyGreater(Node* strictlyGreater) {
        _strictlyGreater = strictlyGreater;
      }

      Node& getChildFor(const ExtEntry& entry, const C& conf) {
        if (getExponent() < conf.getExponent(entry.get(), getVar()))
          return getStrictlyGreater();
        else
          return getEqualOrLess();
      }

      using DivMask::HasDivMask<C::UseTreeDivMask>::updateToLowerBound;
      void updateToLowerBound(Node& node) {
        if (!C::UseTreeDivMask)
          return;
        if (node.isLeaf())
          DivMask::HasDivMask<C::UseTreeDivMask>::
            updateToLowerBound(node.asLeaf().entries());
        else
          DivMask::HasDivMask<C::UseTreeDivMask>::
            updateToLowerBound(node.asInterior());
      }

    private:
      Node* _equalOrLess;
      Node* _strictlyGreater;
      size_t _var;
      Exponent _exponent;
    };

    
    class KDTreeLeaf : public KDTreeNode {
      typedef KDTreeInterior Interior;
      typedef KDTreeLeaf Leaf;
      typedef KDTreeNode Node;
    public:
      typedef typename C::Monomial Monomial;
      typedef ExtEntry* iterator;
      typedef const ExtEntry* const_iterator;
      typedef const ExtEntry& const_reference;
      typedef ExtEntry value_type;
      typedef DivMask::Calculator<C> DivMaskCalculator;

      KDTreeLeaf(memt::Arena& arena, const C& conf);

      /** Copies Entry's in [begin, end) into the new leaf after
       calculating div mask if using those. */
      template<class Iter>
      KDTreeLeaf(Iter begin, Iter end, memt::Arena& arena,
        const DivMaskCalculator& calc, const C& conf);

      /** Copies ExtEntry's [begin, end) into the new leaf. */
      template<class Iter>
      KDTreeLeaf(Iter begin, Iter end, memt::Arena& arena, const C& conf);

      mathic::KDEntryArray<C, ExtEntry>& entries() {return _entries;}
      const KDEntryArray<C, ExtEntry>& entries() const {return _entries;}

      /** When this node is full, call this to insert an element. It
       splits the elements in this noe into two leaves and returns
       a new interior node that is the parent of them. */
      Interior& splitInsert(const ExtEntry& entry, Interior* parent,
        memt::Arena& arena, const C& conf);

    private:
      KDEntryArray<C, ExtEntry> _entries;
    };

  public:
    typedef typename Leaf::iterator LeafIt;

  public:
    BinaryKDTree(const C& configuration);
    ~BinaryKDTree();

    template<class MultipleOutput>
    size_t removeMultiples(const ExtMonoRef& monomial, MultipleOutput& out);

    bool removeElement(const Monomial& monomial);

    void insert(const ExtEntry& entry);

    template<class Iter>
    void reset(Iter begin, Iter end, const DivMaskCalculator& calc);

    inline Entry* findDivisor(const ExtMonoRef& monomial);

    template<class DivisorOutput>
    inline void findAllDivisors
      (const ExtMonoRef& monomial, DivisorOutput& out);

    template<class Output>
    inline void findAllMultiples
      (const ExtMonoRef& monomial, Output& out);

    template<class EntryOutput>
    void forAll(EntryOutput& out);

    void clear();

    size_t getMemoryUse() const;

    C& getConfiguration() {return _conf;}

    void print(std::ostream& out) const;

    /// Asserts internal invariants if asserts are turned on.
    bool debugIsValid() const;

  private:
    BinaryKDTree(const BinaryKDTree<C>&); // unavailable
    void operator=(const BinaryKDTree<C>&); // unavailable
  
    template<class Iter>
    struct InsertTodo {
      Iter begin;
      Iter end;
      Interior* parent;
    };

    memt::Arena _arena; // Everything permanent allocated from here.
    C _conf; // User supplied configuration.
    mutable std::vector<Node*> _tmp; // For navigating the tree.
    Node* _root; // Root of the tree. Can be null!
  };

  template<class C>
  BinaryKDTree<C>::BinaryKDTree(const C& configuration):
  _conf(configuration), _root(0) {
    MATHIC_ASSERT(C::LeafSize > 0);
    MATHIC_ASSERT(debugIsValid());
  }

  template<class C>
  BinaryKDTree<C>::~BinaryKDTree() {
    clear();
  }

  template<class C>
  template<class MO>
  size_t BinaryKDTree<C>::removeMultiples(const ExtMonoRef& extMonomial, MO& out) {
    MATHIC_ASSERT(_tmp.empty());
    if (_root == 0)
      return 0;
    size_t removedCount = 0;
    Node* node = _root;
    while (true) {
      while (node->isInterior()) {
        Interior& interior = node->asInterior();
        if (!(interior.getExponent() <
              _conf.getExponent(extMonomial.get(), interior.getVar())))
          _tmp.push_back(&interior.getEqualOrLess());
        node = &interior.getStrictlyGreater();
      }
      MATHIC_ASSERT(node->isLeaf());
      removedCount += node->asLeaf().entries().removeMultiples(extMonomial, out, _conf);
      if (_tmp.empty())
        break;
      node = _tmp.back();
      _tmp.pop_back();
    }
    MATHIC_ASSERT(_tmp.empty());
    MATHIC_ASSERT(debugIsValid());
    return removedCount;
  }

  template<class C>
  bool BinaryKDTree<C>::removeElement(const Monomial& monomial) {
    MATHIC_ASSERT(_tmp.empty());
    if (_root == 0)
      return 0;
    Node* node = _root;

    while (node->isInterior())
      node = node->asInterior().getChildFor(monomial, _conf);
    const bool value = node->asLeaf().removeElement(monomial);
    MATHIC_ASSERT(debugIsValid());
    return value;
  }

  template<class C>
  void BinaryKDTree<C>::insert(const ExtEntry& extEntry) {
    Interior* parent = 0;
    if (_root == 0)
      _root = new (_arena.allocObjectNoCon<Leaf>()) Leaf(_arena, _conf);
    Node* node = _root;
    while (node->isInterior()) {
      parent = &node->asInterior();
      if (C::UseTreeDivMask)
        parent->updateToLowerBound(extEntry);
      node = &parent->getChildFor(extEntry, _conf);
    }
    Leaf* leaf = &node->asLeaf();

    MATHIC_ASSERT(leaf->entries().size() <= C::LeafSize);
    if (leaf->entries().size() == C::LeafSize) {
      Interior& interior = leaf->splitInsert(extEntry, parent, _arena, _conf);
      if (parent == 0) {
        MATHIC_ASSERT(leaf == _root);
        _root = &interior;
      }
    } else {
      MATHIC_ASSERT(leaf->entries().size() < C::LeafSize);
      leaf->entries().insert(extEntry, _conf);
      MATHIC_ASSERT(debugIsValid());
    }
  }

  template<class C>
  template<class Iter>
  void BinaryKDTree<C>::reset(Iter insertBegin, Iter insertEnd, const DivMaskCalculator& calc) {
    clear();
    if (insertBegin == insertEnd)
      return;

    typedef InsertTodo<Iter> Task;
    typedef std::vector<Task> TaskCont;
    TaskCont todo;

    Interior* parent = 0;
    bool isEqualOrLessChild = false;
    while (true) {
      Node* node = 0;
      const size_t insertCount = std::distance(insertBegin, insertEnd);
      const bool isLeaf = (insertCount <= C::LeafSize);
      if (isLeaf)
        node = new (_arena.allocObjectNoCon<Leaf>())
          Leaf(insertBegin, insertEnd, _arena, calc, _conf);
      else {
        size_t var =
          (parent == 0 ? static_cast<size_t>(-1) : parent->getVar());

        Exponent exp;
        Iter middle = KDEntryArray<C, ExtEntry>::
          split(insertBegin, insertEnd, var, exp, _conf);
        Interior* interior = new (_arena.allocObjectNoCon<Interior>())
          Interior(var, exp);

        MATHIC_ASSERT(middle != insertBegin && middle != insertEnd);
        // push strictly-greater on todo
        Task task;
        task.begin = middle;
        task.end = insertEnd;
        task.parent = interior;
        todo.push_back(task);
        // set up equal-or-less
        insertEnd = middle;
        node = interior;
      }

      if (parent == 0) {
        MATHIC_ASSERT(_root == 0);
        _root = node;
      } else if (isEqualOrLessChild)
        parent->setEqualOrLess(node);
      else
        parent->setStrictlyGreater(node);

      if (isLeaf) {
        // grab next item from todo
        if (todo.empty())
          break;
        Task task = todo.back();
        todo.pop_back();
        insertBegin = task.begin;
        insertEnd = task.end;
        parent = task.parent;
        // only strictlyGreater goes on todo
        isEqualOrLessChild = false;
      } else {
        isEqualOrLessChild = true;
        parent = &node->asInterior();
        // continue with equal-or-less as next item      
      }
    }
    MATHIC_ASSERT(_root != 0);

    if (C::UseTreeDivMask) {
      // record nodes in tree using breadth first search
      typedef std::vector<Interior*> NodeCont;
      NodeCont nodes;
      if (_root->isInterior())
        nodes.push_back(&_root->asInterior());
      for (size_t i = 0; i < nodes.size(); ++i) {
        Interior* node = nodes[i];
        if (node->getEqualOrLess().isInterior())
          nodes.push_back(&node->getEqualOrLess().asInterior());
        if (node->getStrictlyGreater().isInterior())
          nodes.push_back(&node->getStrictlyGreater().asInterior());
      }
      // compute div masks in reverse order of breath first search
      typename NodeCont::reverse_iterator it = nodes.rbegin();
      typename NodeCont::reverse_iterator end = nodes.rend();
      for (; it != end; ++it) {
        Interior* node = *it;
        node->updateToLowerBound(node->getEqualOrLess());
        node->updateToLowerBound(node->getStrictlyGreater());
      }
    }
    MATHIC_ASSERT(debugIsValid());
  }

  template<class C>
  typename BinaryKDTree<C>::Entry* BinaryKDTree<C>::findDivisor
    (const ExtMonoRef& extMonomial) {

    MATHIC_ASSERT(debugIsValid());
    MATHIC_ASSERT(_tmp.empty());
    if (_root == 0)
      return 0;
    Node* node = _root;
    while (true) {
      while (node->isInterior()) {
        Interior& interior = node->asInterior();
        if (C::UseTreeDivMask &&
            !interior.getDivMask().canDivide(extMonomial.getDivMask()))
          goto next;

        if (interior.getExponent() <
            _conf.getExponent(extMonomial.get(), interior.getVar()))
          _tmp.push_back(&interior.getStrictlyGreater());
        node = &interior.getEqualOrLess();
      }

      {
        MATHIC_ASSERT(node->isLeaf());
        Leaf& leaf = node->asLeaf();
        LeafIt leafIt = leaf.entries().findDivisor(extMonomial, _conf);
        if (leafIt != leaf.entries().end()) {
          MATHIC_ASSERT(_conf.divides(leafIt->get(), extMonomial.get()));
          _tmp.clear();
          return &leafIt->get();
        }
      }
    next:
      if (_tmp.empty())
        break;
      node = _tmp.back();
      _tmp.pop_back();
    }
    MATHIC_ASSERT(_tmp.empty());
    return 0;
  }

  template<class C>
  template<class DO>
  void BinaryKDTree<C>::findAllDivisors(const ExtMonoRef& extMonomial, DO& output) {
    MATHIC_ASSERT(_tmp.empty());
    if (_root == 0)
      return;
    Node* node = _root;
    while (true) {
      while (node->isInterior()) {
        Interior& interior = node->asInterior();
        if (C::UseTreeDivMask &&
            !interior.getDivMask().canDivide(extMonomial.getDivMask()))
          goto next;
        if (interior.getExponent() <
            _conf.getExponent(extMonomial.get(), interior.getVar()))
          _tmp.push_back(&interior.getStrictlyGreater());
        node = &interior.getEqualOrLess();
      }
      MATHIC_ASSERT(node->isLeaf());
      {
        Leaf& leaf = node->asLeaf();
        if (!leaf.entries().findAllDivisors(extMonomial, output, _conf)) {
          _tmp.clear();
          break;
        }
      }
next:
      if (_tmp.empty())
        break;
      node = _tmp.back();
      _tmp.pop_back();
    }
    MATHIC_ASSERT(_tmp.empty());
  }

  template<class C>
  template<class DO>
  void BinaryKDTree<C>::findAllMultiples(const ExtMonoRef& extMonomial, DO& output) {
    MATHIC_ASSERT(_tmp.empty());
    if (_root == 0)
      return;
    Node* node = _root;
    while (true) {
      while (node->isInterior()) {
        Interior& interior = node->asInterior();
        if (!(interior.getExponent() <
            _conf.getExponent(extMonomial.get(), interior.getVar())))
          _tmp.push_back(&interior.getEqualOrLess());
        node = &interior.getStrictlyGreater();
      }
      MATHIC_ASSERT(node->isLeaf());
      {
        Leaf& leaf = node->asLeaf();
        if (!leaf.entries().findAllMultiples(extMonomial, output, _conf)) {
          _tmp.clear();
          break;
        }
      }
//next:
      if (_tmp.empty())
        break;
      node = _tmp.back();
      _tmp.pop_back();
    }
    MATHIC_ASSERT(_tmp.empty());
  }

  template<class C>
  template<class EO>
  void BinaryKDTree<C>::forAll(EO& output) {
    MATHIC_ASSERT(_tmp.empty());
    if (_root == 0)
      return;
    Node* node = _root;
    while (true) {
      while (node->isInterior()) {
        Interior& interior = node->asInterior();
        _tmp.push_back(&interior.getStrictlyGreater());
        node = &interior.getEqualOrLess();
      }
      MATHIC_ASSERT(node->isLeaf());
      Leaf& leaf = node->asLeaf();
      if (!leaf.entries().forAll(output)) {
        _tmp.clear();
        break;
      }
      if (_tmp.empty())
        break;
      node = _tmp.back();
      _tmp.pop_back();
    }
    MATHIC_ASSERT(_tmp.empty());
  }

  template<class C>
  void BinaryKDTree<C>::clear() {
    MATHIC_ASSERT(_tmp.empty());
    // Call Entry destructors
    if (_root != 0)
      _tmp.push_back(_root);
    while (!_tmp.empty()) {
      Node* node = _tmp.back();
      _tmp.pop_back();
      while (node->isInterior()) {
        _tmp.push_back(&node->asInterior().getStrictlyGreater());
        node = &node->asInterior().getEqualOrLess();
      }
      MATHIC_ASSERT(node->isLeaf());
      node->asLeaf().entries().clear();
    }
    _arena.freeAllAllocs();
    _root = 0;
  }

  template<class C>
  size_t BinaryKDTree<C>::getMemoryUse() const {
	size_t sum = _arena.getMemoryUse();
	sum += _tmp.capacity() * sizeof(_tmp.front());
	return sum;
  }

  template<class C>
  void BinaryKDTree<C>::print(std::ostream& out) const {
    out << "<<<<<<<< BinaryKDTree >>>>>>>>\n";
    MATHIC_ASSERT(_tmp.empty());
    if (_root != 0) {
      Node* node = _root;
      while (true) {
        if (node->isInterior()) {
          Interior& interior = node->asInterior();
          out << "**** Interior Node " << &interior << '\n';
          out << "Split on " << interior.getVar() <<
            '^' << interior.getExponent() << '\n';
          out << "Child <=: " << &interior.getEqualOrLess() << '\n';
          out << "Child > : " << &interior.getStrictlyGreater() << '\n';
          out << '\n';
          _tmp.push_back(&interior.getEqualOrLess());
          _tmp.push_back(&interior.getStrictlyGreater());
        } else {
          Leaf& leaf = node->asLeaf();
          out << "**** Leaf Node " << &leaf << '\n';
          for (size_t i = 0; i < leaf.entries().size(); ++i) {
            out << "Entry " << (i + 1) << ": "
              << leaf.entries().begin()[i].get() << '\n';
          }
          out << '\n';
        }
        if (_tmp.empty())
          break;
        node = _tmp.back();
        _tmp.pop_back();
      }
      MATHIC_ASSERT(_tmp.empty());
    }
  }

  template<class C>
  bool BinaryKDTree<C>::debugIsValid() const {
#ifndef MATHIC_DEBUG
    return true;
#else
    MATHIC_ASSERT(_tmp.empty());
    MATHIC_ASSERT(!_conf.getDoAutomaticRebuilds() || _conf.getRebuildRatio() > 0);

    if (_root == 0)
      return true;

    // record all nodes
    std::vector<Node*> nodes;
    nodes.push_back(_root);
    size_t sizeSum = 0;
    for (size_t i = 0; i < nodes.size(); ++i) {
      Node* node = nodes[i];
      if (node->isInterior()) {
        MATHIC_ASSERT(node->asInterior().getVar() < _conf.getVarCount());
        nodes.push_back(&node->asInterior().getStrictlyGreater());
        nodes.push_back(&node->asInterior().getEqualOrLess());
      } else
        sizeSum += node->asLeaf().entries().size();
    }

    // check the recorded nodes
    MATHIC_ASSERT(_tmp.empty());
    for (size_t i = 0; i < nodes.size(); ++i) {
      Node* nodei = nodes[i];
      if (nodei->isLeaf()) {
        MATHIC_ASSERT(nodei->asLeaf().entries().debugIsValid());
        continue;
      }
      Interior& interior = nodei->asInterior();
      size_t var = interior.getVar();
      Exponent exp = interior.getExponent();

      MATHIC_ASSERT(_tmp.empty());
      // check equal or less than sub tree
      _tmp.push_back(&interior.getEqualOrLess());
      while (!_tmp.empty()) {
        Node* node = _tmp.back();
        _tmp.pop_back();
        if (C::UseTreeDivMask) {
          if (node->isInterior())
            MATHIC_ASSERT(interior.getDivMask().canDivide(node->asInterior().getDivMask()));
          else
            MATHIC_ASSERT(interior.getDivMask().canDivide(node->asLeaf().entries().getDivMask()));
        }
        if (node->isInterior()) {
          _tmp.push_back(&node->asInterior().getStrictlyGreater());
          _tmp.push_back(&node->asInterior().getEqualOrLess());
        } else {
          MATHIC_ASSERT(node->asLeaf().entries().allLessThanOrEqualTo(var, exp, _conf));
        }
      }

      // check strictly greater
      _tmp.push_back(&interior.getStrictlyGreater());
      while (!_tmp.empty()) {
        Node* node = _tmp.back();
        _tmp.pop_back();
        if (C::UseTreeDivMask) {
          if (node->isInterior())
            MATHIC_ASSERT(interior.getDivMask().canDivide(node->asInterior().getDivMask()));
          else
            MATHIC_ASSERT(interior.getDivMask().canDivide(node->asLeaf().entries().getDivMask()));
        }
        if (node->isInterior()) {
          _tmp.push_back(&node->asInterior().getStrictlyGreater());
          _tmp.push_back(&node->asInterior().getEqualOrLess());
        } else {
          MATHIC_ASSERT(node->asLeaf().entries().allStrictlyGreaterThan(var, exp, _conf));
        }
      }
    }
    return true;
#endif
  }

  template<class C>
  BinaryKDTree<C>::KDTreeLeaf::KDTreeLeaf(memt::Arena& arena, const C& conf):
  Node(true), _entries(arena, conf) {}

  template<class C>  
  template<class Iter>
  BinaryKDTree<C>::KDTreeLeaf::KDTreeLeaf(
    Iter begin,
    Iter end,
    memt::Arena& arena,
    const DivMaskCalculator& calc,
    const C& conf):
    Node(true), _entries(begin, end, arena, calc, conf) {}

  template<class C>  
  template<class Iter>
  BinaryKDTree<C>::KDTreeLeaf::KDTreeLeaf(
    Iter begin,
    Iter end,
    memt::Arena& arena,
    const C& conf):
    Node(true), _entries(begin, end, arena, conf) {}

  template<class C>
  class BinaryKDTree<C>::KDTreeNode::SplitEqualOrLess {
  public:
    typedef typename C::Exponent Exponent;
    typedef typename C::Entry Entry;
  SplitEqualOrLess(size_t var, const Exponent& exp, const C& conf):
    _var(var), _exp(exp), _conf(conf) {}
    bool operator()(const Entry& entry) const {
      return !(_exp < _conf.getExponent(entry, _var));
    }
  private:
    size_t _var;
    const Exponent& _exp;
    const C& _conf;
  };

  template<class C>
  typename BinaryKDTree<C>::KDTreeInterior&
  BinaryKDTree<C>::KDTreeLeaf::splitInsert(const ExtEntry& extEntry, Interior* parent, memt::Arena& arena, const C& conf) {
    MATHIC_ASSERT(conf.getVarCount() > 0);
    MATHIC_ASSERT(entries().size() > 0);
    size_t var = (parent == 0 ? static_cast<size_t>(-1) : parent->getVar());
    typename C::Exponent exp;

    iterator middle = KDEntryArray<C, ExtEntry>::split
      (entries().begin(), entries().end(), var, exp, conf, &extEntry);
    Leaf& other = *new (arena.allocObjectNoCon<Leaf>()) Leaf(middle, entries().end(), arena, conf);
    while (middle != entries().end())
      entries().pop_back();
    if (conf.getSortOnInsert())
      std::sort(entries().begin(), entries().end(), Comparer<C>(conf));

    Interior& interior = *new (arena.allocObjectNoCon<Interior>())
      Interior(*this, other, var, exp);
    if (C::UseTreeDivMask) {
      entries().recalculateTreeDivMask();
      interior.updateToLowerBound(entries());
      interior.updateToLowerBound(other.entries());
    }
    if (parent != 0) {
      if (&parent->getEqualOrLess() == this)
        parent->setEqualOrLess(&interior);
      else {
        MATHIC_ASSERT(&parent->getStrictlyGreater() == this);
        parent->setStrictlyGreater(&interior);
      }
    }

    interior.updateToLowerBound(extEntry);
    Leaf* insertLeaf = &interior.getChildFor(extEntry, conf).asLeaf();
    MATHIC_ASSERT(insertLeaf->entries().size() < C::LeafSize);
    insertLeaf->entries().insert(extEntry, conf);

    return interior;
  }
}

#endif
