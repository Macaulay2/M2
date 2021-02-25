#ifndef MATHIC_PACKED_K_D_TREE_GUARD
#define MATHIC_PACKED_K_D_TREE_GUARD

#include "stdinc.h"
#include "DivMask.h"
#include "KDEntryArray.h"
#include "memtailor/memtailor.h"
#include <ostream>

namespace mathic {
  template<class C>
  class PackedKDTree {
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
    typedef C Configuration;
    static const bool UseDivMask = C::UseDivMask;

    class Node {
    public:
      static Node* makeNode(memt::Arena& arena, const C& conf) {
        return new (arena.alloc(sizeOf(0)))
          Node(arena, conf);
      }

      template<class Iter>
      static Node* makeNode(Iter begin, Iter end, memt::Arena& arena,
        const C& conf, size_t childCount) {
        return new (arena.alloc(sizeOf(childCount)))
          Node(begin, end, arena, conf, childCount);
      }

      template<class Iter>
      static Node* makeNode(Iter begin, Iter end, memt::Arena& arena,
        const DivMaskCalculator& calc, const C& conf, size_t childCount) {
        return new (arena.alloc(sizeOf(childCount)))
          Node(begin, end, arena, calc, conf, childCount);
      }

      static size_t sizeOf(size_t childCount) {
        if (childCount > 0)
          --childCount; // array has size 1, so one element already there
        return sizeof(Node) + childCount * sizeof(Child);
      }

      struct Child : public mathic::DivMask::HasDivMask<C::UseTreeDivMask> {
        size_t var;
        Exponent exponent;
        Node* node;
      };
      typedef Child* iterator;
      typedef Child const* const_iterator;
      iterator childBegin() {return _childrenMemoryBegin;}
      const_iterator childBegin() const {return _childrenMemoryBegin;}
      iterator childEnd() {return _childrenEnd;}
      const_iterator childEnd() const {return _childrenEnd;}

      bool hasChildren() const {return childBegin() != childEnd();}
      template<class ME> // ME is MonomialOrEntry
      bool inChild(const_iterator child, const ME me, const C& conf) const {
        return child->exponent < conf.getExponent(me, child->var);
      }

      mathic::KDEntryArray<C, ExtEntry>& entries() {return _entries;}
      const KDEntryArray<C, ExtEntry>& entries() const {return _entries;}

      Node* splitInsert(
        const ExtEntry& extEntry,
        Child* childFromParent,
        memt::Arena& arena,
        const C& conf);

      /// Asserts internal invariants if asserts are turned on.
      bool debugIsValid() const;

    private:
      Node(const Node&); // unavailable
      void operator=(const Node&); // unavailable

      Node(memt::Arena& arena, const C& conf);
      
      template<class Iter>
      Node(Iter begin, Iter end, memt::Arena& arena,
        const C& conf, size_t childCount);

      template<class Iter>
      Node(Iter begin, Iter end, memt::Arena& arena,
        const DivMaskCalculator& calc, const C& conf, size_t childCount);

      class SplitEqualOrLess;

      KDEntryArray<C, ExtEntry> _entries;
      // Array has size 1 to appease compiler since size 0 produces warnings
      // or errors. Actual size can be greater if more memory has been
      // allocated for the node than sizeof(Node).
      Child* _childrenEnd; // points into _childrenMemoryBegin
      Child _childrenMemoryBegin[1];
    };

  public:
    PackedKDTree(const C& configuration);
    ~PackedKDTree();

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

    template<class DivisorOutput>
    inline void findAllMultiples
      (const ExtMonoRef& monomial, DivisorOutput& out);

    template<class EntryOutput>
    void forAll(EntryOutput& out);

    void clear();

    size_t getMemoryUse() const;

    void print(std::ostream& out) const;

    C& getConfiguration() {return _conf;}

    bool debugIsValid() const;

  private:
    PackedKDTree(const PackedKDTree<C>&); // unavailable
    void operator=(const PackedKDTree<C>&); // unavailable
  
    template<class Iter>
    struct InsertTodo {
      Iter begin;
      Iter end;
      Exponent exp;
      size_t var;
      typename Node::Child* fromParent;
    };

    memt::Arena _arena; // Everything permanent allocated from here.
    C _conf; // User supplied configuration.
    mutable std::vector<Node*> _tmp; // For navigating the tree.
    Node* _root; // Root of the tree. Can be null!
  };

  template<class C>
  PackedKDTree<C>::PackedKDTree(const C& configuration):
  _conf(configuration), _root(0) {
    MATHIC_ASSERT(C::LeafSize > 0);
    MATHIC_ASSERT(debugIsValid());
  }

  template<class C>
  PackedKDTree<C>::~PackedKDTree() {
    clear();
  }

  template<class C>
  PackedKDTree<C>::Node::Node(memt::Arena& arena, const C& conf):
  _entries(arena, conf) {
    _childrenEnd = childBegin();
  }

  template<class C>
  template<class Iter>
  PackedKDTree<C>::Node::Node(
    Iter begin,
    Iter end,
    memt::Arena& arena,
    const C& conf,
    size_t childCount):
    _entries(begin, end, arena, conf) {
    _childrenEnd = childBegin() + childCount;
  }

  template<class C>
  template<class Iter>
  PackedKDTree<C>::Node::Node(
    Iter begin,
    Iter end,
    memt::Arena& arena,
    const DivMaskCalculator& calc,
    const C& conf,
    size_t childCount):
    _entries(begin, end, arena, calc, conf) {
    _childrenEnd = childBegin() + childCount;
  }

  template<class C>
  template<class MO>
  size_t PackedKDTree<C>::removeMultiples(
    const ExtMonoRef& extMonomial,
    MO& out
  ) {
    MATHIC_ASSERT(_tmp.empty());
    if (_root == 0)
      return 0;
    size_t removedCount = 0;
    Node* node = _root;
    while (true) {
      for (typename Node::const_iterator it = node->childBegin();
        it != node->childEnd(); ++it) {
        _tmp.push_back(it->node);
        if (node->inChild(it, extMonomial.get(), _conf))
          goto stopped;
      }
      removedCount += node->entries().removeMultiples(extMonomial, out, _conf);
stopped:;
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
  bool PackedKDTree<C>::removeElement(const Monomial& monomial) {
    MATHIC_ASSERT(_tmp.empty());
    if (_root == 0)
      return false;
    Node* node = _root;
 
    typename Node::iterator child = node->childBegin();
    while (child != node->childEnd()) {
      if (node->inChild(child, monomial, _conf)) {
        node = child->node;
        child = node->childBegin();
      } else
        ++child;
    }
    const bool value = node->entries().removeElement(monomial, _conf);
    MATHIC_ASSERT(debugIsValid());
    return value;
  }

  template<class C>
  void PackedKDTree<C>::insert(const ExtEntry& extEntry) {
    MATHIC_ASSERT(debugIsValid());
    // find node in which to insert extEntry
    typename Node::Child* parentChild = 0;
    if (_root == 0)
      _root = Node::makeNode(_arena, _conf);
    Node* node = _root;
    typename Node::iterator child = node->childBegin();
    while (true) {
      if (child == node->childEnd()) {
        MATHIC_ASSERT(node->entries().size() <= C::LeafSize);
        if (node->entries().size() < C::LeafSize)
          node->entries().insert(extEntry, _conf);
        else { // split full node
          node = node->splitInsert(extEntry, parentChild, _arena, _conf);
          if (parentChild == 0)
            _root = node;
        }
        break;
      }
      if (C::UseTreeDivMask)
        child->updateToLowerBound(extEntry);
      if (node->inChild(child, extEntry.get(), _conf)) {
        parentChild = &*child;
        node = child->node;
        child = node->childBegin();
      } else
        ++child;
    }
    MATHIC_ASSERT(debugIsValid());
  }

  template<class C>
  template<class Iter>
  void PackedKDTree<C>::reset(Iter insertBegin, Iter insertEnd, const DivMaskCalculator& calc) {
    clear();
    if (insertBegin == insertEnd)
      return;

    typedef InsertTodo<Iter> Task;
    typedef std::vector<Task> TaskCont;
    TaskCont todo;
    TaskCont children;

    {
      Task initialTask;
      initialTask.begin = insertBegin;
      initialTask.end = insertEnd;
      initialTask.var = static_cast<size_t>(-1);
      initialTask.fromParent = 0;
      todo.push_back(initialTask);
    }
    while (!todo.empty()) {
      Iter begin = todo.back().begin;
      Iter end = todo.back().end;
      size_t var = todo.back().var;
      typename Node::Child* fromParent = todo.back().fromParent;
      if (fromParent != 0) {
        fromParent->var = var;
        fromParent->exponent = todo.back().exp;
      }
      todo.pop_back();

      // split off children until reaching few enough entries
      while (C::LeafSize < static_cast<size_t>(std::distance(begin, end))) {
        Task child;
        Iter middle = KDEntryArray<C, ExtEntry>::
          split(begin, end, var, child.exp, _conf);
        MATHIC_ASSERT(begin < middle && middle < end);
        MATHIC_ASSERT(var < _conf.getVarCount());
        child.begin = middle;
        child.end = end;
        child.var = var;
        children.push_back(child);
        // now operate on the equal-or-less part of the range
        end = middle;
      }
      Node* node = Node::makeNode
        (begin, end, _arena, calc, _conf, children.size());
      if (_root == 0)
        _root = node;
      if (fromParent != 0)
        fromParent->node = node;
      for (size_t child = 0; child < children.size(); ++child) {
        children[child].fromParent = &*(node->childBegin() + child);
        todo.push_back(children[child]);
      }
      children.clear();
    }
    MATHIC_ASSERT(_root != 0);

    if (C::UseTreeDivMask) {
      // record nodes in tree using breadth first search
      typedef std::vector<Node*> NodeCont;
      NodeCont nodes;
      nodes.push_back(_root);
      for (size_t i = 0; i < nodes.size(); ++i) {
        Node* node = nodes[i];
        for (typename Node::iterator child = node->childBegin();
          child != node->childEnd(); ++child)
          nodes.push_back(child->node);
      }
      // compute div masks in reverse order of breath first search
      typename NodeCont::reverse_iterator it = nodes.rbegin();
      typename NodeCont::reverse_iterator end = nodes.rend();
      for (; it != end; ++it) {
        Node* node = *it;
        typedef std::reverse_iterator<typename Node::iterator> riter;
        riter rbegin = riter(node->childEnd());
        riter rend = riter(node->childBegin());
        for (riter child = rbegin; child != rend; ++child) {
          child->resetDivMask();
          if (child == rbegin)
            child->updateToLowerBound(node->entries());
          else {
            riter prev = child;
            --prev;
            child->updateToLowerBound(*prev);
          }
          if (child->node->hasChildren())
            child->updateToLowerBound(*child->node->childBegin());
          else
            child->updateToLowerBound(child->node->entries());
        }
        MATHIC_ASSERT(node->debugIsValid());
      }
    }
    MATHIC_ASSERT(debugIsValid());
  }

  template<class C>
  typename PackedKDTree<C>::Entry* PackedKDTree<C>::findDivisor
    (const ExtMonoRef& extMonomial) {
    MATHIC_ASSERT(_tmp.empty());
    if (_root == 0)
      return 0;
    Node* node = _root;
    while (true) {
      // record relevant children for later processing
      for (typename Node::const_iterator it = node->childBegin();
        it != node->childEnd(); ++it) {
        if (C::UseTreeDivMask &&
          !it->getDivMask().canDivide(extMonomial.getDivMask()))
          goto next;
        if (node->inChild(it, extMonomial.get(), _conf))
          _tmp.push_back(it->node);
      }

      // look for divisor in entries of node
      {
        typename KDEntryArray<C, ExtEntry>::iterator it =
          node->entries().findDivisor(extMonomial, _conf);
        if (it != node->entries().end()) {
          MATHIC_ASSERT(_conf.divides(it->get(), extMonomial.get()));
          _tmp.clear();
          return &it->get();
        }
      }

next:
      // grab next node to process
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
  void PackedKDTree<C>::findAllDivisors(
    const ExtMonoRef& extMonomial,
    DO& output
  ) {
    MATHIC_ASSERT(_tmp.empty());
    if (_root == 0)
      return;
    Node* node = _root;
    while (true) {
      for (typename Node::const_iterator it = node->childBegin();
        it != node->childEnd(); ++it) {
        if (C::UseTreeDivMask &&
          !it->getDivMask().canDivide(extMonomial.getDivMask()))
          goto next; // div mask rules this sub tree out
        if (node->inChild(it, extMonomial.get(), _conf))
          _tmp.push_back(it->node);
      }
      if (!node->entries().findAllDivisors(extMonomial, output, _conf)) {
        _tmp.clear();
        break;
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
  void PackedKDTree<C>::findAllMultiples(
    const ExtMonoRef& extMonomial,
    DO& output
  ) {
    MATHIC_ASSERT(_tmp.empty());
    if (_root == 0)
      return;
    Node* node = _root;
    while (true) {
      for (typename Node::const_iterator it = node->childBegin();
        it != node->childEnd(); ++it) {
          _tmp.push_back(it->node);
          if (node->inChild(it, extMonomial.get(), _conf))
            goto next;
      }
      if (!node->entries().findAllMultiples(extMonomial, output, _conf)) {
        _tmp.clear();
        break;
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
  template<class EO>
  void PackedKDTree<C>::forAll(EO& output) {
    MATHIC_ASSERT(_tmp.empty());
    if (_root == 0)
      return;
    Node* node = _root;
    while (true) {
      if (!node->entries().forAll(output)) {
        _tmp.clear();
        break;
      }
      for (typename Node::iterator it = node->childBegin();
        it != node->childEnd(); ++it)
        _tmp.push_back(it->node);
      if (_tmp.empty())
        break;
      node = _tmp.back();
      _tmp.pop_back();
    }
    MATHIC_ASSERT(_tmp.empty());
  }

  template<class C>
  void PackedKDTree<C>::clear() {
    MATHIC_ASSERT(_tmp.empty());
    // Call Entry destructors
    if (_root != 0)
      _tmp.push_back(_root);
    while (!_tmp.empty()) {
      Node* node = _tmp.back();
      _tmp.pop_back();
      node->entries().clear(); // calls destructors
      for (typename Node::iterator it = node->childBegin();
        it != node->childEnd(); ++it)
        _tmp.push_back(it->node);
    }
    _arena.freeAllAllocs();
    _root = 0;
  }

  template<class C>
  size_t PackedKDTree<C>::getMemoryUse() const {
    // todo: not accurate
	size_t sum = _arena.getMemoryUse();
	sum += _tmp.capacity() * sizeof(_tmp.front());
	return sum;
  }

  template<class C>
  void PackedKDTree<C>::print(std::ostream& out) const {
    out << "<<<<<<<< PackedKDTree >>>>>>>>\n";
    MATHIC_ASSERT(_tmp.empty());
    if (_root == 0)
      return;
    Node* node = _root;
    while (true) {
      out << "**** Node " << node << "\nchildren:\n";
      for (typename Node::iterator it = node->childBegin();
        it != node->childEnd(); ++it) {
        _tmp.push_back(it->node);
        out << "Child " << ((it - node->childBegin()) + 1) << ": "
          << '>' << (it->var + 1) << '^' << it->exponent
          << ' ' << it->node << '\n';
      }
      for (size_t i = 0; i < node->entries().size(); ++i) {
        out << "Entry " << (i + 1) << ": "
          << node->entries().begin()[i].get() << '\n';
      }
      out << '\n';
      if (_tmp.empty())
        break;
      node = _tmp.back();
      _tmp.pop_back();
    }
    MATHIC_ASSERT(_tmp.empty());
  }

  template<class C>
  bool PackedKDTree<C>::debugIsValid() const {
#ifndef MATHIC_DEBUG
    return true;
#else
    //print(std::cerr); std::cerr << std::flush;
    MATHIC_ASSERT(_tmp.empty());
    MATHIC_ASSERT(!_conf.getDoAutomaticRebuilds() || _conf.getRebuildRatio() > 0);
    if (_root == 0)
      return true;

    // record all nodes
    std::vector<Node*> nodes;
    nodes.push_back(_root);
    for (size_t i = 0; i < nodes.size(); ++i) {
      Node* node = nodes[i];
      MATHIC_ASSERT(node->entries().debugIsValid());
      for (typename Node::iterator it = node->childBegin();
        it != node->childEnd(); ++it) {
        MATHIC_ASSERT(it->var < _conf.getVarCount());
        MATHIC_ASSERT(!C::UseTreeDivMask ||
          it->canDivide(node->entries()));
        nodes.push_back(it->node);
      }
    }

    // check the recorded nodes
    MATHIC_ASSERT(_tmp.empty());
    for (size_t i = 0; i < nodes.size(); ++i) {
      Node* ancestor = nodes[i];

      for (typename Node::iterator ancestorIt = ancestor->childBegin();
        ancestorIt != ancestor->childEnd(); ++ancestorIt) {
        MATHIC_ASSERT(_tmp.empty());
        size_t var = ancestorIt->var;
        Exponent exp = ancestorIt->exponent;
        // check strictly greater than subtree
        _tmp.push_back(ancestorIt->node);
        while (!_tmp.empty()) {
          Node* node = _tmp.back();
          _tmp.pop_back();
          for (typename Node::iterator it = node->childBegin();
            it != node->childEnd(); ++it) {
            MATHIC_ASSERT(!C::UseTreeDivMask || ancestorIt->canDivide(*it));
            _tmp.push_back(it->node);
          }
          MATHIC_ASSERT(node->entries().
            allStrictlyGreaterThan(var, exp, _conf));
          MATHIC_ASSERT(!C::UseTreeDivMask ||
            ancestorIt->canDivide(node->entries()));
        }
        // check less than or equal to sub tree.
        MATHIC_ASSERT(ancestor->entries().
          allLessThanOrEqualTo(var, exp, _conf));
        // go through the rest of the children
        typename Node::iterator restIt = ancestorIt;
        for (++restIt; restIt != ancestor->childEnd(); ++restIt) {
          MATHIC_ASSERT(!C::UseTreeDivMask || ancestorIt->canDivide(*restIt));
          _tmp.push_back(restIt->node);
        }
        while (!_tmp.empty()) {
          Node* node = _tmp.back();
          _tmp.pop_back();
          for (typename Node::iterator it = node->childBegin();
            it != node->childEnd(); ++it) {
            MATHIC_ASSERT(!C::UseTreeDivMask || ancestorIt->canDivide(*it));
            _tmp.push_back(it->node);
          }
          MATHIC_ASSERT(node->entries().
            allLessThanOrEqualTo(var, exp, _conf));
          MATHIC_ASSERT(!C::UseTreeDivMask ||
            ancestorIt->canDivide(node->entries()));
        }
      }
    }
    return true;
#endif
  }

  template<class C>
  class PackedKDTree<C>::Node::SplitEqualOrLess {
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
  typename PackedKDTree<C>::Node* PackedKDTree<C>::Node::splitInsert(
    const ExtEntry& extEntry,
    Child* childFromParent,
    memt::Arena& arena,
    const C& conf
  ) {
    MATHIC_ASSERT(conf.getVarCount() > 0);
    MATHIC_ASSERT(entries().size() > 0);
    size_t var;
    if (hasChildren())
      var = (childEnd() - 1)->var;
    else if (childFromParent != 0)
      var = childFromParent->var;
    else
      var = static_cast<size_t>(-1);
    typename C::Exponent exp;

    // there is not room to add another child, so make a
    // new Node with more space and put the Node we are splitting
    // off into the vacated space. It will fit as it has no
    // children at all.

    typename KDEntryArray<C, ExtEntry>::iterator middle =
      KDEntryArray<C, ExtEntry>::split
      (entries().begin(), entries().end(), var, exp, conf, &extEntry);

    // ** copy relevant part of *this into new space
    Node* copied = makeNode(entries().begin(), middle, arena, conf,
      std::distance(childBegin(), childEnd()) + 1);
    std::copy(childBegin(), childEnd(), copied->childBegin());
    Child& newChild = *(copied->childEnd() - 1);
    newChild.var = var;
    newChild.exponent = exp;
    newChild.node = this;
    if (childFromParent != 0)
      childFromParent->node = copied;
    if (C::UseTreeDivMask) {
      newChild.resetDivMask();
      newChild.updateToLowerBound(entries());
      newChild.updateToLowerBound(extEntry);
    }

    // ** fix up remaining part of *this to be the child of copied
    // OK to call std::copy as begin is not in the range [middle, end).
    std::copy(middle, entries().end(), entries().begin());
    const size_t targetSize = std::distance(middle, entries().end());
    while (entries().size() > targetSize)
      entries().pop_back();
    if (conf.getSortOnInsert())
      std::sort(entries().begin(), entries().end(), Comparer<C>(conf));
    if (C::UseTreeDivMask)
      entries().recalculateTreeDivMask();
    _childrenEnd = childBegin();

    if (copied->inChild(copied->childEnd() - 1, extEntry.get(), conf))
      entries().insert(extEntry, conf);
    else
      copied->entries().insert(extEntry, conf);

    MATHIC_ASSERT(debugIsValid());
    MATHIC_ASSERT(copied->debugIsValid());

    return copied;
  }

  template<class C>
  bool PackedKDTree<C>::Node::debugIsValid() const {
#ifndef MATHIC_DEBUG
    return true;
#else
    MATHIC_ASSERT(entries().debugIsValid());
    MATHIC_ASSERT(childBegin() <= childEnd());
    if (C::UseTreeDivMask) {
      for (const_iterator child = childBegin(); child != childEnd(); ++child) {
        if (child != childEnd() - 1)
          MATHIC_ASSERT(child->canDivide(*(child + 1)));
        else
          MATHIC_ASSERT(child->canDivide(entries()));
        if (child->node->hasChildren())
          MATHIC_ASSERT(child->canDivide(*child->node->childBegin()));
        else
          MATHIC_ASSERT(child->canDivide(child->node->entries()));
      }
    }
    return true;
#endif
  }
}

#endif
