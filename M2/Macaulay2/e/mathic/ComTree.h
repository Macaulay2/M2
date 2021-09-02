#ifndef MATHIC_COM_TREE_GUARD
#define MATHIC_COM_TREE_GUARD

#include "stdinc.h"
#include <ostream>

namespace mathic {
  /** This class packs a complete binary tree in a vector.

   The idea is to have the root at index 1, and then the left child of node n
   will be at index 2n and the right child will be at index 2n + 1. The
   corresponding formulas when indexes start at 0 take more computation, so
   we need a 1-based array so we can't use std::vector.

   Also, when sizeof(Entry) is a power of 2 it is faster to keep track of
   i * sizeof(Entry) than directly keeping track of an index i. This doesn't
   work well when sizeof(Entry) is not a power of two. So we need both
   possibilities. That is why this class never exposes indexes. Instead
   you interact with Node objects that serve the role of an index, but the
   precise value it stores is encapsulated. This way you can't do something
   like _array[i * sizeof(Entry)] by accident. Client code also does not
   need to (indeed, can't) be aware of how indexes are calculated, stored and
   looked up.

   If FastIndex is false, then Nodes contain an index i. If FastIndex is
   true, then Nodes contain the byte offset i * sizeof(Entry). FastIndex must
   be false if sizeof(Entry) is not a power of two.
  */
  template<class Entry, bool FastIndex>
  class ComTree {
  public:
    class Node;
    ComTree(size_t initialCapacity = 0);
    ComTree(const ComTree& tree, size_t minCapacity = 0);
    ComTree(ComTree&& tree);

    ~ComTree() {
      MATHIC_ASSERT(isValid());
      delete[](_arrayKeepAlive);
    }

    Entry& operator[](Node n);
    const Entry& operator[](Node n) const;

    bool empty() const {return _lastLeaf == Node(0);}
    size_t size() const {return _lastLeaf.getNormalIndex();}
    size_t capacity() const {return _capacityEnd.getNormalIndex();}
    Node lastLeaf() const {return _lastLeaf;}

    void pushBack(const Entry& value);
    void pushBackWithCapacity(const Entry& value);
    void popBack();

    bool hasFreeCapacity(size_t extraCapacity) const;
    void increaseCapacity();
    void swap(ComTree& tree);

    class Node {
    public:
      Node(): _index(fi ? S : 1) {} // the root node is the default
      
      Node parent() const;
      Node left() const;
      Node right() const;
      Node sibling() const;
      Node leftSibling() const;
      Node next() const;
      Node next(size_t count) const;
      Node prev() const;
      Node& operator++() {*this = next(); return *this;}

      bool isRoot() const {return *this == Node();}

      // Returns a size_t instead of a bool so that the compiler does not
      // have to convert to bool (this silences a MSVC performance warning).
      size_t isLeft() const {return fi ? !(_index & S) : !(_index & 1);}

      // Returns a size_t instead of a bool so that the compiler does not
      // have to convert to bool (this silences a MSVC performance warning).
      size_t isRight() const {return fi ? _index & S : _index & 1;}

      bool operator<(Node node) const {return _index < node._index;}
      bool operator<=(Node node) const {return _index <= node._index;}
      bool operator>(Node node) const {return _index > node._index;}
      bool operator>=(Node node) const {return _index >= node._index;}
      bool operator==(Node node) const {return _index == node._index;}
      bool operator!=(Node node) const {return _index != node._index;}

      //private:
      friend class ComTree<Entry, FastIndex>;
      static const bool fi = FastIndex;
      static const size_t S = sizeof(Entry);
      explicit Node(size_t i): _index(i) {}
      size_t getNormalIndex() const {return fi ? _index / S : _index;}
      size_t _index;
    };

    void print(std::ostream& out) const;

    void clear();

    size_t getMemoryUse() const;

    /// Asserts internal invariants if asserts are turned on.
    bool isValid() const;

  private:
    ComTree& operator=(const ComTree& tree) const; // not available

    Entry* _array;

    /// Macaulay 2 uses Mathic and Macaulay 2 also uses the Boehm garbage
    /// collector. Since _array points to a place before the array we
    /// are using, that array will be garbage collected if we do not
    /// keep around a second pointer that does point into the array.
    /// That is the purpose of _arrayKeepAlive.
    Entry* _arrayKeepAlive;

    Node _lastLeaf;
    Node _capacityEnd;
  };

  template<class E, bool FI>
  void ComTree<E, FI>::clear() {
    _lastLeaf = Node(0);
  }

  template<class E, bool FI>
  size_t ComTree<E, FI>::getMemoryUse() const {
    return capacity() * sizeof(E);
  }

  template<class E, bool FI>
  std::ostream& operator<<(std::ostream& out, const ComTree<E, FI>& tree) {
    tree.print(out);
    return out;
  }

  template<class E, bool FI>
  ComTree<E, FI>::ComTree(size_t initialCapacity):
    _array(static_cast<E*>(0) - 1),
    _arrayKeepAlive(0),
    _lastLeaf(0),
    _capacityEnd(Node(0).next(initialCapacity))
  {
    if (initialCapacity > 0) {
      _arrayKeepAlive = new E[initialCapacity];
      _array = _arrayKeepAlive - 1;
    }
    MATHIC_ASSERT(isValid());
  }

  template<class E, bool FI>
  ComTree<E, FI>::ComTree(const ComTree& tree, size_t minCapacity):
    _array(static_cast<E*>(0) - 1),
    _arrayKeepAlive(0),
    _lastLeaf(tree._lastLeaf)
  {
    if (tree.size() > minCapacity)
      minCapacity = tree.size();
    _capacityEnd = Node(0).next(minCapacity);
    if (minCapacity != 0) {
      _arrayKeepAlive = new E[minCapacity];
      _array = _arrayKeepAlive - 1;
      for (Node i; i <= tree.lastLeaf(); ++i)
        (*this)[i] = tree[i];
    }
    
    MATHIC_ASSERT(isValid());
  }

  template<class E, bool FI>
  ComTree<E, FI>::ComTree(ComTree&& tree):
    _array(tree._array),
    _arrayKeepAlive(tree._arrayKeepAlive),
    _lastLeaf(tree._lastLeaf),
    _capacityEnd(tree._capacityEnd)
  {
    tree._array = static_cast<E*>(0) - 1;
    tree._arrayKeepAlive = 0;
    _lastLeaf = Node(0);
    _capacityEnd = Node(0);
  }

  template<class E, bool FI>
  inline E& ComTree<E, FI>::operator[](Node n) {
    MATHIC_ASSERT(_array == _arrayKeepAlive - 1);
    if (!FI)
      return _array[n._index];
    char* base = reinterpret_cast<char*>(_array);
    E* element = reinterpret_cast<E*>(base + n._index);
    MATHIC_ASSERT(element == &(_array[n._index / sizeof(E)]));
    return *element;
  }

  template<class E, bool FI>
  inline const E& ComTree<E, FI>::operator[](Node n) const {
    return const_cast<ComTree<E, FI>*>(this)->operator[](n);
  }

  template<class E, bool FI>
  void ComTree<E, FI>::pushBack(const E& value) {
    if (_lastLeaf == _capacityEnd)
      increaseCapacity();
    _lastLeaf = _lastLeaf.next();
    (*this)[lastLeaf()] = value;
  }

  template<class E, bool FI>
  void ComTree<E, FI>::pushBackWithCapacity(const E& value) {
    MATHIC_ASSERT(_lastLeaf != _capacityEnd);
    _lastLeaf = _lastLeaf.next();
    (*this)[lastLeaf()] = value;
  }

  template<class E, bool FI>
  void ComTree<E, FI>::popBack() {
    MATHIC_ASSERT(_lastLeaf >= Node());
    _lastLeaf = _lastLeaf.prev();
  }

  template<class E, bool FI>
  void ComTree<E, FI>::swap(ComTree& tree) {
    MATHIC_ASSERT(isValid());
    MATHIC_ASSERT(tree.isValid());

    std::swap(_array, tree._array);
    std::swap(_arrayKeepAlive, tree._arrayKeepAlive);
    std::swap(_lastLeaf, tree._lastLeaf);
    std::swap(_capacityEnd, tree._capacityEnd);

    MATHIC_ASSERT(isValid());
    MATHIC_ASSERT(tree.isValid());
  }

  template<class E, bool FI>
  typename ComTree<E, FI>::Node ComTree<E, FI>::Node::parent() const {
    return fi ? Node((_index / (2 * S)) * S) : Node(_index / 2);
  }

  template<class E, bool FI>
  typename ComTree<E, FI>::Node ComTree<E, FI>::Node::left() const {
    return Node(2 * _index);
  }

  template<class E, bool FI>
  typename ComTree<E, FI>::Node ComTree<E, FI>::Node::right() const {
    return fi ? Node(2 * _index + S) : Node(2 * _index + 1);
  }

  template<class E, bool FI>
  typename ComTree<E, FI>::Node ComTree<E, FI>::Node::sibling() const {
    return fi ? Node(_index ^ S) : Node(_index ^ 1);
  }

  template<class E, bool FI>
  typename ComTree<E, FI>::Node ComTree<E, FI>::Node::leftSibling() const {
    return fi ? Node(_index & ~S) : Node(_index & ~1);
  }

  template<class E, bool FI>
  typename ComTree<E, FI>::Node ComTree<E, FI>::Node::next() const {
    return fi ? Node(_index + S) : Node(_index + 1);
  }

  template<class E, bool FI>
  typename ComTree<E, FI>::Node ComTree<E, FI>::Node::next(size_t count) const {
    return fi ? Node(_index + S * count) : Node(_index + count);
  }

  template<class E, bool FI>
  typename ComTree<E, FI>::Node ComTree<E, FI>::Node::prev() const {
    return fi ? Node(_index - S) : Node(_index - 1);
  }

  template<class E, bool FI>
  void ComTree<E, FI>::print(std::ostream& out) const {
    Node last = lastLeaf();
    for (Node i; i <= last; i = i.next()) {
      if ((i._index & (i._index - 1)) == 0) // if i._index is a power of 2
        out << "\n " << i._index << ':';
      out << ' ' << (*this)[i];
    }
    out << "}\n";
  }

  template<class E, bool FI>
  bool ComTree<E, FI>::isValid() const {
#ifndef MATHIC_DEBUG
    return true;
#else
    MATHIC_ASSERT(_array == _arrayKeepAlive - 1);

    // sizeof(Entry) must be a power of two if FastIndex is true.
    MATHIC_ASSERT(!FI || (sizeof(E) & (sizeof(E) - 1)) == 0);
    if (capacity() == 0) {
      MATHIC_ASSERT(_array == static_cast<E*>(0) - 1);
      MATHIC_ASSERT(_capacityEnd == Node(0));
      MATHIC_ASSERT(_lastLeaf == Node(0));
    } else {
      MATHIC_ASSERT(_array != static_cast<E*>(0) - 1);
      MATHIC_ASSERT(_capacityEnd > Node(0));
      MATHIC_ASSERT(_lastLeaf <= _capacityEnd);
    }
    return true;
#endif
  }

  template<class E, bool FI>
  bool ComTree<E, FI>::hasFreeCapacity(size_t extraCapacity) const {
    return Node(_capacityEnd._index - _lastLeaf._index) >=
      Node(0).next(extraCapacity);
  }

  template<class E, bool FI>
  void ComTree<E, FI>::increaseCapacity() {
    MATHIC_ASSERT(isValid());
    ComTree<E, FI> newTree(capacity() == 0 ? 16 : capacity() * 2);
    for (Node i; i <= lastLeaf(); i = i.next())
      newTree.pushBack((*this)[i]);
    MATHIC_ASSERT(newTree.isValid());
    std::swap(_array, newTree._array);
    std::swap(_arrayKeepAlive, newTree._arrayKeepAlive);
    std::swap(_capacityEnd, newTree._capacityEnd);
    MATHIC_ASSERT(isValid());
  }
}

#endif
