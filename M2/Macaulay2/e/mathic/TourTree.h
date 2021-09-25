#ifndef MATHIC_TOUR_TREE_GUARD
#define MATHIC_TOUR_TREE_GUARD

#include "stdinc.h"
#include "ComTree.h"
#include <string>
#include <vector>

namespace mathic {
  class TourTreeSuggestedOptions {
  public:
	// there are no tournament tree options so far.
  };

  template<class C>
  class TourTree {
  public:
	typedef C Configuration;
	typedef typename Configuration::Entry Entry;

	TourTree(const Configuration& configuration);
	Configuration& getConfiguration() {return _conf;}
	const Configuration& getConfiguration() const {return _conf;}

    std::string getName() const;
	void push(Entry entry);
    template<class It>
	void push(It begin, It end);
	Entry pop();
	Entry top() const;
	bool empty() const {return _tree.empty();}
	void print(std::ostream& out) const;

	void decreaseTop(Entry newEntry);

    template<class T>
    void forAll(T& t) const {
      typedef typename std::vector<Player>::const_iterator Iter;
      Iter end = _players.end();
      for (Iter it = _players.begin(); it != end; ++it)
        if (!t.proceed(it->entry))
          return;
    }

    template<class T>
    void forAll(T& t) {
      typedef typename std::vector<Player>::iterator Iter;
      Iter end = _players.end();
      for (Iter it = _players.begin(); it != end; ++it)
        if (!t.proceed(it->entry))
          return;
    }

    void clear();

    size_t getMemoryUse() const;

  private:
	class Player;
	// Setting fastIndex to true speeds up left/right child
	// computations. We only compute parents, so there is no reason
	// to enable fastIndex.
	typedef ComTree<Player*, false> Tree;
	typedef typename Tree::Node Node;
	struct Player { 
      Player(const Entry& entry, Node leaf): entry(entry), leaf(leaf) {}
	  Entry entry;
	  Node leaf;
	};

	void reallocate();

    /// Asserts internal invariants if asserts are turned on.
	bool isValid() const;

	Tree _tree;
	std::vector<Player> _players;
	Configuration _conf;
  };

  template<class C>
  void TourTree<C>::clear() {
	MATHIC_ASSERT(isValid());
    _tree.clear();
    _players.clear();
	MATHIC_ASSERT(isValid());
  }

  template<class C>
  size_t TourTree<C>::getMemoryUse() const {
    return _tree.getMemoryUse() +
      _players.capacity() * sizeof(_players.front());
  }

  template<class C>
  TourTree<C>::TourTree(const C& configuration): _conf(configuration) {
	reallocate();
  }

  template<class C>
	std::string TourTree<C>::getName() const {
	return std::string("t-tree (") +
	  (C::fastIndex ? "fi" : "si") +
	  ')';
  }

  template<class C>
	void TourTree<C>::push(Entry entry) {
	MATHIC_SLOW_ASSERT(isValid());
	if (!_tree.hasFreeCapacity(2))
	  reallocate();
	if (empty()) {
	  _players.push_back(Player(entry, Node()));
	  _tree.pushBackWithCapacity(&_players.back());
	  MATHIC_SLOW_ASSERT(isValid());
	  return;
	}
	// move leaf down as left child
	Node posParent = _tree.lastLeaf().next().parent();
	Player* moveDown = _tree[posParent];
	_tree.pushBackWithCapacity(moveDown);
	moveDown->leaf = _tree.lastLeaf();
	MATHIC_ASSERT(_tree.lastLeaf().isLeft());

	// insert entry as right child
	_players.push_back(Player(entry, _tree.lastLeaf().next()));
	_tree.pushBackWithCapacity(&_players.back());
	MATHIC_ASSERT(_tree.lastLeaf().isRight());

	Node pos = _tree.lastLeaf();
	do {
	  MATHIC_ASSERT(!pos.isRoot());
	  MATHIC_ASSERT(posParent == pos.parent());
	  typename C::CompareResult cmp = _conf.compare
		(_tree[posParent]->entry, _tree[pos]->entry);
	  if (!_conf.cmpLessThan(cmp))
		break;
	  _tree[posParent] = _tree[pos];
	  pos = posParent;
	  posParent = pos.parent();
	} while (!pos.isRoot());
	MATHIC_SLOW_ASSERT(isValid());
  }

  template<class C>
  template<class It>
  void TourTree<C>::push(It begin, It end) {
	for (; begin != end; ++begin)
	  push(*begin);
  }

  template<class C>
	void TourTree<C>::decreaseTop(Entry newEntry) {
	MATHIC_ASSERT(!empty());
	Player* player = _tree[Node()];
	player->entry = newEntry;
	for (Node pos = player->leaf; !pos.isRoot(); pos = pos.parent()) {
	  Player* opponent = _tree[pos.sibling()];
	  if (_conf.cmpLessThan(_conf.compare(player->entry, opponent->entry)))
		player = opponent;
	  _tree[pos.parent()] = player;
	}
	MATHIC_SLOW_ASSERT(isValid());
  }

  template<class C>
	typename TourTree<C>::Entry TourTree<C>::pop() {
	MATHIC_ASSERT(!empty());
	Entry top = _tree[Node()]->entry;
	if (_tree.lastLeaf().isRoot()) {
	  _tree.popBack();
	  _players.pop_back();
	  MATHIC_SLOW_ASSERT(isValid());
	  return top;
	}
	Node parentPos = _tree.lastLeaf().parent();
	Player* left = _tree[_tree.lastLeaf().prev()];
	Player* right = _tree[_tree.lastLeaf()];

	if (right == _tree[parentPos]) {
	  // we want right to be the smaller entry so that it can be
	  // removed without that having an impact further up the tree.
	  std::swap(left->entry, right->entry);
	  for (Node pos = parentPos; _tree[pos] == right; pos = pos.parent()) {
		_tree[pos] = left;
		if (pos.isRoot())
		  break;
	  }
	}
	Player* player = _tree[Node()];
	player->entry = right->entry; // let right take the winner's place
	MATHIC_ASSERT(right == &_players.back());
	_players.pop_back(); // remove right
	left->leaf = parentPos; // move up left
	_tree.popBack();
	_tree.popBack();
	for (Node pos = player->leaf; !pos.isRoot();) {
	  Player* opponent = _tree[pos.sibling()];
	  typename C::CompareResult cmp =
		_conf.compare(player->entry, opponent->entry);
	  if (_conf.cmpLessThan(cmp))
		player = opponent;
	  pos = pos.parent();
	  _tree[pos] = player;
	}
	MATHIC_SLOW_ASSERT(isValid());
	return top;
  }

  template<class C>
	typename TourTree<C>::Entry TourTree<C>::top() const {
	MATHIC_ASSERT(!empty());
	return _tree[Node()]->entry;
  }

  template<class C>
	void TourTree<C>::print(std::ostream& out) const {
	out << getName() << ": {\n" << _tree << "}\n";
  }

  template<class C>
	void TourTree<C>::reallocate() {
	MATHIC_ASSERT(isValid());
	_tree.increaseCapacity();
	const size_t newCapacity = _tree.capacity();
	if (_players.empty())
	  _players.reserve(newCapacity / 2 + 2);
	else {
	  Player* oldBegin = &_players.front();
	  _players.reserve(newCapacity / 2 + 2);
	  Player* newBegin = &_players.front();
	  for (Node pos; pos <= _tree.lastLeaf(); ++pos)
		_tree[pos] = newBegin + (_tree[pos] - oldBegin);
	}
	MATHIC_ASSERT(isValid());
  }

  template<class C>
  bool TourTree<C>::isValid() const {
#ifndef MATHIC_DEBUG
    return true;
#else
	MATHIC_ASSERT
	  ((_tree.empty() && _players.empty()) || // just constructed
	   (_tree.capacity() + 1 <= 2 * _players.capacity()));
	MATHIC_ASSERT((empty() && _players.empty()) ||
		   (_tree.size() + 1 == 2 * _players.size()));
	// _tree points into _players
	for (Node pos; pos <= _tree.lastLeaf(); ++pos) {
	  size_t index = _tree[pos] - &(_players.front());
	  MATHIC_ASSERT(index < _players.size());
	}

	for (Node pos; pos <= _tree.lastLeaf(); ++pos) {
	  if (pos.left() >= _tree.lastLeaf()) { // leaf or two children
		MATHIC_ASSERT(pos.right() > _tree.lastLeaf()); // pos is a leaf
		MATHIC_ASSERT(_tree[pos]->leaf == pos);
	  } else {
		MATHIC_ASSERT(pos.right() <= _tree.lastLeaf()); // pos has two children
		// exactly one child wins
		MATHIC_ASSERT(_tree[pos.left()] != _tree[pos.right()]);
		MATHIC_ASSERT(_tree[pos] == _tree[pos.left()] ||
			   _tree[pos] == _tree[pos.right()]);
		MATHIC_ASSERT(!_conf.cmpLessThan(
          _conf.compare(_tree[pos]->entry, _tree[pos.left()]->entry)));
		MATHIC_ASSERT(!_conf.cmpLessThan(
          _conf.compare(_tree[pos]->entry, _tree[pos.right()]->entry)));
	  }
	}
	return true;
#endif
  }
}

#endif
