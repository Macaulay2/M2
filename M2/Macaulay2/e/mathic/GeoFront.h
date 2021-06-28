#ifndef MATHIC_GEO_FRONT_GUARD
#define MATHIC_GEO_FRONT_GUARD

#include "stdinc.h"
#include <vector>
#include <utility>

namespace mathic {
  /**
	 To be used by Geobucket class.

	 Configuration is the Configuration from the owning Geobucket.

	 Bucket is the Bucket class from the owning Geobucket. Having Bucket be a
	 parameter allows it to be used before it is declared so that this header
	 need not include Geobucket's header, which would otherwise be a cyclic
	 dependency. This also grants access to Geobucket<C>::Bucket which is
	 otherwise private.

	 Why not make this an inner template class of Geobucket? Because inner
	 template classes cannot be specialized without also specializing the
	 outer class.
  */

  template<class Configuration>
	class Geobucket;

  template<class Configuration,
	class Bucket = typename Geobucket<Configuration>::Bucket,
	bool Order = Configuration::trackFront>
	class GeoFront;

  // ********************************************************************
  // GeoFront maintaining no special order
  template<class C, class Bucket>
	class GeoFront<C, Bucket, false> {
  public:
	/** A reference is kept to entryCountRef so that it can be updated
		in the event of a deduplication. */
  GeoFront(const C& conf, size_t& entryCountRef):
	_cachedMaxBucket(0), _entryCountRef(&entryCountRef), _conf(conf) {
	  MATHIC_ASSERT(!C::trackFront);
	}

    void clear() {
      _cachedMaxBucket = 0;
    }

	void reserveCapacity(size_t newCapacity) {}
	void bucketsInvalidated(Bucket* oldBegin, Bucket* newBegin) {
	  _cachedMaxBucket = 0;
	}
	bool empty() const {return *_entryCountRef == 0;}

	void insert(Bucket* bucket) {_cachedMaxBucket = 0;}
	void remove(Bucket* bucket) {_cachedMaxBucket = 0;}
	void keyIncreased(Bucket* bucket) {_cachedMaxBucket = 0;}
	void keyDecreased(Bucket* bucket) {_cachedMaxBucket = 0;}
	void swapKeys(Bucket* a, Bucket* b) {}
	bool larger(const Bucket* a, const Bucket* b) const {
	  MATHIC_ASSERT(false);
	  return false;
	}
	const Bucket* getMax(Bucket* bucketBegin, Bucket* bucketEnd) const;

#ifdef MATHIC_DEBUG
	bool debugIsValid(Bucket* bucketBegin, Bucket* bucketEnd) const;
#endif

    size_t getMemoryUse() const;

  private:
	const Bucket* computeMax(Bucket* bucketBegin, Bucket* bucketEnd) const;

	mutable const Bucket* _cachedMaxBucket;
	size_t* _entryCountRef;
	const C& _conf;
  };

  template<class C, class B>
  size_t GeoFront<C, B, false>::getMemoryUse() const {
    return 0;
  }

  template<class C, class B>
  const B* GeoFront<C, B, false>::getMax(B* bucketBegin, B* bucketEnd) const {
	// the point is that the compiler is more likely to compile this
	// pre-calculation so that computeMax won't get called in cases
	// where the maximum has already been computed.
	if (_cachedMaxBucket != 0)
	  return _cachedMaxBucket;
	return computeMax(bucketBegin, bucketEnd);
  }

  template<class C, class B>
	const B* GeoFront<C, B, false>::computeMax
	(B* bucketBegin, B* bucketEnd) const {
	MATHIC_ASSERT(_cachedMaxBucket == 0);
	const B* maxBucket = bucketBegin;
	while (maxBucket->empty()) {
	  ++maxBucket;
	  MATHIC_ASSERT(maxBucket != bucketEnd);
	}
	for (const B* it = maxBucket + 1; it != bucketEnd; ++it) {
	  if (it->empty())
		continue;
	  typename C::CompareResult cmp =
		_conf.compare(it->back(), maxBucket->back());
	  if (_conf.cmpLessThan(cmp))
		continue;
	  if (C::supportDeduplication && _conf.cmpEqual(cmp)) {
		B* mb = const_cast<B*>(maxBucket);
		mb->setEntry(mb->end() - 1,
					 _conf.deduplicate(mb->back(), it->back()));
		const_cast<B*>(it)->pop_back();
		--*_entryCountRef;
		continue;
	  }
	  maxBucket = it;
	}
	MATHIC_ASSERT(maxBucket != bucketEnd);
	return _cachedMaxBucket = maxBucket;
  }

#ifdef MATHIC_DEBUG
  template<class C, class B>
	bool GeoFront<C, B, false>::debugIsValid(B* bucketBegin, B* bucketEnd) const {
	if (_cachedMaxBucket == 0)
	  return true;
	MATHIC_ASSERT(!_cachedMaxBucket->empty());
	for (B* bucket = bucketBegin; bucket != bucketEnd; ++bucket) {
	  if (!bucket->empty()) {
		MATHIC_ASSERT(!_conf.cmpLessThan
			   (_conf.compare(_cachedMaxBucket->back(), bucket->back())));
	  }
	}
	return true;
  }
#endif

  // ********************************************************************
  // GeoFront maintaining an ordered list
  template<class C, class Bucket>
	class GeoFront<C, Bucket, true> {
  public:
	GeoFront(const C& conf, size_t& entryCountRef);
    ~GeoFront() {delete[] _bucketBegin;}

	Bucket** begin() {return _bucketBegin;}
	Bucket*const* begin() const {return _bucketBegin;}
	Bucket** end() {return _bucketEnd;}
	Bucket*const* end() const {return _bucketEnd;}

	/** Can contain this many   */
	void reserveCapacity(size_t newCapacity);
	void bucketsInvalidated(Bucket* oldBegin, Bucket* newBegin);
	bool empty() const {return _bucketBegin == _bucketEnd;}

    void clear();
	void insert(Bucket* bucket);
	void remove(Bucket* bucket);
	void keyIncreased(Bucket* bucket);
	void keyDecreased(Bucket* bucket);
	void swapKeys(Bucket* a, Bucket* b);
	bool larger(const Bucket* a, const Bucket* b) const;
	const Bucket* getMax(Bucket* bucketBegin, Bucket* bucketEnd) const;

#ifdef MATHIC_DEBUG
	bool debugIsValid(Bucket* bucketBegin, Bucket* bucketEnd) const;
#endif

    size_t getMemoryUse() const;

  private:
    size_t size() const {return _bucketEnd - _bucketBegin;}
    size_t capacity() const {return _bucketCapacityEnd - _bucketBegin;}

	Bucket** _bucketBegin;
	Bucket** _bucketEnd;
    Bucket** _bucketCapacityEnd;
	size_t* _entryCountRef;
	const C& _conf;
  };

  template<class C, class B>
  void GeoFront<C, B, true>::clear() {
    _bucketEnd = _bucketBegin;
  }

  template<class C, class B>
  size_t GeoFront<C, B, true>::getMemoryUse() const {
    return capacity() * sizeof(*_bucketBegin);
  }

  template<class C, class B>
  GeoFront<C, B, true>::GeoFront(const C& conf, size_t& entryCountRef):
	_bucketBegin(0),
	_bucketEnd(0),
    _bucketCapacityEnd(0),
	_entryCountRef(&entryCountRef),
	_conf(conf) {
    MATHIC_ASSERT(C::trackFront);
  }

  template<class C, class B>
  void GeoFront<C, B, true>::reserveCapacity(size_t newCapacity) {
    MATHIC_ASSERT(newCapacity >= size());
	if (newCapacity == 0)
	  newCapacity = 1;
    size_t const oldSize = size();
	B** const oldBegin = _bucketBegin;  
    B** const oldEnd = _bucketEnd;
    _bucketBegin = new B*[newCapacity];
	_bucketEnd = _bucketBegin + oldSize;
    _bucketCapacityEnd = _bucketBegin + newCapacity;
    std::copy(oldBegin, oldEnd, _bucketBegin);

	// the tokens point into the old space, so map them to an index
	// and then back to a pointer into the newly allocated memory.
	for (B** bucket = _bucketBegin; bucket != _bucketEnd; ++bucket) {
	  B** token = (*bucket)->getFrontToken();
	  token = (token - oldBegin) + _bucketBegin;
	  (*bucket)->setFrontToken(token);
	}
    delete[] oldBegin;
  }

  template<class C, class B>
	void GeoFront<C, B, true>::bucketsInvalidated
	(B* oldBegin, B* newBegin) {
	// _buckets points into an array of buckets that has been reallocated,
	// so we need to go to an index and back to update the pointers
	// to point into the new memory area.
	if (empty())
	  return;
	for (B** bucket = begin(); bucket != end(); ++bucket)
	  *bucket = (*bucket - oldBegin) + newBegin;
  }

  template<class C, class B>
	void GeoFront<C, B, true>::insert(B* bucket) {
	if (!C::trackFront)
	  return;
	MATHIC_ASSERT(bucket != 0);
	MATHIC_ASSERT(bucket->_frontPos == 0);
	MATHIC_ASSERT(!bucket->empty());
    if (_bucketEnd == _bucketCapacityEnd)
      reserveCapacity(size() + 1);
    *_bucketEnd = bucket;
    ++_bucketEnd;
	bucket->_frontPos = end() - 1;
	keyIncreased(bucket);
  }

  template<class C, class B>
	void GeoFront<C, B, true>::keyIncreased(B* bucket) {
	B** pos = bucket->_frontPos;
	B** begin = this->begin();
	for (; pos != begin; --pos) { // move bucket to lower index
	  B& shouldBeGreater = **(pos - 1);
	  typename C::CompareResult cmp =
		_conf.compare(shouldBeGreater.back(), bucket->back());
	  if (!_conf.cmpLessThan(cmp))
		break;
	  // todo: if equal
	  *pos = *(pos - 1);
	  shouldBeGreater._frontPos = pos;
	}
	// We don't have to adjust bucket's position if it never moved, but
	// detecting that without extra branches makes the code too complex
	// for it to be worth it.
	*pos = bucket;
	bucket->_frontPos = pos;
  }

  template<class C, class B>
	void GeoFront<C, B, true>::keyDecreased(B* bucket) {
	if (bucket->empty()) {
	  remove(bucket);
	  return;
	}
	B** pos = bucket->_frontPos;
	B** end = this->end();
	for (; pos + 1 != end; ++pos) { // move bucket to higher index
	  B& otherBucket = **(pos + 1);
	  typename C::CompareResult cmp =
		_conf.compare(bucket->back(), otherBucket.back());
	  if (!_conf.cmpLessThan(cmp)) {
		if (!C::supportDeduplication || !_conf.cmpEqual(cmp))
		  break; // greater than, found correct position
		otherBucket.setEntry(otherBucket.end() - 1,
							 _conf.deduplicate(bucket->back(), otherBucket.back()));
		bucket->pop_back();
		--*_entryCountRef;
		if (bucket->empty()) {
		  bucket->_frontPos = pos;
		  remove(bucket);
		  return;
		}
	  }
	  *pos = *(pos + 1);
	  otherBucket._frontPos = pos;
	}
	// We don't have to adjust bucket's position if it never moved, but
	// detecting that without extra branches makes the code too complex
	// for it to be worth it.
	*pos = bucket;
	bucket->_frontPos = pos;
  }

  template<class C, class B>
	void GeoFront<C, B, true>::remove(B* bucket) {
	MATHIC_ASSERT(bucket->_frontPos != 0);
	MATHIC_ASSERT(bucket->empty());
	B** end = this->end(); // can't dereference end
	for (B** i = bucket->_frontPos + 1; i != end; ++i) {
	  *(i - 1) = *i;
	  (*i)->_frontPos = i - 1;
	}
    --_bucketEnd;
	bucket->_frontPos = 0;
  }

  template<class C, class B>
	void GeoFront<C, B, true>::swapKeys(B* a, B* b) {
	MATHIC_ASSERT(a != 0);
	MATHIC_ASSERT(b != 0);
	std::swap(a->_frontPos, b->_frontPos);
	if (a->_frontPos != 0)
	  *a->_frontPos = a;
	if (b->_frontPos != 0)
	  *b->_frontPos = b;
  }

  template<class C, class B>
	bool GeoFront<C, B, true>::larger(const B* a, const B* b) const {
	MATHIC_ASSERT(a != 0);
	MATHIC_ASSERT(b != 0);
	return a->_frontPos < b->_frontPos;
  }

  template<class C, class B>
	const B* GeoFront<C, B, true>::getMax(B* bucketBegin, B* bucketEnd) const {
	MATHIC_ASSERT(!empty());
	return *begin();  
  }

#ifdef MATHIC_DEBUG
  template<class C, class B>
	bool GeoFront<C, B, true>::debugIsValid(B* bucketBegin, B* bucketEnd) const {
	std::vector<const B*> nonEmpty;
	size_t size = bucketEnd - bucketBegin;
	for (size_t b = 0; b < size; ++b) {
	  if (bucketBegin[b].empty()) {
		MATHIC_ASSERT(bucketBegin[b]._frontPos == 0);
	  } else {
		MATHIC_ASSERT(bucketBegin[b]._frontPos != 0);
		MATHIC_ASSERT(*(bucketBegin[b]._frontPos) == &(bucketBegin[b]));
		nonEmpty.push_back(&(bucketBegin[b]));
	  }
	}
	std::vector<B*> frontCopy(_bucketBegin, _bucketEnd);
	std::sort(nonEmpty.begin(), nonEmpty.end());
	std::sort(frontCopy.begin(), frontCopy.end());
	MATHIC_ASSERT(std::equal(frontCopy.begin(), frontCopy.end(), nonEmpty.begin()));
    if (!empty()) {
      for (B** it = _bucketBegin + 1; it != _bucketEnd; ++it)
        MATHIC_ASSERT(!_conf.cmpLessThan
          (_conf.compare((*(it - 1))->back(), (*it)->back())));
    }
	return true;
  }
#endif
}

#endif
