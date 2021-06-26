#ifndef MATHIC_GEOBUCKET_GUARD
#define MATHIC_GEOBUCKET_GUARD

#include "stdinc.h"
#include <string>
#include <sstream>
#include <vector>
#include <algorithm>
#include "GeoFront.h"

namespace mathic {
  enum GeobucketBucketStorage {
	GeoStorePlain = 0,
	GeoStoreDoubleBuffer = 1,
	GeoStoreSameSizeBuffer = 2,

	GeobucketBucketStorageBegin = 0,
	GeobucketStorageEnd = 3
  };

  /** A geobucket priority queue.

	  The Configuration template parameter specifies properties of the geobucket.
	  A configuration object is passed to the constructor of geobucket which
	  makes a copy. After the initial copy no further copies are made.

	  Configuration must have these fields:

	  * A type Entry
	  The elements of the geobucket will have this type. Entry must have a copy
	  constructor.

	  * A type CompareResult
	  Intended to be a bool for "less than" or some other type that can encode
	  "less than", "equal" and "greater than". As the Configuration itself
	  interprets what the values of this type mean, the type can be anything.
	  CompareResult must have a copy constructor.

	  * A const or static method: CompareResult compare(Entry, Entry)
	  It is OK for the two parameters to be const Entry&, but compare must not
	  retain a reference to the Entry. The return value is intended to
	  encode the ordering of the two elements.

	  * A const or static method: bool cmpLessThan(CompareResult)
	  The geobucket calls this on a CompareResult returned from compare(a,b).
	  The return value should be true if and only if a is strictly less than b.

	  * A size_t minBucketSize
	  This is the size of the smallest bucket. Must be at least 1.

	  * A size_t geoBase
	  When a new bucket is added, its capacity is geoBase multiplied by the
	  largest capacity among the buckets in the geobucket. Must be at least
	  two.

	  * A static const size_t insertFactor
	  An insert of size S will be done into the smallest bucket whose total
	  capacity (not free capacity) is at least S * insertFactor. 

	  * A static const bool supportDeduplication
	  If this is true, then the geobucket will remove entries that are compared
	  and found to be equal. This requires the use of a method cmpEqual and
	  deduplicate.

	  * A static or const method: bool cmpEqual(CompareResult)
	  The method is only called if supportDeduplication is true.
	  The geobucket calls this on a CompareResult returned from compare(a,b).
	  If the return value is true, then a and b must be equivalent in the ordering
	  imposed by compare and cmpLessThan. When the return value is true,
	  the two elements will be replaced by the return value of deduplicate.

	  * A static or const method: Entry deduplicate(Entry a, Entry b)
	  This method is only called if supportDeduplication is true and
	  cmpEqual(compare(a,b)) is true. The two elements a and b in the geobucket
	  will be replaced by the return value of deduplicate. So the number of
	  elements in the geobucket will decrease by one every time deduplicate
	  is called.

	  * A static const bool minBucketBinarySearch
	  If this field is true, insertions of single elements is done using binary
	  search. Otherwise a linear search is done.

	  * A static const bool trackFront
	  If this field is true, then a sorted list of buckets will be maintained
	  to aid in determining the largest element in the queue. Otherwise a
	  linear search is done through the buckets to find the largest element.

	  * A static const bool premerge
	  Let A be a list of elements and let B be the elements in a bucket. Suppose
	  we are about to merge A and B and put them into the same bucket. If 
	  the combined size of A and B is larger than the capacity of the bucket,
	  and premerge is true, then B will be preemptively merged into the next
	  bucket after which A can be copied into the now empty bucket that B
	  previously occupied. If premerge is false, then A and B will be merged,
	  and the overflow will force a merge of the combined A and B into the
	  next bucket.

	  * A static const bool collectMax
	  The meta data of a bucket is kept in a separate memory area from the
	  elements of the bucket. If collectMax is true, then the maximum element
	  of each bucket is copied into the meta data area. The idea is to
	  minimize cache misses caused when pop does a linear scan through
	  the maximum elements.

	  * A static const GeobucketBucketStorage bucketStorage
	  GeoStorePlain = 0,
	  GeoStoreDoubleBuffer = 1,
	  GeoStoreSameSizeBuffer = 2,

	  If this field is GeoStorePlain, then a bucket of capacity S has a memory
	  area of size S.

	  If this field is GeoStoreDoubleBuffer, then every bucket will have two
	  same-size memory areas. Only one of the two memory areas is in use at
	  any one time. That way a merge into the bucket can store its
	  result into the other memory area so that a merge does not require a copy.

	  If this field is GeoStoreSameSizeBuffer, then all buckets use a
	  memory area of the same size as the maximum bucket. A bucket's noted
	  capacity does not change. This way any memory area can be used in place
	  of any other memory area, which makes it possible to do merges with no
	  copies and also the content of two buckets can be swapped with no copies.
	  Cannot be combined with useDoubleBucket.
  */
  template<class C>
  class Geobucket {
  public:
	typedef C Configuration;
	typedef typename Configuration::Entry Entry;

	Geobucket(const Configuration& configuration);

	Configuration& getConfiguration() {return _conf;}
	const Configuration& getConfiguration() const {return _conf;}

	std::string getName() const;
	void push(Entry entries);

	// [begin, end) must be sorted in decreasing order.
	// If Configuration::supportDeduplication is true, then no value must
	// be present twice in [begin, end).
	template<class It>
    void push(It begin, It end);
	
    Entry pop();
    void clear();
	Entry top() const;
    void decreaseTop(Entry newEntry);

	bool empty() const;
    size_t size() const;
	void print(std::ostream& out) const;

    template<class T>
    void forAll(T& t) const {
      for (Bucket* bucket = _bucketBegin; bucket != _bucketEnd; ++bucket) {
        const Entry* stop = bucket->end();
        for (const Entry* entry = bucket->begin(); entry != stop; ++entry) {
          if (!t.proceed(*entry)) {
            MATHIC_ASSERT(isValid());
            return;
          }
        }
      }
	  MATHIC_ASSERT(isValid());
    }

    size_t getMemoryUse() const;

  private:
	Geobucket(const Geobucket&); // unavailable
	Geobucket& operator=(const Geobucket&); // unavailable

  public: // MES: clang complains when this is private (in GeoFront.h)
	struct Bucket {
	  Bucket(size_t capacity, Entry* buffer, Entry* otherBuffer);
	  size_t size() const {return _size;}
	  size_t capacity() const {return _capacity;}
	  Entry* begin() {return _begin;}
	  Entry* end() {return begin() + size();}
	  const Entry* begin() const {return _begin;}
	  const Entry* end() const {return begin() + size();}
	  bool empty() const {return size() == 0;}
	  const Entry& back() const {
		MATHIC_ASSERT(!empty());
		return Configuration::collectMax ? _back : *(end() - 1);
	  }
	  const Entry& operator[](size_t i) const {
		MATHIC_ASSERT(i < size());
		return *(begin() + i);
	  }
	  void pop_back() {
		MATHIC_ASSERT(size() > 0);
		--_size;
		if (Configuration::collectMax && _size > 0)
		  _back = *(end() - 1);
	  }
	  void setEntry(Entry* pos, const Entry& entry) {
		MATHIC_ASSERT(begin() <= pos && pos < end());
		*pos = entry;
		if (Configuration::collectMax && pos == end() - 1)
		  _back = entry;
	  }
	  void insertAtNotEnd(Entry* pos, const Entry& entry) {
		MATHIC_ASSERT(begin() <= pos && pos < end());
		for (Entry* it = end(); it != pos; --it)
		  *it = *(it - 1);
		*pos = entry;
		++_size;
	  }
	  void setBuffer(Entry* buffer) {
		std::copy(begin(), end(), buffer);
		_begin = buffer;
	  }
	  void setBufferWithNewEntries(Entry* buffer, size_t size) {
		_begin = buffer;
		_size = size;
		if (size > 0)
		  _back = *(end() - 1);        
	  }
	  void setOtherBuffer(Entry* otherBuffer) {
		MATHIC_ASSERT(Configuration::bucketStorage == GeoStoreDoubleBuffer);
		_otherBuffer = otherBuffer;
	  }
	  Entry* getOtherBuffer() {
		MATHIC_ASSERT(Configuration::bucketStorage == GeoStoreDoubleBuffer);
		return _otherBuffer;
	  }
	  void switchToOtherBuffer(size_t size) {
		MATHIC_ASSERT(Configuration::bucketStorage == GeoStoreDoubleBuffer);
		std::swap(_begin, _otherBuffer);
		_size = size;
		if (size > 0)
		  _back = *(end() - 1);
	  }
	  void clear() {
		_size = 0;
	  }
	  template<class It>
	  void assign(It begin, It end, size_t size) {
		MATHIC_ASSERT(static_cast<size_t>(end - begin) == size);
		std::copy(begin, end, _begin);
		_size = size;
		if (size > 0)
		  _back = *(end - 1);
	  }
	  template<class It1, class It2>
		void merge(Geobucket<Configuration>& gb, It1 begin1, It1 end1, It2 begin2, It2 end2) {
		Entry* end = gb.merge(begin1, end1, begin2, end2, _begin);
		_size = end - _begin;
		if (_size > 0)
		  _back = *(end - 1);
	  }
	  void push_back(const Entry& e) {
		MATHIC_ASSERT(size() < capacity());
		*(_begin + _size) = e;
		++_size;
		if (Configuration::collectMax)
		  _back = e;
	  }
	  Bucket** getFrontToken() {return _frontPos;}
	  const Bucket** getFrontToken() const {return _frontPos;}
	  void setFrontToken(Bucket** token) {_frontPos = token;}

	  Bucket** _frontPos;

	private:
	  Entry* _otherBuffer;
	  Entry _back;
	  Entry* _begin;
	  size_t _size;
	  size_t _capacity;
	};
	void addBucket();

	template<bool value> struct Premerge {};
	template<class It>
	  void insert(Bucket* bucket, It begin, It end, size_t size, Premerge<true>);
	template<class It>
	  void insert(Bucket* bucket, It begin, It end, size_t size, Premerge<false>);

	template<class It1, class It2, class ResIt>
	  ResIt merge(It1 begin1, It1 end1, It2 begin2, It2 end2, ResIt res);
	template<class It>
	  size_t singleInsert(It begin, size_t size, const Entry& value) const;

	void moveToEmpty(Bucket& to, Bucket& from);
	template<class It>
	  void mergeToEmpty(Bucket& to, Bucket& from, It begin, It end);
	void mergeToNonEmpty(Bucket& into, Bucket& from);
	template<class It>
	  void mergeToNonEmpty(Bucket& into, It begin, It end);

	size_t _geoBase;
	std::vector<Bucket> _buckets;
	Bucket* _bucketBegin;
	Bucket* _bucketEnd;
	std::vector<Entry> _mem;
	Entry* _tmp; // has capacity equal to the largest bucket
	size_t _entryCount;

	// Has capacity equal to the largest bucket. Only used when
	// C::premerge is false.
	Entry* _tmpForNoPremerge;
	Configuration _conf;
	GeoFront<Configuration> _front;

    /// Asserts internal invariants if asserts are turned on.
	bool isValid() const;
  };

  template<class C>
	Geobucket<C>::Geobucket(const C& configuration):
  _bucketBegin(0),
	_bucketEnd(0),
	_tmp(0),
	_entryCount(0),
	_tmpForNoPremerge(0),
	_conf(configuration),
	_front(_conf, _entryCount) {
	MATHIC_ASSERT(_conf.geoBase > 1);
	addBucket(); // this avoids the special case of no buckets.
    MATHIC_ASSERT_NO_ASSUME(_front.debugIsValid(_bucketBegin, _bucketEnd));
  }

  template<class C>
	std::string Geobucket<C>::getName() const {
	std::ostringstream out;
	out << "Geobucket("
		<< 'b' << _conf.geoBase
		<< 'm' << _conf.minBucketSize
		<< 'i' << C::insertFactor
		<< (C::minBucketBinarySearch ? " mbin" : "")
		<< (C::trackFront ? " tf" : "")
		<< (C::supportDeduplication ? " dedup" : "")
		<< (C::premerge ? " prem" : "")
		<< (C::collectMax ? " col" : "")
		<< (C::bucketStorage == GeoStoreDoubleBuffer ? " db" : "")
		<< (C::bucketStorage == GeoStoreSameSizeBuffer ? " ss" : "")
		<< ')';
	return out.str();
  }

  template<class C>
	void Geobucket<C>::push(Entry entry) {
	++_entryCount;
	Bucket& bucket = *_bucketBegin;
	if (bucket.size() == bucket.capacity()) {
	  const Entry* p = &entry;
	  insert(_bucketBegin, p, p + 1, 1, Premerge<C::premerge>());
	  return;
	}
	if (bucket.empty()) {
	  bucket.push_back(entry);
	  _front.insert(&bucket);
	} else if (C::minBucketBinarySearch) {
	  size_t range = bucket.size();
	  Entry* begin = bucket.begin();
	  while (true) {
		// invariant: the position is in the range [begin, begin + range].
		if (range == 0) {
		  if (begin == bucket.end()) {
			bucket.push_back(entry);
			_front.keyIncreased(&bucket);
		  } else
			bucket.insertAtNotEnd(begin, entry);
		  break;
		}
		Entry* mid = begin + range / 2;
		typename C::CompareResult cmp = _conf.compare(entry, *mid);
		if (_conf.cmpLessThan(cmp))
		  range /= 2;
		else if (!C::supportDeduplication || !_conf.cmpEqual(cmp)) {
		  begin = mid;
		  ++begin;
		  range -= range / 2 + 1;
		} else {
		  --_entryCount;
		  bucket.setEntry(mid, _conf.deduplicate(*mid, entry));
		  break;
		}
	  }
	} else {
	  Entry* end = bucket.end();
	  for (Entry* pos = bucket.begin(); pos != end; ++pos) {
		typename C::CompareResult cmp = _conf.compare(entry, *pos);
		if (_conf.cmpLessThan(cmp)) {
		  bucket.insertAtNotEnd(pos, entry);
		  return;
		}
		if (C::supportDeduplication && _conf.cmpEqual(cmp)) {
		  bucket.setEntry(pos, _conf.deduplicate(*pos, entry));
		  --_entryCount;
		  return;
		}
	  }
	  bucket.push_back(entry);
	  _front.keyIncreased(&bucket);
	}
  }

  template<class C>
	template<class It>
	void Geobucket<C>::push(It begin, It end) {
#ifdef MATHIC_DEBUG
	if (begin != end) {
	  for (It it = begin; it + 1 != end; ++it) {
		// deduplication assumes that [begin, end) does not contain
		// duplicates for efficiency, so support for deduplication implies
		// that no element can be duplicated within a pushed range.
		if (C::supportDeduplication) {
		  MATHIC_ASSERT(_conf.cmpLessThan(_conf.compare(*(it + 1), *it)));
		} else {
		  MATHIC_ASSERT(!_conf.cmpLessThan(_conf.compare(*it, *(it + 1))));
		}
	  }
	}
#endif
	const size_t entryCount = end - begin;
	const size_t adjCount = entryCount * C::insertFactor;
	_entryCount += entryCount;
	while (adjCount > (_bucketEnd - 1)->capacity())
	  addBucket();
	for (Bucket* bucket = _bucketBegin; ; ++bucket) {
	  MATHIC_ASSERT(bucket != _bucketEnd);
	  if (adjCount <= bucket->capacity()) {
		std::reverse_iterator<It> rBegin(end);
		std::reverse_iterator<It> rEnd(begin);
		insert(bucket, rBegin, rEnd, entryCount, Premerge<C::premerge>());
		break;
	  }
	}
	MATHIC_SLOW_ASSERT(isValid());
  }

  template<class C>
  void Geobucket<C>::clear() {
	MATHIC_ASSERT(isValid());
    _entryCount = 0;
    _front.clear();
    for (Bucket* bucket = _bucketBegin; bucket != _bucketEnd; ++bucket)
      bucket->clear();
	MATHIC_ASSERT(isValid());
  }

  template<class C>
  typename Geobucket<C>::Entry Geobucket<C>::pop() {
	Bucket* maxBucket =
	  const_cast<Bucket*>(_front.getMax(_bucketBegin, _bucketEnd));
	Entry top = maxBucket->back();
	maxBucket->pop_back();
	--_entryCount;
	_front.keyDecreased(maxBucket);
	MATHIC_SLOW_ASSERT(isValid());
	return top;
  }

  template<class C>
  typename Geobucket<C>::Entry Geobucket<C>::top() const {
	MATHIC_ASSERT(!empty());
	return _front.getMax(_bucketBegin, _bucketEnd)->back();
  }

  template<class C>
  void Geobucket<C>::decreaseTop(Entry newEntry) {
    MATHIC_ASSERT(!empty());
    pop();
    push(newEntry);
  }
  
  template<class C>
  size_t Geobucket<C>::size() const {
#ifdef MATHIC_DEBUG
	size_t sum = 0;
	for (size_t b = 0; b < _buckets.size(); ++b)
      sum += _buckets[b].size();
	MATHIC_ASSERT(sum == _entryCount);
#endif
    return _entryCount;
  }

  template<class C>
  bool Geobucket<C>::empty() const {
    return size() == 0;
  }

  template<class C>
	void Geobucket<C>::print(std::ostream& out) const {
	out << getName() << ": {\n";
	for (const Bucket* bucket = _bucketBegin; bucket != _bucketEnd; ++bucket) {
	  out << ' ' << (bucket - _bucketBegin) << ":";
	  if (bucket->size() > 4) {
		out << ' ' << (*bucket)[0] << ' ' << (*bucket)[1] << " ... "
			<< (*bucket)[bucket->size() - 2] << ' '
			<< (*bucket)[bucket->size() - 1];
	  } else
		for (size_t i = 0; i < bucket->size(); ++i)
		  out << ' ' << (*bucket)[i];
	  out << " [" << bucket->size() << '/' << bucket->capacity() << "]\n";
	}
	out << "}\n";
  }

  template<class C>
	void Geobucket<C>::addBucket() {
	// calculate required amount of memory
	const size_t newBucketSize = _bucketBegin == _bucketEnd ?
	  _conf.minBucketSize : (_bucketEnd - 1)->capacity() * _conf.geoBase;
	size_t required = 0;
	typedef typename std::vector<Bucket>::iterator It;
	for (Bucket* it = _bucketBegin; it != _bucketEnd; ++it) {
	  required += C::bucketStorage == GeoStoreSameSizeBuffer ?
		newBucketSize : it->capacity();
	  if (C::bucketStorage == GeoStoreDoubleBuffer)
		required += it->capacity(); // for other buffer
	}
	required += newBucketSize; // for new bucket
	if (C::bucketStorage == GeoStoreDoubleBuffer)
	  required += newBucketSize; // for other buffer

	required += newBucketSize; // for _tmp
	if (!C::premerge)
	  required += newBucketSize; // for _tmpForNoPremerge

	// allocate new memory
	std::vector<Entry> newMem(required);
	std::copy(_mem.begin(), _mem.end(), newMem.begin());

	// move to new memory buffer
	size_t offset = 0;
	for (Bucket* it = _bucketBegin; it != _bucketEnd; ++it) {
	  it->setBuffer(&(newMem[offset]));
	  offset += C::bucketStorage == GeoStoreSameSizeBuffer ?
		newBucketSize : it->capacity();
	  if (C::bucketStorage == GeoStoreDoubleBuffer) {
		it->setOtherBuffer(&(newMem[offset]));
		offset += it->capacity();
	  }
	}
	Entry* buffer = &(newMem[offset]);
	offset += newBucketSize;
	Entry* otherBuffer = 0;
	if (C::bucketStorage == GeoStoreDoubleBuffer) {
	  otherBuffer = &(newMem[offset]);
	  offset += newBucketSize;
	}
	// redirect pointers in _front to point to new memory for _buckets
	Bucket* oldBucketBegin = _bucketBegin;
	_buckets.push_back(Bucket(newBucketSize, buffer, otherBuffer));
	_bucketBegin = &_buckets.front();
	_bucketEnd = _bucketBegin + _buckets.size();
	_front.bucketsInvalidated(oldBucketBegin, _bucketBegin);
	_front.reserveCapacity(_bucketEnd - _bucketBegin);

	_tmp = &(newMem[offset]);
	offset += newBucketSize;
	if (!C::premerge) {
	  _tmpForNoPremerge = &(newMem[offset]);
	  offset += newBucketSize;
	}

	MATHIC_ASSERT(offset == required);
	_mem.swap(newMem);
  }

  template<class C>
	void Geobucket<C>::mergeToNonEmpty(Bucket& to, Bucket& from) {
	MATHIC_ASSERT(!to.empty());
	MATHIC_ASSERT(!from.empty());
	MATHIC_ASSERT(&to != &from);
	MATHIC_ASSERT(to.size() + from.size() <= to.capacity());
	if (!C::trackFront || _front.larger(&from, &to))
	  _front.swapKeys(&to, &from);
	mergeToNonEmpty(to, from.begin(), from.end());
	from.clear();
	_front.remove(&from);
  }

  template<class C>
	template<class It>
	void Geobucket<C>::mergeToNonEmpty(Bucket& into, It begin, It end) {
	// todo: template select instead of if
	// todo: into => to
	MATHIC_ASSERT(!into.empty());
	MATHIC_ASSERT(begin != end);
	MATHIC_ASSERT(static_cast<size_t>(end - begin) <= into.capacity());
	if (C::bucketStorage == GeoStoreDoubleBuffer) {
	  Entry* other = into.getOtherBuffer();
	  Entry* otherEnd = merge(into.begin(), into.end(), begin, end, other);
	  into.switchToOtherBuffer(otherEnd - other);
	} else if (C::bucketStorage == GeoStoreSameSizeBuffer) {
	  Entry* tmpEnd = merge(into.begin(), into.end(), begin, end, _tmp);
	  Entry* previousBufferOfInto = into.begin();
	  into.setBufferWithNewEntries(_tmp, tmpEnd - _tmp);
	  _tmp = previousBufferOfInto;
	} else {
	  Entry* tmpEnd = std::copy(into.begin(), into.end(), _tmp);
	  into.merge(*this, begin, end, _tmp, tmpEnd);
	}
	_front.keyIncreased(&into);
  }

  template<class C>
	template<class It>
	void Geobucket<C>::mergeToEmpty(Bucket& to, Bucket& from, It begin, It end) {
	MATHIC_ASSERT(to.empty());
	MATHIC_ASSERT(!from.empty()); // so has place in front
	MATHIC_ASSERT(&to != &from);
	MATHIC_ASSERT(begin != end);
	MATHIC_ASSERT(static_cast<size_t>(end - begin) + from.size() <= to.capacity());
	to.merge(*this, begin, end, from.begin(), from.end());
	from.clear();
	_front.swapKeys(&from, &to);
	_front.keyIncreased(&to);
  }

  template<class C>
	void Geobucket<C>::moveToEmpty(Bucket& to, Bucket& from) {
	MATHIC_ASSERT(&to != &from);
	MATHIC_ASSERT(to.empty());
	MATHIC_ASSERT(!from.empty());
	MATHIC_ASSERT(from.size() <= to.capacity());

	if (C::bucketStorage == GeoStoreSameSizeBuffer) {
	  Entry* previousBufferOfTo = to.begin();
	  to.setBufferWithNewEntries(from.begin(), from.size());
	  from.setBufferWithNewEntries(previousBufferOfTo, 0);
	} else {
	  to.assign(from.begin(), from.end(), from.size());
	  from.clear();
	}
	_front.swapKeys(&from, &to);
  }

  template<class C>
	template<class It>
	void Geobucket<C>::insert
	(Bucket* p, It begin, It end, size_t incomingSize, Premerge<false>) {
	MATHIC_ASSERT(!C::premerge);
	MATHIC_ASSERT(incomingSize == static_cast<size_t>(end - begin));
	MATHIC_ASSERT(0 < incomingSize);
	MATHIC_ASSERT(begin != end);

	size_t b = p - _bucketBegin;
	Bucket& bucket = _buckets[b];

	if (bucket.empty()) {
	  bucket.assign(begin, end, incomingSize);
	  _front.insert(&bucket);
	  return;
	}
	if (bucket.size() + incomingSize <= bucket.capacity()) {
	  mergeToNonEmpty(bucket, begin, end);
	  return;
	}

	size_t accumulatedSize = bucket.size() + incomingSize;
	for (size_t i = b + 1; ; ++i) {
	  if (i == _buckets.size())
		addBucket(); // can invalidate bucket reference variable
	  accumulatedSize += _buckets[i].size();
	  if (accumulatedSize <= _buckets[i].capacity())
		break;
	}

	// deal with first step separately as the iterator can have
	// different type (grrrrr)
	Bucket& current = _buckets[b];
	MATHIC_ASSERT(b + 1 < _buckets.size());
	Bucket& next = _buckets[b + 1];
	if (next.empty()) {
	  mergeToEmpty(next, current, begin, end);
	  return;
	}
	Entry* tmpForNoPremergeEnd = merge(
									   begin, end, current.begin(), current.end(), _tmpForNoPremerge);
	current.clear();
	_front.remove(&current);
	++b;

	while (true) {
	  Bucket& current = _buckets[b];
	  const size_t predictedSize =
		(tmpForNoPremergeEnd - _tmpForNoPremerge) + current.size();
	  if (predictedSize <= current.capacity()) {
		mergeToNonEmpty(current, _tmpForNoPremerge, tmpForNoPremergeEnd);
		return;
	  }
	  MATHIC_ASSERT(b + 1 < _buckets.size());
	  Bucket& next = _buckets[b + 1];
	  if (next.empty()) {
		mergeToEmpty(next, current, _tmpForNoPremerge, tmpForNoPremergeEnd);
		return;
	  }
	  tmpForNoPremergeEnd = merge(_tmpForNoPremerge, tmpForNoPremergeEnd,
								  current.begin(), current.end(), _tmp);
	  std::swap(_tmp, _tmpForNoPremerge);
	  current.clear();
	  _front.remove(&current);
	  ++b;
	}
  }

  template<class C>
	template<class It>
	void Geobucket<C>::insert
	(Bucket* bucket, It begin, It end, size_t size, Premerge<true>) {
	MATHIC_ASSERT(C::premerge);
	// ** determine premerge range (bucket, rbegin]
	Bucket* rbegin = bucket;
	size_t incomingSize = size;
	while (true) {
	  if (rbegin->size() + incomingSize <= rbegin->capacity())
		break;
	  incomingSize = rbegin->size(); // this will be incoming for next bucket
	  ++rbegin;
	  if (rbegin == _bucketEnd) {
		Bucket* oldBucketBegin = _bucketBegin;
		addBucket();
		bucket = (bucket - oldBucketBegin) + _bucketBegin;
		rbegin = (rbegin - oldBucketBegin) + _bucketBegin;
		break;
	  }
	}
	// ** perform premerges
	if (bucket != rbegin) {
	  if (!rbegin->empty()) {
		mergeToNonEmpty(*rbegin, *(rbegin - 1));
		--rbegin;
	  }
	  for (Bucket* rit = rbegin; rit != bucket; --rit)
		moveToEmpty(*rit, *(rit - 1));
	}
	// ** perform insertion
	if (bucket->empty()) {
	  bucket->assign(begin, end, size);
      _front.insert(bucket);
	} else
	  mergeToNonEmpty(*bucket, begin, end);
  }

  template<class C>
  template<class It1, class It2, class ResIt>
  ResIt Geobucket<C>::merge
	(It1 begin1, It1 end1, It2 begin2, It2 end2, ResIt res) {
	if (begin1 == end1) goto range1Done;
	if (begin2 == end2) goto range2Done;
	while (true) {
	  typename C::CompareResult cmp = _conf.compare(*begin1, *begin2);
	  if (_conf.cmpLessThan(cmp)) {
		*res = *begin1;
		++res;
		++begin1;
		if (begin1 == end1) goto range1Done;
	  } else if (!C::supportDeduplication || !_conf.cmpEqual(cmp)) {
		*res = *begin2;
		++res;
		++begin2;
		if (begin2 == end2) goto range2Done;
	  } else {
		*res = _conf.deduplicate(*begin1, *begin2);
		++res;
		++begin1;
		++begin2;
		--_entryCount;
		if (begin1 == end1) goto range1Done;
		if (begin2 == end2) goto range2Done;
	  }
	}
  range1Done: return std::copy(begin2, end2, res);
  range2Done: return std::copy(begin1, end1, res);
  }

  template<class C>
  Geobucket<C>::Bucket::Bucket
	(size_t capacity, Entry* buffer, Entry* otherBuffer):
  _frontPos(0),
	_otherBuffer(otherBuffer),
	_begin(buffer),
	_size(0),
	_capacity(capacity) {
	  MATHIC_ASSERT(C::bucketStorage == GeoStoreDoubleBuffer ?
			 otherBuffer != 0 : otherBuffer == 0);
	}

  template<class C>
  size_t Geobucket<C>::getMemoryUse() const {
    return _mem.capacity() * sizeof(_mem.front()) +
      _buckets.capacity() * sizeof(_buckets.front()) +
      _front.getMemoryUse();
  }

  template<class C>
  bool Geobucket<C>::isValid() const {
#ifndef MATHIC_DEBUG
    return true;
#else
	MATHIC_ASSERT(_conf.geoBase >= 2);
	MATHIC_ASSERT(!_buckets.empty());
	MATHIC_ASSERT(_bucketBegin == &_buckets.front());
	MATHIC_ASSERT(_bucketEnd == _bucketBegin + _buckets.size());
	MATHIC_ASSERT(_buckets.front().capacity() >= 1);

	size_t entryCount = 0;
	for (size_t b = 0; b < _buckets.size(); ++b) {
	  const Bucket& bucket = _buckets[b];
	  entryCount += bucket.size();
	  if (bucket.empty())
		continue;
	  for (const Entry* it = bucket.begin(); it != bucket.end() - 1; ++it) {
		MATHIC_ASSERT(!_conf.cmpLessThan(_conf.compare(*(it + 1), *it)));
	  }
	}
	MATHIC_ASSERT(entryCount == _entryCount);
	return _front.debugIsValid(_bucketBegin, _bucketEnd);
#endif
  }
};

#endif
