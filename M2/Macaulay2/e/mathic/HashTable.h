// MathicGB copyright 2012 all rights reserved. MathicGB comes with ABSOLUTELY
// NO WARRANTY and is licensed as GPL v2.0 or later - see LICENSE.txt.


// issues:
//  1. memory management of nodes
//  2. statistics gathering
//  3. resizing
//  4. put ASSERT's in, to check for all manner of contracts

// Notes to self:
// see http://attractivechaos.wordpress.com/2008/08/28/comparison-of-hash-table-libraries/

#ifndef MATHIC_HASHTABLE_GUARD
#define MATHIC_HASHTABLE_GUARD

#include "stdinc.h"

#include "memtailor/memtailor.h"
#include <cmath>
#include <utility>
#include <string>

namespace mathic {

  template<class Configuration> 
  class HashTable;

  class BasicHashTableConfiguration {
    typedef int Key;
    typedef int Value;

    size_t hash(Key k);
    bool keysEqual(Key k1, Key k2);
  };

  template<class Configuration> 
  class HashTable;

  template<class C> 
  class HashTable {
  public:
    typedef C Configuration;
    typedef typename C::Key Key;
    typedef typename C::Value Value;

    // Allowed actions for a Node* returned by lookup or insert:
    //  1. Key is const, while the Node* is in the hash table.
    //  2. Value can be modified by the callee at any time
    //  3. The 'next' field should not be referenced.
    // As for allocating and deallocating Node*:
    //  Do not allocate your own: only use Node* 's returned by the package
    //  There is no need to deallocate a Node*:  when a 'reset() is done, all
    //   Node* 's which have been allocated are deallocated at one time.
    //   When this happens, the Key and Value are not deallocated?
    // If 'remove' returns a Node*, then it is safe to change the key and/or value, e.g.
    //  to free the space pointed to by 'key' (if that is a pointer value, for instance).

    class Handle {
    public:
      friend class HashTable;

      Handle(Key k, Value v):
	next(0),
	entry(k,v)
      {}

      const Key& key() const {return entry.first;}
      const Value& value() const {return entry.second;}
      Value& value() {return entry.second;}
      void setKeyButOnlyDoSoIfThisHandleIsNotInHashTable(Key &new_k) {entry.first=new_k;}
    private:
      Handle *next;
      std::pair<Key, Value> entry;
    };

    // Create a hash table
    HashTable(const Configuration &conf, unsigned int nbits = 10);

    ~HashTable() {}

    // Returns the stored configuration.
    Configuration& configuration() {return mConf;}
    
    // Returns the stored configuration.
    Configuration const& configuration() const {return mConf;}

    // insert the key 'k' into the hash table.  If the key is already there,
    // and return std::pair(false, ...)
    // else return std::pair(true, node in the hash table).
    std::pair<bool, Handle*> insert(Key const& k, Value const& v);

    // If 'k' is present in the hash table, then its 'Node*' is returned.
    // If not, then NULL is returned.
    Handle* lookup(const Key &k);

    // remove 'p' from the hash table.  'p' itself is also removed.
    // what about the entries in 'p'?  Are the destructors called?
    void remove(Handle* & p);

    void reset(); // Major assumption: all nodes have been removed from the table already

    void hardReset(); // Slow, avoid if possible.

    // Returns how many bytes of memory this data structure consumes
    // not including sizeof(*this).
    size_t memoryUse() const;
    
    // Returns a string that describes how this data structure was
    // configured.
    std::string name() const;

  private:
    Handle* makeNode(const Key &k, const Value &v);

    void grow(unsigned int nbits);

    // Used for consistency checking.  Returns the number of nodes in the table.
    // Should match mNodeCount.
    size_t computeNodeCount() const;

    // Used for consistency checking.  Returns the number of nonempty bins in the hash table.
    // Should match mBinCount.
    size_t computeBinCount() const;
  
    size_t mHashMask; // this is the number, in binary:  00001111...1, where
                      // the number of 1's is mLogTableSize
    size_t mTableSize;
    size_t mLogTableSize; // number of bits in the table: mTableSize should be 2^mLogTableSize

    size_t mNodeCount;  // current number of nodes in the hash table
    size_t mBinCount; // number of nonzero bins

    size_t mMaxCountBeforeRebuild;

    // tweakable parameters
    double mRebuildThreshold;
    bool mAlwaysInsertAtEnd;

    memt::BufferPool mNodePool;
    std::vector<Handle *> mHashTable;
    Configuration mConf;
  };

  template<class C>
  HashTable<C>::HashTable(const Configuration &conf, unsigned int nbits):
    mLogTableSize(nbits),
    mTableSize(static_cast<size_t>(1) << nbits),
    mHashMask((static_cast<size_t>(1) << nbits) - 1),
    mNodeCount(0),
    mBinCount(0),
    mRebuildThreshold(0.1),
    mAlwaysInsertAtEnd(true),
    mNodePool(sizeof(Handle)),
    mConf(conf) 
  {
    mHashTable.resize(mTableSize);
    mMaxCountBeforeRebuild = static_cast<size_t>(mRebuildThreshold * mTableSize);
  }
  
  template<class C>
  void HashTable<C>::reset() {
    mNodePool.freeAllBuffers();
  }

  template<class C>
  typename HashTable<C>::Handle *HashTable<C>::makeNode(const Key &k, const Value &v)
  {
    mNodeCount++;
    void *buf = mNodePool.alloc();
    Handle* result = new (buf) Handle(k,v);
    return result;
  }

  template<class C>
  std::pair<bool, typename HashTable<C>::Handle *> HashTable<C>::insert(const Key &k, const Value &v) 
  {
    size_t hashval = mConf.hash(k) & mHashMask;
    
    MATHIC_ASSERT(hashval < mHashTable.size());
    Handle *tmpNode = mHashTable[hashval];
    Handle *result = 0;
    if (tmpNode == 0)
      {
	result = makeNode(k,v);
	mHashTable[hashval] = result;
      }
    else
      {
	while (true)
	  {
	    if (mConf.keysEqual(tmpNode->key(), k))
	      {
		result = tmpNode;
		return std::pair<bool,Handle *>(false,result);
	      }
	    if (tmpNode->next == 0)
	      {
		// time to insert the monomial
		result = makeNode(k, v);
		if (mAlwaysInsertAtEnd)
		  {
		    tmpNode->next = result;
		  }
		else
		  {
		    result->next = mHashTable[hashval];
		    mHashTable[hashval] = result;
		  }
		break;
	      }
	    tmpNode = tmpNode->next;
	  }
      }
    
    if (mNodeCount > mMaxCountBeforeRebuild)
      grow(static_cast<unsigned int>(mLogTableSize + 2));  // increase by a factor of 4??
    
    MATHIC_ASSERT(computeNodeCount() == mNodeCount);
    return std::pair<bool, Handle *>(true,result);
  }

  template<class C>
  typename HashTable<C>::Handle * HashTable<C>::lookup(const Key &k)
  {
    size_t hashval = mConf.hash(k) & mHashMask;
    
    MATHIC_ASSERT(hashval < mHashTable.size());
    for (Handle *p = mHashTable[hashval]; p != 0; p = p->next)
      {
	if (mConf.keysEqual(p->key(), k))
	  return p;
      }
    return NULL;
  }
    
  template<class C>
  void HashTable<C>::remove(Handle *& p) 
  {
    mNodeCount--;
    size_t const hashval = mConf.hashvalue(p->key) & mHashMask;
    Handle head;
    Handle* tmpNode = mHashTable[hashval];
    head.next = tmpNode;
    for (Handle* q = &head; q->next != 0; q = q->next) 
      {
	if (q->next == p) 
	  {
	    q->next = p->next;
	    mHashTable[hashval] = head.next;
	    if (head.next == 0) mBinCount--;
	    //TODO: call destructor for pair, then call 'free' with the mNodePool
	    return;
	  }
      }
    // If we get here, then the node is not at its supposed hash value.
    // That probably means either that the node has been deleted twice
    // or that the value in the node changed so that its hash value
    // changed. That is not allowed.
    MATHIC_ASSERT(false);
  }
  
  template<class C>
  void HashTable<C>::grow(unsigned int new_nbits) 
  {
    MATHIC_ASSERT(computeNodeCount() == mNodeCount);
    size_t const old_table_size = mTableSize;
    mTableSize = static_cast<size_t>(1) << new_nbits;
    mLogTableSize = new_nbits;
    mHashMask = mTableSize-1;
    std::vector<Handle *> old_table(mTableSize);
    std::swap(old_table, mHashTable);

    mBinCount = 0;
    for (size_t i = 0; i < old_table_size; ++i)
      {
	Handle *p = old_table[i];
	while (p != 0)
	  {
	    Handle *q = p;
	    p = p->next;
	    q->next = 0;
	    // Reinsert node.  We know that it is unique
	    size_t hashval = mConf.hash(q->key()) & mHashMask;
	    Handle *r = mHashTable[hashval];
	    if (r == 0) mBinCount++;
	    if (r == 0 || !mAlwaysInsertAtEnd) 
	      {
		q->next = r;
		mHashTable[hashval] = q;
	      }
	    else
	      {
		// put it at the end
		for ( ; r->next != 0; r = r->next) { }
		r->next = q;
	      }
	  }
      }
    
    mMaxCountBeforeRebuild =
      static_cast<size_t>(std::floor(mTableSize * mRebuildThreshold));
    
    MATHIC_ASSERT(computeNodeCount() == mNodeCount);
  }

  template<class C>
  size_t HashTable<C>::memoryUse() const
  {
    size_t result = mHashTable.capacity() * sizeof(Handle *);
    result += mNodePool.getMemoryUse();
    return result;
  }
  
  template<class C>
  size_t HashTable<C>::computeNodeCount() const
  {
    size_t result = 0;
    for (size_t i=0; i<mTableSize; i++)
      {
	for (Handle *p = mHashTable[i]; p != 0; p = p->next) result++;
      }
    return result;
  }

  template<class C>
  size_t HashTable<C>::computeBinCount() const
  {
    size_t result = 0;
    for (size_t i=0; i<mTableSize; i++)
      {
	if (mHashTable[i] != 0) result++;
      }
    return result;
  }

  template<class C>
  std::string HashTable<C>::name() const
  {
    return std::string("HashTable");
  }
  
} // namespace mathic

#endif

// Local Variables:
// indent-tabs-mode: nil
// mode: c++
// compile-command: "make -C $MATHIC/mathic "
// End:

