#include "mathic/HashTable.h"
#include <gtest/gtest.h>
#include <string>
#include <functional>

namespace {
  class HashTableConf 
  {
  public:
    typedef int Key;
    typedef int Value;

    size_t hash(Key k) {return k;}
    bool keysEqual(Key k1, Key k2) {return k1==k2;}
  };

  typedef mathic::HashTable<HashTableConf> HashTab;
}

TEST(HashTable, NoOp) {
  HashTableConf C;
  HashTab H(C);

  H.insert(1,3);
  H.insert(14,7);
  H.insert(17,7);
  H.insert(14,4);

  HashTab::Handle *p = H.lookup(14);
  ASSERT_FALSE(p == NULL);
  ASSERT_EQ(p->key(),14);
  ASSERT_EQ(p->value(),7);
};

namespace {
  class HashTableStringConf 
  {
  public:
    typedef std::string Key;
    typedef size_t Value;
    typedef std::hash<std::string> hashfcn;
    size_t hash(Key k) { 
      hashfcn fcn; 
      return fcn(k);
    }

    bool keysEqual(Key k1, Key k2) {return k1==k2;}
  };

  typedef mathic::HashTable<HashTableStringConf> HashStringTab;
}

TEST(HashTable, StringKeys) {
  HashTableStringConf C;
  HashStringTab H(C);


  H.insert("hi there",3);
  H.insert("whooa",7);
  H.insert("whoah",7);
  H.insert("hi there",4);

  HashStringTab::Handle *p = H.lookup("hi there");
  ASSERT_FALSE(p == NULL);
  ASSERT_EQ(p->key(),"hi there");
  ASSERT_EQ(p->value(),3);

  p = H.lookup("hi There");
  ASSERT_TRUE(p == NULL);
};

