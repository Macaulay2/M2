// Copyright 2002  Michael E. Stillman
#ifndef _hash_hh_
#define _hash_hh_

class immutable_object {
protected:
  unsigned long _hashval;
public:
  immutable_object(unsigned long hashval) : _hashval(hashval) {}
  unsigned long get_hash_value() const { return _hashval; }
};

class mutable_object {
  static unsigned long next_hash_sequence_number;
  unsigned long _hashval;
public:
  mutable_object() : _hashval(next_hash_sequence_number++) {}
  unsigned long get_hash_value() const { return _hashval; }
};
#endif
