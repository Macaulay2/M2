// Copyright 1998  Michael E. Stillman

#ifndef _serial_hpp_
#define _serial_hpp_

#include <stream.h>
#include "object.hpp"

const int CLASS_Stub = 1000;

// Each object_element class (that is, a class which inherits from object_element,
// and also has instances) needs to define two routines
//
// void write_object(object_writer &o) const;
// static object_element *read_object(object_reader &i);
//
// Both of these routines should use: o << R, or i >> R,
// As well as the binary I/O of integers: o << 3, i >> 3;

class object_writer
{
  ostream &o;
  array<const object_element *> objs;

  bool find_object(const object_element *obj, int &stub) const;
public:
  object_writer(ostream &o);  // Perhaps other file types or socket types, etc?
  ~object_writer() {}  // The user is still responsible for these objects...

  object_writer &operator<<(const object_element *obj);
  object_writer &operator<<(int a); // Place 'a' onto the output.
  object_writer &operator<<(mpz_ptr a); // Place 'a' onto the output.

  // Each object_element class needs to have the following routine defined:
  // write_object will write its data to 'o', and its objects via 's.write_object(obj)'.
  //
  // void write_object(object_writer &s) const;
};

class object_reader
{
  istream &i;
  array<object_element *> objs;

  object_element *get_object(int stub) { return objs[stub]; }
public:
  object_reader(istream &i) : i(i) {}

  object_reader &operator>>(object_element *& obj);
  object_reader &operator>>(int &a);
  object_reader &operator>>(mpz_ptr &a);

  // Each type needs a static routine:
  //   TYPE *read_object(object_reader &ir);
  // This routine calls 'ir.read_stub(i), and casts it to the correct type, for
  // each object occuring.  It also needs to be able to read from the istream.
  
};

#endif
