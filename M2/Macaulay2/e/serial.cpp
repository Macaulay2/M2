// Copyright 1998  Michael E. Stillman

#include "serial.hpp"
#include <gmp.h>
///////////////////
// object_writer //
///////////////////

object_writer::object_writer(ostream &o)
  : o(o)
{
}

bool object_writer::find_object(const object_element *obj, int &stub) const
{
  for (int i=0; i<objs.length(); i++)
    if (objs[i] == obj)
      {
	stub = i;
	return true;
      }
  return false;
}

object_writer &object_writer::operator<<(int a)
{
  // MES: rewrite?
  o << a;
  return *this;
}

object_writer &object_writer::operator<<(mpz_ptr a)
{
  buffer o1;
  // MESXX  o1 << a;
  o << o1.str();
  return *this;
}

object_writer &object_writer::operator<<(const object_element *obj)
{
  // Check to see whether 'obj' has been written out yet.
  int stub;
  if (find_object(obj, stub))
    {
      *this << CLASS_Stub << stub;
    }
  else
    {
      obj->write_object(*this);
      objs.append(obj);
    }
  return *this;
}

///////////////////
// object_reader //
///////////////////
object_reader &object_reader::operator>>(int &a)
{
  i >> a;
  return *this;
}

object_reader &object_reader::operator>>(mpz_ptr &a)
{
  // MESXX: nead bin_int_in in more general setting...
  return *this;
}

object_reader &object_reader::operator>>(object_element *&obj)
{
  int stub;
  i >> stub;
  if (stub == CLASS_Stub)
    {
      i >> stub;
      obj = get_object(stub);
      return *this;
    }

  // This is the biggy: it calls the 'read_object' static constructor for all types.
  // In turn, that routine will call this one on any objects that it needs.

  switch (stub)
    {
    }

  // The above stmt should set obj.
  objs.append(obj);
  return *this;
}
