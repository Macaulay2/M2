// (c) 1994 Michael E. Stillman

#ifndef _object_intarray_h_
#define _object_intarray_h_

#include "object.hpp"

class object_intarray : public object_element 
{
public:
  intarray val;

  object_intarray() : object_element(0) {}
  object_intarray(const intarray &v) : object_element(0), val(v) {}
  object_intarray(char *&s, int &len) : object_element(0)
    {
      int n = bin_int_in(s,len);
      int *t = val.alloc(n);
      for (int i=0; i<n; i++)
	*t++ = bin_int_in(s,len);
    }
  ~object_intarray() {}

  intarray *   intarray_of     ()       { return &val; }

  class_identifier class_id() const { return CLASS_intarray; }
  type_identifier  type_id () const { return TY_INTARRAY; }
  const char * type_name   () const { return "IntegerArray"; }

  object_intarray *cast_to_intarray()   { return this; }
  
  int length_of() const { return val.length(); }
  object get_index(int i)
    {
      return(make_object_int(val[i]));
    }

  void bin_out(buffer &o) const 
    { 
      bin_int_out(o, val.length());
      for (int i=0; i<val.length(); i++)
	bin_int_out(o, val[i]);
    }

  void text_out(buffer &o) const 
    {
      o << '[';
      for (int i=0; i<val.length()-1; i++)
	o << val[i] << ' ';
      if (val.length() > 0) o << val[val.length()-1];
      o << ']';
    }

  friend void i_stashes();
  static stash *mystash;
  void *operator new(size_t) { return mystash->new_elem(); }
  void operator delete(void *p) { mystash->delete_elem(p); }
};

#endif
