// (c) 1994 Michael E. Stillman

#ifndef _object_string_hh_
#define _object_string_hh_

#include "object.hpp"

class object_string : public object_element 
{
public:
  char *val;
  int len;

  object_string(char *s) : object_element(0)
    {
      len = strlen(s);
      val = new char[len+1];
      engine_alloc(len+1);
      strcpy(val, s);
    }
  object_string(char *& s, int &n) : object_element(0)
    {
      len = bin_int_in(s,n);
      if (len > 0)
	{
	  // cout << "obj string, len = " << len << endl;
	  val = new char[len];
	  engine_alloc(len);
	  memcpy(val,s,len);
	}
      else 
	val = NULL;
      s += len;
      n -= len;
    }
  ~object_string() { engine_dealloc(len); delete val; }

  int          length_of     () const { return len; }
  char *       string_of     () { return val; }
  object_types type_of       () const { return TY_STRING; }
  const char * type_name     () const { return "string"; }
  object_string *cast_to_string() { return this; }

  void bin_out(buffer &o) const 
    { 
      bin_int_out(o,len);
      for (int i=0; i<len; i++)
	o << val[i];
    }
  void text_out(buffer &o) const
    {
      for (int i=0; i<len; i++) o << val[i];
//    o << '\0';
    }

  friend void i_stashes();
  static stash *mystash;
  void *operator new(size_t) { return mystash->new_elem(); }
  void operator delete(void *p) { mystash->delete_elem(p); }
};

#endif
