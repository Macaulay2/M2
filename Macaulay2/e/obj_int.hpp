// (c) 1994 Michael E. Stillman

#ifndef _object_int_h_
#define _object_int_h_

#include "object.hpp"
#include "bin_io.hpp"
#include "text_io.hpp"
#include <gmp.h>

class object_int : public object_element 
{
public:
  mpz_t val;
  //int val;
  
  object_int(int v) : object_element(0) { mpz_init_set_si(val, v); }
  object_int(char *&s, int &len) : object_element(0)
    { mpz_init(val); bin_mpz_in(val,s,len); }
  ~object_int() { mpz_clear(val); }

  int          int_of     () const { return mpz_get_si(val); }
  object_types type_of    () const { return TY_INT; }
  const char * type_name  () const { return "int"; }
  object_int * cast_to_int()       { return this; }
  
  void bin_out(buffer &o) const { bin_mpz_out(o,((object_int *)this)->val); }
  void text_out(buffer &o) const {  bignum_text_out(o, ((object_int *)this)->val); }

  friend void i_stashes();
  static stash *mystash;
  void *operator new(size_t) { return mystash->new_elem(); }
  void operator delete(void *p) { mystash->delete_elem(p); }
};

#endif
