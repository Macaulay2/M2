// (c) 1994 Michael E. Stillman

#ifndef _object_hh_
#define _object_hh_

#include "style.hpp"
#include "array.hpp"
#include "intarray.hpp"
#include "obj_ptr.hpp"

extern int refcount_check;

extern buffer gError;
extern buffer gOutput;
extern char *gInput;
extern int gInputLen;

class caster_oil
{
public:
  caster_oil() {}
  ~caster_oil() {}
};
extern caster_oil caster;

enum {
  TY_IntegerArray,
  TY_String,
  TY_PrimitiveFunction,
  TY_MonomialOrder,
  TY_Monoid,
  TY_MonomialIdeal,
  TY_TermIdeal,
  TY_Ring,
  TY_RingElement,
  TY_FreeModule,
  TY_Matrix,
  TY_RingMap,

  TY_Vector,
  TY_Monomial,

  TY_Computation
};

//const object_types TY_Integer = TY_RingElement;

enum object_types {
    TY_NONE, TY_STRING, TY_INTARRAY, TY_INTARRAY2, TY_PRIMITIVE,
    TY_RING, TY_FREEMODULE, TY_RING_ELEM, TY_VECTOR, TY_MATRIX,
    TY_RING_MAP, TY_MONIDEAL, TY_TERMIDEAL,
    TY_MON_ORDER,
    TY_MONOID, TY_MONOMIAL,
    TY_COMP, 
    TY_HILB_COMP,
    TY_GB_COMP,
    TY_RES_COMP, 
    TY_RES2_COMP,
    TY_GBRES_COMP
};
const object_types TY_INT = TY_RING_ELEM;

class object_element
{
private:
  friend class object;
public:
  int refcount;			// Does this really have to be public?

  object_element(int ref=1) : refcount(ref) {}
  // Types needing ref count set to 0 instead of 1:
  // object_{int,string,intarray,prim}, all types.
  // This is because new is done directly on these objects, whereas
  // for all other types, they are placed in a "smart" pointer,
  // which increments their ref count.

  virtual ~object_element() {}

  // Type information
  virtual object_types type_of() const = 0;
  virtual const char *type_name() const = 0;

  // Downcasting
  virtual object_int       * cast_to_int()       { return 0; }
  virtual object_string    * cast_to_string()    { return 0; }
  virtual object_intarray  * cast_to_intarray()  { return 0; }
  
  virtual type             * cast_to_type()       { return 0; }
  virtual computation      * cast_to_computation(){ return 0; }

  virtual object_mon_order   * cast_to_mon_order()      { return 0; }
  virtual Monoid      * cast_to_Monoid()      { return 0; }
  virtual TermIdeal   * cast_to_TermIdeal()  { return 0; }
  virtual gb_comp     * cast_to_gb_comp()     { return 0; }
  virtual hilb_comp   * cast_to_hilb_comp()   { return 0; }
  virtual res_comp    * cast_to_res_comp()    { return 0; }
  virtual res2_comp   * cast_to_res2_comp()    { return 0; }
  virtual gbres_comp  * cast_to_gbres_comp()    { return 0; }
  virtual Ring        * cast_to_Ring()       { return 0; }
  virtual FreeModule  * cast_to_FreeModule() { return 0; }

  virtual const object_mon_order   * cast_to_mon_order()      const { return 0; }
  virtual const Monoid      * cast_to_Monoid()      const { return 0; }
  virtual const TermIdeal   * cast_to_TermIdeal()  const { return 0; }
  virtual const gb_comp     * cast_to_gb_comp()     const { return 0; }
  virtual const hilb_comp   * cast_to_hilb_comp()   const { return 0; }
  virtual const res_comp    * cast_to_res_comp()    const { return 0; }
  virtual const res2_comp   * cast_to_res2_comp()   const { return 0; }
  virtual const gbres_comp  * cast_to_gbres_comp()   const { return 0; }
  virtual const Ring        * cast_to_Ring()       const { return 0; }
  virtual const FreeModule  * cast_to_FreeModule() const { return 0; }

  virtual RingElement   cast_to_RingElement();
  virtual Matrix        cast_to_Matrix();
  virtual MonomialIdeal cast_to_MonomialIdeal();
  virtual RingMap       cast_to_RingMap();

  virtual Vector        cast_to_Vector();
  virtual Monomial      cast_to_Monomial();

  // Equality checks, hash function
  virtual bool is_equal(const object_element *o) const;
  virtual int hash() const { return 0; }

  // Display
  virtual void bin_out(buffer &) const { }
  virtual void text_out(buffer &o) const { o << type_name(); }
  virtual void debug_out(buffer &o) const;

  // Getting values.  Shorthands for casting and getting values.
  virtual int int_of() const { return 0; }
  virtual char *string_of() { return NULL; }
  virtual intarray *intarray_of() { return 0; }

  // Informational
  virtual int length_of() const { return 0; }
  virtual object index_of(int i);
};

class object
{
  friend class handles;
protected:
  object_element *obj;
public:
  object() : obj(NULL) {}

  object(const object &v) : obj(v.obj)
    { 
      if (obj != NULL) 
	obj->refcount++; 
    }

  object(object_element *v) : obj(v) 
    { 
      if (obj != NULL) 
	obj->refcount++; 
    }

  object &operator=(const object &v)
    {
      if (this == &v) return *this;
      if (obj == v.obj) return *this;
      if (obj != NULL)
	{
	  if (--obj->refcount == 0) 
	    delete obj;
	}
      obj = v.obj;
      if (obj != NULL) 
	{
	  obj->refcount++;
	}

      return *this;
    }

  ~object() 
    { 
      if (obj != NULL) 
	{
	  if (--obj->refcount <= 0) 
	    delete obj; 
	}
    }

  bool operator==(const object &b) const 
    { return obj == b.obj; }
  bool operator!=(const object &b) const
     {  
        return obj != b.obj; 
      }
  bool operator!() const 
    { return obj != NULL; }
  bool is_null() const 
    { return obj == NULL; }
  object_element *operator->() 
    { assert(obj != NULL); 
      return obj; 
    }
  const object_element *operator->() const 
    { 
      assert(obj != NULL); 
      return obj; 
    }

public:
  void bin_out(buffer &o) const { assert(obj != NULL); obj->bin_out(o); }
  void text_out(buffer &o) const { assert(obj != NULL); obj->text_out(o); }
};

class type : public object_element 
{
public:
  type() : object_element(0) {}
  ~type() {}
};

inline void bump_up(const object_element *p)
{
  object_element *q = (object_element *) p;
  assert(q != NULL);
  q->refcount++;
}
inline void bump_down(const object_element *p)
{
  object_element *q = (object_element *) p;
  assert(q != NULL);
  q->refcount--;
  if (q->refcount == 0) delete q;
}

extern object make_object_int(int n);
extern object make_object_int(mpz_t n);

#endif
