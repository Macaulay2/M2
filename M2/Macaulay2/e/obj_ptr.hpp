#define OBJECT(TYPE,T)							   \
protected:								   \
  T *obj;								   \
public:									   \
  TYPE() : obj(NULL) {}							   \
									   \
  TYPE(const TYPE &v) : obj(v.obj)					   \
    { if (obj != NULL) obj->refcount++; }				   \
									   \
  TYPE(T *v, caster_oil &) : obj(v) { if (obj != NULL) obj->refcount++; } \
									   \
  TYPE &operator=(const TYPE &v)					   \
    {									   \
      if (this == &v) return *this;					   \
      if (obj == v.obj) return *this;					   \
      if (obj != NULL)							   \
	if (--obj->refcount == 0) delete obj;				   \
      obj = v.obj;							   \
      if (obj != NULL) obj->refcount++;					   \
      return *this;							   \
    }									   \
									   \
  ~TYPE() { if (obj != NULL) if (--obj->refcount <= 0) delete obj; }	   \
									   \
  int operator==(const TYPE &b) const { return obj == b.obj; }		   \
  int operator!=(const TYPE &b) const					   \
     {  return obj != b.obj; }						   \
  int operator!() const { return obj != NULL; }				   \
  int is_null() const { return obj == NULL; }				   \
  T *operator->() { assert(obj != NULL); return obj; }			   \
  const T *operator->() const { assert(obj != NULL); return obj; }

#define POINTER(TYPE,T)				\
  OBJECT(TYPE,T)				\
  operator object() { return object(obj); }
