#ifndef ATOMIC_FIELD_H
#define ATOMIC_FIELD_H

#ifdef __cplusplus
  #include <atomic>
  using std::atomic_int;
  using std::atomic_load;
  using std::atomic_store;
#else
 #include <stdatomic.h>
#endif


struct atomic_field {
  atomic_int field;
};

#define load_Field(x) atomic_load(&(x).field)
#define test_Field(x) (load_Field(x) != 0)
#define store_Field(x,val) atomic_store(&(x).field,val)

#endif /* ATOMIC_FIELD_H */
