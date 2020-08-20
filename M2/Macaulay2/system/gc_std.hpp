#include <M2/gc-include.h>
#include <gc/gc_allocator.h>
#define gc_map(T,U) std::map<T,U,std::less<T>,gc_allocator<std::pair<const T,U>>>
#define gc_set(T)   std::set<T,std::less<T>,gc_allocator<T>>
