// (c) 1994  Michael E. Stillman

#ifndef _Index_hh_
#define _Index_hh_

template <class T>
class Index
{
  void *ind;

  const T *collection;

 public:
  Index() : ind(nullptr), collection(nullptr) {}
  Index(void *i, const T *c) : ind(i), collection(c) {}
  Index(const Index<T> &i) = default; 
  Index<T>& operator=(const Index<T> &i) = default;
  Index<T> operator++()
  {
    ind = collection->next(ind);
    return *this;
  }

  Index<T> operator--()
  {
    ind = collection->prev(ind);
    return *this;
  }

  Index<T> operator++(int)
  {
    Index<T> tmp = *this;
    ind = collection->next(ind);
    return tmp;
  }

  Index<T> operator--(int)
  {
    Index<T> tmp = *this;
    ind = collection->prev(ind);
    return tmp;
  }

  int valid() { return collection->valid(ind); }
  void *val() { return ind; }
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
