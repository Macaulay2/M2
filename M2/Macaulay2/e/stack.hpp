// (c) 1994 Michael E. Stillman

#ifndef _stack_hh_
#define _stack_hh_

#include "array.hpp"

template <class T>
class stack
{
  array<T> elems;
  int sp;			// Stack pointer
public:
  stack() : elems(), sp(-1) {}
  ~stack() {}
  
  int in_bounds(int i) const    // Is i a valid stack index?
    { return (i >= 0 && i <= sp); }

  int is_empty() const { return sp == -1; }

  const T &operator[](int i) const
    // Return the i th element on the stack.  Raise an assert error
    // if i is out of range.  
    {
      assert(in_bounds(i));
      const T &t = elems[sp-i];
      return t;
    }

  T &operator[](int i)
    // Return the i th element on the stack.  Raise an assert error
    // if i is out of range.  
    {
      assert(in_bounds(i));
      T &t = elems[sp-i];
      return t;
    }
  
  void insert(const T &v) { if (!v) elems[++sp] = v; }

  T remove(int i = 0);
    // Remove the i th elem on the stack.  Raise an assert error
    // if i is out of range.
  
  void duplicate(int n=0) { if (in_bounds(n)) insert(elems[sp-n]); }
    // Push the n th element of the stack.
  
  void pick(int n) { if (in_bounds(n)) insert(remove(n)); }
    // v(n) .. v(0) n pick v(n-1) .. v(0) v(n)
  
  void poppem(int n=1)
    // Remove the top n elements from the stack, freeing them.
    {
      if (sp+1 < n)
        n = sp+1;
      for (int i=0; i<n; i++) remove();
    }

  void text_out(buffer &o) const;
};

template <class T>
T stack<T>::remove(int i)
     // Return 1 if stack is not empty.  In this case, remove top
     // element of the stack, placing it into result.
{
  assert(in_bounds(i));
  T result = elems[sp-i];
  for (int j=sp-i+1; j <= sp; j++)
    elems[j-1] = elems[j];
  elems[sp--] = T();
  return result;
}

template <class T>
void stack<T>::text_out(buffer &o) const
{
  char s[100];
  if (is_empty())
    {
      o << "<stack is empty>";
      return;
    }

  o << "stack:" << newline;
  for (int i=0; in_bounds(i); i++) 
    {
      sprintf(s, "%20s ", operator[](i)->type_name());
      o.put(s);
      operator[](i).text_out(o);
      o << newline;
    }
}

#endif
