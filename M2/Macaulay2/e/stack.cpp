// (c) 1994 Michael E. Stillman

#include "stack.hpp"

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

#include "object.hpp"
template class stack<object>;
