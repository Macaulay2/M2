// Copyright 1997 by  Michael E. Stillman

#ifndef _buffer_hpp_
#define _buffer_hpp_

#include "../d/M2mem.h"
#include "newdelete.hpp"
#include "engine-includes.hpp"
#include <string>

const int BUFFER_INITIAL_CAPACITY = 100;

struct indent
{
  int n;
  indent(int n0) : n(n0) {}
};

class buffer : public our_new_delete
{
  int _size;
  int _capacity;
  char *_buf;
  void expand(int newcap);

 public:
  buffer()
      : _size(0),
        _capacity(BUFFER_INITIAL_CAPACITY),
        _buf(newarray_atomic(char, BUFFER_INITIAL_CAPACITY))
  {
  }
  ~buffer() { freemem(_buf); }
  void reset() { _size = 0; }
  int size() { return _size; }
  int capacity() { return _capacity; }
  char *str()
  {
    _buf[_size] = '\0';
    return _buf;
  }
  char *truncate(int newsize)
  {
    if (newsize < _size) _size = newsize;
    _buf[_size] = '\0';
    return _buf;
  }

  M2_string to_string();  // Copies the string, leaves buffer intact

  void put(const char *s);  // Place null-terminated string into buffer
  void put(const char *s,
           long len);           // Place a string possible containing null chars
  void put(char c);             // Place a single character
  void put(int n);              // Format the integer, place into buffer
  void put(int n, int width);   // Format the integer, with given width field.
  void put(long n);             // Format the integer, place into buffer
  void put(double n);             // Format the double, place into buffer
  void put(long n, int width);  // Format the integer, with given width field.
  void put(unsigned int n);     // Format the integer, place into buffer
  void put(unsigned int n,
           int width);        // Format the integer, with given width field.
  void put(unsigned long n);  // Format the integer, place into buffer
  void put(unsigned long long n);  // Format the integer, place into buffer
  void put(unsigned long n,
           int width);  // Format the integer, with given width field.
  void put(std::string s) { put(s.data(), s.size()); }
  // To put an endline in:
  // o.put(newline);

  // To print the resulting string
  // buffer o;
  // o.put("hi there");
  // o.put(5);
  // o.put(newline);
  // cerr << o.str();

  buffer &operator<<(const char *s)
  {
    put(s);
    return *this;
  }
  buffer &operator<<(M2_string s)
  {
    put((char *)s->array, s->len);
    return *this;
  }
  buffer &operator<<(std::string s)
  {
    put(s);
    return *this;
  }
  buffer &operator<<(long n)
  {
    put(n);
    return *this;
  }
  buffer &operator<<(double n)
  {
    put(n);
    return *this;
  }
  buffer &operator<<(unsigned int n)
  {
    put(n);
    return *this;
  }
  buffer &operator<<(unsigned long n)
  {
    put(n);
    return *this;
  }
  buffer &operator<<(unsigned long long n)
  {
    put(n);
    return *this;
  }
  buffer &operator<<(unsigned short n)
  {
    put(static_cast<unsigned int>(n));
    return *this;
  }
  buffer &operator<<(int n)
  {
    put(n);
    return *this;
  }
  buffer &operator<<(char c)
  {
    put(c);
    return *this;
  }
  buffer &operator<<(unsigned char c)
  {
    put(static_cast<char>(c));
    return *this;
  }
  buffer &operator<<(indent s)
  {
    buffer &o = *this;
    const int &n = s.n;
    if (n < 10)
      o << "  ";
    else if (n < 100)
      o << " ";
    o << n;
    return o;
  }
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
