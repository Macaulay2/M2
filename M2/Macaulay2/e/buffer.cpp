#include "buffer.hpp"
#include <cstdio>
#include <cstring>
#include <cassert>

void buffer::expand(int newcap)
{
  int n = 2 * _capacity;
  if (newcap > n) n = newcap;
  char *newbuf = newarray_atomic(char, n);
  _capacity = n;
  memcpy(newbuf, _buf, _size);
  freemem(_buf);
  _buf = newbuf;
}

M2_string buffer::to_string() { return M2_tostringn(_buf, _size); }
void buffer::put(char c)
{
  if (_capacity <= _size + 1) expand(1);
  _buf[_size++] = c;
}

void buffer::put(const char *s, long len)
{
  int len0 = static_cast<int>(len);
  if (_capacity <= _size + len0 + 1) expand(_size + len0 + 1);
  memcpy(_buf + _size, s, len0);
  _size += len0;
}

void buffer::put(const char *s) { put(s, strlen(s)); }
void buffer::put(int n)
{
  char s[100];
  sprintf(s, "%d", n);
  put(s, strlen(s));
}

void buffer::put(int n, int width)
{
  char s[100];
  sprintf(s, "%*d", width, n);
  put(s, strlen(s));
}

void buffer::put(long n)
{
  char s[100];
  sprintf(s, "%ld", n);
  put(s, strlen(s));
}

void buffer::put(double n)
{
  char s[100];
  sprintf(s, "%lf", n);
  put(s, strlen(s));
}

void buffer::put(long n, int width)
{
  char s[100];
  sprintf(s, "%*ld", width, n);
  put(s, strlen(s));
}

void buffer::put(unsigned int n)
{
  char s[100];
  sprintf(s, "%u", n);
  put(s, strlen(s));
}

void buffer::put(unsigned long n)
{
  char s[100];
  sprintf(s, "%lu", n);
  put(s, strlen(s));
}

void buffer::put(unsigned long long n)
{
  char s[100];
  sprintf(s, "%llu", n);
  put(s, strlen(s));
}

void buffer::put(unsigned int n, int width)
{
  char s[100];
  sprintf(s, "%*u", width, n);
  put(s, strlen(s));
}

void buffer::put(unsigned long n, int width)
{
  char s[100];
  sprintf(s, "%*lu", width, n);
  put(s, strlen(s));
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
